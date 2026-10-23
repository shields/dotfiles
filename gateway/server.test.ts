import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import {
  createServer,
  type ClientRequest,
  type IncomingHttpHeaders,
  type RequestListener,
} from "node:http";
import https from "node:https";
import type { Socket } from "node:net";
import {
  mkdtempSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test, { mock, type TestContext } from "node:test";
import { setImmediate as nextTurn } from "node:timers/promises";
import { gzipSync } from "node:zlib";
import { AppAuth } from "./auth.ts";
import { Gateway } from "./server.ts";
import { State, type Instance } from "./state.ts";

const idRef = (id: string): string => `refs/heads/task/${id}/one`;
const pages = (repository: string): string =>
  `<https://api.github.com/${repository}/pulls?page=2>; rel="next", <https://api.github.com/${repository}/pulls?page=3>; rel="last"`;

function packet(line: string): Buffer {
  const body = Buffer.from(line);
  return Buffer.concat([
    Buffer.from((body.length + 4).toString(16).padStart(4, "0")),
    body,
  ]);
}

function send(
  socket: string,
  method: string,
  target: string,
  body: Buffer | Buffer[] = Buffer.alloc(0),
  host = "github.com",
  headers: Record<string, string> = {},
  agent: https.Agent | false = false,
): Promise<{
  status: number;
  type: string | undefined;
  body: string;
  headers: IncomingHttpHeaders;
}> {
  return new Promise((resolve, reject) => {
    const request = https.request(
      {
        socketPath: socket,
        servername: host,
        rejectUnauthorized: false,
        method,
        path: target,
        agent,
        headers: {
          host,
          ...(Array.isArray(body) ? {} : { "content-length": body.length }),
          authorization: "Bearer guest-token",
          cookie: "session=guest",
          ...headers,
        },
      },
      (response) => {
        const chunks: Buffer[] = [];
        response.on("data", (chunk: Buffer) => {
          chunks.push(chunk);
        });
        response.on("end", () => {
          resolve({
            status: response.statusCode ?? 0,
            type: response.headers["content-type"],
            body: Buffer.concat(chunks).toString(),
            headers: response.headers,
          });
        });
      },
    );
    request.once("error", reject);
    if (Array.isArray(body)) {
      const [first = Buffer.alloc(0), ...rest] = body;
      request.write(first);
      // The pause makes the gateway read the first chunk on its own.
      setTimeout(() => {
        for (const chunk of rest) {
          request.write(chunk);
        }
        request.end();
      }, 20);
    } else {
      request.end(body);
    }
  });
}

interface Forwarded {
  target: string;
  authorization: string | undefined;
  cookie: string | undefined;
  contentEncoding: string | undefined;
  transferEncoding: string | undefined;
  body: Buffer;
}

async function startGateway(
  upstreamOptions: { handler: RequestListener; tls: boolean } | null = null,
): Promise<{
  socket: string;
  instance: Instance;
  state: State;
  auth: AppAuth;
  gateway: Gateway;
  incoming: Forwarded[];
  listener: https.Server;
  closed: () => Promise<void>;
  stop: () => Promise<void>;
}> {
  const root = mkdtempSync(path.join(tmpdir(), "vm-access-server-"));
  const certificate = path.join(root, "cert.pem");
  const key = path.join(root, "key.pem");
  const generated = spawnSync(
    "openssl",
    [
      "req",
      "-x509",
      "-newkey",
      "rsa:2048",
      "-nodes",
      "-keyout",
      key,
      "-out",
      certificate,
      "-days",
      "1",
      "-subj",
      "/CN=github.com",
      "-addext",
      "subjectAltName=DNS:github.com",
    ],
    { encoding: "utf8" },
  );
  assert.equal(generated.status, 0, generated.stderr);
  const state = new State(path.join(root, "state"));
  const secretDirectory = path.join(root, "secrets");
  mkdirSync(secretDirectory);
  const auth = new AppAuth(secretDirectory);
  const incoming: Forwarded[] = [];
  const record: RequestListener = (request, response) => {
    const chunks: Buffer[] = [];
    request.on("data", (chunk: Buffer) => {
      chunks.push(chunk);
    });
    request.on("end", () => {
      incoming.push({
        target: request.url ?? "",
        authorization: request.headers.authorization,
        cookie: request.headers.cookie,
        contentEncoding: request.headers["content-encoding"],
        transferEncoding: request.headers["transfer-encoding"],
        body: Buffer.concat(chunks),
      });
      response
        .writeHead(request.url?.includes("/shields/private") ? 404 : 200, {
          link: pages("repositories/1296269"),
        })
        .end("ok");
    });
  };
  const serve = upstreamOptions?.handler ?? record;
  const upstream = upstreamOptions?.tls
    ? https.createServer(
        { cert: readFileSync(certificate), key: readFileSync(key) },
        serve,
      )
    : createServer(serve);
  const agentOptions = https.globalAgent.options;
  const trusted = agentOptions.ca;
  if (upstreamOptions?.tls) {
    agentOptions.ca = readFileSync(certificate);
  }
  await new Promise<void>((resolve) => {
    upstream.listen(0, "127.0.0.1", resolve);
  });
  const address = upstream.address();
  assert.ok(address && typeof address !== "string");
  const socket = path.join(root, "work.sock");
  const instance = state.register("lima:work", socket, "tester");
  state.update(
    "lima:work",
    0,
    { "shields/dotfiles": { read: true, pushTaskBranches: true } },
    "tester",
  );
  writeFileSync(
    path.join(
      secretDirectory,
      `${Buffer.from("shields/dotfiles").toString("base64url")}.json`,
    ),
    JSON.stringify({
      client_id: "Iv-test",
      repository: "shields/dotfiles",
      access_token: "ghu_secret",
      access_expires_at: Math.floor(Date.now() / 1000) + 3600,
      refresh_token: "ghr_secret",
      refresh_expires_at: Math.floor(Date.now() / 1000) + 86_400,
      web_url: "https://github.com",
      api_url: "https://api.github.com",
    }),
  );
  const origin = `${upstreamOptions?.tls ? "https" : "http"}://127.0.0.1:${String(address.port)}`;
  const gateway = new Gateway(state, auth, {
    cert: certificate,
    key,
    adminSocket: path.join(root, "admin.sock"),
    webUpstream: origin,
    apiUpstream: origin,
  });
  const created = mock.method(https, "createServer");
  const stop = async (): Promise<void> => {
    created.mock.restore();
    await gateway.stop();
    upstream.closeAllConnections();
    agentOptions.ca = trusted;
    await new Promise<void>((resolve) => {
      upstream.close(() => {
        resolve();
      });
    });
    state.close();
    rmSync(root, { recursive: true, force: true });
  };
  try {
    await gateway.start();
    const listener = created.mock.calls[0]?.result;
    assert.ok(listener);
    const responses: Promise<void>[] = [];
    listener.on("request", (_request, response) => {
      responses.push(
        new Promise((resolve) => {
          response.once("close", resolve);
        }),
      );
    });
    const closed = async (): Promise<void> => {
      await Promise.all(responses);
    };
    return {
      socket,
      instance,
      state,
      auth,
      gateway,
      incoming,
      listener,
      closed,
      stop,
    };
  } catch (error) {
    await stop();
    throw error;
  }
}

void test("the HTTPS socket injects a repo token only after checking the full push and live grant", async () => {
  const { socket, instance, state, incoming, listener, stop } =
    await startGateway();
  try {
    assert.equal(listener.requestTimeout, 3_600_000);
    assert.equal(listener.headersTimeout, 60_000);
    assert.equal(
      (
        await send(
          socket,
          "GET",
          "/__vm_access__/health",
          Buffer.alloc(0),
          "api.github.com",
        )
      ).status,
      200,
    );
    assert.equal(incoming.length, 0);
    const fetched = await send(
      socket,
      "GET",
      "/shields/dotfiles.git/info/refs?service=git-upload-pack",
    );
    assert.equal(fetched.status, 200);
    assert.equal(
      incoming[0]?.authorization,
      `Basic ${Buffer.from("x-access-token:ghu_secret").toString("base64")}`,
    );
    const good = Buffer.concat([
      packet(
        `${"0".repeat(40)} ${"1".repeat(40)} ${idRef(instance.id)}\0report-status\n`,
      ),
      Buffer.from("0000PACK"),
    ]);
    assert.equal(
      (
        await send(
          socket,
          "POST",
          "/shields/dotfiles.git/git-receive-pack",
          good,
        )
      ).status,
      200,
    );
    assert.deepEqual(incoming[1]?.body, good);
    const identity = await send(
      socket,
      "GET",
      "/user",
      Buffer.alloc(0),
      "api.github.com",
    );
    assert.equal(identity.status, 200);
    assert.equal(identity.headers.link, pages("repositories/1296269"));
    assert.equal(incoming[2]?.authorization, "Bearer ghu_secret");
    const discovery = Buffer.from(
      JSON.stringify({
        query:
          '{ viewer { login } repo_000: repository(owner: "shields", name: "dotfiles") { name } }',
        variables: null,
      }),
    );
    assert.equal(
      (await send(socket, "POST", "/graphql", discovery, "api.github.com"))
        .status,
      200,
    );
    assert.equal(incoming[3]?.authorization, "Bearer ghu_secret");
    assert.deepEqual(incoming[3].body, discovery);
    const bad = Buffer.concat([
      packet(
        `${"0".repeat(40)} ${"1".repeat(40)} ${idRef(instance.id)}\0report-status\n`,
      ),
      packet(`${"0".repeat(40)} ${"1".repeat(40)} refs/heads/main\n`),
      Buffer.from("0000PACK"),
      Buffer.alloc(4_194_304),
    ]);
    const reported = await send(
      socket,
      "POST",
      "/shields/dotfiles.git/git-receive-pack",
      bad,
    );
    assert.deepEqual(
      { status: reported.status, type: reported.type, body: reported.body },
      {
        status: 200,
        type: "application/x-git-receive-pack-result",
        body: Buffer.concat([
          packet("unpack ok\n"),
          packet(
            `ng ${idRef(instance.id)} another ref update in this push is denied\n`,
          ),
          packet(
            `ng refs/heads/main push destination is outside this VM's task namespace; allowed namespace: task/${instance.id}/*\n`,
          ),
          Buffer.from("0000"),
        ]).toString(),
      },
    );
    const unreported = await send(
      socket,
      "POST",
      "/shields/dotfiles.git/git-receive-pack",
      Buffer.concat([
        packet(`${"0".repeat(40)} ${"1".repeat(40)} refs/heads/main\0quiet`),
        Buffer.from("0000PACK"),
      ]),
    );
    assert.equal(unreported.status, 403);
    assert.equal(unreported.type, "text/plain; charset=utf-8");
    assert.equal(
      unreported.body.split("\n", 1)[0],
      `push destination is outside this VM's task namespace; allowed namespace: task/${instance.id}/*`,
    );
    assert.match(unreported.body, /^[^\n]+\nrequest ID: [0-9a-f-]{36}\n$/);
    assert.equal(incoming.length, 4);
    for (const log of state.logs(instance.id).slice(0, 2)) {
      assert.equal(log.allowed, false);
      assert.equal(
        log.reason,
        `push destination is outside this VM's task namespace; allowed namespace: task/${instance.id}/*`,
      );
      assert.equal(log.outcome, "denied");
    }
    const probe = Buffer.from("0000");
    assert.equal(
      (
        await send(
          socket,
          "POST",
          "/shields/dotfiles.git/git-receive-pack",
          probe,
        )
      ).status,
      200,
    );
    assert.equal(
      incoming[4]?.authorization,
      `Basic ${Buffer.from("x-access-token:ghu_secret").toString("base64")}`,
    );
    assert.deepEqual(incoming[4].body, probe);
    const large = Buffer.concat([good, Buffer.alloc(2 * 1_048_576, 1)]);
    assert.equal(
      (
        await send(socket, "POST", "/shields/dotfiles.git/git-receive-pack", [
          large.subarray(0, 20),
          large.subarray(20),
        ])
      ).status,
      200,
    );
    assert.equal(incoming[5]?.transferEncoding, "chunked");
    assert.equal(incoming[5].authorization, incoming[4].authorization);
    assert.equal(incoming[5].body.length, large.length);
    assert.ok(incoming[5].body.equals(large));
    const shallow = Buffer.concat([
      packet(`shallow ${"2".repeat(40)}\n`),
      good,
    ]);
    assert.equal(
      (
        await send(socket, "POST", "/shields/dotfiles.git/git-receive-pack", [
          shallow.subarray(0, 20),
          shallow.subarray(20),
        ])
      ).status,
      200,
    );
    assert.equal(incoming[6]?.authorization, incoming[4].authorization);
    assert.deepEqual(incoming[6].body, shallow);
    assert.equal(
      (
        await send(
          socket,
          "POST",
          "/shields/dotfiles.git/git-receive-pack",
          Buffer.from("0000PACK"),
        )
      ).status,
      403,
    );
    assert.equal(
      (
        await send(socket, "POST", "/shields/dotfiles.git/git-receive-pack", [
          probe,
          good,
        ])
      ).status,
      403,
    );
    assert.equal(incoming.length, 7);
    const listed = await send(
      socket,
      "GET",
      "/repos/Shields/Dotfiles/pulls",
      Buffer.alloc(0),
      "api.github.com",
    );
    assert.equal(listed.headers.link, pages("repos/shields/dotfiles"));
    const next = /<https:\/\/api\.github\.com([^>]+)>; rel="next"/.exec(
      listed.headers.link,
    )?.[1];
    assert.equal(next, "/repos/shields/dotfiles/pulls?page=2");
    assert.equal(
      (await send(socket, "GET", next, Buffer.alloc(0), "api.github.com"))
        .status,
      200,
    );
    assert.equal(incoming[8]?.target, next);
    assert.equal(incoming[8].authorization, "Bearer ghu_secret");
    state.update("lima:work", 1, {}, "tester");
    assert.equal(
      (await send(socket, "POST", "/graphql", discovery, "api.github.com"))
        .status,
      403,
    );
    assert.equal(
      (
        await send(
          socket,
          "GET",
          "/shields/dotfiles.git/info/refs?service=git-upload-pack",
        )
      ).status,
      200,
    );
    assert.equal(incoming.length, 10);
    assert.equal(incoming[9]?.authorization, undefined);
    const logs = state.logs(instance.id);
    assert.equal(logs.length, 16);
    assert.equal(logs[0]?.revision, 2);
    assert.equal(logs[0].allowed, true);
    assert.equal(logs[0].reason, "anonymous read");

    for (const repository of [
      "shields/dotfiles",
      "octocat/Hello-World",
      "shields/private",
    ]) {
      for (const [host, method, target, body] of [
        [
          "github.com",
          "GET",
          `/${repository}.git/info/refs?service=git-upload-pack`,
          Buffer.alloc(0),
        ],
        [
          "github.com",
          "POST",
          `/${repository}.git/git-upload-pack`,
          Buffer.from("0000"),
        ],
        [
          "github.com",
          "GET",
          `/${repository}/info/refs?service=git-upload-pack`,
          Buffer.alloc(0),
        ],
        [
          "github.com",
          "POST",
          `/${repository}/git-upload-pack`,
          Buffer.from("0000"),
        ],
        [
          "api.github.com",
          "GET",
          `/repos/${repository}/pulls`,
          Buffer.alloc(0),
        ],
        ["api.github.com", "HEAD", `/repos/${repository}`, Buffer.alloc(0)],
        ["github.com", "GET", `/${repository}`, Buffer.alloc(0)],
        [
          "github.com",
          "HEAD",
          `/${repository}/archive/refs/heads/main.zip`,
          Buffer.alloc(0),
        ],
      ] as const) {
        const response = await send(socket, method, target, body, host);
        assert.equal(
          response.status,
          repository === "shields/private" ? 404 : 200,
          target,
        );
        assert.equal(incoming.at(-1)?.authorization, undefined, target);
        assert.equal(incoming.at(-1)?.cookie, undefined, target);
        assert.deepEqual(incoming.at(-1)?.body, body, target);
        assert.equal(
          response.headers.link,
          pages(
            host === "api.github.com"
              ? `repos/${repository.toLowerCase()}`
              : "repositories/1296269",
          ),
          target,
        );
      }
    }
    const forwarded = incoming.length;
    for (const [host, method, target, body] of [
      [
        "github.com",
        "GET",
        "/shields/dotfiles.git/info/refs?service=git-receive-pack",
        Buffer.alloc(0),
      ],
      ["github.com", "POST", "/shields/dotfiles.git/git-receive-pack", good],
      ["github.com", "POST", "/shields/dotfiles.git/git-receive-pack", probe],
      [
        "api.github.com",
        "POST",
        "/repos/shields/dotfiles/issues",
        Buffer.from("{}"),
      ],
      ["api.github.com", "POST", "/graphql", discovery],
      ["api.github.com", "GET", "/user", Buffer.alloc(0)],
    ] as const) {
      const denied = await send(socket, method, target, body, host);
      assert.equal(denied.status, 403, target);
      if (host === "github.com") {
        assert.equal(denied.type, "text/plain; charset=utf-8", target);
        assert.match(denied.body, /^repository is not granted to this VM\n/);
      } else {
        assert.equal(denied.type, "application/json", target);
        assert.equal(
          (JSON.parse(denied.body) as { message: unknown }).message,
          target === "/repos/shields/dotfiles/issues"
            ? "unsupported GitHub API operation"
            : "repository is not granted to this VM",
          target,
        );
      }
    }
    state.disable("lima:work", "tester");
    assert.equal((await send(socket, "GET", "/shields/dotfiles")).status, 403);
    assert.equal(incoming.length, forwarded);
  } finally {
    await stop();
  }
});

void test("Content-Encoding reaches GitHub only on fetch bodies, and inspected bodies must be unencoded", async () => {
  const { socket, instance, incoming, stop } = await startGateway();
  try {
    const negotiation = gzipSync(
      Buffer.concat([packet(`want ${"1".repeat(40)}\n`), Buffer.from("0000")]),
    );
    assert.equal(
      (
        await send(
          socket,
          "POST",
          "/shields/dotfiles.git/git-upload-pack",
          negotiation,
          "github.com",
          { "content-encoding": "gzip" },
        )
      ).status,
      200,
    );
    assert.equal(incoming.at(-1)?.contentEncoding, "gzip");
    assert.deepEqual(incoming.at(-1)?.body, negotiation);
    assert.equal(
      (
        await send(
          socket,
          "GET",
          "/shields/dotfiles.git/info/refs?service=git-upload-pack",
          Buffer.alloc(0),
          "github.com",
          { "content-encoding": "gzip" },
        )
      ).status,
      200,
    );
    assert.equal(incoming.at(-1)?.contentEncoding, undefined);

    const push = Buffer.concat([
      packet(
        `${"0".repeat(40)} ${"1".repeat(40)} ${idRef(instance.id)}\0report-status\n`,
      ),
      Buffer.from("0000PACK"),
    ]);
    const query = Buffer.from(
      JSON.stringify({
        query: '{ repository(owner: "shields", name: "dotfiles") { name } }',
      }),
    );
    const forwarded = incoming.length;
    for (const [host, target, body, encoding] of [
      ["github.com", "/shields/dotfiles.git/git-receive-pack", push, "gzip"],
      [
        "github.com",
        "/shields/dotfiles.git/git-receive-pack",
        gzipSync(push),
        "gzip",
      ],
      ["github.com", "/shields/dotfiles.git/git-receive-pack", push, ""],
      ["api.github.com", "/graphql", query, "br"],
      ["api.github.com", "/graphql", query, "identity, gzip"],
    ] as const) {
      const response = await send(socket, "POST", target, body, host, {
        "content-encoding": encoding,
      });
      assert.equal(response.status, 403, `${target} ${encoding}`);
      assert.match(
        response.body,
        host === "github.com"
          ? /^encoded request bodies are unsupported\n/
          : /"message":"encoded request bodies are unsupported"/,
      );
    }
    assert.equal(incoming.length, forwarded);

    for (const [host, target, body, encoding] of [
      [
        "github.com",
        "/shields/dotfiles.git/git-receive-pack",
        push,
        "identity",
      ],
      ["api.github.com", "/graphql", query, "Identity"],
    ] as const) {
      assert.equal(
        (
          await send(socket, "POST", target, body, host, {
            "content-encoding": encoding,
          })
        ).status,
        200,
        target,
      );
      assert.equal(incoming.at(-1)?.contentEncoding, undefined, target);
      assert.deepEqual(incoming.at(-1)?.body, body, target);
    }
  } finally {
    await stop();
  }
});

function signal(): { promise: Promise<undefined>; resolve: () => void } {
  const { promise, resolve } = Promise.withResolvers<undefined>();
  return {
    promise,
    resolve: () => {
      resolve(undefined);
    },
  };
}

async function within<T>(what: string, pending: Promise<T>): Promise<T> {
  let timer: NodeJS.Timeout | undefined;
  const deadline = new Promise<never>((_resolve, reject) => {
    timer = setTimeout(() => {
      reject(new Error(`${what} did not happen within 5 s`));
    }, 5000);
  });
  try {
    return await Promise.race([pending, deadline]);
  } finally {
    clearTimeout(timer);
  }
}

const requestShapes: Record<
  "get" | "post" | "upload" | "push" | "deniedPush",
  { method: string; path: string; body?: string; ends: boolean }
> = {
  get: {
    method: "GET",
    path: "/shields/dotfiles.git/info/refs?service=git-upload-pack",
    ends: true,
  },
  post: {
    method: "POST",
    path: "/shields/dotfiles.git/git-upload-pack",
    body: "0000",
    ends: true,
  },
  upload: {
    method: "POST",
    path: "/shields/dotfiles.git/git-upload-pack",
    body: "0000",
    ends: false,
  },
  push: {
    method: "POST",
    path: "/shields/dotfiles.git/git-receive-pack",
    body: "00",
    ends: false,
  },
  deniedPush: {
    method: "POST",
    path: "/shields/dotfiles.git/git-receive-pack",
    body: `${packet(`${"0".repeat(40)} ${"1".repeat(40)} refs/heads/main\0report-status\n`).toString()}0000`,
    ends: false,
  },
};

function open(
  socket: string,
  mode: keyof typeof requestShapes = "get",
): {
  request: ClientRequest;
  firstData: Promise<void>;
  response: Promise<{ status: number; body: string; complete: boolean }>;
} {
  const { method, path, body, ends } = requestShapes[mode];
  const request = https.request({
    socketPath: socket,
    servername: "github.com",
    rejectUnauthorized: false,
    method,
    path,
    agent: false,
    headers: { host: "github.com" },
  });
  const firstData = signal();
  const response = new Promise<{
    status: number;
    body: string;
    complete: boolean;
  }>((resolve, reject) => {
    request.once("error", reject);
    request.once("response", (incoming) => {
      const chunks: Buffer[] = [];
      incoming.on("data", (chunk: Buffer) => {
        chunks.push(chunk);
        firstData.resolve();
      });
      incoming.once("close", () => {
        resolve({
          status: incoming.statusCode ?? 0,
          body: Buffer.concat(chunks).toString(),
          complete: incoming.complete,
        });
      });
    });
  });
  if (ends) {
    request.end(body);
  } else {
    request.write(body ?? "");
  }
  return { request, firstData: firstData.promise, response };
}

function recordFinishes(
  t: TestContext,
  state: State,
): {
  calls: { status: number | null; outcome: string }[];
  first: Promise<void>;
} {
  const calls: { status: number | null; outcome: string }[] = [];
  const first = signal();
  const finish = state.finish.bind(state);
  t.mock.method(
    state,
    "finish",
    (
      id: string,
      status: number | null,
      durationMs: number,
      outcome: string,
    ): void => {
      finish(id, status, durationMs, outcome);
      calls.push({ status, outcome });
      first.resolve();
    },
  );
  return { calls, first: first.promise };
}

function auditRows(
  state: State,
  instance: Instance,
): { outcome: string; upstreamStatus: number | null }[] {
  return state
    .logs(instance.id)
    .map(({ outcome, upstreamStatus }) => ({ outcome, upstreamStatus }));
}

const endsEarly: {
  name: string;
  tls: boolean;
  begin: RequestListener;
  end: (socket: Socket) => void;
}[] = [
  {
    name: "FIN after a chunk, without the final chunk",
    tls: false,
    begin: (_request, response) => {
      response.writeHead(200);
      response.write("partial");
    },
    end: (socket) => {
      socket.end();
    },
  },
  {
    name: "FIN in the middle of a chunk",
    tls: false,
    begin: (request, response) => {
      response.writeHead(200, { "transfer-encoding": "chunked" });
      response.flushHeaders();
      request.socket.write("64\r\npart");
    },
    end: (socket) => {
      socket.end();
    },
  },
  {
    name: "FIN before the end of a Content-Length body",
    tls: false,
    begin: (_request, response) => {
      response.writeHead(200, { "content-length": 100_000 });
      response.write("partial");
    },
    end: (socket) => {
      socket.end();
    },
  },
  {
    name: "TLS close_notify without the final chunk",
    tls: true,
    begin: (_request, response) => {
      response.writeHead(200);
      response.write("partial");
    },
    end: (socket) => {
      socket.end();
    },
  },
  {
    name: "TLS EOF without close_notify",
    tls: true,
    begin: (_request, response) => {
      response.writeHead(200);
      response.write("partial");
    },
    end: (socket) => {
      socket.destroy();
    },
  },
  {
    name: "TCP reset",
    tls: false,
    begin: (_request, response) => {
      response.writeHead(200, { "content-length": 100_000 });
      response.write("partial");
    },
    end: (socket) => {
      socket.resetAndDestroy();
    },
  },
];

for (const { name, tls, begin, end } of endsEarly) {
  void test(`an upstream that ends its body early (${name}) fails the client's response and is audited as a gateway error`, async (t) => {
    const clientHasData = signal();
    const { socket, instance, state, closed, stop } = await startGateway({
      handler: (request, response) => {
        begin(request, response);
        void clientHasData.promise.then(() => {
          end(request.socket);
        });
      },
      tls,
    });
    const audit = recordFinishes(t, state);
    const { request, firstData, response } = open(socket);
    void firstData.then(clientHasData.resolve);
    let rows;
    let fetched;
    try {
      fetched = await within("the client's response to close", response);
      await within("the audit row", audit.first);
      await within("the gateway's response to close", closed());
      rows = auditRows(state, instance);
    } finally {
      request.destroy();
      await stop();
    }
    assert.equal(fetched.status, 200);
    assert.equal(fetched.complete, false);
    assert.deepEqual(rows, [
      { outcome: "gateway-error", upstreamStatus: null },
    ]);
    assert.deepEqual(audit.calls, [{ status: null, outcome: "gateway-error" }]);
  });
}

for (const { name, respond } of [
  {
    name: "Content-Length body",
    respond: ((_request, response) => {
      response.writeHead(200, { "content-length": 7 }).end("partial");
    }) satisfies RequestListener,
  },
  {
    name: "chunked body",
    respond: ((_request, response) => {
      response.writeHead(200);
      response.write("par");
      response.end("tial");
    }) satisfies RequestListener,
  },
  {
    name: "body that ends when the upstream closes the connection",
    respond: ((request) => {
      request.socket.end("HTTP/1.1 200 OK\r\nconnection: close\r\n\r\npartial");
    }) satisfies RequestListener,
  },
]) {
  void test(`a complete response (${name}) is audited once, as completed`, async (t) => {
    const { socket, instance, state, closed, stop } = await startGateway({
      handler: respond,
      tls: false,
    });
    const audit = recordFinishes(t, state);
    const { request, response } = open(socket);
    let rows;
    let fetched;
    try {
      fetched = await within("the client's response to close", response);
      await within("the audit row", audit.first);
      await within("the gateway's response to close", closed());
      rows = auditRows(state, instance);
    } finally {
      request.destroy();
      await stop();
    }
    assert.deepEqual(fetched, { status: 200, body: "partial", complete: true });
    assert.deepEqual(rows, [{ outcome: "completed", upstreamStatus: 200 }]);
    assert.deepEqual(audit.calls, [{ status: 200, outcome: "completed" }]);
  });
}

for (const [name, respond] of [
  [
    "hangs up",
    (request) => {
      request.socket.destroy();
    },
  ],
  [
    "resets the connection",
    (request) => {
      request.socket.resetAndDestroy();
    },
  ],
] as [string, RequestListener][]) {
  void test(`an upstream that ${name} before sending headers is a gateway error with a 503`, async (t) => {
    const { socket, instance, state, closed, stop } = await startGateway({
      handler: respond,
      tls: false,
    });
    const audit = recordFinishes(t, state);
    const { request, response } = open(socket);
    let rows;
    let fetched;
    try {
      fetched = await within("the client's response to close", response);
      await within("the audit row", audit.first);
      await within("the gateway's response to close", closed());
      rows = auditRows(state, instance);
    } finally {
      request.destroy();
      await stop();
    }
    assert.equal(fetched.status, 503);
    assert.equal(fetched.complete, true);
    assert.deepEqual(rows, [
      { outcome: "gateway-error", upstreamStatus: null },
    ]);
    assert.deepEqual(audit.calls, [{ status: null, outcome: "gateway-error" }]);
  });
}

void test("a client that leaves mid-body is audited as interrupted and the upstream request is aborted", async (t) => {
  const upstreamClosed = signal();
  const { socket, instance, state, closed, stop } = await startGateway({
    handler: (request, response) => {
      request.socket.once("close", upstreamClosed.resolve);
      response.writeHead(200, { "content-length": 100_000 });
      response.write("partial");
    },
    tls: false,
  });
  const audit = recordFinishes(t, state);
  const { request, firstData } = open(socket);
  let rows;
  try {
    await within("the first byte of the body", firstData);
    request.destroy();
    await within("the audit row", audit.first);
    await within("the upstream connection to close", upstreamClosed.promise);
    await within("the gateway's response to close", closed());
    rows = auditRows(state, instance);
  } finally {
    request.destroy();
    await stop();
  }
  assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: 200 }]);
  assert.deepEqual(audit.calls, [{ status: 200, outcome: "interrupted" }]);
});

void test("a client that leaves before the upstream answers is audited as interrupted and the upstream request is aborted", async (t) => {
  const asked = signal();
  const upstreamClosed = signal();
  const { socket, instance, state, closed, stop } = await startGateway({
    handler: (request) => {
      asked.resolve();
      request.socket.once("close", upstreamClosed.resolve);
    },
    tls: false,
  });
  const audit = recordFinishes(t, state);
  const { request, response } = open(socket);
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  let rows;
  try {
    await within("the upstream to receive the request", asked.promise);
    request.destroy();
    await within("the client's request to fail", hungUp);
    await within("the audit row", audit.first);
    await within("the upstream connection to close", upstreamClosed.promise);
    await within("the gateway's response to close", closed());
    rows = auditRows(state, instance);
  } finally {
    request.destroy();
    await stop();
  }
  assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: null }]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
});

for (const [name, mode] of [
  ["a GET", "get"],
  ["a POST that arrived whole", "post"],
] as const) {
  void test(`a client that leaves while its token is being fetched (${name}) is audited as interrupted and never reaches the upstream`, async (t) => {
    const { socket, instance, state, auth, incoming, closed, stop } =
      await startGateway();
    const audit = recordFinishes(t, state);
    const asked = signal();
    const resume = signal();
    t.mock.method(auth, "token", async (): Promise<string> => {
      asked.resolve();
      await resume.promise;
      return "ghu_secret";
    });
    const { request, response } = open(socket, mode);
    const hungUp = assert.rejects(response, { message: "socket hang up" });
    let rows;
    try {
      await within("the gateway to ask for a token", asked.promise);
      request.destroy();
      await within("the client's request to fail", hungUp);
      await within("the gateway's response to close", closed());
      resume.resolve();
      await within("the audit row", audit.first);
      rows = auditRows(state, instance);
    } finally {
      request.destroy();
      resume.resolve();
      await stop();
    }
    assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: null }]);
    assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
    assert.deepEqual(incoming, []);
  });
}

for (const [name, mode] of [
  ["a GET", "get"],
  ["a POST that arrived whole", "post"],
] as const) {
  void test(`a client whose socket is destroyed as its token arrives (${name}) is not forwarded over a warm upstream connection`, async (t) => {
    const { socket, instance, state, auth, listener, incoming, closed, stop } =
      await startGateway();
    const target = "/shields/dotfiles.git/info/refs?service=git-upload-pack";
    let rows;
    let forwarded;
    let calls;
    try {
      const warm = await send(socket, "GET", target);
      assert.equal(warm.status, 200);
      const audit = recordFinishes(t, state);
      let serverSide: Socket | undefined;
      listener.prependOnceListener("request", (request) => {
        serverSide = request.socket;
      });
      t.mock.method(auth, "token", (): Promise<string> => {
        serverSide?.destroy();
        return Promise.resolve("ghu_secret");
      });
      const { request, response } = open(socket, mode);
      const hungUp = assert.rejects(response, { message: "socket hang up" });
      await within("the client's request to fail", hungUp);
      await within("the audit row", audit.first);
      await within("the gateway's response to close", closed());
      request.destroy();
      const sentinel = await send(socket, "GET", target);
      assert.equal(sentinel.status, 200);
      rows = auditRows(state, instance).filter(
        ({ outcome }) => outcome === "interrupted",
      );
      forwarded = incoming.map(({ target: sent }) => sent);
      calls = audit.calls;
    } finally {
      await stop();
    }
    assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: null }]);
    assert.deepEqual(calls, [
      { status: null, outcome: "interrupted" },
      { status: 200, outcome: "completed" },
    ]);
    assert.deepEqual(forwarded, [target, target]);
  });
}

void test("a client that leaves while uploading is audited as interrupted and the upstream request is aborted", async (t) => {
  const asked = signal();
  const upstreamClosed = signal();
  const { socket, instance, state, closed, stop } = await startGateway({
    handler: (request) => {
      request.resume();
      asked.resolve();
      request.socket.once("close", upstreamClosed.resolve);
    },
    tls: false,
  });
  const audit = recordFinishes(t, state);
  const { request, response } = open(socket, "upload");
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  let rows;
  try {
    await within("the upstream to receive the request", asked.promise);
    request.destroy();
    await within("the client's request to fail", hungUp);
    await within("the audit row", audit.first);
    await within("the upstream connection to close", upstreamClosed.promise);
    await within("the gateway's response to close", closed());
    rows = auditRows(state, instance);
  } finally {
    request.destroy();
    await stop();
  }
  assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: null }]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
});

void test("a client that leaves while its push commands are being read is audited as interrupted, not denied", async (t) => {
  const { socket, instance, state, listener, incoming, closed, stop } =
    await startGateway();
  const audit = recordFinishes(t, state);
  const requested = signal();
  listener.on("request", requested.resolve);
  const { request, response } = open(socket, "push");
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  let logs;
  try {
    await within("the gateway to receive the request", requested.promise);
    request.destroy();
    await within("the client's request to fail", hungUp);
    await within("the audit row", audit.first);
    await within("the gateway's response to close", closed());
    logs = state.logs(instance.id);
  } finally {
    request.destroy();
    await stop();
  }
  assert.deepEqual(
    logs.map(({ allowed, operation, reason, outcome }) => ({
      allowed,
      operation,
      reason,
      outcome,
    })),
    [
      {
        allowed: false,
        operation: "unknown",
        reason: "request aborted",
        outcome: "interrupted",
      },
    ],
  );
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
  assert.deepEqual(incoming, []);
});

void test("a client that leaves while the rest of a denied push is being read does not crash the gateway", async (t) => {
  const { socket, instance, state, closed, stop } = await startGateway();
  const audit = recordFinishes(t, state);
  const { request, response } = open(socket, "deniedPush");
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  let rows;
  try {
    await within("the audit row", audit.first);
    request.destroy();
    await within("the client's request to fail", hungUp);
    await within("the gateway's response to close", closed());
    // Yield so Node reports an unhandled rejection from the gateway while this
    // test is still running.
    await nextTurn();
    rows = auditRows(state, instance);
  } finally {
    request.destroy();
    await stop();
  }
  assert.deepEqual(rows, [{ outcome: "denied", upstreamStatus: null }]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "denied" }]);
});

void test("a failed listener refresh does not fail the refreshes queued behind it", async (t) => {
  const { gateway, state, stop } = await startGateway();
  try {
    t.mock.method(
      state,
      "active",
      (): never => {
        throw new Error("state unavailable");
      },
      { times: 1 },
    );
    const settled = await Promise.allSettled([
      gateway.refresh(),
      gateway.refresh(),
    ]);
    assert.deepEqual(
      settled.map(({ status }) => status),
      ["rejected", "fulfilled"],
    );
  } finally {
    await stop();
  }
});

void test("an upstream that answers before the upload ends has its request aborted when the client connection closes", async (t) => {
  const upstreamClosed = signal();
  const { socket, instance, state, closed, stop } = await startGateway({
    handler: (request, response) => {
      request.socket.once("close", upstreamClosed.resolve);
      response.writeHead(200, { "content-length": 2 }).end("ok");
    },
    tls: false,
  });
  const audit = recordFinishes(t, state);
  const { request, response } = open(socket, "upload");
  let rows;
  let fetched;
  try {
    fetched = await within("the client's response to close", response);
    await within("the audit row", audit.first);
    await within("the upstream connection to close", upstreamClosed.promise);
    await within("the gateway's response to close", closed());
    rows = auditRows(state, instance);
  } finally {
    request.destroy();
    await stop();
  }
  assert.deepEqual(fetched, { status: 200, body: "ok", complete: true });
  assert.deepEqual(rows, [{ outcome: "completed", upstreamStatus: 200 }]);
  assert.deepEqual(audit.calls, [{ status: 200, outcome: "completed" }]);
});

function watchCloseListeners(listener: https.Server): {
  connections: Set<Socket>;
  counts: number[];
} {
  const connections = new Set<Socket>();
  const counts: number[] = [];
  listener.on("request", (request) => {
    connections.add(request.socket);
    counts.push(request.socket.listenerCount("close"));
  });
  return { connections, counts };
}

void test("GraphQL queries in sequence on one connection leave no listeners behind on its socket", async () => {
  const { socket, listener, stop } = await startGateway();
  const watched = watchCloseListeners(listener);
  const agent = new https.Agent({ keepAlive: true, maxSockets: 1 });
  try {
    for (let round = 0; round < 8; round += 1) {
      const query = await send(
        socket,
        "POST",
        "/graphql",
        Buffer.from(
          JSON.stringify({
            query:
              '{ repo_000: repository(owner: "shields", name: "dotfiles") { name } }',
            variables: null,
          }),
        ),
        "api.github.com",
        {},
        agent,
      );
      assert.equal(query.status, 200);
    }
  } finally {
    agent.destroy();
    await stop();
  }
  assert.equal(watched.connections.size, 1);
  assert.equal(new Set(watched.counts).size, 1);
});

void test("uploads in sequence on one connection leave no listeners behind on its socket", async () => {
  const uploads = Array.from({ length: 8 }, () => signal());
  let forwarded = 0;
  const { socket, listener, stop } = await startGateway({
    handler: (request, response) => {
      uploads[forwarded]?.resolve();
      forwarded += 1;
      request.resume();
      request.once("end", () => {
        response.writeHead(200, { "content-length": 2 }).end("ok");
      });
    },
    tls: false,
  });
  const watched = watchCloseListeners(listener);
  const agent = new https.Agent({ keepAlive: true, maxSockets: 1 });
  try {
    for (const upload of uploads) {
      const request = https.request({
        socketPath: socket,
        servername: "github.com",
        rejectUnauthorized: false,
        agent,
        method: "POST",
        path: "/shields/dotfiles.git/git-upload-pack",
        headers: { host: "github.com" },
      });
      const answered = new Promise<void>((resolve, reject) => {
        request.once("error", reject);
        request.once("response", (incoming) => {
          incoming.resume();
          incoming.once("end", resolve);
        });
      });
      request.write("0000");
      await within("the gateway to forward the upload", upload.promise);
      request.end("0000");
      await within("the response to end", answered);
    }
  } finally {
    agent.destroy();
    await stop();
  }
  assert.equal(watched.connections.size, 1);
  assert.equal(new Set(watched.counts).size, 1);
});

void test("uploads that fail in sequence on one connection leave no listeners behind on its socket", async () => {
  const { socket, listener, stop } = await startGateway({
    handler: (request) => {
      request.socket.destroy();
    },
    tls: false,
  });
  const watched = watchCloseListeners(listener);
  const agent = new https.Agent({ keepAlive: true, maxSockets: 1 });
  try {
    for (let round = 0; round < 8; round += 1) {
      const request = https.request({
        socketPath: socket,
        servername: "github.com",
        rejectUnauthorized: false,
        agent,
        method: "POST",
        path: "/shields/dotfiles.git/git-upload-pack",
        headers: { host: "github.com" },
      });
      const failed = signal();
      const answered = new Promise<number>((resolve, reject) => {
        request.once("error", reject);
        request.once("response", (incoming) => {
          failed.resolve();
          incoming.resume();
          incoming.once("end", () => {
            resolve(incoming.statusCode ?? 0);
          });
        });
      });
      request.write("0000");
      await within("the gateway to answer the failed upload", failed.promise);
      request.end("0000");
      assert.equal(await within("the response to end", answered), 503);
    }
  } finally {
    agent.destroy();
    await stop();
  }
  assert.equal(watched.connections.size, 1);
  assert.equal(new Set(watched.counts).size, 1, watched.counts.join(","));
});
