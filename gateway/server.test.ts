import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import http, {
  createServer,
  request as httpRequest,
  type ClientRequest,
  type IncomingHttpHeaders,
  type RequestListener,
} from "node:http";
import { once, type EventEmitter } from "node:events";
import https from "node:https";
import {
  connect,
  createServer as createNetServer,
  type Server,
  type Socket,
} from "node:net";
import { connect as tlsConnect } from "node:tls";
import {
  mkdtempSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import {
  afterAll,
  afterEach,
  beforeAll,
  mock,
  type Mock,
  onTestFinished,
  spyOn,
  test,
} from "bun:test";
import { setImmediate as nextTurn } from "node:timers/promises";
import { promisify } from "node:util";
import { gzipSync } from "node:zlib";
import { AppAuth } from "./auth.ts";
import { Gateway } from "./server.ts";
import { State, type Instance } from "./state.ts";

afterEach(() => {
  mock.restore();
});

const createHttpsServer = https.createServer;
const createHttpServer = http.createServer;

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

const throwaway = mkdtempSync(path.join(tmpdir(), "vm-access-cert-"));
const certificate = path.join(throwaway, "cert.pem");
const key = path.join(throwaway, "key.pem");

beforeAll(() => {
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
});

afterAll(() => {
  rmSync(throwaway, { recursive: true, force: true });
});

// Undo functions run last-in-first-out. A gateway's stop() waits for open
// client connections, which only undo functions registered after it can close.
let teardown: AsyncDisposableStack | undefined;

function defer(undo: () => void | Promise<void>): void {
  if (!teardown) {
    const created = new AsyncDisposableStack();
    teardown = created;
    onTestFinished(() => {
      teardown = undefined;
      return created.disposeAsync();
    });
  }
  teardown.defer(undo);
}

function rawUpstream(connection: (socket: Socket) => void): Server & {
  closeAllConnections: () => void;
} {
  const sockets = new Set<Socket>();
  const server = createNetServer((socket) => {
    sockets.add(socket);
    socket.once("close", () => {
      sockets.delete(socket);
    });
    connection(socket);
  });
  return Object.assign(server, {
    closeAllConnections: () => {
      for (const socket of sockets) {
        socket.destroy();
      }
    },
  });
}

async function startGateway(
  upstreamOptions: {
    handler?: RequestListener;
    tls?: boolean;
    connection?: (socket: Socket) => void;
    githubOrigins?: boolean;
  } | null = null,
): Promise<{
  origin: string;
  socket: string;
  instance: Instance;
  state: State;
  auth: AppAuth;
  gateway: Gateway;
  incoming: Forwarded[];
  listener: https.Server;
  created: Mock<typeof https.createServer>;
  closed: () => Promise<void>;
}> {
  const root = mkdtempSync(path.join(tmpdir(), "vm-access-server-"));
  defer(() => {
    rmSync(root, { recursive: true, force: true });
  });
  const state = new State(path.join(root, "state"));
  defer(() => {
    if (state.db.isOpen) {
      state.close();
    }
  });
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
  const upstream = upstreamOptions?.connection
    ? rawUpstream(upstreamOptions.connection)
    : upstreamOptions?.tls
      ? https.createServer(
          { cert: readFileSync(certificate), key: readFileSync(key) },
          serve,
        )
      : createServer(serve);
  const agentOptions = https.globalAgent.options;
  const trusted = agentOptions.ca;
  defer(() => {
    agentOptions.ca = trusted;
  });
  defer(async () => {
    upstream.closeAllConnections();
    await new Promise<void>((resolve) => {
      upstream.close(() => {
        resolve();
      });
    });
  });
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
    webUpstream: upstreamOptions?.githubOrigins ? "https://github.com" : origin,
    apiUpstream: upstreamOptions?.githubOrigins
      ? "https://api.github.com"
      : origin,
  });
  const created = spyOn(https, "createServer");
  defer(() => gateway.stop());
  await gateway.start();
  const result = created.mock.results[0];
  assert.equal(result?.type, "return");
  const listener = result.value;
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
    origin,
    socket,
    instance,
    state,
    auth,
    gateway,
    incoming,
    listener,
    created,
    closed,
  };
}

test("the HTTPS socket injects a repo token only after checking the full push and live grant", async () => {
  const { socket, instance, state, incoming, listener } = await startGateway();
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
    (await send(socket, "POST", "/shields/dotfiles.git/git-receive-pack", good))
      .status,
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
  const shallow = Buffer.concat([packet(`shallow ${"2".repeat(40)}\n`), good]);
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
    (await send(socket, "GET", next, Buffer.alloc(0), "api.github.com")).status,
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
      ["api.github.com", "GET", `/repos/${repository}/pulls`, Buffer.alloc(0)],
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
});

test("a push to a task branch name that Git would refuse is denied before anything reaches the upstream", async () => {
  const { socket, instance, incoming, state } = await startGateway();
  const reply = await send(
    socket,
    "POST",
    "/shields/dotfiles.git/git-receive-pack",
    Buffer.concat([
      packet(
        `${"0".repeat(40)} ${"1".repeat(40)} refs/heads/task/${instance.id}/../../main\0report-status\n`,
      ),
      Buffer.from("0000PACK"),
    ]),
  );
  assert.equal(reply.status, 403);
  assert.match(reply.body, /push destination is not a valid Git ref name/);
  assert.equal(incoming.length, 0);
  assert.equal(state.logs(instance.id)[0]?.allowed, false);
  assert.equal(
    state.logs(instance.id)[0]?.reason,
    "push destination is not a valid Git ref name",
  );
});

test("Content-Encoding reaches GitHub only on fetch bodies, and inspected bodies must be unencoded", async () => {
  const { socket, instance, incoming } = await startGateway();
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
    ["github.com", "/shields/dotfiles.git/git-receive-pack", push, "identity"],
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
});

async function exchange(
  socket: string,
  host: string,
  request: string,
): Promise<string> {
  const client = tlsConnect({
    path: socket,
    servername: host,
    rejectUnauthorized: false,
  });
  defer(() => {
    client.destroy();
  });
  const chunks: Buffer[] = [];
  client.on("data", (chunk: Buffer) => {
    chunks.push(chunk);
  });
  client.write(request);
  await within("the gateway to close the connection", once(client, "close"));
  return Buffer.concat(chunks).toString();
}

function smugglingUpstream(): {
  received: Buffer[];
  connection: (socket: Socket) => void;
} {
  const received: Buffer[] = [];
  return {
    received,
    connection: (socket) => {
      let replied = false;
      socket.on("data", (chunk: Buffer) => {
        received.push(chunk);
        if (replied || !Buffer.concat(received).includes("\r\n\r\n")) {
          return;
        }
        replied = true;
        socket.write("HTTP/1.1 200 OK\r\ncontent-length: 2\r\n\r\nok");
      });
    },
  };
}

function rawRequest(
  method: string,
  target: string,
  host: string,
  headers = "",
  payload = "",
): string {
  const lines = [`host: ${host}`, "connection: close", headers].filter(Boolean);
  return `${method} ${target} HTTP/1.1\r\n${lines.join("\r\n")}\r\n\r\n${payload}`;
}

const hiddenRequest = "GET /user HTTP/1.1\r\nhost: api.github.com\r\n\r\n";
const chunkedBody = `${hiddenRequest.length.toString(16)}\r\n${hiddenRequest}\r\n0\r\n\r\n`;

test("a GET or HEAD with a chunked body is denied and none of it reaches the upstream", async () => {
  const { received, connection } = smugglingUpstream();
  const { socket, instance, state } = await startGateway({ connection });
  const requests = [
    ["api.github.com", "GET", "/repos/shields/dotfiles/pulls"],
    ["api.github.com", "HEAD", "/repos/shields/dotfiles"],
    [
      "github.com",
      "GET",
      "/shields/dotfiles.git/info/refs?service=git-upload-pack",
    ],
  ] as const;
  for (const [host, method, target] of requests) {
    const reply = await exchange(
      socket,
      host,
      rawRequest(
        method,
        target,
        host,
        "transfer-encoding: chunked",
        chunkedBody,
      ),
    );
    assert.equal(Buffer.concat(received).toString(), "", `${method} ${target}`);
    assert.match(reply, /^HTTP\/1\.1 403 /, `${method} ${target}`);
  }
  assert.deepEqual(
    state
      .logs(instance.id)
      .map(({ allowed, reason, outcome }) => ({ allowed, reason, outcome })),
    requests.map(() => ({
      allowed: false,
      reason: "GET and HEAD requests must not have a body",
      outcome: "denied",
    })),
  );
});

test("ambiguous request framing never reaches the upstream, and a GET with an empty body does", async () => {
  const { received, connection } = smugglingUpstream();
  const { socket, instance, state } = await startGateway({ connection });
  const outcomes: [string, string, string, number][] = [
    // The HTTP parser or the gateway rejects these; either way nothing
    // reaches the upstream.
    [
      "GET",
      "transfer-encoding: chunked\r\ncontent-length: 5",
      chunkedBody,
      400,
    ],
    [
      "GET",
      "content-length: 5\r\ntransfer-encoding: chunked",
      chunkedBody,
      400,
    ],
    [
      "POST",
      "transfer-encoding: chunked\r\ncontent-length: 5",
      chunkedBody,
      400,
    ],
    ["GET", "transfer-encoding: chunked, identity", chunkedBody, 400],
    [
      "GET",
      "transfer-encoding: chunked\r\ntransfer-encoding: chunked",
      chunkedBody,
      400,
    ],
    ["GET", "transfer-encoding : chunked", chunkedBody, 400],
    ["GET", "transfer-encoding: chunked\r\n\tfolded", chunkedBody, 400],
    ["GET", "content-length: 5\r\ncontent-length: 6", "abcde", 400],
    ["GET", "content-length: +5", "abcde", 400],
    ["GET", "content-length: 5", "abcde", 403],
    ["GET", "content-length: 1", "a", 403],
    ["GET", "transfer-encoding: gzip, chunked", chunkedBody, 403],
    ["GET", "transfer-encoding: gzip", chunkedBody, 403],
    ["GET", "transfer-encoding: identity", "", 403],
    ["GET", "transfer-encoding:\tchunked", chunkedBody, 403],
    ["GET", "content-length: 0", "", 200],
  ];
  for (const [method, headers, payload, status] of outcomes) {
    received.length = 0;
    const reply = await exchange(
      socket,
      "api.github.com",
      rawRequest(
        method,
        "/repos/shields/dotfiles/pulls",
        "api.github.com",
        headers,
        payload,
      ),
    );
    const label = `${method} ${JSON.stringify(headers)}`;
    const replied = Number(reply.split(" ", 2)[1]);
    assert.ok(
      status === 400 ? replied === 400 || replied === 403 : replied === status,
      `${label}: ${String(replied)}`,
    );
    const request = Buffer.concat(received).toString();
    if (status === 200) {
      assert.equal(request.split("\r\n\r\n").at(-1), "", label);
      assert.doesNotMatch(request, /GET \/user/, label);
    } else {
      assert.equal(request, "", label);
    }
  }
  const denied = state.logs(instance.id).filter(({ allowed }) => !allowed);
  const must = outcomes.filter((outcome) => outcome[3] === 403).length;
  const may = outcomes.filter((outcome) => outcome[3] === 400).length;
  assert.ok(
    denied.length >= must && denied.length <= must + may,
    `${String(denied.length)} denials`,
  );
});

test("a request target that names an authority is denied and sends no credentials upstream", async () => {
  const { socket, instance, state, incoming, origin } = await startGateway({
    githubOrigins: true,
  });
  const local = new URL(origin);
  const reached: string[] = [];
  const redirect = (url: URL, options: https.RequestOptions): ClientRequest => {
    reached.push(url.href);
    const redirected = new URL(url);
    redirected.protocol = local.protocol;
    redirected.host = local.host;
    return httpRequest(redirected, options);
  };
  spyOn(https, "request").mockImplementation(redirect as typeof https.request);
  const targets = [
    [
      "github.com",
      "//attacker:ghp_token@github.com/octocat/Hello-World.git/info/refs?service=git-upload-pack",
    ],
    [
      "github.com",
      "//attacker:ghp_token@github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
    ],
    [
      "api.github.com",
      "//attacker:ghp_token@api.github.com/repos/octocat/Hello-World/pulls",
    ],
    ["github.com", "//github.com/octocat/Hello-World"],
    [
      "github.com",
      String.raw`/\attacker:ghp_token@github.com/octocat/Hello-World`,
    ],
  ] as const;
  for (const [host, target] of targets) {
    const reply = await exchange(
      socket,
      host,
      rawRequest("GET", target, host, "accept: */*", ""),
    );
    assert.equal(Number(reply.split(" ", 2)[1]), 403, `${host} ${target}`);
  }
  assert.equal(reached.length, 0);
  assert.equal(incoming.length, 0);
  const logs = state.logs(instance.id);
  assert.equal(logs.length, targets.length);
  for (const log of logs) {
    assert.equal(log.allowed, false);
    assert.equal(log.reason, "request target must stay on its GitHub host");
    assert.equal(log.outcome, "denied");
  }
  const control = await exchange(
    socket,
    "github.com",
    rawRequest(
      "GET",
      "/octocat/Hello-World.git/info/refs?service=git-upload-pack",
      "github.com",
      "accept: */*",
      "",
    ),
  );
  assert.equal(Number(control.split(" ", 2)[1]), 200);
  assert.deepEqual(reached, [
    "https://github.com/octocat/Hello-World.git/info/refs?service=git-upload-pack",
  ]);
  assert.equal(incoming.length, 1);
  assert.equal(incoming[0]?.authorization, undefined);
});

test("a state failure as a request starts is a 503 with an audit row and does not crash the gateway", async () => {
  const { socket, instance, state } = await startGateway();
  spyOn(state, "byId").mockImplementationOnce((): never => {
    throw new Error("database is locked");
  });
  const { response } = open(socket);
  assert.equal((await within("the gateway to answer", response)).status, 503);
  await nextTurn();
  assert.deepEqual(
    state
      .logs(instance.id)
      .map(({ allowed, reason, outcome }) => ({ allowed, reason, outcome })),
    [{ allowed: false, reason: "gateway request failed", outcome: "denied" }],
  );
});

test("a state failure that also stops the audit write is reported on stderr and answered with a 503", async () => {
  const { socket, state } = await startGateway();
  const stderr = spyOn(process.stderr, "write").mockImplementation(() => true);
  const failure = (): never => {
    throw new Error("database is locked");
  };
  spyOn(state, "byId").mockImplementation(failure);
  spyOn(state, "logDecision").mockImplementation(failure);
  const { response } = open(socket);
  const reply = await within("the gateway to answer", response);
  await nextTurn();
  assert.equal(reply.status, 503);
  assert.match(reply.body, /gateway audit unavailable/);
  assert.deepEqual(
    stderr.mock.calls.map((call) => String(call[0])),
    ["vm-access audit failure: Error: database is locked\n"],
  );
});

test("the health path is answered locally however the target spells it", async () => {
  const { socket, incoming } = await startGateway();
  for (const target of [
    "/__vm_access__/health",
    "/x/../__vm_access__/health",
    "/__vm_access__/./health",
  ]) {
    const response = await send(
      socket,
      "GET",
      target,
      Buffer.alloc(0),
      "api.github.com",
    );
    assert.equal(response.status, 200, target);
    assert.equal(response.body, "vm-access ready\n", target);
  }
  assert.equal(incoming.length, 0);
  const page = await send(socket, "GET", "/shields/dotfiles");
  assert.equal(page.body, "ok");
  assert.equal(incoming.length, 1);
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
  agent: https.Agent | false = false,
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
    agent,
    headers: { host: "github.com" },
  });
  defer(() => {
    request.destroy();
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

function recordFinishes(state: State): {
  calls: { status: number | null; outcome: string }[];
  first: Promise<void>;
} {
  const calls: { status: number | null; outcome: string }[] = [];
  const first = signal();
  const finish = state.finish.bind(state);
  spyOn(state, "finish").mockImplementation(
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

const endings = {
  fin: (socket: Socket) => {
    socket.end();
  },
  destroy: (socket: Socket) => {
    socket.destroy();
  },
  reset: (socket: Socket) => {
    socket.resetAndDestroy();
  },
};

const endsEarly: {
  name: string;
  tls: boolean;
  begin: RequestListener;
  end: keyof typeof endings;
}[] = [
  {
    name: "FIN after a chunk, without the final chunk",
    tls: false,
    begin: (_request, response) => {
      response.writeHead(200);
      response.write("partial");
    },
    end: "fin",
  },
  {
    name: "FIN in the middle of a chunk",
    tls: false,
    begin: (request, response) => {
      response.writeHead(200, { "transfer-encoding": "chunked" });
      response.flushHeaders();
      request.socket.write("64\r\npart");
    },
    end: "fin",
  },
  {
    name: "FIN before the end of a Content-Length body",
    tls: false,
    begin: (_request, response) => {
      response.writeHead(200, { "content-length": 100_000 });
      response.write("partial");
    },
    end: "fin",
  },
  {
    name: "TLS close_notify without the final chunk",
    tls: true,
    begin: (_request, response) => {
      response.writeHead(200);
      response.write("partial");
    },
    end: "fin",
  },
  {
    name: "TLS EOF without close_notify",
    tls: true,
    begin: (_request, response) => {
      response.writeHead(200);
      response.write("partial");
    },
    end: "destroy",
  },
  {
    name: "TCP reset",
    tls: false,
    begin: (_request, response) => {
      response.writeHead(200, { "content-length": 100_000 });
      response.write("partial");
    },
    end: "reset",
  },
];

for (const { name, tls, begin, end } of endsEarly) {
  test(`an upstream that ends its body early (${name}) fails the client's response and is audited as a gateway error`, async () => {
    const clientHasData = signal();
    const { socket, instance, state, closed } = await startGateway({
      handler: (request, response) => {
        begin(request, response);
        void clientHasData.promise.then(() => {
          endings[end](request.socket);
        });
      },
      tls,
    });
    const audit = recordFinishes(state);
    const { firstData, response } = open(socket);
    void firstData.then(clientHasData.resolve);
    const fetched = await within("the client's response to close", response);
    await within("the audit row", audit.first);
    await within("the gateway's response to close", closed());
    assert.equal(fetched.status, 200);
    assert.equal(fetched.complete, false);
    assert.deepEqual(auditRows(state, instance), [
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
  test(`a complete response (${name}) is audited once, as completed`, async () => {
    const { socket, instance, state, closed } = await startGateway({
      handler: respond,
      tls: false,
    });
    const audit = recordFinishes(state);
    const { response } = open(socket);
    const fetched = await within("the client's response to close", response);
    await within("the audit row", audit.first);
    await within("the gateway's response to close", closed());
    assert.deepEqual(fetched, { status: 200, body: "partial", complete: true });
    assert.deepEqual(auditRows(state, instance), [
      { outcome: "completed", upstreamStatus: 200 },
    ]);
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
  test(`an upstream that ${name} before sending headers is a gateway error with a 503`, async () => {
    const { socket, instance, state, closed } = await startGateway({
      handler: respond,
      tls: false,
    });
    const audit = recordFinishes(state);
    const { response } = open(socket);
    const fetched = await within("the client's response to close", response);
    await within("the audit row", audit.first);
    await within("the gateway's response to close", closed());
    assert.equal(fetched.status, 503);
    assert.equal(fetched.complete, true);
    assert.deepEqual(auditRows(state, instance), [
      { outcome: "gateway-error", upstreamStatus: null },
    ]);
    assert.deepEqual(audit.calls, [{ status: null, outcome: "gateway-error" }]);
  });
}

test("a client that leaves mid-body is audited as interrupted and the upstream request is aborted", async () => {
  const upstreamClosed = signal();
  const { socket, instance, state, closed } = await startGateway({
    handler: (request, response) => {
      request.socket.once("close", upstreamClosed.resolve);
      response.writeHead(200, { "content-length": 100_000 });
      response.write("partial");
    },
    tls: false,
  });
  const audit = recordFinishes(state);
  const { request, firstData } = open(socket);
  await within("the first byte of the body", firstData);
  request.destroy();
  await within("the audit row", audit.first);
  await within("the upstream connection to close", upstreamClosed.promise);
  await within("the gateway's response to close", closed());
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "interrupted", upstreamStatus: 200 },
  ]);
  assert.deepEqual(audit.calls, [{ status: 200, outcome: "interrupted" }]);
});

test("a client that leaves before the upstream answers is audited as interrupted and the upstream request is aborted", async () => {
  const asked = signal();
  const upstreamClosed = signal();
  const { socket, instance, state, closed } = await startGateway({
    handler: (request) => {
      asked.resolve();
      request.socket.once("close", upstreamClosed.resolve);
    },
    tls: false,
  });
  const audit = recordFinishes(state);
  const { request, response } = open(socket);
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the upstream to receive the request", asked.promise);
  request.destroy();
  await within("the client's request to fail", hungUp);
  await within("the audit row", audit.first);
  await within("the upstream connection to close", upstreamClosed.promise);
  await within("the gateway's response to close", closed());
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "interrupted", upstreamStatus: null },
  ]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
});

for (const [name, mode] of [
  ["a GET", "get"],
  ["a POST that arrived whole", "post"],
] as const) {
  test(`a client that leaves while its token is being fetched (${name}) is audited as interrupted and never reaches the upstream`, async () => {
    const { socket, instance, state, auth, incoming, closed } =
      await startGateway();
    const audit = recordFinishes(state);
    const asked = signal();
    const resume = signal();
    defer(resume.resolve);
    spyOn(auth, "token").mockImplementation(async (): Promise<string> => {
      asked.resolve();
      await resume.promise;
      return "ghu_secret";
    });
    const { request, response } = open(socket, mode);
    const hungUp = assert.rejects(response, { message: "socket hang up" });
    await within("the gateway to ask for a token", asked.promise);
    request.destroy();
    await within("the client's request to fail", hungUp);
    await within("the gateway's response to close", closed());
    resume.resolve();
    await within("the audit row", audit.first);
    assert.deepEqual(auditRows(state, instance), [
      { outcome: "interrupted", upstreamStatus: null },
    ]);
    assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
    assert.deepEqual(incoming, []);
  });
}

for (const [name, mode] of [
  ["a GET", "get"],
  ["a POST that arrived whole", "post"],
] as const) {
  test(`a client whose socket is destroyed as its token arrives (${name}) is not forwarded over a warm upstream connection`, async () => {
    const { socket, instance, state, auth, listener, incoming, closed } =
      await startGateway();
    const target = "/shields/dotfiles.git/info/refs?service=git-upload-pack";
    const warm = await send(socket, "GET", target);
    assert.equal(warm.status, 200);
    const audit = recordFinishes(state);
    let serverSide: Socket | undefined;
    listener.prependOnceListener("request", (request) => {
      serverSide = request.socket;
    });
    spyOn(auth, "token").mockImplementation((): Promise<string> => {
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
    assert.deepEqual(
      auditRows(state, instance).filter(
        ({ outcome }) => outcome === "interrupted",
      ),
      [{ outcome: "interrupted", upstreamStatus: null }],
    );
    assert.deepEqual(audit.calls, [
      { status: null, outcome: "interrupted" },
      { status: 200, outcome: "completed" },
    ]);
    assert.deepEqual(
      incoming.map(({ target: sent }) => sent),
      [target, target],
    );
  });
}

test("a client whose socket is destroyed as the answer is relayed to it is audited as interrupted", async () => {
  const { socket, instance, state, listener, closed } = await startGateway();
  const audit = recordFinishes(state);
  listener.prependOnceListener("request", (request, reply) => {
    reply.once("pipe", () => {
      request.socket.destroy();
    });
  });
  const { response } = open(socket);
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the client's request to fail", hungUp);
  await within("the audit row", audit.first);
  await within("the gateway's response to close", closed());
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "interrupted", upstreamStatus: 200 },
  ]);
  assert.deepEqual(audit.calls, [{ status: 200, outcome: "interrupted" }]);
});

test("a client that leaves while uploading is audited as interrupted and the upstream request is aborted", async () => {
  const asked = signal();
  const upstreamClosed = signal();
  const { socket, instance, state, closed } = await startGateway({
    handler: (request) => {
      request.resume();
      asked.resolve();
      request.socket.once("close", upstreamClosed.resolve);
    },
    tls: false,
  });
  const audit = recordFinishes(state);
  const { request, response } = open(socket, "upload");
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the upstream to receive the request", asked.promise);
  request.destroy();
  await within("the client's request to fail", hungUp);
  await within("the audit row", audit.first);
  await within("the upstream connection to close", upstreamClosed.promise);
  await within("the gateway's response to close", closed());
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "interrupted", upstreamStatus: null },
  ]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
});

test("a client that leaves while its push commands are being read is audited as interrupted, not denied", async () => {
  const { socket, instance, state, listener, incoming, closed } =
    await startGateway();
  const audit = recordFinishes(state);
  const requested = signal();
  listener.on("request", requested.resolve);
  const { request, response } = open(socket, "push");
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the gateway to receive the request", requested.promise);
  request.destroy();
  await within("the client's request to fail", hungUp);
  await within("the audit row", audit.first);
  await within("the gateway's response to close", closed());
  assert.deepEqual(
    state.logs(instance.id).map(({ allowed, operation, reason, outcome }) => ({
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

test("a client that leaves while the rest of a denied push is being read does not crash the gateway", async () => {
  const { socket, instance, state, closed } = await startGateway();
  const audit = recordFinishes(state);
  const { request, response } = open(socket, "deniedPush");
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the audit row", audit.first);
  request.destroy();
  await within("the client's request to fail", hungUp);
  await within("the gateway's response to close", closed());
  // Yield so an unhandled rejection from the gateway is reported while this
  // test is still running.
  await nextTurn();
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "denied", upstreamStatus: null },
  ]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "denied" }]);
});

test("a failed listener refresh does not fail the refreshes queued behind it", async () => {
  const { gateway, state } = await startGateway();
  spyOn(state, "active").mockImplementationOnce((): never => {
    throw new Error("state unavailable");
  });
  const settled = await Promise.allSettled([
    gateway.refresh(),
    gateway.refresh(),
  ]);
  assert.deepEqual(
    settled.map(({ status }) => status),
    ["rejected", "fulfilled"],
  );
});

test("an upstream that answers before the upload ends has its request aborted when the client connection closes", async () => {
  const upstreamClosed = signal();
  const { socket, instance, state, closed } = await startGateway({
    handler: (request, response) => {
      request.socket.once("close", upstreamClosed.resolve);
      response.writeHead(200, { "content-length": 2 }).end("ok");
    },
    tls: false,
  });
  const audit = recordFinishes(state);
  const { response } = open(socket, "upload");
  const fetched = await within("the client's response to close", response);
  await within("the audit row", audit.first);
  await within("the upstream connection to close", upstreamClosed.promise);
  await within("the gateway's response to close", closed());
  assert.deepEqual(fetched, { status: 200, body: "ok", complete: true });
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "completed", upstreamStatus: 200 },
  ]);
  assert.deepEqual(audit.calls, [{ status: 200, outcome: "completed" }]);
});

test("an upstream that answers before the upload ends has its request aborted when a keep-alive client leaves", async () => {
  const upstreamClosed = signal();
  const { socket, instance, state, listener, closed } = await startGateway({
    handler: (request, response) => {
      request.socket.once("close", upstreamClosed.resolve);
      response.writeHead(403, { "content-length": 2 }).end("no");
    },
    tls: false,
  });
  let clientSide: Socket | undefined;
  listener.prependOnceListener("request", (request) => {
    clientSide = request.socket;
  });
  const audit = recordFinishes(state);
  const agent = new https.Agent({ keepAlive: true });
  defer(() => {
    agent.destroy();
  });
  const { request, response } = open(socket, "upload", agent);
  const fetched = await within("the client's response to close", response);
  await within("the audit row", audit.first);
  await within("the gateway's response to close", closed());
  assert.equal(clientSide?.destroyed, false);
  request.destroy();
  await within("the upstream connection to close", upstreamClosed.promise);
  assert.deepEqual(fetched, { status: 403, body: "no", complete: true });
  assert.deepEqual(auditRows(state, instance), [
    { outcome: "completed", upstreamStatus: 403 },
  ]);
  assert.deepEqual(audit.calls, [{ status: 403, outcome: "completed" }]);
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

test("GraphQL queries in sequence on one connection leave no listeners behind on its socket", async () => {
  const { socket, listener } = await startGateway();
  const watched = watchCloseListeners(listener);
  const agent = new https.Agent({ keepAlive: true, maxSockets: 1 });
  defer(() => {
    agent.destroy();
  });
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
  assert.equal(watched.connections.size, 1);
  assert.equal(new Set(watched.counts).size, 1);
});

test("uploads in sequence on one connection leave no listeners behind on its socket", async () => {
  const uploads = Array.from({ length: 8 }, () => signal());
  let forwarded = 0;
  const { socket, listener } = await startGateway({
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
  defer(() => {
    agent.destroy();
  });
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
  assert.equal(watched.connections.size, 1);
  assert.equal(new Set(watched.counts).size, 1);
});

test("uploads that fail in sequence on one connection leave no listeners behind on its socket", async () => {
  const { socket, listener } = await startGateway({
    handler: (request) => {
      request.socket.destroy();
    },
    tls: false,
  });
  const watched = watchCloseListeners(listener);
  const agent = new https.Agent({ keepAlive: true, maxSockets: 1 });
  defer(() => {
    agent.destroy();
  });
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
  assert.equal(watched.connections.size, 1);
  assert.equal(new Set(watched.counts).size, 1, watched.counts.join(","));
});

function accepting(socketPath: string): Promise<boolean> {
  return new Promise((resolve) => {
    const connection = connect(socketPath);
    connection.once("connect", () => {
      connection.destroy();
      resolve(true);
    });
    connection.once("error", () => {
      resolve(false);
    });
  });
}

async function holdAuthorization(): Promise<
  Awaited<ReturnType<typeof startGateway>> & {
    events: string[];
    release: () => void;
  }
> {
  const started = await startGateway();
  const events: string[] = [];
  const asked = signal();
  const resume = signal();
  defer(resume.resolve);
  spyOn(started.auth, "token").mockImplementation(async (): Promise<string> => {
    asked.resolve();
    await resume.promise;
    events.push("token");
    return "ghu_secret";
  });
  const { request, response } = open(started.socket);
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the gateway to ask for a token", asked.promise);
  request.destroy();
  await within("the client's request to fail", hungUp);
  await within("the gateway's response to close", started.closed());
  return { ...started, events, release: resume.resolve };
}

test("stop() waits for a request whose client left while it was authorizing, so its audit row is written before the database closes", async () => {
  const { gateway, state, instance, listener, events, release } =
    await holdAuthorization();
  const audit = recordFinishes(state);
  const decisions = spyOn(state, "logDecision");
  const shutdown = (async () => {
    await gateway.stop();
    events.push("stopped");
    const rows = auditRows(state, instance);
    state.close();
    return rows;
  })();
  await within("the listener to close", once(listener, "close"));
  // A stop() that does not wait for requests has resolved by now.
  await nextTurn();
  release();
  const rows = await within("the gateway to stop", shutdown);
  // Yield so a late write to the closed database is reported while this test
  // is still running.
  await nextTurn();
  assert.deepEqual(
    decisions.mock.results.map(({ type }) => type),
    ["return"],
  );
  assert.deepEqual(events, ["token", "stopped"]);
  assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: null }]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
});

test("every one of several concurrent stop() calls waits for the in-flight requests", async () => {
  const { gateway, state, instance, listener, events, release } =
    await holdAuthorization();
  const shutdown = Promise.all(
    [0, 1, 2].map(async (call) => {
      await gateway.stop();
      events.push(`stopped ${String(call)}`);
      return auditRows(state, instance);
    }),
  );
  await within("the listener to close", once(listener, "close"));
  await nextTurn();
  release();
  const rows = await within("the gateway to stop", shutdown);
  assert.equal(events[0], "token");
  assert.deepEqual(
    events.slice(1).toSorted((a, b) => a.localeCompare(b)),
    ["stopped 0", "stopped 1", "stopped 2"],
  );
  assert.deepEqual(
    rows,
    Array.from({ length: 3 }, () => [
      { outcome: "interrupted", upstreamStatus: null },
    ]),
  );
});

test("a request that fails while stop() waits for it is still reported as an unhandled rejection, once", () => {
  const result = spawnSync(
    process.execPath,
    [
      path.join(import.meta.dirname, "stop-rejection.fixture.ts"),
      certificate,
      key,
    ],
    { encoding: "utf8", timeout: 30_000 },
  );
  assert.equal(result.status, 0, result.stderr);
  assert.deepEqual(JSON.parse(result.stdout), {
    events: ["token", "stopped"],
    rejections: ["audit write failed"],
  });
}, 60_000);

function holdNextServer(created: Mock<typeof https.createServer>): {
  starting: Promise<undefined>;
  proceed: () => void;
} {
  const starting = signal();
  const proceed = signal();
  created.mockImplementationOnce(((
    ...args: Parameters<typeof https.createServer>
  ) => {
    const server = createHttpsServer(...args);
    const emit = server.emit.bind(server);
    server.emit = (event: string | symbol, ...rest: unknown[]) => {
      if (event !== "listening") {
        return emit(event, ...rest);
      }
      starting.resolve();
      void proceed.promise.then(() => emit(event, ...rest));
      return true;
    };
    return server;
  }) as typeof https.createServer);
  return { starting: starting.promise, proceed: proceed.resolve };
}

function requestSync(adminSocket: string): Promise<number> {
  return new Promise((resolve, reject) => {
    const sync = httpRequest(
      { socketPath: adminSocket, path: "/sync", method: "POST", agent: false },
      (response) => {
        response.resume();
        response.once("end", () => {
          resolve(response.statusCode ?? 0);
        });
      },
    );
    sync.once("error", reject);
    sync.end();
  });
}

interface Intervals {
  tick: (ms: number) => void;
}

function mockIntervals(): Intervals {
  const registered = new Map<object, { callback: () => void; delay: number }>();
  const clear = clearInterval;
  spyOn(globalThis, "setInterval").mockImplementation(((
    callback: () => void,
    delay: number,
  ) => {
    const handle = {
      ref: () => handle,
      unref: () => handle,
      hasRef: () => true,
      refresh: () => handle,
    };
    registered.set(handle, { callback, delay });
    return handle;
  }) as unknown as typeof setInterval);
  spyOn(globalThis, "clearInterval").mockImplementation(((
    handle: Parameters<typeof clearInterval>[0],
  ) => {
    if (typeof handle !== "object" || !registered.delete(handle)) {
      clear(handle);
    }
  }) as typeof clearInterval);
  return {
    tick: (ms) => {
      for (const { callback, delay } of registered.values()) {
        for (let elapsed = delay; elapsed <= ms; elapsed += delay) {
          callback();
        }
      }
    },
  };
}

interface RefreshRace {
  name: string;
  servers: number;
  race: (context: {
    gateway: Gateway;
    state: State;
    created: Mock<typeof https.createServer>;
    adminSocket: string;
    intervals: Intervals;
  }) => Promise<void>;
}

for (const { name, servers, race } of [
  {
    name: "stop() begins after it read the active VMs",
    servers: 1,
    race: async ({ gateway, state }) => {
      const stops: Promise<void>[] = [];
      const active = state.active.bind(state);
      spyOn(state, "active").mockImplementationOnce((): Instance[] => {
        stops.push(gateway.stop());
        return active();
      });
      await gateway.refresh();
      await Promise.all(stops);
    },
  },
  {
    name: "stop() begins before it runs",
    servers: 1,
    race: async ({ gateway }) => {
      await Promise.all([gateway.refresh(), gateway.stop()]);
    },
  },
  {
    name: "stop() begins while its server is starting",
    servers: 2,
    race: async ({ gateway, created }) => {
      const held = holdNextServer(created);
      const refreshed = gateway.refresh();
      await within("the new server to start", held.starting);
      const stopped = gateway.stop();
      held.proceed();
      await Promise.all([refreshed, stopped]);
    },
  },
  {
    name: "stop() begins while the periodic refresh's server is starting",
    servers: 2,
    race: async ({ gateway, created, intervals }) => {
      const held = holdNextServer(created);
      intervals.tick(60_000);
      await within("the new server to start", held.starting);
      const stopped = gateway.stop();
      held.proceed();
      await stopped;
    },
  },
  {
    name: "stop() begins while a /sync request's server is starting",
    servers: 2,
    race: async ({ gateway, created, adminSocket }) => {
      const held = holdNextServer(created);
      const synced = requestSync(adminSocket);
      await within("the new server to start", held.starting);
      const stopped = gateway.stop();
      held.proceed();
      assert.equal(await within("the /sync response", synced), 200);
      await stopped;
    },
  },
] satisfies RefreshRace[]) {
  test(`a listener refresh in flight when ${name} leaves no listener behind`, async () => {
    const intervals = mockIntervals();
    const { socket, gateway, state, created } = await startGateway();
    defer(() => {
      for (const result of created.mock.results) {
        if (result.type === "return") {
          result.value.close();
        }
      }
    });
    const added = path.join(path.dirname(socket), "added.sock");
    const adminSocket = path.join(path.dirname(socket), "admin.sock");
    state.register("lima:added", added, "tester");
    await race({ gateway, state, created, adminSocket, intervals });
    await gateway.refresh();
    assert.equal(await accepting(added), false);
    assert.equal(created.mock.calls.length, servers);
    assert.deepEqual(
      created.mock.results.map((result) =>
        result.type === "return" ? result.value.listening : undefined,
      ),
      Array.from({ length: servers }, () => false),
    );
  });
}

function unstartedGateway(
  sockets: string[],
  adminSocket = "admin.sock",
): {
  root: string;
  gateway: Gateway;
  created: Mock<typeof https.createServer>;
} {
  const root = mkdtempSync(path.join(tmpdir(), "vm-access-server-"));
  const state = new State(path.join(root, "state"));
  const secrets = path.join(root, "secrets");
  mkdirSync(secrets);
  defer(() => {
    state.close();
    rmSync(root, { recursive: true, force: true });
  });
  for (const [index, socket] of sockets.entries()) {
    state.register(
      `lima:vm${String(index)}`,
      path.join(root, socket),
      "tester",
    );
  }
  const gateway = new Gateway(state, new AppAuth(secrets), {
    cert: certificate,
    key,
    adminSocket: path.join(root, adminSocket),
  });
  const created = spyOn(https, "createServer");
  defer(() => gateway.stop());
  return { root, gateway, created };
}

for (const { name, listener, admin, starts } of [
  {
    name: "before start()",
    listener: "work.sock",
    admin: "admin.sock",
    starts: false,
  },
  {
    name: "after start() failed to open a VM listener",
    listener: "x/work.sock",
    admin: "admin.sock",
    starts: true,
  },
  {
    name: "after start() failed to open the admin socket",
    listener: "work.sock",
    admin: "x/admin.sock",
    starts: true,
  },
]) {
  test(`stop() ${name} and again afterwards does not throw, and the stopped gateway cannot start`, async () => {
    const { root, gateway } = unstartedGateway([listener], admin);
    if (starts) {
      await assert.rejects(gateway.start(), {
        code: "ENOENT",
        syscall: "listen",
      });
    }
    await gateway.stop();
    await gateway.stop();
    await assert.rejects(gateway.start(), { message: "gateway is stopped" });
    assert.equal(await accepting(path.join(root, listener)), false);
    assert.equal(await accepting(path.join(root, admin)), false);
  });
}

for (const { name, sockets, admin } of [
  {
    name: "a later VM listener cannot be opened",
    sockets: ["first.sock", "gone/second.sock"],
    admin: "admin.sock",
  },
  {
    name: "the admin socket cannot be opened",
    sockets: ["first.sock"],
    admin: "gone/admin.sock",
  },
]) {
  test(`start() fails with the original error and leaves nothing listening when ${name}`, async () => {
    const { root, gateway, created } = unstartedGateway(sockets, admin);
    await assert.rejects(gateway.start(), { syscall: "listen" });
    assert.equal(created.mock.calls.length, sockets.length);
    assert.equal(await accepting(path.join(root, "first.sock")), false);
    assert.equal(await accepting(path.join(root, admin)), false);
    assert.deepEqual(
      created.mock.results.map((result) =>
        result.type === "return" ? result.value.listening : undefined,
      ),
      sockets.map(() => false),
    );
  });
}

function interceptAdmin(intercept: (server: http.Server) => void): void {
  spyOn(http, "createServer").mockImplementation(((
    ...args: Parameters<typeof http.createServer>
  ) => {
    const server = createHttpServer(...args);
    intercept(server);
    return server;
  }) as typeof http.createServer);
}

interface StartRace {
  name: string;
  arm: (context: {
    gateway: Gateway;
    created: Mock<typeof https.createServer>;
    stops: Promise<void>[];
  }) => {
    race?: () => Promise<void>;
    onAdmin?: (server: http.Server) => void;
  };
}

const startRaces: StartRace[] = [
  {
    name: "start() is waiting for its first refresh",
    arm: ({ gateway, stops }) => ({
      race: () => {
        stops.push(gateway.stop());
        return Promise.resolve();
      },
    }),
  },
  {
    name: "a VM listener is opening",
    arm: ({ gateway, created, stops }) => {
      const held = holdNextServer(created);
      return {
        race: async () => {
          await within("the VM listener to open", held.starting);
          stops.push(gateway.stop());
          held.proceed();
        },
      };
    },
  },
  {
    name: "the admin server has just been asked to listen",
    arm: ({ gateway, stops }) => ({
      onAdmin: (server) => {
        const listen = server.listen.bind(server);
        server.listen = ((...args: Parameters<typeof server.listen>) => {
          const result = listen(...args);
          stops.push(gateway.stop());
          return result;
        }) as typeof server.listen;
      },
    }),
  },
  {
    name: "the admin server emits listening",
    arm: ({ gateway, stops }) => ({
      onAdmin: (server) => {
        const emit = server.emit.bind(server);
        server.emit = (event: string | symbol, ...rest: unknown[]) => {
          if (event === "listening") {
            stops.push(gateway.stop());
          }
          return emit(event, ...rest);
        };
      },
    }),
  },
];

for (const { name, arm } of startRaces) {
  test(`stop() while ${name} makes start() fail and leaves nothing listening or scheduled`, async () => {
    const intervals = mockIntervals();
    const { root, gateway, created } = unstartedGateway(["vm.sock"]);
    const refresh = spyOn(gateway, "refresh");
    const stops: Promise<void>[] = [];
    const { race, onAdmin } = arm({ gateway, created, stops });
    const admins: http.Server[] = [];
    interceptAdmin((server) => {
      admins.push(server);
      onAdmin?.(server);
    });
    const started = assert.rejects(gateway.start(), {
      message: "gateway is stopped",
    });
    await race?.();
    await within("start() to settle", started);
    assert.equal(stops.length, 1);
    await within("stop()", Promise.all(stops));
    assert.equal(admins.length, onAdmin ? 1 : 0);
    assert.equal(await accepting(path.join(root, "vm.sock")), false);
    assert.equal(await accepting(path.join(root, "admin.sock")), false);
    intervals.tick(60_000);
    await nextTurn();
    assert.equal(refresh.mock.calls.length, 1);
  });
}

test("a second start() fails without touching the gateway that is starting or started", async () => {
  const { root, gateway } = unstartedGateway(["vm.sock"]);
  const admins = spyOn(http, "createServer");
  const first = gateway.start();
  await assert.rejects(gateway.start(), {
    message: "gateway is already started",
  });
  await first;
  await assert.rejects(gateway.start(), {
    message: "gateway is already started",
  });
  assert.equal(admins.mock.calls.length, 1);
  assert.equal(await requestSync(path.join(root, "admin.sock")), 200);
});

function connectionCount(server: https.Server): Promise<number> {
  return promisify(server.getConnections.bind(server))();
}

function holdEvent(
  emitter: EventEmitter,
  name: string,
  until: Promise<unknown>,
  reached?: () => void,
): void {
  const emit = emitter.emit.bind(emitter);
  emitter.emit = (event: string | symbol, ...rest: unknown[]) => {
    if (event !== name) {
      return emit(event, ...rest);
    }
    reached?.();
    void until.then(() => emit(event, ...rest));
    return true;
  };
}

test("stop() waits for the server of a VM that was disabled, and for its idle keep-alive connections to end", async () => {
  const { socket, gateway, state, auth, listener } = await startGateway();
  listener.keepAliveTimeout = 600_000;
  const lingering = signal();
  defer(lingering.resolve);
  holdEvent(listener, "close", lingering.promise);
  const agent = new https.Agent({ keepAlive: true });
  defer(() => {
    agent.destroy();
  });
  const asked = signal();
  const resume = signal();
  defer(resume.resolve);
  spyOn(auth, "token").mockImplementation(async (): Promise<string> => {
    asked.resolve();
    await resume.promise;
    return "ghu_secret";
  });
  const { response } = open(socket, "get", agent);
  await within("the gateway to ask for a token", asked.promise);
  state.disable("lima:work", "tester");
  await gateway.refresh();
  assert.equal(listener.listening, false);
  assert.equal(await connectionCount(listener), 1);
  let closed = false;
  listener.once("close", () => {
    closed = true;
  });
  const stopping = gateway.stop();
  resume.resolve();
  assert.equal((await within("the response", response)).status, 403);
  let stopped = false;
  void stopping.then(() => {
    stopped = true;
  });
  await nextTurn();
  await nextTurn();
  assert.equal(stopped, false);
  lingering.resolve();
  await within("stop()", stopping);
  assert.equal(closed, true);
  assert.equal(await connectionCount(listener), 0);
});

test("stop() lets a transfer in flight finish and then ends its keep-alive connection without waiting for the keep-alive timeout", async () => {
  const finish = signal();
  const { socket, gateway, listener } = await startGateway({
    handler: (_request, response) => {
      response.writeHead(200, { "content-length": 2 });
      response.write("a");
      void finish.promise.then(() => response.end("b"));
    },
    tls: false,
  });
  listener.keepAliveTimeout = 600_000;
  defer(finish.resolve);
  const agent = new https.Agent({ keepAlive: true });
  defer(() => {
    agent.destroy();
  });
  const { firstData, response } = open(socket, "get", agent);
  await within("the first part of the response", firstData);
  let stopped = false;
  const stopping = (async () => {
    await gateway.stop();
    stopped = true;
  })();
  await nextTurn();
  await nextTurn();
  assert.equal(stopped, false);
  finish.resolve();
  assert.deepEqual(await within("the response", response), {
    status: 200,
    body: "ab",
    complete: true,
  });
  await within("stop()", stopping);
  assert.equal(await connectionCount(listener), 0);
});

test("overlapping stop() calls during start() are one shutdown that waits for start() to fail and leaves nothing listening", async () => {
  const { root, gateway, created } = unstartedGateway(["vm.sock"]);
  const held = holdNextServer(created);
  const started = assert.rejects(gateway.start(), {
    message: "gateway is stopped",
  });
  await within("the VM listener to open", held.starting);
  const stops = [gateway.stop(), gateway.stop()];
  assert.equal(stops[0], stops[1]);
  held.proceed();
  await within("start() to settle", started);
  await within("stop()", Promise.all(stops));
  await gateway.stop();
  assert.equal(await accepting(path.join(root, "vm.sock")), false);
  assert.equal(await accepting(path.join(root, "admin.sock")), false);
});

function holdRequests(listener: https.Server): {
  parsed: Promise<undefined>;
  deliver: () => void;
} {
  const parsed = signal();
  const delivered = signal();
  defer(delivered.resolve);
  holdEvent(listener, "request", delivered.promise, parsed.resolve);
  return { parsed: parsed.promise, deliver: delivered.resolve };
}

test("a request that is dispatched once stop() has begun is answered with Connection: close", async () => {
  const { socket, gateway, listener } = await startGateway();
  const held = holdRequests(listener);
  const agent = new https.Agent({ keepAlive: true });
  defer(() => {
    agent.destroy();
  });
  const answered = send(
    socket,
    "GET",
    "/shields/dotfiles.git/info/refs?service=git-upload-pack",
    Buffer.alloc(0),
    "github.com",
    {},
    agent,
  );
  await within("the request to be parsed", held.parsed);
  const stopping = gateway.stop();
  held.deliver();
  const response = await within("the response", answered);
  assert.equal(response.status, 200);
  assert.equal(response.headers.connection, "close");
  await within("stop()", stopping);
});

test("stop() waits for a request that is dispatched after it closed the idle connections, so its audit row is written before the database closes", async () => {
  const { socket, gateway, state, instance, auth, listener, closed } =
    await startGateway();
  const events: string[] = [];
  const asked = signal();
  const resume = signal();
  defer(resume.resolve);
  spyOn(auth, "token").mockImplementation(async (): Promise<string> => {
    asked.resolve();
    await resume.promise;
    events.push("token");
    return "ghu_secret";
  });
  const held = holdRequests(listener);
  const { request, response } = open(socket);
  const hungUp = assert.rejects(response, { message: "socket hang up" });
  await within("the request to be parsed", held.parsed);
  const audit = recordFinishes(state);
  const decisions = spyOn(state, "logDecision");
  const shutdown = (async () => {
    await gateway.stop();
    events.push("stopped");
    const rows = auditRows(state, instance);
    state.close();
    return rows;
  })();
  await nextTurn();
  held.deliver();
  await within("the gateway to ask for a token", asked.promise);
  const closing = once(listener, "close");
  request.destroy();
  await within("the client's request to fail", hungUp);
  await within("the gateway's response to close", closed());
  await within("the listener to close", closing);
  // A stop() that does not wait for requests has resolved by now.
  await nextTurn();
  resume.resolve();
  const rows = await within("the gateway to stop", shutdown);
  // Yield so a late write to the closed database is reported while this test
  // is still running.
  await nextTurn();
  assert.deepEqual(
    decisions.mock.results.map(({ type }) => type),
    ["return"],
  );
  assert.deepEqual(events, ["token", "stopped"]);
  assert.deepEqual(rows, [{ outcome: "interrupted", upstreamStatus: null }]);
  assert.deepEqual(audit.calls, [{ status: null, outcome: "interrupted" }]);
});

test("stop() does not sweep a server that had no request in flight, because a second sweep resets a connection still sending its last response", async () => {
  const { gateway, listener } = await startGateway();
  const sweep = spyOn(listener, "closeIdleConnections");
  await gateway.stop();
  assert.equal(sweep.mock.calls.length, 0);
});

test("stop() ends a refused upload that a disabled VM never finishes, without waiting for it", async () => {
  const { socket, gateway, state } = await startGateway();
  const client = tlsConnect({
    path: socket,
    servername: "github.com",
    rejectUnauthorized: false,
  });
  defer(() => {
    client.destroy();
  });
  const failures: Error[] = [];
  client.on("error", (error) => {
    failures.push(error);
  });
  const refused = signal();
  client.once("data", refused.resolve);
  client.write(
    "POST /shields/dotfiles.git/git-receive-pack HTTP/1.1\r\nhost: example.invalid\r\ncontent-length: 1000\r\n\r\nx",
  );
  await within("the refusal", refused.promise);
  state.disable("lima:work", "tester");
  await gateway.refresh();
  await within("stop()", gateway.stop());
});

test("stop() leaves alone a disabled VM's server that has already closed", async () => {
  const { gateway, state, listener } = await startGateway();
  const closed = once(listener, "close");
  state.disable("lima:work", "tester");
  await gateway.refresh();
  await within("the listener to close", closed);
  await nextTurn();
  const cut = spyOn(listener, "closeAllConnections");
  await gateway.stop();
  assert.equal(cut.mock.calls.length, 0);
});

test("start() rejects only after the servers it opened have closed", async () => {
  const { gateway, created } = unstartedGateway([
    "first.sock",
    "gone/second.sock",
  ]);
  const closing = signal();
  const lingering = signal();
  defer(lingering.resolve);
  created.mockImplementationOnce(((
    ...args: Parameters<typeof https.createServer>
  ) => {
    const server = createHttpsServer(...args);
    const close = server.close.bind(server);
    server.close = (...rest: Parameters<typeof server.close>) => {
      closing.resolve();
      return close(...rest);
    };
    holdEvent(server, "close", lingering.promise);
    return server;
  }) as typeof https.createServer);
  let rejected = false;
  const started = (async () => {
    await assert.rejects(gateway.start(), { syscall: "listen" });
    rejected = true;
  })();
  await within("the first server to be closed", closing.promise);
  await nextTurn();
  await nextTurn();
  assert.equal(rejected, false);
  lingering.resolve();
  await within("start() to reject", started);
});

test("stop() cancels the periodic listener refresh", async () => {
  const intervals = mockIntervals();
  const { gateway } = await startGateway();
  const refresh = spyOn(gateway, "refresh");
  intervals.tick(60_000);
  await nextTurn();
  assert.equal(refresh.mock.calls.length, 1);
  await gateway.stop();
  intervals.tick(60_000);
  await nextTurn();
  assert.equal(refresh.mock.calls.length, 1);
});
