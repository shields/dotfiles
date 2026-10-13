import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createServer, type IncomingHttpHeaders } from "node:http";
import https from "node:https";
import { mkdtempSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
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
        agent: false,
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

async function startGateway(): Promise<{
  socket: string;
  instance: Instance;
  state: State;
  incoming: Forwarded[];
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
  const upstream = createServer((request, response) => {
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
  });
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
  const gateway = new Gateway(state, auth, {
    cert: certificate,
    key,
    adminSocket: path.join(root, "admin.sock"),
    webUpstream: `http://127.0.0.1:${String(address.port)}`,
    apiUpstream: `http://127.0.0.1:${String(address.port)}`,
  });
  const stop = async (): Promise<void> => {
    await gateway.stop();
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
  } catch (error) {
    await stop();
    throw error;
  }
  return { socket, instance, state, incoming, stop };
}

void test("the HTTPS socket injects a repo token only after checking the full push and live grant", async (t) => {
  const listeners = t.mock.method(https, "createServer");
  const { socket, instance, state, incoming, stop } = await startGateway();
  try {
    const listener = listeners.mock.calls[0]?.result;
    assert.ok(listener);
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
            "ng refs/heads/main push destination is outside this VM's task namespace\n",
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
    assert.match(
      unreported.body,
      /^push destination is outside this VM's task namespace\nrequest ID: [0-9a-f-]{36}\n$/,
    );
    assert.equal(incoming.length, 4);
    for (const log of state.logs(instance.id).slice(0, 2)) {
      assert.equal(log.allowed, false);
      assert.equal(
        log.reason,
        "push destination is outside this VM's task namespace",
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
