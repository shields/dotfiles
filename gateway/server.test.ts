import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createServer } from "node:http";
import https from "node:https";
import { mkdtempSync, mkdirSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { AppAuth } from "./auth.ts";
import { Gateway } from "./server.ts";
import { State } from "./state.ts";

const idRef = (id: string): string => `refs/heads/task/${id}/one`;

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
  body = Buffer.alloc(0),
  host = "github.com",
): Promise<{ status: number; body: string }> {
  return new Promise((resolve, reject) => {
    const request = https.request(
      {
        socketPath: socket,
        servername: host,
        rejectUnauthorized: false,
        method,
        path: target,
        agent: false,
        headers: { host, "content-length": body.length },
      },
      (response) => {
        const chunks: Buffer[] = [];
        response.on("data", (chunk: Buffer) => {
          chunks.push(chunk);
        });
        response.on("end", () => {
          resolve({
            status: response.statusCode ?? 0,
            body: Buffer.concat(chunks).toString(),
          });
        });
      },
    );
    request.once("error", reject);
    request.end(body);
  });
}

void test("the HTTPS socket injects a repo token only after checking the full push and live grant", async () => {
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
  const incoming: {
    target: string;
    authorization: string | undefined;
    body: Buffer;
  }[] = [];
  const upstream = createServer((request, response) => {
    const chunks: Buffer[] = [];
    request.on("data", (chunk: Buffer) => {
      chunks.push(chunk);
    });
    request.on("end", () => {
      incoming.push({
        target: request.url ?? "",
        authorization: request.headers.authorization,
        body: Buffer.concat(chunks),
      });
      response.writeHead(200).end("ok");
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
  try {
    await gateway.start();
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
    assert.equal(
      (await send(socket, "GET", "/user", Buffer.alloc(0), "api.github.com"))
        .status,
      200,
    );
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
    ]);
    assert.equal(
      (
        await send(
          socket,
          "POST",
          "/shields/dotfiles.git/git-receive-pack",
          bad,
        )
      ).status,
      403,
    );
    assert.equal(incoming.length, 4);
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
      403,
    );
    assert.equal(incoming.length, 4);
    const logs = state.logs(instance.id);
    assert.equal(logs.length, 8);
    assert.equal(logs[0]?.revision, 2);
    assert.equal(logs[0].allowed, false);
  } finally {
    await gateway.stop();
    await new Promise<void>((resolve) => {
      upstream.close(() => {
        resolve();
      });
    });
    state.close();
    rmSync(root, { recursive: true, force: true });
  }
});
