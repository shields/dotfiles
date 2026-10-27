import assert from "node:assert/strict";
import { createServer } from "node:http";
import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { AppAuth } from "./auth.ts";

void test("device flow requests one repository and checks the resulting token", async (t) => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-device-"));
  const gh = path.join(directory, "gh");
  writeFileSync(gh, "#!/bin/sh\nprintf '4242\\n'\n");
  chmodSync(gh, 0o700);
  const originalPath = process.env.PATH;
  process.env.PATH = `${directory}:${originalPath ?? ""}`;
  let verifiedId = 4242;
  const requests: {
    pathname: string;
    repositoryId: string | null;
    authorization: string | null;
  }[] = [];
  const request: typeof fetch = (input, init) => {
    const url = new URL(
      input instanceof Request
        ? input.url
        : input instanceof URL
          ? input.href
          : input,
    );
    const form =
      init?.body instanceof URLSearchParams ? init.body : new URLSearchParams();
    const headers = new Headers(init?.headers);
    requests.push({
      pathname: url.pathname,
      repositoryId: form.get("repository_id"),
      authorization: headers.get("authorization"),
    });
    const payload =
      url.pathname === "/login/device/code"
        ? {
            device_code: "device-code",
            user_code: "USER-CODE",
            verification_uri: "https://github.com/login/device",
            expires_in: 900,
            interval: 0,
          }
        : url.pathname === "/login/oauth/access_token"
          ? {
              access_token: "ghu_scoped",
              expires_in: 3600,
              refresh_token: "ghr_scoped",
              refresh_token_expires_in: 86_400,
            }
          : { id: verifiedId, full_name: "shields/dotfiles" };
    return Promise.resolve(Response.json(payload));
  };
  t.mock.method(globalThis, "fetch", request);
  try {
    const auth = new AppAuth(path.join(directory, "secrets"));
    const issued = await auth.issue("shields/dotfiles");
    assert.equal(issued.access_token, "ghu_scoped");
    assert.equal(auth.has("shields/dotfiles"), false);
    requests.length = 0;
    await auth.authorize("shields/dotfiles");
    assert.equal(auth.load("shields/dotfiles").access_token, "ghu_scoped");
    assert.deepEqual(
      requests.map((request) => request.pathname),
      [
        "/login/device/code",
        "/login/oauth/access_token",
        "/repos/shields/dotfiles",
      ],
    );
    assert.equal(requests[1]?.repositoryId, "4242");
    assert.equal(requests[2]?.authorization, "Bearer ghu_scoped");
    verifiedId = 77;
    await assert.rejects(
      auth.authorize("shields/dotfiles"),
      /wrong repository/,
    );
    assert.equal(auth.load("shields/dotfiles").access_token, "ghu_scoped");
  } finally {
    process.env.PATH = originalPath;
    rmSync(directory, { recursive: true, force: true });
  }
});

void test("concurrent callers spend one refresh token and persist its replacement", async () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-auth-"));
  let refreshes = 0;
  const upstream = createServer((request, response) => {
    refreshes++;
    const chunks: Buffer[] = [];
    request.on("data", (chunk: Buffer) => {
      chunks.push(chunk);
    });
    request.on("end", () => {
      assert.equal(
        new URLSearchParams(Buffer.concat(chunks).toString()).get(
          "refresh_token",
        ),
        "ghr_old",
      );
      setTimeout(() => {
        response.writeHead(200, { "content-type": "application/json" }).end(
          JSON.stringify({
            access_token: "ghu_new",
            expires_in: 3600,
            refresh_token: "ghr_new",
            refresh_token_expires_in: 86_400,
          }),
        );
      }, 40);
    });
  });
  await new Promise<void>((resolve) => {
    upstream.listen(0, "127.0.0.1", resolve);
  });
  const address = upstream.address();
  assert.ok(address && typeof address !== "string");
  const repository = "shields/dotfiles";
  const file = path.join(
    directory,
    `${Buffer.from(repository).toString("base64url")}.json`,
  );
  const now = Math.floor(Date.now() / 1000);
  writeFileSync(
    file,
    JSON.stringify({
      client_id: "Iv-test",
      repository,
      access_token: "ghu_old",
      access_expires_at: now + 1,
      refresh_token: "ghr_old",
      refresh_expires_at: now + 86_400,
      web_url: `http://127.0.0.1:${String(address.port)}`,
      api_url: "https://api.github.com",
    }),
  );
  try {
    const first = new AppAuth(directory);
    const second = new AppAuth(directory);
    assert.deepEqual(
      await Promise.all([first.token(repository), second.token(repository)]),
      ["ghu_new", "ghu_new"],
    );
    assert.equal(refreshes, 1);
    assert.equal(first.load(repository).refresh_token, "ghr_new");
  } finally {
    await new Promise<void>((resolve) => {
      upstream.close(() => {
        resolve();
      });
    });
    rmSync(directory, { recursive: true, force: true });
  }
});
