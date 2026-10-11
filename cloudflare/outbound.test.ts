import assert from "node:assert/strict";
import test from "node:test";
import { authorize, type Classification } from "../gateway/policy.ts";
import { handleOutbound, type OutboundAuthority } from "./outbound.ts";

const instanceId = "01234567-89ab-4cde-8fab-0123456789ab";

function authority(events: string[]): OutboundAuthority {
  return {
    allow(_id: string, classification: Classification) {
      events.push(`allow:${classification.operation}`);
      authorize(
        classification.repository === "shields/dotfiles"
          ? { read: true, pushTaskBranches: true }
          : null,
        classification.operation,
      );
      return Promise.resolve({
        requestId: "request-1",
        token: "upstream-secret",
      });
    },
    deny(_id: string, _classification: Classification | null, reason: string) {
      events.push(`deny:${reason}`);
      return Promise.resolve("request-1");
    },
    finish(
      _requestId: string,
      status: number | null,
      _durationMs: number,
      outcome: "completed" | "gateway-error" | "denied",
    ) {
      events.push(`finish:${outcome}:${String(status)}`);
      return Promise.resolve();
    },
  };
}

function push(destination: string): Uint8Array {
  const line = `${"1".repeat(40)} ${"2".repeat(40)} ${destination}\0report-status\n`;
  const encoder = new TextEncoder();
  const packet = encoder.encode(line);
  const prefix = encoder.encode(
    (packet.length + 4).toString(16).padStart(4, "0"),
  );
  const suffix = encoder.encode("0000PACK");
  const body = new Uint8Array(prefix.length + packet.length + suffix.length);
  body.set(prefix);
  body.set(packet, prefix.length);
  body.set(suffix, prefix.length + packet.length);
  return body;
}

void test("GitHub reads inject a token without forwarding guest credentials", async () => {
  const events: string[] = [];
  let authorization: string | null = null;
  let cookie: string | null = null;
  const response = await handleOutbound(
    new Request("https://api.github.com/repos/shields/dotfiles/pulls", {
      headers: { authorization: "Bearer guest-token", cookie: "session=guest" },
    }),
    instanceId,
    authority(events),
    (request) => {
      authorization = request.headers.get("authorization");
      cookie = request.headers.get("cookie");
      return Promise.resolve(
        new Response("[]", {
          status: 200,
          headers: { "set-cookie": "upstream=secret" },
        }),
      );
    },
  );
  assert.equal(authorization, "Bearer upstream-secret");
  assert.equal(cookie, null);
  assert.equal(response.headers.get("set-cookie"), null);
  assert.deepEqual(events, ["allow:api.read", "finish:completed:200"]);
});

void test("task pushes stream after their commands are checked", async () => {
  const events: string[] = [];
  const body = push(`refs/heads/task/${instanceId}/check`);
  let authorization: string | null = null;
  let forwardedBody: Uint8Array | null = null;
  const response = await handleOutbound(
    new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
      method: "POST",
      body,
      headers: { authorization: "Basic Z3Vlc3Q6Z3Vlc3Q=" },
    }),
    instanceId,
    authority(events),
    async (request) => {
      authorization = request.headers.get("authorization");
      forwardedBody = new Uint8Array(await request.arrayBuffer());
      return new Response("ok", { status: 200 });
    },
  );
  assert.equal(response.status, 200, JSON.stringify(events));
  assert.equal(
    authorization,
    `Basic ${btoa("x-access-token:upstream-secret")}`,
  );
  assert.deepEqual(forwardedBody, body);
  assert.deepEqual(events, ["allow:git.push", "finish:completed:200"]);
});

void test("unsupported writes and foreign refs are denied before forwarding", async () => {
  for (const request of [
    new Request("https://api.github.com/repos/shields/dotfiles/pulls", {
      method: "POST",
      body: "{}",
    }),
    new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
      method: "POST",
      body: push("refs/heads/main"),
    }),
  ]) {
    const events: string[] = [];
    const response = await handleOutbound(
      request,
      instanceId,
      authority(events),
      () => {
        throw new Error("denied request reached GitHub");
      },
    );
    assert.equal(response.status, 403);
    assert.match(events[0] ?? "", /^deny:/);
    assert.equal(events[1], "finish:denied:null");
  }
});

void test("GraphQL reads retain their body and mutations are denied", async () => {
  const events: string[] = [];
  const query = JSON.stringify({
    query: '{ repository(owner: "shields", name: "dotfiles") { name } }',
  });
  let forwardedBody: string | null = null;
  const allowed = await handleOutbound(
    new Request("https://api.github.com/graphql", {
      method: "POST",
      body: query,
    }),
    instanceId,
    authority(events),
    async (request) => {
      forwardedBody = await request.text();
      return Response.json({ data: { repository: { name: "dotfiles" } } });
    },
  );
  assert.equal(allowed.status, 200);
  assert.equal(forwardedBody, query);
  assert.deepEqual(events, ["allow:graphql.read", "finish:completed:200"]);

  const denied = await handleOutbound(
    new Request("https://api.github.com/graphql", {
      method: "POST",
      body: JSON.stringify({
        query: "mutation { createIssue(input: {}) { clientMutationId } }",
      }),
    }),
    instanceId,
    authority([]),
    () => {
      throw new Error("mutation reached GitHub");
    },
  );
  assert.equal(denied.status, 403);
});

void test("GitHub over HTTP is denied", async () => {
  const response = await handleOutbound(
    new Request(
      "http://github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
    ),
    instanceId,
    authority([]),
    () => {
      throw new Error("insecure request reached GitHub");
    },
  );
  assert.equal(response.status, 403);
});

void test("oversized GraphQL bodies are denied before forwarding", async () => {
  const response = await handleOutbound(
    new Request("https://api.github.com/graphql", {
      method: "POST",
      body: "x".repeat(1_048_577),
    }),
    instanceId,
    authority([]),
    () => {
      throw new Error("oversized request reached GitHub");
    },
  );
  assert.equal(response.status, 403);
});

void test("audit failure stops forwarding", async () => {
  const unavailable: OutboundAuthority = {
    allow() {
      return Promise.reject(new Error("audit unavailable"));
    },
    deny() {
      return Promise.reject(new Error("audit unavailable"));
    },
    finish() {
      return Promise.reject(new Error("audit unavailable"));
    },
  };
  const response = await handleOutbound(
    new Request("https://api.github.com/repos/shields/dotfiles"),
    instanceId,
    unavailable,
    () => {
      throw new Error("audit failure reached GitHub");
    },
  );
  assert.equal(response.status, 503);
});

void test("a missing repository token stops forwarding", async () => {
  const events: string[] = [];
  const missing = authority(events);
  missing.allow = () =>
    Promise.resolve({ requestId: "request-1", token: null });
  const response = await handleOutbound(
    new Request("https://api.github.com/repos/shields/dotfiles"),
    instanceId,
    missing,
    () => {
      throw new Error("request without token reached GitHub");
    },
  );
  assert.equal(response.status, 503);
  assert.deepEqual(events, ["finish:gateway-error:null"]);
});

void test("public release downloads receive no repository token", async () => {
  let authorization: string | null = null;
  const response = await handleOutbound(
    new Request(
      "https://github.com/shields/dotfiles/releases/download/v1/archive.zip",
    ),
    instanceId,
    authority([]),
    (request) => {
      authorization = request.headers.get("authorization");
      return Promise.resolve(new Response("release"));
    },
  );
  assert.equal(response.status, 200);
  assert.equal(authorization, null);
});
