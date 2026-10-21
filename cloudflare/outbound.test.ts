import assert from "node:assert/strict";
import test from "node:test";
import { gzipSync } from "node:zlib";
import { authorize, type Classification } from "../gateway/policy.ts";
import { handleOutbound, type OutboundAuthority } from "./outbound.ts";

const instanceId = "01234567-89ab-4cde-8fab-0123456789ab";

function authority(events: string[]): OutboundAuthority {
  return {
    allow(_id: string, classification: Classification) {
      events.push(`allow:${classification.operation}`);
      const access = authorize(
        classification.repository === "shields/dotfiles"
          ? { read: true, pushTaskBranches: true }
          : null,
        classification.operation,
      );
      return Promise.resolve({
        requestId: "request-1",
        token: access === "authenticated" ? "upstream-secret" : null,
        access,
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

function push(
  destination: string,
  {
    capabilities = "report-status",
    shallow = [],
  }: { capabilities?: string; shallow?: string[] } = {},
): Uint8Array<ArrayBuffer> {
  const encoder = new TextEncoder();
  const packets = [
    ...shallow.map((oid) => `shallow ${oid}\n`),
    `${"1".repeat(40)} ${"2".repeat(40)} ${destination}\0${capabilities}\n`,
  ].map(
    (line) =>
      `${(encoder.encode(line).length + 4).toString(16).padStart(4, "0")}${line}`,
  );
  return encoder.encode(`${packets.join("")}0000PACK`);
}

function streamedPush(...chunks: Uint8Array[]): Request {
  const init: RequestInit & { duplex: "half" } = {
    method: "POST",
    body: new ReadableStream({
      start(controller) {
        for (const chunk of chunks) {
          controller.enqueue(chunk);
        }
        controller.close();
      },
    }),
    duplex: "half",
  };
  return new Request(
    "https://github.com/shields/dotfiles.git/git-receive-pack",
    init,
  );
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

void test("task pushes and push probes are forwarded with the repository token", async () => {
  for (const body of [
    push(`refs/heads/task/${instanceId}/check`),
    push(`refs/heads/task/${instanceId}/check`, {
      shallow: ["3".repeat(40), "4".repeat(40)],
    }),
    new TextEncoder().encode("0000"),
  ]) {
    const events: string[] = [];
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
  }
});

void test("a streamed push beyond the inspection limit is forwarded whole", async () => {
  for (const commands of [
    push(`refs/heads/task/${instanceId}/check`),
    push(`refs/heads/task/${instanceId}/check`, { shallow: ["3".repeat(40)] }),
  ]) {
    const body = new Uint8Array(commands.length + 2 * 1_048_576);
    body.set(commands);
    let forwarded = new Uint8Array();
    const response = await handleOutbound(
      streamedPush(body.subarray(0, 20), body.subarray(20)),
      instanceId,
      authority([]),
      async (request) => {
        forwarded = new Uint8Array(await request.arrayBuffer());
        return new Response(null, { status: 200 });
      },
    );
    assert.equal(response.status, 200);
    assert.equal(forwarded.length, body.length);
    assert.ok(forwarded.every((byte, index) => byte === body[index]));
  }
});

void test("a push probe followed by more data is denied before forwarding", async () => {
  const events: string[] = [];
  const response = await handleOutbound(
    streamedPush(
      new TextEncoder().encode("0000"),
      push(`refs/heads/task/${instanceId}/check`),
    ),
    instanceId,
    authority(events),
    () => {
      throw new Error("an uninspected push reached GitHub");
    },
  );
  assert.equal(response.status, 403);
  assert.deepEqual(events, [
    "deny:Git push has no ref updates",
    "finish:denied:null",
  ]);
});

void test("gzipped fetch negotiation reaches GitHub with its Content-Encoding", async () => {
  const events: string[] = [];
  const body = new Uint8Array(gzipSync(`0032want ${"1".repeat(40)}\n0000`));
  let encoding: string | null = null;
  let forwardedBody: Uint8Array | null = null;
  const response = await handleOutbound(
    new Request("https://github.com/shields/dotfiles.git/git-upload-pack", {
      method: "POST",
      body,
      headers: { "content-encoding": "gzip" },
    }),
    instanceId,
    authority(events),
    async (request) => {
      encoding = request.headers.get("content-encoding");
      forwardedBody = new Uint8Array(await request.arrayBuffer());
      return new Response("ok", { status: 200 });
    },
  );
  assert.equal(response.status, 200, JSON.stringify(events));
  assert.equal(encoding, "gzip");
  assert.deepEqual(forwardedBody, body);
  assert.deepEqual(events, ["allow:git.read", "finish:completed:200"]);
});

void test("inspected bodies must be unencoded", async () => {
  const body = push(`refs/heads/task/${instanceId}/check`);
  const query = JSON.stringify({
    query: '{ repository(owner: "shields", name: "dotfiles") { name } }',
  });
  for (const [url, payload, encoding] of [
    ["https://github.com/shields/dotfiles.git/git-receive-pack", body, "gzip"],
    [
      "https://github.com/shields/dotfiles.git/git-receive-pack",
      new Uint8Array(gzipSync(body)),
      "gzip",
    ],
    ["https://github.com/shields/dotfiles.git/git-receive-pack", body, ""],
    ["https://api.github.com/graphql", query, "br"],
    ["https://api.github.com/graphql", query, "identity, gzip"],
  ] as const) {
    const events: string[] = [];
    const response = await handleOutbound(
      new Request(url, {
        method: "POST",
        body: payload,
        headers: { "content-encoding": encoding },
      }),
      instanceId,
      authority(events),
      () => {
        throw new Error("encoded body reached GitHub");
      },
    );
    assert.equal(response.status, 403, `${url} ${encoding}`);
    assert.deepEqual(events, [
      "deny:encoded request bodies are unsupported",
      "finish:denied:null",
    ]);
  }

  for (const [url, payload, encoding] of [
    [
      "https://github.com/shields/dotfiles.git/git-receive-pack",
      body,
      "identity",
    ],
    ["https://api.github.com/graphql", query, "Identity"],
  ] as const) {
    const response = await handleOutbound(
      new Request(url, {
        method: "POST",
        body: payload,
        headers: { "content-encoding": encoding },
      }),
      instanceId,
      authority([]),
      async (request) => {
        assert.equal(request.headers.get("content-encoding"), null, url);
        assert.deepEqual(
          new Uint8Array(await request.arrayBuffer()),
          typeof payload === "string"
            ? new TextEncoder().encode(payload)
            : payload,
          url,
        );
        return new Response("ok", { status: 200 });
      },
    );
    assert.equal(response.status, 200, url);
  }
});

void test("unsupported writes and foreign refs are denied before forwarding", async () => {
  const pushed = push("refs/heads/main");
  let drained = 0;
  for (const [request, status, type, body] of [
    [
      new Request("https://api.github.com/repos/shields/dotfiles/pulls", {
        method: "POST",
        body: "{}",
      }),
      403,
      "application/json",
      '{"message":"unsupported GitHub API operation","request_id":"request-1"}',
    ],
    [
      new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
        method: "POST",
        body: new ReadableStream<Uint8Array>({
          pull(controller) {
            if (drained === 0) {
              controller.enqueue(pushed);
            } else if (drained <= 4) {
              controller.enqueue(new Uint8Array(1_048_576));
            } else {
              controller.close();
            }
            drained += 1;
          },
        }),
        duplex: "half",
      } as RequestInit),
      200,
      "application/x-git-receive-pack-result",
      `000eunpack ok\n008cng refs/heads/main push destination is outside this VM's task namespace; allowed namespace: task/${instanceId}/*\n0000`,
    ],
    [
      new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
        method: "POST",
        body: push("refs/heads/main", { capabilities: "quiet" }),
      }),
      403,
      "text/plain; charset=utf-8",
      `push destination is outside this VM's task namespace; allowed namespace: task/${instanceId}/*\nrequest ID: request-1\n`,
    ],
    [
      new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
        method: "POST",
        body: "0000PACK",
      }),
      403,
      "text/plain; charset=utf-8",
      "Git push has no ref updates\nrequest ID: request-1\n",
    ],
  ] as const) {
    const events: string[] = [];
    const response = await handleOutbound(
      request,
      instanceId,
      authority(events),
      () => {
        throw new Error("denied request reached GitHub");
      },
    );
    assert.equal(response.status, status);
    assert.equal(response.headers.get("content-type"), type);
    assert.equal(await response.text(), body);
    assert.match(events[0] ?? "", /^deny:/);
    assert.equal(events[1], "finish:denied:null");
  }
  assert.equal(drained, 6);
});

void test("a denied push larger than the drain limit gets a plain denial", async () => {
  let pulled = 0;
  let cancelled = false;
  const response = await handleOutbound(
    new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
      method: "POST",
      body: new ReadableStream<Uint8Array>({
        pull(controller) {
          controller.enqueue(
            pulled === 0 ? push("refs/heads/main") : new Uint8Array(1_048_576),
          );
          pulled += 1;
        },
        cancel() {
          cancelled = true;
        },
      }),
      duplex: "half",
    } as RequestInit),
    instanceId,
    authority([]),
    () => {
      throw new Error("denied request reached GitHub");
    },
  );
  assert.equal(response.status, 403);
  assert.match(await response.text(), /^push destination is outside/);
  assert.ok(pulled > 64 && pulled < 80, String(pulled));
  assert.ok(cancelled);
});

void test("GraphQL reads retain their body and mutations are denied", async () => {
  const events: string[] = [];
  const query = JSON.stringify({
    query:
      '{ viewer { login } repo_000: repository(owner: "shields", name: "dotfiles") { name } }',
    variables: null,
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
    Promise.resolve({
      requestId: "request-1",
      token: null,
      access: "authenticated",
    });
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

void test("ungranted reads forward anonymously and preserve GitHub's private-repository denial", async () => {
  for (const repository of [
    "shields/public",
    "octocat/Hello-World",
    "shields/private",
  ]) {
    for (const [method, url, body] of [
      [
        "GET",
        `https://github.com/${repository}.git/info/refs?service=git-upload-pack`,
        undefined,
      ],
      ["POST", `https://github.com/${repository}.git/git-upload-pack`, "0000"],
      [
        "GET",
        `https://github.com/${repository}/info/refs?service=git-upload-pack`,
        undefined,
      ],
      ["POST", `https://github.com/${repository}/git-upload-pack`, "0000"],
      ["GET", `https://api.github.com/repos/${repository}/pulls`, undefined],
      ["HEAD", `https://api.github.com/repos/${repository}`, undefined],
      ["GET", `https://github.com/${repository}`, undefined],
      [
        "HEAD",
        `https://github.com/${repository}/blob/main/README.md`,
        undefined,
      ],
    ] as const) {
      const events: string[] = [];
      const status = repository === "shields/private" ? 404 : 200;
      const response = await handleOutbound(
        new Request(url, {
          method,
          ...(body ? { body } : {}),
          headers: {
            authorization: "Bearer guest-token",
            cookie: "session=guest",
          },
        }),
        instanceId,
        authority(events),
        async (request) => {
          assert.equal(request.headers.get("authorization"), null, url);
          assert.equal(request.headers.get("cookie"), null, url);
          assert.equal(request.redirect, "manual", url);
          assert.equal(await request.text(), body ?? "", url);
          return new Response(null, { status });
        },
      );
      assert.equal(response.status, status, url);
      assert.equal(events.at(-1), `finish:completed:${String(status)}`);
    }
  }
});

void test("anonymous access does not allow push discovery, pushes, or API writes", async () => {
  for (const request of [
    new Request(
      "https://github.com/shields/public.git/info/refs?service=git-receive-pack",
    ),
    new Request("https://github.com/shields/public/git-receive-pack", {
      method: "POST",
      body: push(`refs/heads/task/${instanceId}/check`),
    }),
    new Request("https://github.com/shields/public/git-receive-pack", {
      method: "POST",
      body: "0000",
    }),
    new Request("https://api.github.com/repos/shields/public/issues", {
      method: "POST",
      body: "{}",
    }),
    new Request("https://api.github.com/graphql", {
      method: "POST",
      body: JSON.stringify({
        query: '{repository(owner:"shields",name:"public"){name}}',
      }),
    }),
  ]) {
    const response = await handleOutbound(
      request,
      instanceId,
      authority([]),
      () => {
        throw new Error("ungranted write or GraphQL request reached GitHub");
      },
    );
    assert.equal(response.status, 403, request.url);
  }
});

void test("pagination links name the repository so later pages are checked like the first", async () => {
  const link = (repository: string): string =>
    `<https://api.github.com/${repository}/actions/runs?per_page=1&page=2>; rel="next", <https://api.github.com/${repository}/actions/runs?per_page=1&page=9>; rel="last"`;
  for (const [requested, repository, authorization] of [
    ["Shields/Dotfiles", "shields/dotfiles", "Bearer upstream-secret"],
    ["octocat/Hello-World", "octocat/hello-world", null],
  ] as const) {
    const events: string[] = [];
    const forwarded: { url: string; authorization: string | null }[] = [];
    const get = (url: string): Promise<Response> =>
      handleOutbound(
        new Request(url),
        instanceId,
        authority(events),
        (request) => {
          forwarded.push({
            url: request.url,
            authorization: request.headers.get("authorization"),
          });
          return Promise.resolve(
            new Response("[]", { headers: { link: link("repositories/42") } }),
          );
        },
      );
    const first = await get(
      `https://api.github.com/repos/${requested}/actions/runs?per_page=1`,
    );
    assert.equal(first.headers.get("link"), link(`repos/${repository}`));
    const next = /<([^>]+)>; rel="next"/.exec(
      first.headers.get("link") ?? "",
    )?.[1];
    assert.equal(
      next,
      `https://api.github.com/repos/${repository}/actions/runs?per_page=1&page=2`,
    );
    assert.equal((await get(next)).status, 200);
    assert.deepEqual(forwarded, [
      {
        url: `https://api.github.com/repos/${requested}/actions/runs?per_page=1`,
        authorization,
      },
      { url: next, authorization },
    ]);
    assert.deepEqual(events, [
      "allow:api.read",
      "finish:completed:200",
      "allow:api.read",
      "finish:completed:200",
    ]);
  }

  const moved = await handleOutbound(
    new Request("https://api.github.com/repos/octocat/old-name/pulls"),
    instanceId,
    authority([]),
    () =>
      Promise.resolve(
        new Response(null, {
          status: 301,
          headers: { location: "https://api.github.com/repositories/42/pulls" },
        }),
      ),
  );
  assert.equal(
    moved.headers.get("location"),
    "https://api.github.com/repositories/42/pulls",
  );
  const followed = await handleOutbound(
    new Request("https://api.github.com/repositories/42/pulls"),
    instanceId,
    authority([]),
    () => {
      throw new Error("a repository ID request reached GitHub");
    },
  );
  assert.equal(followed.status, 403);
});
