import assert from "node:assert/strict";
import { afterEach, beforeEach, mock, spyOn, test } from "bun:test";
import { gzipSync } from "node:zlib";
import { authorize, type Classification } from "../gateway/policy.ts";
import { handleOutbound, type OutboundAuthority } from "./outbound.ts";

const NativeRequest = Request;

// In workerd a request that arrives over HTTP keeps the client's spelling in
// request.url, copies of it keep that too, and a RequestInit dictionary cannot
// give a GET or HEAD a body. Bun does none of these, so these classes supply
// them.
class RawRequest extends NativeRequest {
  readonly #raw: string;
  constructor(raw: string, init?: RequestInit) {
    super("https://placeholder.invalid/", init);
    this.#raw = raw;
  }
  override get url(): string {
    return this.#raw;
  }
}

class CopyingRequest extends NativeRequest {
  readonly #raw: string | null;
  constructor(input: RequestInfo | URL, init?: RequestInit) {
    if (
      !(init instanceof NativeRequest) &&
      (init?.method === "GET" || init?.method === "HEAD") &&
      (init.body ?? null) !== null
    ) {
      throw new TypeError(
        "Request with a GET or HEAD method cannot have a body.",
      );
    }
    super(input, init);
    this.#raw = input instanceof NativeRequest ? input.url : null;
  }
  override get url(): string {
    return this.#raw ?? super.url;
  }
}

beforeEach(() => {
  Object.defineProperty(globalThis, "Request", { value: CopyingRequest });
});

afterEach(() => {
  mock.restore();
  Object.defineProperty(globalThis, "Request", { value: NativeRequest });
});

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

test("GitHub reads inject a token without forwarding guest credentials", async () => {
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

test("a response header that names trailers is not relayed", async () => {
  const response = await handleOutbound(
    new Request("https://api.github.com/repos/shields/dotfiles/pulls"),
    instanceId,
    authority([]),
    () =>
      Promise.resolve(
        new Response("[]", {
          headers: { trailer: "x-checksum", "x-ratelimit-limit": "5000" },
        }),
      ),
  );
  assert.deepEqual(
    [
      response.headers.get("trailer"),
      response.headers.get("x-ratelimit-limit"),
    ],
    [null, "5000"],
  );
});

test("task pushes and push probes are forwarded with the repository token", async () => {
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

test("a streamed push beyond the inspection limit is forwarded whole", async () => {
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

test("a push streamed in small writes costs work linear in its length", async () => {
  const encoder = new TextEncoder();
  const body = encoder.encode(
    `${Array.from({ length: 1200 }, (_, index) => {
      const line = `${"1".repeat(40)} ${"2".repeat(40)} refs/heads/task/${instanceId}/${String(index)}${index === 0 ? "\0report-status" : ""}\n`;
      return `${(line.length + 4).toString(16).padStart(4, "0")}${line}`;
    }).join("")}0000PACK`,
  );
  const chunks: Uint8Array[] = [];
  for (let offset = 0; offset < body.length; offset += 64) {
    chunks.push(body.subarray(offset, offset + 64));
  }
  const decode = spyOn(TextDecoder.prototype, "decode");
  const response = await handleOutbound(
    streamedPush(...chunks),
    instanceId,
    authority([]),
    async (request) => {
      await request.arrayBuffer();
      return new Response("ok", { status: 200 });
    },
  );
  assert.equal(response.status, 200);
  assert.ok(body.length > 100_000);
  assert.ok(
    decode.mock.calls.length < 5 * chunks.length,
    `${String(decode.mock.calls.length)} decodes for ${String(chunks.length)} chunks`,
  );
});

test("a push probe followed by more data is denied before forwarding", async () => {
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

test("gzipped fetch negotiation reaches GitHub with its Content-Encoding", async () => {
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

test("inspected bodies must be unencoded", async () => {
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

test("unsupported writes and foreign refs are denied before forwarding", async () => {
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

test("a path that begins with a second slash is denied before forwarding", async () => {
  for (const url of [
    "https://github.com//github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
    "https://api.github.com//api.github.com/repos/shields/dotfiles/pulls",
  ]) {
    const events: string[] = [];
    const response = await handleOutbound(
      new Request(url),
      instanceId,
      authority(events),
      () => {
        throw new Error("denied request reached GitHub");
      },
    );
    assert.equal(response.status, 403, url);
    assert.deepEqual(events, [
      "deny:request target must stay on its GitHub host",
      "finish:denied:null",
    ]);
  }
});

test("a denied push larger than the drain limit gets a plain denial", async () => {
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

test("a denied push whose body fails while it is being read out gets a plain denial", async () => {
  spyOn(console, "error").mockImplementation(mock());
  let pulled = 0;
  const response = await handleOutbound(
    new Request("https://github.com/shields/dotfiles.git/git-receive-pack", {
      method: "POST",
      body: new ReadableStream<Uint8Array>({
        pull(controller) {
          if (pulled === 0) {
            controller.enqueue(push("refs/heads/main"));
          } else {
            controller.error(new Error("client connection lost"));
          }
          pulled += 1;
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
});

test("GraphQL reads retain their body and mutations are denied", async () => {
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

test("a trailing dot on a GitHub host is denied, not forwarded", async () => {
  for (const url of [
    "https://github.com./shields/dotfiles.git/info/refs?service=git-upload-pack",
    "https://GITHUB.COM./x",
    "https://api.github.com./user",
    "https://github.com%2e/x",
  ]) {
    const events: string[] = [];
    const response = await handleOutbound(
      new Request(url, { headers: { authorization: "Bearer guest-secret" } }),
      instanceId,
      authority(events),
      () => {
        throw new Error(`${url} was forwarded`);
      },
    );
    assert.equal(response.status, 403, url);
    assert.equal(
      response.headers.get("content-type"),
      url.includes("/info/refs")
        ? "text/plain; charset=utf-8"
        : "application/json",
      url,
    );
    assert.deepEqual(
      events,
      ["deny:unsupported GitHub host", "finish:denied:null"],
      url,
    );
  }
  const insecure: string[] = [];
  const refused = await handleOutbound(
    new Request("http://github.com./x"),
    instanceId,
    authority(insecure),
    () => {
      throw new Error("an HTTP request to a dotted host was forwarded");
    },
  );
  assert.equal(refused.status, 403);
  assert.deepEqual(insecure, [
    "deny:GitHub requests must use HTTPS",
    "finish:denied:null",
  ]);
});

test("a host that is not github.com or api.github.com is forwarded without the policy", async () => {
  for (const url of [
    "https://some-other-host.example/x",
    "https://github.com.evil.example/x",
    "https://uploads.github.com/x",
  ]) {
    const events: string[] = [];
    const response = await handleOutbound(
      new Request(url),
      instanceId,
      authority(events),
      () => Promise.resolve(new Response("ok")),
    );
    assert.equal(response.status, 200, url);
    assert.deepEqual(events, [], url);
  }
});

test("the Worker buffers a body only for a route the policy reads", async () => {
  const oversized = new Uint8Array(1_048_577);
  for (const [url, reason] of [
    ["https://github.com/git-receive-pack", "unsupported GitHub web operation"],
    [
      "https://github.com/shields/git-receive-pack",
      "unsupported GitHub web operation",
    ],
    [
      "https://github.com/shields/dotfiles.git/git-receive-pack?x=1",
      "unsupported GitHub Git operation",
    ],
    ["https://api.github.com/graphql?x=1", "unsupported GitHub API operation"],
  ] as const) {
    const events: string[] = [];
    const response = await handleOutbound(
      new Request(url, { method: "POST", body: oversized }),
      instanceId,
      authority(events),
      () => {
        throw new Error(`${url} reached GitHub`);
      },
    );
    assert.equal(response.status, 403, url);
    assert.deepEqual(events, [`deny:${reason}`, "finish:denied:null"], url);
  }
});

test("GitHub over HTTP is denied", async () => {
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

test("oversized GraphQL bodies are denied before forwarding", async () => {
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

test("an inspected body is read only up to the inspection limit", async () => {
  let pulled = 0;
  const events: string[] = [];
  const response = await handleOutbound(
    new Request("https://api.github.com/graphql", {
      method: "POST",
      body: new ReadableStream<Uint8Array>({
        pull(controller) {
          if (pulled === 128) {
            controller.close();
            return;
          }
          controller.enqueue(new Uint8Array(65_536));
          pulled += 1;
        },
      }),
      duplex: "half",
    } as RequestInit),
    instanceId,
    authority(events),
    () => {
      throw new Error("oversized request reached GitHub");
    },
  );
  assert.equal(response.status, 403);
  assert.deepEqual(events, [
    "deny:request prefix is too large",
    "finish:denied:null",
  ]);
  assert.ok(pulled > 16 && pulled < 25, String(pulled));
});

test("audit failure stops forwarding, is logged, and the client sees only that the audit is unavailable", async () => {
  const logged = spyOn(console, "error").mockReturnValue();
  let forwarded = false;
  const unavailable: OutboundAuthority = {
    allow() {
      return Promise.reject(new Error("audit unavailable"));
    },
    deny() {
      return Promise.reject(new Error("deny unavailable"));
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
      forwarded = true;
      return Promise.resolve(new Response("[]"));
    },
  );
  assert.equal(forwarded, false);
  assert.equal(response.status, 503);
  assert.deepEqual(await response.json(), {
    message: "gateway audit unavailable",
  });
  assert.deepEqual(logged.mock.calls, [
    ["vm-access: gateway request failed", new Error("audit unavailable")],
    ["vm-access audit failure", new Error("deny unavailable")],
  ]);
});

test("a missing repository token stops forwarding and is logged", async () => {
  const logged = spyOn(console, "error").mockReturnValue();
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
  assert.deepEqual(logged.mock.calls, [
    [
      "vm-access: gateway request failed",
      new Error("repository authorization is unavailable"),
    ],
  ]);
});

test("public release downloads receive no repository token", async () => {
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

test("ungranted reads forward anonymously and preserve GitHub's private-repository denial", async () => {
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
      const original = new Request(url, {
        method,
        ...(body ? { body } : {}),
        headers: {
          authorization: "Bearer guest-token",
          cookie: "session=guest",
        },
      });
      const response = await handleOutbound(
        original,
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
      assert.equal(original.headers.get("authorization"), "Bearer guest-token");
      assert.equal(original.headers.get("cookie"), "session=guest");
    }
  }
});

test("anonymous access does not allow push discovery, pushes, or API writes", async () => {
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

test("pagination links name the repository so later pages are checked like the first", async () => {
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

test("the health path is answered locally on api.github.com and forwarded on github.com", async () => {
  const events: string[] = [];
  const health = await handleOutbound(
    new Request("https://api.github.com/__vm_access__/health"),
    instanceId,
    authority(events),
    () => {
      throw new Error("the health check reached GitHub");
    },
  );
  assert.equal(health.status, 200);
  assert.equal(await health.text(), "vm-access ready\n");
  assert.deepEqual(events, ["allow:public.read", "finish:completed:200"]);

  for (const method of ["GET", "HEAD"]) {
    const forwarded: string[] = [];
    const page = await handleOutbound(
      new Request("https://github.com/__vm_access__/health", { method }),
      instanceId,
      authority([]),
      (request) => {
        forwarded.push(request.url);
        assert.equal(request.method, method);
        return Promise.resolve(new Response(null, { status: 404 }));
      },
    );
    assert.equal(page.status, 404, method);
    assert.deepEqual(
      forwarded,
      ["https://github.com/__vm_access__/health"],
      method,
    );
  }
});

test("a GitHub request is forwarded to the URL the policy classified", async () => {
  for (const raw of [
    "https://github.com/shields/dotfiles.git/git-receive-pack/%2e%2e/git-upload-pack",
    "https://github.com/shields/dotfiles.git/git-receive-pack/.%2E/git-upload-pack",
    "https://github.com/shields/dotfiles.git/git-receive-pack\\..\\git-upload-pack",
    "https://GITHUB.com:443/shields/dotfiles.git/git-receive-pack/%2e%2e/git-upload-pack",
  ]) {
    const events: string[] = [];
    const forwarded: string[] = [];
    let authorization: string | null = null;
    const response = await handleOutbound(
      new RawRequest(raw, {
        method: "POST",
        headers: { authorization: "Bearer guest-secret" },
        body: push("refs/heads/main"),
      }),
      instanceId,
      authority(events),
      (request) => {
        forwarded.push(request.url);
        authorization = request.headers.get("authorization");
        return Promise.resolve(new Response("ok"));
      },
    );
    assert.equal(response.status, 200, raw);
    assert.deepEqual(
      forwarded,
      ["https://github.com/shields/dotfiles.git/git-upload-pack"],
      raw,
    );
    assert.deepEqual(events, ["allow:git.read", "finish:completed:200"], raw);
    assert.equal(
      authorization,
      `Basic ${btoa("x-access-token:upstream-secret")}`,
      raw,
    );
  }
});

test("a GitHub GET or HEAD whose body is empty is forwarded without one", async () => {
  for (const [method, url] of [
    [
      "GET",
      "https://github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
    ],
    ["GET", "https://api.github.com/repos/shields/dotfiles/pulls"],
    ["HEAD", "https://api.github.com/repos/shields/dotfiles"],
  ] as const) {
    const bodies: (ReadableStream<Uint8Array> | null)[] = [];
    const response = await handleOutbound(
      new NativeRequest(url, {
        method,
        headers: { "content-length": "0" },
        body: "",
      }),
      instanceId,
      authority([]),
      (request) => {
        bodies.push(request.body);
        return Promise.resolve(new Response("ok"));
      },
    );
    assert.equal(response.status, 200, `${method} ${url}`);
    assert.deepEqual(bodies, [null], `${method} ${url}`);
  }
});

test("a GitHub GET or HEAD that announces a body is denied, as the gateway denies it", async () => {
  const logged = spyOn(console, "error").mockReturnValue();
  for (const [method, headers] of [
    ["GET", { "content-length": "5" }],
    ["HEAD", { "content-length": "5" }],
    ["GET", { "transfer-encoding": "chunked" }],
  ] as const) {
    const events: string[] = [];
    const classified: (Classification | null)[] = [];
    const inner = authority(events);
    const recording: OutboundAuthority = {
      ...inner,
      deny(id, classification, reason) {
        classified.push(classification);
        return inner.deny(id, classification, reason);
      },
    };
    const response = await handleOutbound(
      new NativeRequest("https://api.github.com/repos/shields/dotfiles/pulls", {
        method,
        headers,
        body: "hello",
      }),
      instanceId,
      recording,
      () => {
        throw new Error("a GET body reached GitHub");
      },
    );
    const label = `${method} ${JSON.stringify(headers)}`;
    assert.equal(response.status, 403, label);
    assert.deepEqual(
      events,
      ["deny:GET and HEAD requests must not have a body", "finish:denied:null"],
      label,
    );
    assert.deepEqual(
      classified,
      [{ repository: "shields/dotfiles", operation: "api.read", refs: [] }],
      label,
    );
  }
  assert.deepEqual(logged.mock.calls, []);
});

test("a URL that WHATWG and RFC 3986 read differently is forwarded only to the host the Worker parsed", async () => {
  for (const [raw, expected] of [
    [
      "https://evil.example\\@github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
      "https://evil.example/@github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
    ],
    [
      "https://evil.example\\@api.github.com/user",
      "https://evil.example/@api.github.com/user",
    ],
    ["https://example.com/a/%2e%2e/b", "https://example.com/b"],
  ] as const) {
    const events: string[] = [];
    const forwarded: string[] = [];
    let authorization: string | null = null;
    const response = await handleOutbound(
      new RawRequest(raw),
      instanceId,
      authority(events),
      (request) => {
        forwarded.push(request.url);
        authorization = request.headers.get("authorization");
        return Promise.resolve(new Response("ok"));
      },
    );
    assert.equal(response.status, 200, raw);
    assert.deepEqual(forwarded, [expected], raw);
    assert.equal(authorization, null, raw);
    assert.deepEqual(events, [], raw);
  }
});

test("a pass-through request whose URL is already canonical is forwarded as it arrived", async () => {
  for (const init of [
    { method: "GET" },
    { method: "GET", body: "" },
    { method: "GET", body: "query" },
    { method: "PUT", body: "payload" },
  ]) {
    const request = new NativeRequest("https://example.com/search?q=1", init);
    const forwarded: Request[] = [];
    await handleOutbound(request, instanceId, authority([]), (seen) => {
      forwarded.push(seen);
      return Promise.resolve(new Response("ok"));
    });
    assert.equal(forwarded.length, 1, init.method);
    assert.equal(forwarded[0], request, init.method);
  }
});

test("a pass-through request keeps its method, headers, body, and redirect mode when its URL is rebuilt", async () => {
  let seen: {
    url: string;
    method: string;
    type: string | null;
    authorization: string | null;
    redirect: string;
    body: string;
  } | null = null;
  await handleOutbound(
    new RawRequest("https://example.com/a/%2e%2e/upload?x=1", {
      method: "PUT",
      headers: { "content-type": "text/plain", authorization: "Bearer guest" },
      body: "payload",
      redirect: "manual",
    }),
    instanceId,
    authority([]),
    async (request) => {
      seen = {
        url: request.url,
        method: request.method,
        type: request.headers.get("content-type"),
        authorization: request.headers.get("authorization"),
        redirect: request.redirect,
        body: await request.text(),
      };
      return new Response("ok");
    },
  );
  assert.deepEqual(seen, {
    url: "https://example.com/upload?x=1",
    method: "PUT",
    type: "text/plain",
    authorization: "Bearer guest",
    redirect: "manual",
    body: "payload",
  });
});

test("a pass-through GET or HEAD keeps its body when its URL is rebuilt", async () => {
  for (const [method, body] of [
    ["GET", ""],
    ["GET", "query"],
    ["HEAD", ""],
    ["HEAD", "query"],
  ] as const) {
    const forwarded: { url: string; body: string }[] = [];
    const response = await handleOutbound(
      new RawRequest("https://example.com/a/%2e%2e/b", { method, body }),
      instanceId,
      authority([]),
      async (request) => {
        forwarded.push({ url: request.url, body: await request.text() });
        return new Response("ok");
      },
    );
    const label = `${method} ${JSON.stringify(body)}`;
    assert.equal(response.status, 200, label);
    assert.deepEqual(
      forwarded,
      [{ url: "https://example.com/b", body }],
      label,
    );
  }
});

test("URLs that a backslash, userinfo, or a port make look like GitHub stay denied", async () => {
  for (const [raw, reason] of [
    [
      "https://github.com\\@evil.example/shields/dotfiles.git/info/refs?service=git-upload-pack",
      "unsupported GitHub web operation",
    ],
    [
      "https://evil.example@github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
      "GitHub requests must use HTTPS",
    ],
    [
      "https://:secret@github.com/shields/dotfiles.git/info/refs?service=git-upload-pack",
      "GitHub requests must use HTTPS",
    ],
    [
      "https://github.com:8443/shields/dotfiles.git/info/refs?service=git-upload-pack",
      "GitHub requests must use HTTPS",
    ],
    ["https://evil.example@github.com", "GitHub requests must use HTTPS"],
  ] as const) {
    const events: string[] = [];
    const response = await handleOutbound(
      new RawRequest(raw),
      instanceId,
      authority(events),
      () => {
        throw new Error(`${raw} was forwarded`);
      },
    );
    assert.equal(response.status, 403, raw);
    assert.deepEqual(events, [`deny:${reason}`, "finish:denied:null"], raw);
  }
});
