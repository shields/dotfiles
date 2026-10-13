import {
  authorize,
  classify,
  parsePushCommands,
  PolicyError,
  requireIdentityEncoding,
  type Access,
  type Classification,
} from "../gateway/policy.ts";

export interface OutboundAuthority {
  allow(
    instanceId: string,
    classification: Classification,
  ): Promise<{ requestId: string; token: string | null; access: Access }>;
  deny(
    instanceId: string,
    classification: Classification | null,
    reason: string,
  ): Promise<string>;
  finish(
    requestId: string,
    status: number | null,
    durationMs: number,
    outcome: "completed" | "gateway-error" | "denied",
  ): Promise<void>;
}

const forwardedHeaders = [
  "accept",
  "content-type",
  "user-agent",
  "git-protocol",
  "if-none-match",
  "range",
  "x-github-api-version",
] as const;
const blockedResponseHeaders = new Set([
  "connection",
  "keep-alive",
  "proxy-authenticate",
  "proxy-authorization",
  "te",
  "trailers",
  "transfer-encoding",
  "upgrade",
  "set-cookie",
]);

async function bodyForPolicy(
  request: Request,
  limit: number,
  instanceId?: string,
): Promise<Uint8Array> {
  requireIdentityEncoding(request.headers.get("content-encoding"));
  if (!request.body) {
    return new Uint8Array();
  }
  const reader = request.clone().body?.getReader();
  if (!reader) {
    return new Uint8Array();
  }
  const buffered = new Uint8Array(limit + 1);
  let length = 0;
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) {
        return buffered.subarray(0, length);
      }
      const remaining = buffered.length - length;
      buffered.set(value.subarray(0, remaining), length);
      length += Math.min(value.length, remaining);
      if (
        instanceId !== undefined &&
        parsePushCommands(buffered.subarray(0, length), instanceId) !== null
      ) {
        return buffered.subarray(0, length);
      }
      if (length > limit || value.length > remaining) {
        throw new PolicyError("request prefix is too large");
      }
    }
  } finally {
    void reader.cancel().catch((error: unknown) => {
      console.error("vm-access: inspection stream cancellation failed", error);
    });
  }
}

function upstreamRequest(
  request: Request,
  token: string | null,
  forwardEncoding: boolean,
): Request {
  const url = new URL(request.url);
  const headers = new Headers();
  for (const name of forwardedHeaders) {
    const value = request.headers.get(name);
    if (value !== null) {
      headers.set(name, value);
    }
  }
  const encoding = request.headers.get("content-encoding");
  if (forwardEncoding && encoding !== null) {
    headers.set("content-encoding", encoding);
  }
  if (token !== null) {
    headers.set(
      "authorization",
      url.hostname === "github.com"
        ? `Basic ${btoa(`x-access-token:${token}`)}`
        : `Bearer ${token}`,
    );
  }
  return new Request(request, { headers, redirect: "manual" });
}

function downstreamResponse(response: Response): Response {
  const headers = new Headers();
  for (const [name, value] of response.headers) {
    if (!blockedResponseHeaders.has(name)) {
      headers.set(name, value);
    }
  }
  return new Response(response.body, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

export async function handleOutbound(
  request: Request,
  instanceId: string,
  authority: OutboundAuthority,
  forward: (request: Request) => Promise<Response> = fetch,
): Promise<Response> {
  const url = new URL(request.url);
  if (url.hostname !== "github.com" && url.hostname !== "api.github.com") {
    return forward(request);
  }
  const started = Date.now();
  let classification: Classification | null = null;
  let requestId: string | null = null;
  try {
    if (url.protocol !== "https:" || url.port || url.username || url.password) {
      throw new PolicyError("GitHub requests must use HTTPS");
    }
    let body: Uint8Array | undefined;
    if (
      url.hostname === "github.com" &&
      request.method === "POST" &&
      url.pathname.endsWith("/git-receive-pack")
    ) {
      body = await bodyForPolicy(request, 1_048_576, instanceId);
    } else if (
      url.hostname === "api.github.com" &&
      request.method === "POST" &&
      url.pathname === "/graphql"
    ) {
      body = await bodyForPolicy(request, 1_048_576);
    }
    classification = classify(
      {
        host: url.hostname,
        method: request.method,
        target: `${url.pathname}${url.search}`,
        ...(body ? { body } : {}),
      },
      instanceId,
    );
    const allowed = await authority.allow(instanceId, classification);
    requestId = allowed.requestId;
    if (allowed.access === "anonymous") {
      authorize(null, classification.operation);
    } else if (!allowed.token) {
      throw new Error("repository authorization is unavailable");
    }
    if (url.pathname === "/__vm_access__/health") {
      await authority.finish(requestId, 200, Date.now() - started, "completed");
      return new Response("vm-access ready\n", { status: 200 });
    }
    const response = await forward(
      upstreamRequest(
        request,
        allowed.access === "anonymous" ||
          classification.operation === "public.read"
          ? null
          : allowed.token,
        classification.operation === "git.read" && request.method === "POST",
      ),
    );
    await authority.finish(
      requestId,
      response.status,
      Date.now() - started,
      "completed",
    );
    return downstreamResponse(response);
  } catch (error) {
    const reason =
      error instanceof PolicyError ? error.message : "gateway request failed";
    try {
      if (requestId === null) {
        requestId = await authority.deny(instanceId, classification, reason);
        await authority.finish(requestId, null, Date.now() - started, "denied");
      } else {
        await authority.finish(
          requestId,
          null,
          Date.now() - started,
          "gateway-error",
        );
      }
    } catch {
      return Response.json(
        { error: "gateway audit unavailable" },
        { status: 503 },
      );
    }
    return Response.json(
      { error: reason, request_id: requestId },
      { status: error instanceof PolicyError ? 403 : 503 },
    );
  }
}
