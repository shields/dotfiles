import {
  authorize,
  classify,
  denial,
  isBlockedResponseHeader,
  inspectedBody,
  isHealthCheck,
  namesGitHubHost,
  PolicyError,
  requireIdentityEncoding,
  rewriteLinks,
  PushDenied,
  PushParser,
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
  // The result is a copy of the filled part, so that this limit-sized buffer is
  // not held while the request is forwarded.
  const buffered = new Uint8Array(limit + 1);
  const parser = instanceId === undefined ? null : new PushParser(instanceId);
  let length = 0;
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) {
        return buffered.slice(0, length);
      }
      const remaining = buffered.length - length;
      buffered.set(value.subarray(0, remaining), length);
      length += Math.min(value.length, remaining);
      if (
        parser !== null &&
        parser.parse(buffered.subarray(0, length)) !== null
      ) {
        return buffered.slice(0, length);
      }
      if (length > limit) {
        throw new PolicyError("request prefix is too large");
      }
    }
  } finally {
    void reader.cancel().catch((error: unknown) => {
      console.error("vm-access: inspection stream cancellation failed", error);
    });
  }
}

async function drained(request: Request, limit: number): Promise<boolean> {
  const reader = request.body?.getReader();
  if (!reader) {
    return true;
  }
  let length = 0;
  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) {
        return true;
      }
      length += value.length;
      if (length > limit) {
        return false;
      }
    }
  } finally {
    void reader.cancel().catch((error: unknown) => {
      console.error("vm-access: denied body cancellation failed", error);
    });
  }
}

// A RequestInit dictionary cannot give a GET or HEAD a body, even the empty one
// that an incoming Content-Length: 0 produces.
function initBody(request: Request): ReadableStream<Uint8Array> | null {
  const method = request.method.toUpperCase();
  return method === "GET" || method === "HEAD" ? null : request.body;
}

function upstreamRequest(
  request: Request,
  url: URL,
  token: string | null,
  forwardEncoding: boolean,
): Request {
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
  return new Request(url.href, {
    method: request.method,
    headers,
    body: initBody(request),
    redirect: "manual",
  });
}

function downstreamResponse(
  response: Response,
  classification: Classification,
): Response {
  const headers = new Headers();
  for (const [name, value] of response.headers) {
    if (!isBlockedResponseHeader(name)) {
      headers.set(
        name,
        name === "link" ? rewriteLinks(value, classification) : value,
      );
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
  // In workerd, request.url and copies made from the request keep the client's
  // spelling, and fetch can resolve that spelling to a different host than URL
  // does, so only url.href is ever forwarded.
  const url = new URL(request.url);
  if (!namesGitHubHost(url.hostname)) {
    return forward(
      request.url === url.href ? request : new Request(url.href, request),
    );
  }
  const started = Date.now();
  let classification: Classification | null = null;
  let requestId: string | null = null;
  try {
    if (url.protocol !== "https:" || url.port || url.username || url.password) {
      throw new PolicyError("GitHub requests must use HTTPS");
    }
    const target = `${url.pathname}${url.search}`;
    const inspected = inspectedBody(url.hostname, request.method, target);
    const body =
      inspected === null
        ? undefined
        : await bodyForPolicy(
            request,
            1_048_576,
            inspected === "push" ? instanceId : undefined,
          );
    classification = classify(
      {
        host: url.hostname,
        method: request.method,
        target,
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
    if (isHealthCheck(classification)) {
      await authority.finish(requestId, 200, Date.now() - started, "completed");
      return new Response("vm-access ready\n", { status: 200 });
    }
    const response = await forward(
      upstreamRequest(
        request,
        url,
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
    return downstreamResponse(response, classification);
  } catch (error) {
    if (!(error instanceof PolicyError)) {
      console.error("vm-access: gateway request failed", error);
    }
    const reason =
      error instanceof PolicyError ? error.message : "gateway request failed";
    const respond = (
      status: number,
      message: string,
      id: string | null,
    ): Response => {
      const { contentType, body } = denial(
        url.hostname,
        url.pathname,
        message,
        id,
      );
      return new Response(body, {
        status,
        headers: { "content-type": contentType },
      });
    };
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
    } catch (auditError) {
      console.error("vm-access audit failure", auditError);
      return respond(503, "gateway audit unavailable", null);
    }
    if (
      // git keeps sending after a 200 and shows a transport error, not these
      // ref statuses, unless the whole body is read.
      error instanceof PushDenied &&
      error.report !== null
    ) {
      let bodyDrained: boolean;
      try {
        bodyDrained = await drained(request, 67_108_864);
      } catch {
        bodyDrained = false;
      }
      if (bodyDrained) {
        return new Response(error.report, {
          status: 200,
          headers: {
            "content-type": "application/x-git-receive-pack-result",
            "cache-control": "no-cache",
          },
        });
      }
    }
    return respond(error instanceof PolicyError ? 403 : 503, reason, requestId);
  }
}
