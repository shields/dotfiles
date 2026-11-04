import { randomUUID } from "node:crypto";
import { existsSync, readFileSync, rmSync } from "node:fs";
import http, {
  type IncomingHttpHeaders,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import https from "node:https";
import { once } from "node:events";
import { AppAuth } from "./auth.ts";
import {
  authorize,
  classify,
  denial,
  PolicyError,
  requireIdentityEncoding,
  rewriteLinks,
  PushDenied,
  PushParser,
  type Classification,
} from "./policy.ts";
import { State } from "./state.ts";

interface RequestBody {
  buffered: Buffer;
  remaining: AsyncIterator<Buffer>;
}

export interface GatewayOptions {
  cert: string;
  key: string;
  adminSocket: string;
  webUpstream?: string;
  apiUpstream?: string;
}

const forwardedHeaders = [
  "accept",
  "content-type",
  "user-agent",
  "git-protocol",
  "if-none-match",
  "range",
  "x-github-api-version",
  "content-length",
] as const;
const responseBlocked = new Set([
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

function bodyIterator(request: IncomingMessage): AsyncIterator<Buffer> {
  return request[Symbol.asyncIterator]() as AsyncIterator<Buffer>;
}

export async function readBody(
  remaining: AsyncIterator<Buffer>,
  contentEncoding: string | undefined,
  limit: number,
  pushInstanceId?: string,
): Promise<RequestBody> {
  requireIdentityEncoding(contentEncoding);
  const parser =
    pushInstanceId === undefined ? null : new PushParser(pushInstanceId);
  let storage = Buffer.alloc(0);
  let length = 0;
  for (;;) {
    const next = await remaining.next();
    if (next.done) {
      return { buffered: storage.subarray(0, length), remaining };
    }
    if (length + next.value.length > storage.length) {
      const grown = Buffer.allocUnsafe(
        Math.max(length + next.value.length, storage.length * 2),
      );
      storage.copy(grown, 0, 0, length);
      storage = grown;
    }
    next.value.copy(storage, length);
    length += next.value.length;
    const buffered = storage.subarray(0, length);
    if (parser !== null && parser.parse(buffered) !== null) {
      return { buffered, remaining };
    }
    if (length > limit) {
      throw new PolicyError("request prefix is too large");
    }
  }
}

function closeServer(server: http.Server): Promise<void> {
  return new Promise((resolve) => {
    server.close(() => {
      resolve();
    });
  });
}

async function drained(
  remaining: AsyncIterator<Buffer>,
  limit: number,
): Promise<boolean> {
  let length = 0;
  for (;;) {
    const next = await remaining.next();
    if (next.done) {
      return true;
    }
    length += next.value.length;
    if (length > limit) {
      return false;
    }
  }
}

async function sendBody(
  upstream: http.ClientRequest,
  body: RequestBody,
): Promise<void> {
  if (body.buffered.length > 0 && !upstream.write(body.buffered)) {
    await once(upstream, "drain");
  }
  for (;;) {
    const next = await body.remaining.next();
    if (next.done) {
      break;
    }
    if (!upstream.write(next.value)) {
      await once(upstream, "drain");
    }
  }
  upstream.end();
}

function carriesBody(headers: IncomingHttpHeaders): boolean {
  return (
    headers["transfer-encoding"] !== undefined ||
    Number(headers["content-length"] ?? 0) > 0
  );
}

function requestHeaders(
  headers: IncomingHttpHeaders,
  token: string | null,
  host: string,
  forwardEncoding: boolean,
): http.OutgoingHttpHeaders {
  const outgoing: http.OutgoingHttpHeaders = { host };
  for (const name of forwardedHeaders) {
    const value = headers[name];
    if (typeof value === "string") {
      outgoing[name] = value;
    }
  }
  if (forwardEncoding && headers["content-encoding"] !== undefined) {
    outgoing["content-encoding"] = headers["content-encoding"];
  }
  if (token) {
    outgoing.authorization =
      host === "github.com"
        ? `Basic ${Buffer.from(`x-access-token:${token}`).toString("base64")}`
        : `Bearer ${token}`;
  }
  return outgoing;
}

function copyResponseHeaders(
  headers: IncomingHttpHeaders,
  classification: Classification,
): http.OutgoingHttpHeaders {
  const outgoing: http.OutgoingHttpHeaders = {};
  for (const [name, value] of Object.entries(headers)) {
    if (!responseBlocked.has(name) && value !== undefined) {
      outgoing[name] =
        name === "link" && typeof value === "string"
          ? rewriteLinks(value, classification)
          : value;
    }
  }
  return outgoing;
}

export class Gateway {
  readonly state: State;
  readonly auth: AppAuth;
  readonly options: GatewayOptions;
  private readonly listeners = new Map<string, https.Server>();
  private admin: http.Server | null = null;
  private refreshTimer: NodeJS.Timeout | null = null;
  private syncing: Promise<void> = Promise.resolve();
  private readonly pending = new Set<Promise<void>>();
  private readonly retired = new Map<https.Server, Promise<void>>();
  private stopped = false;
  private starting: Promise<void> | null = null;
  private stopping: Promise<void> | null = null;

  constructor(state: State, auth: AppAuth, options: GatewayOptions) {
    this.state = state;
    this.auth = auth;
    this.options = options;
  }

  async start(): Promise<void> {
    this.requireRunning();
    if (this.starting) {
      throw new Error("gateway is already started");
    }
    this.starting = this.open();
    try {
      await this.starting;
    } catch (error) {
      await this.stop();
      throw error;
    }
  }

  private async open(): Promise<void> {
    await this.refresh();
    this.requireRunning();
    await this.startAdmin();
    this.requireRunning();
    this.refreshTimer = setInterval(() => {
      this.track(this.refreshAndPrune());
    }, 60_000);
  }

  // stop() closes servers only once open() has settled, so open() fails at
  // the first step that finds the gateway stopped and leaves the closing to it.
  private requireRunning(): void {
    if (this.stopped) {
      throw new Error("gateway is stopped");
    }
  }

  private async refreshAndPrune(): Promise<void> {
    try {
      await this.refresh();
      this.state.prune();
    } catch (error) {
      process.stderr.write(
        `vm-access: listener refresh failed: ${String(error)}\n`,
      );
      throw error;
    }
  }

  // The set holds the work itself. The promise that finally() returns stays
  // unobserved so that a failure of the work is reported as an unhandled
  // rejection; waiting on it in stop() would mark the failure as handled.
  private track(work: Promise<void>): void {
    this.pending.add(work);
    void work.finally(() => {
      this.pending.delete(work);
    });
  }

  refresh(): Promise<void> {
    const next = (async () => {
      await Promise.allSettled([this.syncing]);
      await this.refreshNow();
    })();
    this.syncing = next;
    return next;
  }

  private async refreshNow(): Promise<void> {
    const active = new Map(
      this.state.active().map((instance) => [instance.id, instance]),
    );
    for (const [id, server] of this.listeners) {
      if (!active.has(id)) {
        const closed = closeServer(server);
        this.retired.set(server, closed);
        void closed.then(() => {
          this.retired.delete(server);
        });
        this.listeners.delete(id);
      }
    }
    for (const instance of active.values()) {
      if (this.listeners.has(instance.id)) {
        continue;
      }
      if (this.stopped) {
        return;
      }
      if (existsSync(instance.socket)) {
        rmSync(instance.socket);
      }
      const server = https.createServer(
        {
          cert: readFileSync(this.options.cert),
          key: readFileSync(this.options.key),
          // requestTimeout covers a push's whole upload, and GitHub accepts
          // pushes up to 2 GiB: about an hour at 5 Mbit/s.
          requestTimeout: 3_600_000,
          headersTimeout: 60_000,
        },
        (request, response) => {
          // A connection that Bun hands over after the close has not been
          // swept, and would stay open until the keep-alive timeout.
          if (this.stopped) {
            response.setHeader("connection", "close");
          }
          this.track(this.handle(instance.id, request, response));
        },
      );
      await new Promise<void>((resolve, reject) => {
        server.once("error", reject);
        server.listen(instance.socket, () => {
          server.off("error", reject);
          server.on("error", (error) => {
            throw error;
          });
          resolve();
        });
      });
      this.listeners.set(instance.id, server);
    }
  }

  private async startAdmin(): Promise<void> {
    if (existsSync(this.options.adminSocket)) {
      rmSync(this.options.adminSocket);
    }
    this.admin = http.createServer((request, response) => {
      if (request.method === "POST" && request.url === "/sync") {
        this.track(this.sync(response));
      } else if (request.method === "GET" && request.url === "/health") {
        response
          .writeHead(200, { "content-type": "application/json" })
          .end('{"ok":true}');
      } else {
        response.writeHead(404).end();
      }
    });
    await new Promise<void>((resolve, reject) => {
      this.admin?.once("error", reject);
      this.admin?.listen(this.options.adminSocket, () => {
        this.admin?.off("error", reject);
        this.admin?.on("error", (error) => {
          throw error;
        });
        resolve();
      });
    });
  }

  private async sync(response: ServerResponse): Promise<void> {
    try {
      await this.refresh();
      response
        .writeHead(200, { "content-type": "application/json" })
        .end('{"ok":true}');
    } catch (error) {
      process.stderr.write(
        `vm-access: listener sync failed: ${String(error)}\n`,
      );
      response.writeHead(503).end();
    }
  }

  stop(): Promise<void> {
    this.stopped = true;
    this.stopping ??= this.shutdown();
    return this.stopping;
  }

  private async shutdown(): Promise<void> {
    if (this.refreshTimer) {
      clearInterval(this.refreshTimer);
      this.refreshTimer = null;
    }
    // A refresh that is starting a server, and a start() that is opening the
    // admin server, add theirs before the servers to close are collected.
    await Promise.allSettled([this.starting, this.syncing]);
    const serving = [
      ...this.listeners.values(),
      ...(this.admin ? [this.admin] : []),
    ];
    const closes = [
      ...serving.map((server) => closeServer(server)),
      ...this.retired.values(),
    ];
    // A close completes only when every connection has ended. Connections
    // that were busy are idle once their handlers finish, and would otherwise
    // stay open until the keep-alive timeout. When nothing was busy, close()
    // has already swept the idle connections, and Bun resets a connection that
    // a second sweep finds still sending its last response.
    const busy = this.pending.size > 0;
    await this.awaitHandlers();
    if (busy) {
      for (const server of serving) {
        server.closeIdleConnections();
      }
    }
    // A retired server belongs to a disabled VM, whose requests are refused,
    // so the connections it still has are cut.
    for (const server of this.retired.keys()) {
      server.closeAllConnections();
    }
    await Promise.all(closes);
    this.listeners.clear();
    this.admin = null;
    await this.awaitHandlers();
  }

  private async awaitHandlers(): Promise<void> {
    while (this.pending.size > 0) {
      await Promise.allSettled(this.pending);
    }
  }

  private async handle(
    instanceId: string,
    request: IncomingMessage,
    response: ServerResponse,
  ): Promise<void> {
    const requestId = randomUUID();
    const started = Date.now();
    let classification: Classification | null = null;
    let revision = 0;
    let logged = false;
    const remaining = bodyIterator(request);
    try {
      revision = this.state.byId(instanceId)?.revision ?? 0;
      const host = request.headers.host?.toLowerCase();
      if (host !== "github.com" && host !== "api.github.com") {
        throw new PolicyError("unsupported GitHub host");
      }
      const target = request.url ?? "";
      const method = request.method ?? "";
      let body: RequestBody;
      if (
        host === "github.com" &&
        method === "POST" &&
        target.endsWith("/git-receive-pack")
      ) {
        body = await readBody(
          remaining,
          request.headers["content-encoding"],
          1_048_576,
          instanceId,
        );
      } else if (
        host === "api.github.com" &&
        method === "POST" &&
        target === "/graphql"
      ) {
        body = await readBody(
          remaining,
          request.headers["content-encoding"],
          1_048_576,
        );
      } else {
        body = { buffered: Buffer.alloc(0), remaining };
      }
      classification = classify(
        { host, method, target, body: body.buffered },
        instanceId,
      );
      // Node writes the body of a GET or HEAD upstream without framing unless
      // the client gave a Content-Length, and GitHub would read it as the next
      // request on a connection that every VM shares.
      const verb = method.toUpperCase();
      if ((verb === "GET" || verb === "HEAD") && carriesBody(request.headers)) {
        throw new PolicyError("GET and HEAD requests must not have a body");
      }
      const beforeRefresh = this.state.byId(instanceId);
      if (!beforeRefresh || beforeRefresh.disabled) {
        throw new PolicyError("VM access has been revoked");
      }
      if (classification.operation === "identity.read") {
        classification.repository =
          Object.keys(this.state.grants(instanceId))[0] ?? null;
        if (classification.repository === null) {
          throw new PolicyError("repository is not granted to this VM");
        }
      }
      const access = authorize(
        classification.repository === null
          ? null
          : this.state.grant(instanceId, classification.repository),
        classification.operation,
      );
      const token =
        access === "authenticated" && classification.repository !== null
          ? await this.auth.token(classification.repository)
          : null;
      const current = this.state.byId(instanceId);
      if (!current || current.disabled) {
        throw new PolicyError("VM access has been revoked");
      }
      revision = current.revision;
      const currentAccess = authorize(
        classification.repository === null
          ? null
          : this.state.grant(instanceId, classification.repository),
        classification.operation,
      );
      if (access === "authenticated" && currentAccess !== access) {
        throw new PolicyError("repository is not granted to this VM");
      }
      this.state.logDecision({
        id: requestId,
        instanceId,
        repository: classification.repository,
        operation: classification.operation,
        refs: classification.refs,
        allowed: true,
        reason: access === "anonymous" ? "anonymous read" : "granted",
        revision,
      });
      logged = true;
      // The health path is the only public read with no repository.
      if (
        classification.operation === "public.read" &&
        classification.repository === null
      ) {
        response
          .writeHead(200, { "content-type": "text/plain" })
          .end("vm-access ready\n");
        this.state.finish(requestId, 200, Date.now() - started, "completed");
        return;
      }
      await this.forward(
        request,
        response,
        host,
        target,
        requestHeaders(
          request.headers,
          token,
          host,
          classification.operation === "git.read" && method === "POST",
        ),
        classification,
        body,
        requestId,
        started,
      );
    } catch (error) {
      const reason =
        error instanceof PolicyError ? error.message : "gateway request failed";
      const respond = (status: number, message: string): void => {
        const { contentType, body } = denial(
          request.headers.host?.toLowerCase() ?? "",
          request.url ?? "",
          message,
          requestId,
        );
        response.writeHead(status, { "content-type": contentType }).end(body);
      };
      if (logged) {
        this.state.finish(
          requestId,
          null,
          Date.now() - started,
          "gateway-error",
        );
      } else {
        // Reading the body fails with the request's own error when the
        // connection closes before the body ends.
        const aborted = error === request.errored;
        try {
          this.state.logDecision({
            id: requestId,
            instanceId,
            repository: classification?.repository ?? null,
            operation: classification?.operation ?? "unknown",
            refs: classification?.refs ?? [],
            allowed: false,
            reason: aborted ? "request aborted" : reason,
            revision,
          });
          this.state.finish(
            requestId,
            null,
            Date.now() - started,
            aborted ? "interrupted" : "denied",
          );
        } catch (auditError) {
          process.stderr.write(
            `vm-access audit failure: ${String(auditError)}\n`,
          );
          if (response.headersSent) {
            response.destroy();
          } else {
            respond(503, "gateway audit unavailable");
          }
          return;
        }
      }
      if (response.headersSent) {
        response.destroy();
        return;
      }
      if (
        // git keeps sending after a 200 and shows a transport error, not these
        // ref statuses, unless the whole body is read.
        error instanceof PushDenied &&
        error.report !== null
      ) {
        let bodyDrained: boolean;
        try {
          bodyDrained = await drained(remaining, 67_108_864);
        } catch {
          bodyDrained = false;
        }
        if (bodyDrained) {
          response
            .writeHead(200, {
              "content-type": "application/x-git-receive-pack-result",
              "cache-control": "no-cache",
            })
            .end(error.report);
          return;
        }
      }
      respond(error instanceof PolicyError ? 403 : 503, reason);
    }
  }

  private async forward(
    incoming: IncomingMessage,
    response: ServerResponse,
    host: string,
    target: string,
    headers: http.OutgoingHttpHeaders,
    classification: Classification,
    body: RequestBody,
    requestId: string,
    started: number,
  ): Promise<void> {
    const base = new URL(
      host === "github.com"
        ? (this.options.webUpstream ?? "https://github.com")
        : (this.options.apiUpstream ?? "https://api.github.com"),
    );
    if (
      base.username ||
      base.password ||
      base.search ||
      base.hash ||
      (base.pathname !== "/" && base.pathname !== "/api/v3")
    ) {
      throw new Error("invalid upstream configuration");
    }
    const upstreamUrl = new URL(
      `${base.pathname.replace(/\/$/, "")}${target}`,
      base,
    );
    if (upstreamUrl.origin !== base.origin) {
      throw new Error("upstream target escaped its host");
    }
    // The client may have left while the request was being authorized. The
    // response counts as destroyed only once it emits "close", which can trail
    // the destruction of its socket, so both are checked.
    if (response.destroyed || incoming.socket.destroyed) {
      this.state.finish(requestId, null, Date.now() - started, "interrupted");
      return;
    }
    const transport = upstreamUrl.protocol === "https:" ? https : http;
    const upstream = transport.request(upstreamUrl, {
      method: incoming.method,
      headers,
      timeout: 60_000,
    });
    let settled = false;
    let upstreamStatus: number | null = null;
    const finish = (status: number | null, outcome: string): void => {
      if (settled) {
        return;
      }
      settled = true;
      this.state.finish(requestId, status, Date.now() - started, outcome);
    };
    const result = new Promise<void>((resolve, reject) => {
      // handle() records a rejection as a gateway error, and the response it
      // destroys then emits "close", which must not record an interruption.
      const fail = (error: Error): void => {
        settled = true;
        reject(error);
      };
      response.once("close", () => {
        // Bun reports writableFinished for a response that was ended after its
        // socket was destroyed, and never emits "finish" for it, so only
        // settled shows that the response completed.
        if (settled) {
          return;
        }
        finish(upstreamStatus, "interrupted");
        upstream.destroy();
        resolve();
      });
      upstream.once("response", (upstreamResponse) => {
        upstreamStatus = upstreamResponse.statusCode ?? 502;
        response.writeHead(
          upstreamStatus,
          copyResponseHeaders(upstreamResponse.headers, classification),
        );
        upstreamResponse.pipe(response);
        // An upstream that closes mid-body never ends the pipe, and Node
        // reports that on the response only while an "error" listener exists.
        upstreamResponse.once("error", fail);
        response.once("finish", () => {
          finish(upstreamStatus, "completed");
          resolve();
        });
      });
      upstream.once("error", fail);
      upstream.once("timeout", () => {
        upstream.destroy(new Error("upstream timed out"));
      });
    });
    // The server stops reporting a client that leaves mid-upload once the
    // response has finished, so an upstream that answers early would keep its
    // half-sent request open.
    if (!incoming.complete) {
      const { socket } = incoming;
      const abandon = (): void => {
        upstream.destroy();
      };
      socket.once("close", abandon);
      upstream.once("close", () => {
        socket.off("close", abandon);
      });
    }
    void sendBody(upstream, body).catch((error: unknown) => {
      upstream.destroy(
        error instanceof Error ? error : new Error("request body failed"),
      );
    });
    await result;
  }
}
