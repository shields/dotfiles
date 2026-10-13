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
  parsePushCommands,
  PolicyError,
  requireIdentityEncoding,
  rewriteLinks,
  PushDenied,
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

async function readBody(
  remaining: AsyncIterator<Buffer>,
  contentEncoding: string | undefined,
  limit: number,
  pushInstanceId?: string,
): Promise<RequestBody> {
  requireIdentityEncoding(contentEncoding);
  let buffered = Buffer.alloc(0);
  for (;;) {
    const next = await remaining.next();
    if (next.done) {
      return { buffered, remaining };
    }
    buffered = Buffer.concat([buffered, next.value]);
    if (
      pushInstanceId !== undefined &&
      parsePushCommands(buffered, pushInstanceId) !== null
    ) {
      return { buffered, remaining };
    }
    if (buffered.length > limit) {
      throw new PolicyError("request prefix is too large");
    }
  }
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

  constructor(state: State, auth: AppAuth, options: GatewayOptions) {
    this.state = state;
    this.auth = auth;
    this.options = options;
  }

  async start(): Promise<void> {
    await this.refresh();
    await this.startAdmin();
    this.refreshTimer = setInterval(() => {
      void this.refresh()
        .then(() => {
          this.state.prune();
        })
        .catch((error: unknown) => {
          process.stderr.write(
            `vm-access: listener refresh failed: ${String(error)}\n`,
          );
          throw error;
        });
    }, 60_000);
  }

  refresh(): Promise<void> {
    const next = this.syncing.then(
      () => this.refreshNow(),
      () => this.refreshNow(),
    );
    this.syncing = next;
    return next;
  }

  private async refreshNow(): Promise<void> {
    const active = new Map(
      this.state.active().map((instance) => [instance.id, instance]),
    );
    for (const [id, server] of this.listeners) {
      if (!active.has(id)) {
        server.close();
        this.listeners.delete(id);
      }
    }
    for (const instance of active.values()) {
      if (this.listeners.has(instance.id)) {
        continue;
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
          void this.handle(instance.id, request, response);
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
        void this.refresh()
          .then(() => {
            response
              .writeHead(200, { "content-type": "application/json" })
              .end('{"ok":true}');
          })
          .catch((error: unknown) => {
            process.stderr.write(
              `vm-access: listener sync failed: ${String(error)}\n`,
            );
            response.writeHead(503).end();
          });
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

  async stop(): Promise<void> {
    if (this.refreshTimer) {
      clearInterval(this.refreshTimer);
      this.refreshTimer = null;
    }
    const servers = [
      ...this.listeners.values(),
      ...(this.admin ? [this.admin] : []),
    ];
    await Promise.all(
      servers.map(async (server) => {
        await new Promise<void>((resolve) => {
          server.close(() => {
            resolve();
          });
        });
      }),
    );
    this.listeners.clear();
    this.admin = null;
  }

  private async handle(
    instanceId: string,
    request: IncomingMessage,
    response: ServerResponse,
  ): Promise<void> {
    const requestId = randomUUID();
    const started = Date.now();
    let classification: Classification | null = null;
    let revision = this.state.byId(instanceId)?.revision ?? 0;
    let logged = false;
    const remaining = bodyIterator(request);
    try {
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
      let token: string | null = null;
      if (access === "authenticated" && classification.repository !== null) {
        token = await this.auth.token(classification.repository);
      }
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
      if (
        host === "api.github.com" &&
        method === "GET" &&
        target === "/__vm_access__/health"
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
        try {
          this.state.logDecision({
            id: requestId,
            instanceId,
            repository: classification?.repository ?? null,
            operation: classification?.operation ?? "unknown",
            refs: classification?.refs ?? [],
            allowed: false,
            reason,
            revision,
          });
          this.state.finish(requestId, null, Date.now() - started, "denied");
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
      } else if (
        // git keeps sending after a 200 and shows a transport error, not these
        // ref statuses, unless the whole body is read.
        error instanceof PushDenied &&
        error.report !== null &&
        (await drained(remaining, 67_108_864).catch(() => false))
      ) {
        response
          .writeHead(200, {
            "content-type": "application/x-git-receive-pack-result",
            "cache-control": "no-cache",
          })
          .end(error.report);
      } else {
        respond(error instanceof PolicyError ? 403 : 503, reason);
      }
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
    const transport = upstreamUrl.protocol === "https:" ? https : http;
    const upstream = transport.request(upstreamUrl, {
      method: incoming.method,
      headers,
      timeout: 60_000,
    });
    let completed = false;
    const finish = (status: number | null, outcome: string): void => {
      if (completed) {
        return;
      }
      completed = true;
      this.state.finish(requestId, status, Date.now() - started, outcome);
    };
    const result = new Promise<void>((resolve, reject) => {
      upstream.once("response", (upstreamResponse) => {
        const status = upstreamResponse.statusCode ?? 502;
        response.writeHead(
          status,
          copyResponseHeaders(upstreamResponse.headers, classification),
        );
        upstreamResponse.pipe(response);
        response.once("finish", () => {
          finish(status, "completed");
          resolve();
        });
        response.once("close", () => {
          if (!response.writableFinished) {
            finish(status, "interrupted");
            upstream.destroy();
            resolve();
          }
        });
      });
      upstream.once("error", reject);
      upstream.once("timeout", () => {
        upstream.destroy(new Error("upstream timed out"));
      });
    });
    void sendBody(upstream, body).catch((error: unknown) => {
      upstream.destroy(
        error instanceof Error ? error : new Error("request body failed"),
      );
    });
    await result;
  }
}
