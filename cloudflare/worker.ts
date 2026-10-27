import { DurableObject, WorkerEntrypoint } from "cloudflare:workers";
import { PolicyError, type Classification } from "../gateway/policy.ts";
import {
  CloudAuthority,
  type AllowResult,
  type CloudAccessLog,
  type CloudInstance,
  type CloudPolicyEvent,
} from "./authority.ts";
import { handleOutbound, type OutboundAuthority } from "./outbound.ts";

interface Env {
  AUTHORITY: DurableObjectNamespace<AuthorityDO>;
  AGENTS: DurableObjectNamespace<AgentContainer>;
  GITHUB_CLIENT_ID: string;
  ADMIN_TOKEN: string;
}

function authority(env: Env): DurableObjectStub<AuthorityDO> {
  return env.AUTHORITY.get(env.AUTHORITY.idFromName("global"));
}

export class AuthorityDO extends DurableObject<Env> {
  private readonly state = new CloudAuthority(
    this.ctx.storage,
    this.env.GITHUB_CLIENT_ID,
  );

  async register(name: string): Promise<CloudInstance> {
    const instance = await this.state.register(name, "admin");
    if ((await this.ctx.storage.getAlarm()) === null) {
      await this.ctx.storage.setAlarm(Date.now() + 86_400_000);
    }
    return instance;
  }

  instance(id: string): Promise<CloudInstance | null> {
    return this.state.instance(id);
  }

  instances(): Promise<CloudInstance[]> {
    return this.state.instances();
  }

  update(
    id: string,
    revision: number,
    grants: unknown,
  ): Promise<CloudInstance> {
    return this.state.update(id, revision, grants, "admin");
  }

  disable(id: string): Promise<void> {
    return this.state.disable(id, "admin");
  }

  installAuthorization(record: unknown): Promise<void> {
    return this.state.installAuthorization(record);
  }

  allow(id: string, classification: Classification): Promise<AllowResult> {
    return this.state.allow(id, classification);
  }

  deny(
    id: string,
    classification: Classification | null,
    reason: string,
  ): Promise<string> {
    return this.state.deny(id, classification, reason);
  }

  finish(
    requestId: string,
    status: number | null,
    durationMs: number,
    outcome: "completed" | "gateway-error" | "denied",
  ): Promise<void> {
    return this.state.finish(requestId, status, durationMs, outcome);
  }

  logs(id: string, limit: number): Promise<CloudAccessLog[]> {
    return this.state.logs(id, limit);
  }

  policyEvents(id: string, limit: number): Promise<CloudPolicyEvent[]> {
    return this.state.events(id, limit);
  }

  async alarm(): Promise<void> {
    try {
      await this.state.prune();
    } finally {
      await this.ctx.storage.setAlarm(Date.now() + 86_400_000);
    }
  }
}

export class Outbound extends WorkerEntrypoint<Env, { instanceId: string }> {
  fetch(request: Request): Promise<Response> {
    const stub = authority(this.env);
    const connector: OutboundAuthority = {
      async allow(id, classification) {
        const result = await stub.allow(id, classification);
        if (!result.allowed) {
          throw new PolicyError(result.reason);
        }
        return {
          requestId: result.requestId,
          token: result.token,
          access: result.access,
        };
      },
      deny(id, classification, reason) {
        return stub.deny(id, classification, reason);
      },
      finish(requestId, status, durationMs, outcome) {
        return stub.finish(requestId, status, durationMs, outcome);
      },
    };
    return handleOutbound(request, this.ctx.props.instanceId, connector);
  }
}

export class AgentContainer extends DurableObject<Env> {
  constructor(ctx: DurableObjectState, env: Env) {
    super(ctx, env);
    const container = ctx.container;
    if (container?.running) {
      void ctx.blockConcurrencyWhile(() =>
        container.setInactivityTimeout(21_600_000),
      );
    }
  }

  async start(): Promise<void> {
    const instanceId = this.ctx.id.name;
    if (!instanceId) {
      throw new Error("container identity is missing");
    }
    const instance = await authority(this.env).instance(instanceId);
    if (!instance || instance.disabled) {
      throw new PolicyError("VM access has been revoked");
    }
    const container = this.ctx.container;
    if (!container) {
      throw new Error("container binding is missing");
    }
    const exports = this.ctx.exports as unknown as {
      Outbound: (options: { props: { instanceId: string } }) => Fetcher;
    };
    const outbound = exports.Outbound({ props: { instanceId } });
    console.log("configuring HTTP outbound interception", instanceId);
    await container.interceptAllOutboundHttp(outbound);
    console.log("configuring HTTPS outbound interception", instanceId);
    await container.interceptOutboundHttps("*", outbound);
    console.log("outbound interception configured", instanceId);
    if (container.running) {
      return;
    }
    const image = container.images.base;
    if (!image) {
      throw new Error("container image is missing");
    }
    container.start({
      image,
      instance: { vcpu: 1, memoryMib: 4096, diskMb: 20_000 },
      enableInternet: false,
      labels: { vm_id: instanceId },
      entrypoint: [
        "/home/shields/src/github.com/shields/dotfiles/cloudflare/entrypoint.sh",
      ],
    });
    await container.setInactivityTimeout(21_600_000);
  }

  async stop(): Promise<void> {
    if (this.ctx.container?.running) {
      await this.ctx.container.destroy("VM removed");
    }
  }

  running(): boolean {
    return this.ctx.container?.running ?? false;
  }
}

async function authorized(request: Request, secret: string): Promise<boolean> {
  if (!secret || secret.length < 32) {
    return false;
  }
  const supplied = request.headers.get("authorization");
  if (!supplied?.startsWith("Bearer ")) {
    return false;
  }
  const encoder = new TextEncoder();
  const [expected, actual] = await Promise.all([
    crypto.subtle.digest("SHA-256", encoder.encode(secret)),
    crypto.subtle.digest("SHA-256", encoder.encode(supplied.slice(7))),
  ]);
  const left = new Uint8Array(expected);
  const right = new Uint8Array(actual);
  let difference = 0;
  for (let index = 0; index < left.length; index += 1) {
    difference |= (left[index] ?? 0) ^ (right[index] ?? 0);
  }
  return difference === 0;
}

async function input(request: Request): Promise<Record<string, unknown>> {
  const body = await request.arrayBuffer();
  if (body.byteLength > 65_536) {
    throw new PolicyError("request body is too large");
  }
  let payload: unknown;
  try {
    payload = JSON.parse(new TextDecoder().decode(body)) as unknown;
  } catch {
    throw new PolicyError("invalid JSON request");
  }
  if (!payload || typeof payload !== "object" || Array.isArray(payload)) {
    throw new PolicyError("request body must be a JSON object");
  }
  return payload as Record<string, unknown>;
}

async function route(request: Request, env: Env): Promise<Response> {
  const url = new URL(request.url);
  if (url.pathname === "/health" && request.method === "GET") {
    return Response.json({ ready: true });
  }
  if (!(await authorized(request, env.ADMIN_TOKEN))) {
    return new Response(null, { status: 401 });
  }
  const state = authority(env);
  if (url.pathname === "/authorizations" && request.method === "POST") {
    await state.installAuthorization(await input(request));
    return new Response(null, { status: 204 });
  }
  if (url.pathname === "/instances" && request.method === "POST") {
    const body = await input(request);
    if (typeof body.name !== "string") {
      throw new PolicyError("VM name is missing");
    }
    return Response.json(await state.register(body.name), { status: 201 });
  }
  if (url.pathname === "/instances" && request.method === "GET") {
    return Response.json(await state.instances());
  }
  const match =
    /^\/instances\/([0-9a-f-]{36})(?:\/(grants|start|logs|events))?$/.exec(
      url.pathname,
    );
  if (!match || url.search) {
    return new Response(null, { status: 404 });
  }
  const id = match[1] ?? "";
  const action = match[2];
  const agent = env.AGENTS.get(env.AGENTS.idFromName(id));
  if (!action && request.method === "GET") {
    const instance = await state.instance(id);
    return instance
      ? Response.json({ ...instance, running: await agent.running() })
      : new Response(null, { status: 404 });
  }
  if (!action && request.method === "DELETE") {
    const instance = await state.instance(id);
    if (!instance) {
      return new Response(null, { status: 404 });
    }
    if (!instance.disabled) {
      await state.disable(id);
    }
    await agent.stop();
    return new Response(null, { status: 204 });
  }
  if (action === "grants" && request.method === "PUT") {
    const body = await input(request);
    if (!Number.isSafeInteger(body.revision)) {
      throw new PolicyError("policy revision is missing");
    }
    return Response.json(
      await state.update(id, body.revision as number, body.grants),
    );
  }
  if (action === "start" && request.method === "POST") {
    await agent.start();
    return Response.json({ started: true, running: await agent.running() });
  }
  if (action === "logs" && request.method === "GET") {
    return Response.json(await state.logs(id, 100));
  }
  if (action === "events" && request.method === "GET") {
    return Response.json(await Promise.resolve(state.policyEvents(id, 100)));
  }
  return new Response(null, { status: 405 });
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    try {
      return await route(request, env);
    } catch (error) {
      if (
        error instanceof PolicyError ||
        (error instanceof Error && error.name === "PolicyError")
      ) {
        return Response.json({ error: error.message }, { status: 403 });
      }
      console.error("vm-access Cloudflare administration failed", error);
      return Response.json(
        { error: "administration unavailable" },
        { status: 503 },
      );
    }
  },
} satisfies ExportedHandler<Env>;
