import {
  authorize,
  normalizeRepository,
  PolicyError,
  type Access,
  type Classification,
  type Grant,
} from "../gateway/policy.ts";

export interface CloudInstance {
  id: string;
  name: string;
  revision: number;
  disabled: boolean;
  grants: Record<string, Grant>;
}

export interface CloudAuthorization {
  repository: string;
  accessToken: string;
  accessExpiresAt: number;
  refreshToken: string;
  refreshExpiresAt: number;
}

export interface CloudAccessLog {
  id: string;
  createdAt: number;
  instanceId: string;
  repository: string | null;
  operation: string;
  refs: string[];
  allowed: boolean;
  reason: string;
  revision: number;
  upstreamStatus: number | null;
  durationMs: number | null;
  outcome: string;
}

export interface CloudPolicyEvent {
  id: string;
  createdAt: number;
  actor: string;
  instanceId: string;
  oldRevision: number;
  newRevision: number;
  changes: Record<string, unknown>;
}

export type AllowResult =
  | { allowed: true; requestId: string; token: string | null; access: Access }
  | { allowed: false; reason: string };

const tokenPattern = /^[A-Za-z0-9_.~+/=-]+$/;
const namePattern = /^[A-Za-z0-9]+(?:[._-][A-Za-z0-9]+)*$/;
const clientIdPattern = /^[A-Za-z0-9._-]+$/;
const day = 86_400_000;

function grantsFrom(raw: unknown): Record<string, Grant> {
  if (!raw || typeof raw !== "object" || Array.isArray(raw)) {
    throw new PolicyError("repositories must be a mapping");
  }
  const normalized: Record<string, Grant> = {};
  for (const [repository, input] of Object.entries(
    raw as Record<string, unknown>,
  )) {
    if (
      !input ||
      typeof input !== "object" ||
      Array.isArray(input) ||
      Object.keys(input)
        .toSorted((a, b) => Number(a > b) - Number(a < b))
        .join(",") !== "pushTaskBranches,read" ||
      (input as Record<string, unknown>).read !== true ||
      typeof (input as Record<string, unknown>).pushTaskBranches !== "boolean"
    ) {
      throw new PolicyError(`invalid grant for ${repository}`);
    }
    const key = normalizeRepository(repository);
    if (Object.hasOwn(normalized, key)) {
      throw new PolicyError(`duplicate repository ${repository}`);
    }
    normalized[key] = {
      read: true,
      pushTaskBranches: (input as Grant).pushTaskBranches,
    };
  }
  return normalized;
}

function validAuthorization(input: unknown): CloudAuthorization {
  if (!input || typeof input !== "object" || Array.isArray(input)) {
    throw new PolicyError("invalid GitHub authorization record");
  }
  const record = input as Record<string, unknown>;
  const now = Math.floor(Date.now() / 1000);
  if (
    typeof record.repository !== "string" ||
    typeof record.accessToken !== "string" ||
    !tokenPattern.test(record.accessToken) ||
    typeof record.refreshToken !== "string" ||
    !tokenPattern.test(record.refreshToken) ||
    typeof record.accessExpiresAt !== "number" ||
    !Number.isSafeInteger(record.accessExpiresAt) ||
    record.accessExpiresAt <= now + 60 ||
    typeof record.refreshExpiresAt !== "number" ||
    !Number.isSafeInteger(record.refreshExpiresAt) ||
    record.refreshExpiresAt <= now
  ) {
    throw new PolicyError("invalid GitHub authorization record");
  }
  return {
    repository: normalizeRepository(record.repository),
    accessToken: record.accessToken,
    accessExpiresAt: record.accessExpiresAt,
    refreshToken: record.refreshToken,
    refreshExpiresAt: record.refreshExpiresAt,
  };
}

function logKey(instanceId: string, createdAt: number, id: string): string {
  return `access:${instanceId}:${String(createdAt).padStart(13, "0")}:${id}`;
}

function eventKey(instanceId: string, revision: number): string {
  return `event:${instanceId}:${String(revision).padStart(12, "0")}`;
}

export class CloudAuthority {
  private readonly refreshing = new Map<string, Promise<string>>();
  private readonly storage: DurableObjectStorage;
  private readonly clientId: string;
  private readonly request: typeof fetch;

  constructor(
    storage: DurableObjectStorage,
    clientId: string,
    request: typeof fetch = (input, init) => fetch(input, init),
  ) {
    this.storage = storage;
    this.clientId = clientId;
    this.request = request;
    if (!clientIdPattern.test(clientId)) {
      throw new Error("invalid GitHub App client ID");
    }
  }

  async register(name: string, actor: string): Promise<CloudInstance> {
    if (!namePattern.test(name) || name.length > 64) {
      throw new PolicyError("invalid VM name");
    }
    return this.storage.transaction(async (transaction) => {
      if (await transaction.get(`name:${name}`)) {
        throw new PolicyError("VM is already registered");
      }
      const instance: CloudInstance = {
        id: crypto.randomUUID(),
        name,
        revision: 0,
        disabled: false,
        grants: {},
      };
      await transaction.put(`instance:${instance.id}`, instance);
      await transaction.put(`name:${name}`, instance.id);
      await transaction.put(eventKey(instance.id, 0), {
        id: crypto.randomUUID(),
        createdAt: Date.now(),
        actor,
        instanceId: instance.id,
        oldRevision: 0,
        newRevision: 0,
        changes: { registered: true },
      } satisfies CloudPolicyEvent);
      return instance;
    });
  }

  async instance(id: string): Promise<CloudInstance | null> {
    return (await this.storage.get<CloudInstance>(`instance:${id}`)) ?? null;
  }

  async instances(): Promise<CloudInstance[]> {
    return [
      ...(
        await this.storage.list<CloudInstance>({ prefix: "instance:" })
      ).values(),
    ];
  }

  async update(
    id: string,
    expectedRevision: number,
    rawGrants: unknown,
    actor: string,
  ): Promise<CloudInstance> {
    const grants = grantsFrom(rawGrants);
    for (const repository of Object.keys(grants)) {
      if (!(await this.storage.get(`auth:${repository}`))) {
        throw new PolicyError(
          `GitHub authorization for ${repository} is missing`,
        );
      }
    }
    return this.storage.transaction(async (transaction) => {
      const instance = await transaction.get<CloudInstance>(`instance:${id}`);
      if (!instance || instance.disabled) {
        throw new PolicyError("VM access has been revoked");
      }
      if (instance.revision !== expectedRevision) {
        throw new PolicyError("policy changed while editing");
      }
      if (
        Object.keys(instance.grants).length === Object.keys(grants).length &&
        Object.entries(grants).every(
          ([repository, grant]) =>
            instance.grants[repository]?.read === grant.read &&
            instance.grants[repository].pushTaskBranches ===
              grant.pushTaskBranches,
        )
      ) {
        return instance;
      }
      const next = { ...instance, revision: instance.revision + 1, grants };
      await transaction.put(`instance:${id}`, next);
      const createdAt = Date.now();
      await transaction.put(eventKey(id, next.revision), {
        id: crypto.randomUUID(),
        createdAt,
        actor,
        instanceId: id,
        oldRevision: instance.revision,
        newRevision: next.revision,
        changes: { previous: instance.grants, current: grants },
      } satisfies CloudPolicyEvent);
      return next;
    });
  }

  async disable(id: string, actor: string): Promise<void> {
    await this.storage.transaction(async (transaction) => {
      const instance = await transaction.get<CloudInstance>(`instance:${id}`);
      if (!instance || instance.disabled) {
        throw new PolicyError("VM access has been revoked");
      }
      await transaction.put(`instance:${id}`, {
        ...instance,
        revision: instance.revision + 1,
        disabled: true,
        grants: {},
      });
      await transaction.delete(`name:${instance.name}`);
      const createdAt = Date.now();
      await transaction.put(eventKey(id, instance.revision + 1), {
        id: crypto.randomUUID(),
        createdAt,
        actor,
        instanceId: id,
        oldRevision: instance.revision,
        newRevision: instance.revision + 1,
        changes: { disabled: true, removed: instance.grants },
      } satisfies CloudPolicyEvent);
    });
  }

  async installAuthorization(record: unknown): Promise<void> {
    const validated = validAuthorization(record);
    const response = await this.request(
      `https://api.github.com/repos/${validated.repository}`,
      {
        headers: {
          accept: "application/vnd.github+json",
          authorization: `Bearer ${validated.accessToken}`,
          "user-agent": "dotfiles-vm-access",
        },
        redirect: "manual",
      },
    );
    if (response.status !== 200) {
      throw new PolicyError(
        "GitHub token cannot read the requested repository",
      );
    }
    await response.body?.cancel();
    await this.storage.put(`auth:${validated.repository}`, validated);
  }

  private async token(repository: string): Promise<string> {
    const key = normalizeRepository(repository);
    const pending = this.refreshing.get(key);
    if (pending) {
      return pending;
    }
    const task = this.refreshToken(key);
    this.refreshing.set(key, task);
    try {
      return await task;
    } finally {
      this.refreshing.delete(key);
    }
  }

  private async refreshToken(repository: string): Promise<string> {
    const record = await this.storage.get<CloudAuthorization>(
      `auth:${repository}`,
    );
    if (!record) {
      throw new Error(`GitHub authorization for ${repository} is missing`);
    }
    const now = Math.floor(Date.now() / 1000);
    if (record.accessExpiresAt - now >= 600) {
      return record.accessToken;
    }
    if (record.refreshExpiresAt <= now) {
      throw new Error(`GitHub authorization for ${repository} has expired`);
    }
    const response = await this.request(
      "https://github.com/login/oauth/access_token",
      {
        method: "POST",
        headers: {
          accept: "application/json",
          "content-type": "application/x-www-form-urlencoded",
          "user-agent": "dotfiles-vm-access",
        },
        body: new URLSearchParams({
          client_id: this.clientId,
          grant_type: "refresh_token",
          refresh_token: record.refreshToken,
        }),
        redirect: "manual",
      },
    );
    if (response.status !== 200) {
      throw new Error("GitHub refused authorization refresh");
    }
    const payload: Record<string, unknown> = await response.json();
    if (
      typeof payload.access_token !== "string" ||
      !tokenPattern.test(payload.access_token) ||
      typeof payload.refresh_token !== "string" ||
      !tokenPattern.test(payload.refresh_token) ||
      !Number.isSafeInteger(payload.expires_in) ||
      Number(payload.expires_in) <= 0 ||
      !Number.isSafeInteger(payload.refresh_token_expires_in) ||
      Number(payload.refresh_token_expires_in) <= 0
    ) {
      throw new Error("GitHub returned an invalid authorization refresh");
    }
    const renewed: CloudAuthorization = {
      repository,
      accessToken: payload.access_token,
      accessExpiresAt: now + Number(payload.expires_in),
      refreshToken: payload.refresh_token,
      refreshExpiresAt: now + Number(payload.refresh_token_expires_in),
    };
    return this.storage.transaction(async (transaction) => {
      const current = await transaction.get<CloudAuthorization>(
        `auth:${repository}`,
      );
      if (current?.refreshToken !== record.refreshToken) {
        if (!current) {
          throw new Error("GitHub authorization disappeared during refresh");
        }
        return current.accessToken;
      }
      await transaction.put(`auth:${repository}`, renewed);
      return renewed.accessToken;
    });
  }

  async allow(
    id: string,
    classification: Classification,
  ): Promise<AllowResult> {
    const before = await this.instance(id);
    if (!before || before.disabled) {
      return { allowed: false, reason: "VM access has been revoked" };
    }
    const repository =
      classification.operation === "identity.read"
        ? (Object.keys(before.grants).toSorted(
            (a, b) => Number(a > b) - Number(a < b),
          )[0] ?? null)
        : classification.repository;
    let access: Access;
    try {
      access = authorize(
        repository ? (before.grants[repository] ?? null) : null,
        classification.operation,
      );
    } catch (error) {
      if (error instanceof PolicyError) {
        return { allowed: false, reason: error.message };
      }
      throw error;
    }
    const token =
      access === "authenticated" && repository !== null
        ? await this.token(repository)
        : null;
    const current = await this.instance(id);
    if (!current || current.disabled) {
      return { allowed: false, reason: "VM access has been revoked" };
    }
    try {
      const currentAccess = authorize(
        repository ? (current.grants[repository] ?? null) : null,
        classification.operation,
      );
      if (access === "authenticated" && currentAccess !== access) {
        throw new PolicyError("repository is not granted to this VM");
      }
    } catch (error) {
      if (error instanceof PolicyError) {
        return { allowed: false, reason: error.message };
      }
      throw error;
    }
    const requestId = logKey(id, Date.now(), crypto.randomUUID());
    await this.storage.put(requestId, {
      id: requestId,
      createdAt: Date.now(),
      instanceId: id,
      repository,
      operation: classification.operation,
      refs: classification.refs,
      allowed: true,
      reason: access === "anonymous" ? "anonymous read" : "granted",
      revision: current.revision,
      upstreamStatus: null,
      durationMs: null,
      outcome: "unknown",
    } satisfies CloudAccessLog);
    return { allowed: true, requestId, token, access };
  }

  async deny(
    id: string,
    classification: Classification | null,
    reason: string,
  ): Promise<string> {
    const instance = await this.instance(id);
    const requestId = logKey(id, Date.now(), crypto.randomUUID());
    await this.storage.put(requestId, {
      id: requestId,
      createdAt: Date.now(),
      instanceId: id,
      repository: classification?.repository ?? null,
      operation: classification?.operation ?? "unknown",
      refs: classification?.refs ?? [],
      allowed: false,
      reason,
      revision: instance?.revision ?? 0,
      upstreamStatus: null,
      durationMs: null,
      outcome: "denied",
    } satisfies CloudAccessLog);
    return requestId;
  }

  async finish(
    requestId: string,
    status: number | null,
    durationMs: number,
    outcome: string,
  ): Promise<void> {
    const log = await this.storage.get<CloudAccessLog>(requestId);
    if (!log) {
      throw new Error("access log disappeared");
    }
    await this.storage.put(requestId, {
      ...log,
      upstreamStatus: status,
      durationMs,
      outcome,
    });
  }

  async logs(id: string, limit = 100): Promise<CloudAccessLog[]> {
    if (!Number.isSafeInteger(limit) || limit < 1 || limit > 500) {
      throw new PolicyError("invalid log limit");
    }
    return [
      ...(
        await this.storage.list<CloudAccessLog>({
          prefix: `access:${id}:`,
          reverse: true,
          limit,
        })
      ).values(),
    ];
  }

  async events(id: string, limit = 100): Promise<CloudPolicyEvent[]> {
    if (!Number.isSafeInteger(limit) || limit < 1 || limit > 500) {
      throw new PolicyError("invalid event limit");
    }
    return [
      ...(
        await this.storage.list<CloudPolicyEvent>({
          prefix: `event:${id}:`,
          reverse: true,
          limit,
        })
      ).values(),
    ];
  }

  async prune(now = Date.now()): Promise<void> {
    for (const [prefix, retention] of [
      ["access:", 30 * day],
      ["event:", 365 * day],
    ] as const) {
      let cursor: string | undefined;
      for (;;) {
        const batch = await this.storage.list<
          CloudAccessLog | CloudPolicyEvent
        >({
          prefix,
          limit: 500,
          ...(cursor ? { startAfter: cursor } : {}),
        });
        if (batch.size === 0) {
          break;
        }
        const expired: string[] = [];
        for (const [key, record] of batch) {
          if (record.createdAt < now - retention) {
            expired.push(key);
          }
          cursor = key;
        }
        if (expired.length > 0) {
          await this.storage.delete(expired);
        }
      }
    }
  }
}
