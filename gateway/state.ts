import { randomUUID } from "node:crypto";
import { chmodSync, mkdirSync } from "node:fs";
import path from "node:path";
import { DatabaseSync } from "node:sqlite";
import { normalizeRepository, type Grant, type Operation } from "./policy.ts";

export interface Instance {
  id: string;
  name: string;
  socket: string;
  revision: number;
  disabled: boolean;
}

export interface AccessLog {
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

export interface PolicyEvent {
  id: string;
  createdAt: number;
  actor: string;
  instanceId: string;
  oldRevision: number;
  newRevision: number;
  changes: Record<string, unknown>;
}

interface InstanceRow {
  id: string;
  name: string;
  socket: string;
  revision: number;
  disabled: number;
}

interface GrantRow {
  repository: string;
  capabilities: string;
}

export class State {
  readonly db: DatabaseSync;

  constructor(directory: string) {
    mkdirSync(directory, { recursive: true, mode: 0o700 });
    chmodSync(directory, 0o700);
    this.db = new DatabaseSync(path.join(directory, "state.sqlite"));
    chmodSync(path.join(directory, "state.sqlite"), 0o600);
    this.db.exec(
      "PRAGMA foreign_keys = ON; PRAGMA journal_mode = WAL; PRAGMA busy_timeout = 5000;",
    );
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS instances (
        id TEXT PRIMARY KEY,
        name TEXT NOT NULL,
        socket TEXT NOT NULL UNIQUE,
        revision INTEGER NOT NULL DEFAULT 0,
        disabled INTEGER NOT NULL DEFAULT 0,
        created_at INTEGER NOT NULL
      );
      CREATE UNIQUE INDEX IF NOT EXISTS instances_active_name ON instances(name) WHERE disabled = 0;
      CREATE TABLE IF NOT EXISTS grants (
        instance_id TEXT NOT NULL REFERENCES instances(id),
        repository TEXT NOT NULL,
        capabilities TEXT NOT NULL,
        PRIMARY KEY (instance_id, repository)
      );
      CREATE TABLE IF NOT EXISTS policy_events (
        id TEXT PRIMARY KEY,
        created_at INTEGER NOT NULL,
        actor TEXT NOT NULL,
        instance_id TEXT NOT NULL,
        old_revision INTEGER NOT NULL,
        new_revision INTEGER NOT NULL,
        changes TEXT NOT NULL
      );
      CREATE TABLE IF NOT EXISTS access_logs (
        id TEXT PRIMARY KEY,
        created_at INTEGER NOT NULL,
        instance_id TEXT NOT NULL,
        repository TEXT,
        operation TEXT NOT NULL,
        refs TEXT NOT NULL,
        allowed INTEGER NOT NULL,
        reason TEXT NOT NULL,
        revision INTEGER NOT NULL,
        upstream_status INTEGER,
        duration_ms INTEGER,
        outcome TEXT NOT NULL
      );
      CREATE INDEX IF NOT EXISTS access_logs_instance_time ON access_logs(instance_id, created_at);
    `);
  }

  close(): void {
    this.db.close();
  }

  private transaction<T>(work: () => T): T {
    this.db.exec("BEGIN IMMEDIATE");
    try {
      const result = work();
      this.db.exec("COMMIT");
      return result;
    } catch (error) {
      this.db.exec("ROLLBACK");
      throw error;
    }
  }

  private instance(row: InstanceRow | undefined): Instance | null {
    if (!row) {
      return null;
    }
    return {
      id: row.id,
      name: row.name,
      socket: row.socket,
      revision: row.revision,
      disabled: row.disabled !== 0,
    };
  }

  byName(name: string): Instance | null {
    const row = this.db
      .prepare(
        "SELECT id, name, socket, revision, disabled FROM instances WHERE name = ? AND disabled = 0",
      )
      .get(name) as unknown as InstanceRow | undefined;
    return this.instance(row);
  }

  byId(id: string): Instance | null {
    const row = this.db
      .prepare(
        "SELECT id, name, socket, revision, disabled FROM instances WHERE id = ?",
      )
      .get(id) as unknown as InstanceRow | undefined;
    return this.instance(row);
  }

  active(): Instance[] {
    const rows = this.db
      .prepare(
        "SELECT id, name, socket, revision, disabled FROM instances WHERE disabled = 0",
      )
      .all() as unknown as InstanceRow[];
    return rows
      .map((row) => this.instance(row))
      .filter((instance): instance is Instance => instance !== null);
  }

  register(name: string, socket: string, actor: string): Instance {
    if (
      !/^lima:[A-Za-z0-9]+(?:[._-][A-Za-z0-9]+)*$/.test(name) ||
      name === "lima:dotfiles-base"
    ) {
      throw new Error("invalid VM name");
    }
    return this.transaction(() => {
      const existing = this.byName(name);
      if (existing && !existing.disabled) {
        throw new Error("VM is already registered");
      }
      const id = randomUUID();
      this.db
        .prepare(
          "INSERT INTO instances(id, name, socket, created_at) VALUES (?, ?, ?, ?)",
        )
        .run(id, name, socket, Date.now());
      this.event(actor, id, 0, 0, { registered: true });
      return (
        this.byId(id) ??
        (() => {
          throw new Error("registered VM vanished");
        })()
      );
    });
  }

  disable(name: string, actor: string): void {
    this.transaction(() => {
      const instance = this.byName(name);
      if (!instance || instance.disabled) {
        throw new Error("VM is not active");
      }
      const old = this.grants(instance.id);
      const next = instance.revision + 1;
      this.db
        .prepare("UPDATE instances SET disabled = 1, revision = ? WHERE id = ?")
        .run(next, instance.id);
      this.db
        .prepare("DELETE FROM grants WHERE instance_id = ?")
        .run(instance.id);
      this.event(actor, instance.id, instance.revision, next, {
        disabled: true,
        removed: old,
      });
    });
  }

  grants(instanceId: string): Record<string, Grant> {
    const rows = this.db
      .prepare(
        "SELECT repository, capabilities FROM grants WHERE instance_id = ? ORDER BY repository",
      )
      .all(instanceId) as unknown as GrantRow[];
    return Object.fromEntries(
      rows.map((row) => [
        row.repository,
        JSON.parse(row.capabilities) as Grant,
      ]),
    );
  }

  grant(instanceId: string, repository: string): Grant | null {
    const row = this.db
      .prepare(
        "SELECT capabilities FROM grants WHERE instance_id = ? AND repository = ?",
      )
      .get(instanceId, normalizeRepository(repository)) as unknown as
      | { capabilities: string }
      | undefined;
    return row ? (JSON.parse(row.capabilities) as Grant) : null;
  }

  update(
    name: string,
    expectedRevision: number | null,
    nextGrants: Record<string, Grant>,
    actor: string,
  ): number {
    return this.transaction(() => {
      const instance = this.byName(name);
      if (!instance || instance.disabled) {
        throw new Error("VM is not active");
      }
      if (expectedRevision !== null && instance.revision !== expectedRevision) {
        throw new Error(
          `policy changed while editing: expected revision ${String(expectedRevision)}, now ${String(instance.revision)}`,
        );
      }
      const normalized: Record<string, Grant> = {};
      for (const [repository, rawGrant] of Object.entries(
        nextGrants as Record<string, unknown>,
      )) {
        if (
          !rawGrant ||
          typeof rawGrant !== "object" ||
          Array.isArray(rawGrant)
        ) {
          throw new Error(`invalid grant for ${repository}`);
        }
        const grant = rawGrant as Record<string, unknown>;
        if (
          Object.keys(grant).toSorted().join(",") !== "pushTaskBranches,read" ||
          grant.read !== true ||
          typeof grant.pushTaskBranches !== "boolean"
        ) {
          throw new Error(`invalid grant for ${repository}`);
        }
        const key = normalizeRepository(repository);
        if (Object.hasOwn(normalized, key)) {
          throw new Error(`duplicate repository ${repository}`);
        }
        normalized[key] = {
          read: true,
          pushTaskBranches: grant.pushTaskBranches,
        };
      }
      const old = this.grants(instance.id);
      const added = Object.fromEntries(
        Object.entries(normalized).filter(
          ([repository, grant]) =>
            JSON.stringify(old[repository]) !== JSON.stringify(grant),
        ),
      );
      const removed = Object.fromEntries(
        Object.entries(old).filter(
          ([repository]) => !Object.hasOwn(normalized, repository),
        ),
      );
      if (
        Object.keys(added).length === 0 &&
        Object.keys(removed).length === 0
      ) {
        return instance.revision;
      }
      const revision = instance.revision + 1;
      this.db
        .prepare("DELETE FROM grants WHERE instance_id = ?")
        .run(instance.id);
      const insert = this.db.prepare(
        "INSERT INTO grants(instance_id, repository, capabilities) VALUES (?, ?, ?)",
      );
      for (const [repository, grant] of Object.entries(normalized)) {
        insert.run(instance.id, repository, JSON.stringify(grant));
      }
      this.db
        .prepare("UPDATE instances SET revision = ? WHERE id = ?")
        .run(revision, instance.id);
      this.event(actor, instance.id, instance.revision, revision, {
        added,
        removed,
      });
      return revision;
    });
  }

  private event(
    actor: string,
    instanceId: string,
    oldRevision: number,
    newRevision: number,
    changes: object,
  ): void {
    this.db
      .prepare("INSERT INTO policy_events VALUES (?, ?, ?, ?, ?, ?, ?)")
      .run(
        randomUUID(),
        Date.now(),
        actor,
        instanceId,
        oldRevision,
        newRevision,
        JSON.stringify(changes),
      );
  }

  logDecision(input: {
    id: string;
    instanceId: string;
    repository: string | null;
    operation: Operation | "unknown";
    refs: string[];
    allowed: boolean;
    reason: string;
    revision: number;
  }): void {
    this.db
      .prepare(
        "INSERT INTO access_logs VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, NULL, NULL, ?)",
      )
      .run(
        input.id,
        Date.now(),
        input.instanceId,
        input.repository,
        input.operation,
        JSON.stringify(input.refs),
        input.allowed ? 1 : 0,
        input.reason,
        input.revision,
        input.allowed ? "unknown" : "denied",
      );
  }

  finish(
    id: string,
    upstreamStatus: number | null,
    durationMs: number,
    outcome: string,
  ): void {
    this.db
      .prepare(
        "UPDATE access_logs SET upstream_status = ?, duration_ms = ?, outcome = ? WHERE id = ?",
      )
      .run(upstreamStatus, durationMs, outcome, id);
  }

  logs(instanceId: string, limit = 100): AccessLog[] {
    const rows = this.db
      .prepare(
        "SELECT id, created_at, instance_id, repository, operation, refs, allowed, reason, revision, upstream_status, duration_ms, outcome FROM access_logs WHERE instance_id = ? ORDER BY created_at DESC LIMIT ?",
      )
      .all(instanceId, limit) as unknown as Record<string, unknown>[];
    return rows.map((row) => ({
      id: String(row.id),
      createdAt: Number(row.created_at),
      instanceId: String(row.instance_id),
      repository: row.repository === null ? null : (row.repository as string),
      operation: String(row.operation),
      refs: JSON.parse(String(row.refs)) as string[],
      allowed: Number(row.allowed) !== 0,
      reason: String(row.reason),
      revision: Number(row.revision),
      upstreamStatus:
        row.upstream_status === null ? null : Number(row.upstream_status),
      durationMs: row.duration_ms === null ? null : Number(row.duration_ms),
      outcome: String(row.outcome),
    }));
  }

  events(instanceId: string, limit = 100): PolicyEvent[] {
    const rows = this.db
      .prepare(
        "SELECT id, created_at, actor, instance_id, old_revision, new_revision, changes FROM policy_events WHERE instance_id = ? ORDER BY created_at DESC LIMIT ?",
      )
      .all(instanceId, limit) as unknown as Record<string, unknown>[];
    return rows.map((row) => ({
      id: row.id as string,
      createdAt: Number(row.created_at),
      actor: row.actor as string,
      instanceId: row.instance_id as string,
      oldRevision: Number(row.old_revision),
      newRevision: Number(row.new_revision),
      changes: JSON.parse(row.changes as string) as Record<string, unknown>,
    }));
  }

  prune(now = Date.now()): void {
    this.transaction(() => {
      this.db
        .prepare("DELETE FROM access_logs WHERE created_at < ?")
        .run(now - 30 * 86_400_000);
      this.db
        .prepare("DELETE FROM policy_events WHERE created_at < ?")
        .run(now - 365 * 86_400_000);
    });
  }
}
