import assert from "node:assert/strict";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { afterEach, mock, spyOn, test } from "bun:test";
import { State } from "./state.ts";

afterEach(() => {
  mock.restore();
});

test("live grants are additive, conflict checked, and tied to immutable VM identities", () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-state-"));
  const state = new State(directory);
  try {
    const instance = state.register(
      "lima:work",
      path.join(directory, "work.sock"),
      "tester",
    );
    assert.equal(
      state.update(
        "lima:work",
        0,
        { "shields/dotfiles": { read: true, pushTaskBranches: true } },
        "tester",
      ),
      1,
    );
    assert.equal(
      state.update(
        "lima:work",
        1,
        {
          ...state.grants(instance.id),
          "shields/library": { read: true, pushTaskBranches: false },
        },
        "tester",
      ),
      2,
    );
    assert.deepEqual(state.grant(instance.id, "SHIELDS/LIBRARY"), {
      read: true,
      pushTaskBranches: false,
    });
    assert.throws(
      () => state.update("lima:work", 1, {}, "tester"),
      /policy changed/,
    );
    assert.deepEqual(state.grants(instance.id), {
      "shields/dotfiles": { read: true, pushTaskBranches: true },
      "shields/library": { read: true, pushTaskBranches: false },
    });
    assert.throws(
      () =>
        state.update(
          "lima:work",
          2,
          {
            "shields/new": "invalid" as unknown as {
              read: true;
              pushTaskBranches: true;
            },
          },
          "tester",
        ),
      /invalid grant/,
    );
    state.disable("lima:work", "tester");
    const replacement = state.register(
      "lima:work",
      path.join(directory, "new.sock"),
      "tester",
    );
    assert.notEqual(replacement.id, instance.id);
    assert.deepEqual(state.grants(replacement.id), {});
  } finally {
    state.close();
    rmSync(directory, { recursive: true, force: true });
  }
});

test("a policy that repeats a repository in another case changes nothing", () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-state-"));
  const state = new State(directory);
  try {
    const instance = state.register(
      "lima:work",
      path.join(directory, "work.sock"),
      "tester",
    );
    const read = { read: true, pushTaskBranches: false };
    const task = { read: true, pushTaskBranches: true };
    state.update("lima:work", 0, { "shields/dotfiles": read }, "tester");
    assert.throws(
      () =>
        state.update(
          "lima:work",
          1,
          { "Shields/Dotfiles": read, "shields/dotfiles": task },
          "tester",
        ),
      /duplicate repository shields\/dotfiles/,
    );
    assert.equal(state.byName("lima:work")?.revision, 1);
    assert.deepEqual(state.grants(instance.id), { "shields/dotfiles": read });
  } finally {
    state.close();
    rmSync(directory, { recursive: true, force: true });
  }
});

test("audit record precedes a request and an interrupted request retains an unknown outcome", () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-audit-"));
  const state = new State(directory);
  try {
    const instance = state.register(
      "lima:work",
      path.join(directory, "work.sock"),
      "tester",
    );
    state.logDecision({
      id: "request-1",
      instanceId: instance.id,
      repository: "shields/dotfiles",
      operation: "git.read",
      refs: [],
      allowed: true,
      reason: "granted",
      revision: 0,
    });
    assert.equal(state.logs(instance.id)[0]?.outcome, "unknown");
    state.finish("request-1", 200, 15, "completed");
    assert.equal(state.logs(instance.id)[0]?.upstreamStatus, 200);
  } finally {
    state.close();
    rmSync(directory, { recursive: true, force: true });
  }
});

const day = 86_400_000;

function withState(run: (state: State) => void): void {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-prune-"));
  const state = new State(directory);
  try {
    run(state);
  } finally {
    state.close();
    rmSync(directory, { recursive: true, force: true });
  }
}

function insertLog(state: State, id: string, createdAt: number): void {
  state.db
    .prepare(
      "INSERT INTO access_logs (id, created_at, instance_id, operation, refs, allowed, reason, revision, outcome) VALUES (?, ?, 'instance', 'git.read', '[]', 1, 'granted', 0, 'completed')",
    )
    .run(id, createdAt);
}

function insertEvent(state: State, id: string, createdAt: number): void {
  state.db
    .prepare(
      "INSERT INTO policy_events (id, created_at, actor, instance_id, old_revision, new_revision, changes) VALUES (?, ?, 'tester', 'instance', 0, 1, '{}')",
    )
    .run(id, createdAt);
}

function ids(state: State, table: string): string[] {
  return state.db
    .prepare(`SELECT id FROM ${table} ORDER BY id`)
    .all()
    .map((row) => row.id as string);
}

test("pruning finds expired audit rows through an index on their time", () => {
  withState((state) => {
    const prepared: string[] = [];
    const prepare = state.db.prepare.bind(state.db);
    spyOn(state.db, "prepare").mockImplementation((sql: string) => {
      prepared.push(sql);
      return prepare(sql);
    });
    state.prune();
    mock.restore();
    const deletes = prepared.filter((sql) =>
      sql.startsWith("DELETE FROM access_logs"),
    );
    assert.equal(deletes.length, 1);
    const plan = state.db
      .prepare(`EXPLAIN QUERY PLAN ${deletes[0] ?? ""}`)
      .all(0)
      .map((row) => row.detail as string);
    assert.equal(plan.length, 1);
    assert.match(
      plan[0] ?? "",
      /^SEARCH access_logs USING (COVERING )?INDEX access_logs_created \(created_at<\?\)$/,
    );
  });
});

test("pruning deletes exactly the rows older than the retention periods", () => {
  withState((state) => {
    const now = 1_800_000_000_000;
    const logCutoff = now - 30 * day;
    const eventCutoff = now - 365 * day;
    for (const [id, createdAt] of [
      ["a-expired", logCutoff - 1],
      ["b-boundary", logCutoff],
      ["c-recent", logCutoff + 1],
      ["d-now", now],
    ] as const) {
      insertLog(state, id, createdAt);
    }
    for (const [id, createdAt] of [
      ["a-expired", eventCutoff - 1],
      ["b-boundary", eventCutoff],
      ["c-recent", eventCutoff + 1],
      ["d-now", now],
    ] as const) {
      insertEvent(state, id, createdAt);
    }
    state.prune(now);
    const kept = ["b-boundary", "c-recent", "d-now"];
    assert.deepEqual(ids(state, "access_logs"), kept);
    assert.deepEqual(ids(state, "policy_events"), kept);
  });
});

test("opening a database that lacks the time index adds it and keeps its rows", () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-index-"));
  try {
    const legacy = new State(directory);
    try {
      insertLog(legacy, "kept", Date.now());
      legacy.db.exec("DROP INDEX access_logs_created");
    } finally {
      legacy.close();
    }
    const reopened = new State(directory);
    try {
      assert.deepEqual(
        reopened.db
          .prepare(
            "SELECT name FROM sqlite_master WHERE type = 'index' AND name = 'access_logs_created'",
          )
          .all()
          .map((row) => row.name as string),
        ["access_logs_created"],
      );
      assert.deepEqual(ids(reopened, "access_logs"), ["kept"]);
    } finally {
      reopened.close();
    }
    const again = new State(directory);
    again.close();
  } finally {
    rmSync(directory, { recursive: true, force: true });
  }
});
