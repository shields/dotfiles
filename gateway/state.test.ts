import assert from "node:assert/strict";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import test from "node:test";
import { State } from "./state.ts";

void test("live grants are additive, conflict checked, and tied to immutable VM identities", () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-state-"));
  const state = new State(directory);
  try {
    const instance = state.register(
      "work",
      path.join(directory, "work.sock"),
      "tester",
    );
    assert.equal(
      state.update("work", 0, { "shields/dotfiles": "task" }, "tester"),
      1,
    );
    assert.equal(
      state.update(
        "work",
        1,
        { ...state.grants(instance.id), "shields/library": "read" },
        "tester",
      ),
      2,
    );
    assert.equal(state.profile(instance.id, "SHIELDS/LIBRARY"), "read");
    assert.throws(
      () => state.update("work", 1, {}, "tester"),
      /policy changed/,
    );
    assert.deepEqual(state.grants(instance.id), {
      "shields/dotfiles": "task",
      "shields/library": "read",
    });
    assert.throws(
      () =>
        state.update(
          "work",
          2,
          { "shields/new": "invalid" as "task" },
          "tester",
        ),
      /invalid profile/,
    );
    state.disable("work", "tester");
    const replacement = state.register(
      "work",
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

void test("audit record precedes a request and an interrupted request retains an unknown outcome", () => {
  const directory = mkdtempSync(path.join(tmpdir(), "vm-access-audit-"));
  const state = new State(directory);
  try {
    const instance = state.register(
      "work",
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
