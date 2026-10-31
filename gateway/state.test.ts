import assert from "node:assert/strict";
import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { test } from "bun:test";
import { State } from "./state.ts";

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
