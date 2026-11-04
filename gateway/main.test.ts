import assert from "node:assert/strict";
import { spawn, spawnSync, type SpawnSyncReturns } from "node:child_process";
import { once } from "node:events";
import {
  chmodSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import http from "node:http";
import path from "node:path";
import { onTestFinished, test } from "bun:test";
import YAML from "yaml";
import { State } from "./state.ts";

test("CLI grants and edits apply live and disabled VM logs stay addressable by ID", async () => {
  const root = mkdtempSync("/tmp/vma-");
  const lima = path.join(root, "lima");
  const secrets = path.join(root, "secrets");
  mkdirSync(path.join(lima, "sock"), { recursive: true });
  mkdirSync(secrets);
  const repository = "shields/dotfiles";
  writeFileSync(
    path.join(secrets, `${Buffer.from(repository).toString("base64url")}.json`),
    JSON.stringify({
      client_id: "Iv-test",
      repository,
      access_token: "ghu_test",
      access_expires_at: Math.floor(Date.now() / 1000) + 3600,
      refresh_token: "ghr_test",
      refresh_expires_at: Math.floor(Date.now() / 1000) + 86_400,
      web_url: "https://github.com",
      api_url: "https://api.github.com",
    }),
  );
  const environment = {
    ...process.env,
    VM_ACCESS_ROOT: root,
    VM_ACCESS_PROFILES: path.join(
      import.meta.dirname,
      "../.config/vm-access/profiles.yaml",
    ),
  };
  const main = path.join(root, "main.mjs");
  const build = spawnSync(
    process.execPath,
    [
      "build",
      path.join(import.meta.dirname, "main.ts"),
      "--target=bun",
      "--outfile",
      main,
    ],
    { encoding: "utf8", timeout: 30_000 },
  );
  assert.equal(build.status, 0, build.stderr);
  const service = spawn(process.execPath, [main, "serve"], {
    env: environment,
    stdio: ["ignore", "ignore", "pipe"],
  });
  let serviceError = "";
  service.stderr.on("data", (chunk: Buffer) => {
    serviceError += chunk.toString();
  });
  const call = (args: string[], extra: NodeJS.ProcessEnv = {}): string => {
    const result = spawnSync(process.execPath, [main, ...args], {
      env: { ...environment, ...extra },
      encoding: "utf8",
      timeout: 30_000,
    });
    assert.equal(result.status, 0, result.stderr);
    return result.stdout;
  };
  try {
    const admin = path.join(root, "state", "admin.sock");
    let healthy = false;
    for (let attempt = 0; attempt < 600; attempt++) {
      healthy = await new Promise<boolean>((resolve) => {
        const request = http.get(
          { socketPath: admin, path: "/health" },
          (response) => {
            response.resume();
            resolve(response.statusCode === 200);
          },
        );
        request.once("error", () => {
          resolve(false);
        });
      });
      if (healthy || service.exitCode !== null || service.signalCode !== null) {
        break;
      }
      await new Promise((resolve) => {
        setTimeout(resolve, 50);
      });
    }
    assert.equal(healthy, true, serviceError);
    const instance = JSON.parse(
      call(["register", "lima:trial", "--lima-dir", lima]),
    ) as {
      id: string;
      socket: string;
    };
    assert.match(instance.id, /^[0-9a-f-]{36}$/);
    assert.match(
      call(["grant", "lima:trial", repository, "--profile", "task"]),
      /revision 1/,
    );
    const initial = YAML.parse(call(["show", "lima:trial"])) as {
      repositories: Record<string, { pushTaskBranches: boolean }>;
    };
    assert.equal(initial.repositories[repository]?.pushTaskBranches, true);
    const editor = path.join(root, "editor");
    writeFileSync(
      editor,
      '#!/bin/sh\nset -eu\nsed \'s/pushTaskBranches: true/pushTaskBranches: false/\' "$1" > "$1.tmp"\nmv "$1.tmp" "$1"\n',
    );
    chmodSync(editor, 0o700);
    assert.match(
      call(["edit", "lima:trial"], { EDITOR: editor, VISUAL: editor }),
      /revision 2/,
    );
    const edited = YAML.parse(call(["show", "lima:trial"])) as {
      repositories: Record<string, { pushTaskBranches: boolean }>;
    };
    assert.equal(edited.repositories[repository]?.pushTaskBranches, false);
    call(["disable", "lima:trial"]);
    const state = new State(path.join(root, "state"));
    try {
      for (let index = 0; index < 101; index++) {
        state.logDecision({
          id: `request-${String(index)}`,
          instanceId: instance.id,
          repository,
          operation: "git.read",
          refs: [],
          allowed: true,
          reason: "granted",
          revision: 2,
        });
      }
    } finally {
      state.close();
    }
    assert.equal(call(["logs", instance.id]).trim().split("\n").length, 104);
    const history = call(["logs", instance.id, "--all"])
      .trim()
      .split("\n")
      .map((line) => JSON.parse(line) as { kind: string; vm: string });
    assert.equal(history.length, 105);
    assert.ok(history.every((entry) => entry.vm === "lima:trial"));
    assert.equal(history.filter((entry) => entry.kind === "policy").length, 4);
  } finally {
    if (service.exitCode === null && service.signalCode === null) {
      const exited = once(service, "exit");
      service.kill("SIGTERM");
      await exited;
    }
    rmSync(root, { recursive: true, force: true });
  }
}, 60_000);

interface Sandbox {
  root: string;
  ghCalls: string;
  run: (args: string[], extra?: NodeJS.ProcessEnv) => SpawnSyncReturns<string>;
}

// gh records its arguments and fails, so reaching GitHub authorization shows
// up as a call.
function sandbox(): Sandbox {
  const root = mkdtempSync("/tmp/vma-");
  onTestFinished(() => {
    rmSync(root, { recursive: true, force: true });
  });
  const bin = path.join(root, "bin");
  const ghCalls = path.join(root, "gh-calls");
  mkdirSync(bin);
  writeFileSync(
    path.join(bin, "gh"),
    `#!/bin/sh\necho "$*" >> ${ghCalls}\nexit 1\n`,
    { mode: 0o700 },
  );
  const state = new State(path.join(root, "state"));
  try {
    state.register("lima:trial", path.join(root, "trial.sock"), "tester");
  } finally {
    state.close();
  }
  return {
    root,
    ghCalls,
    run: (args, extra = {}) =>
      spawnSync(
        process.execPath,
        [path.join(import.meta.dirname, "main.ts"), ...args],
        {
          env: {
            ...process.env,
            PATH: `${bin}:${process.env.PATH ?? ""}`,
            VM_ACCESS_ROOT: root,
            ...extra,
          },
          encoding: "utf8",
          timeout: 30_000,
        },
      ),
  };
}

function repositoriesEditor(root: string): string {
  const editor = path.join(root, "editor");
  writeFileSync(
    editor,
    '#!/bin/sh\nset -eu\nsed "/^repositories:/,\\$d" "$1" > "$1.tmp"\nprintf "repositories: %s\\n" "$REPOSITORIES" >> "$1.tmp"\nmv "$1.tmp" "$1"\n',
    { mode: 0o700 },
  );
  return editor;
}

test("grant rejects an invalid profile definition before it authorizes anything", () => {
  const { root, ghCalls, run } = sandbox();
  const read = { read: true, pushTaskBranches: false };
  const task = { read: true, pushTaskBranches: true };
  const profiles = path.join(root, "profiles.yaml");
  for (const [name, profile] of [
    ["task", { ...task, pushTaskBranches: "false" }],
    ["Task", task],
  ] as const) {
    writeFileSync(profiles, YAML.stringify({ read, task, [name]: profile }));
    const result = run(
      ["grant", "lima:trial", "shields/dotfiles", "--profile", "task"],
      { VM_ACCESS_PROFILES: profiles },
    );
    assert.equal(result.status, 1, result.stdout);
    assert.equal(result.stderr, `vm-access: invalid profile ${name}\n`);
  }
  assert.equal(existsSync(ghCalls), false);
});

test("grant rejects a profile name inherited from Object.prototype before it authorizes anything", () => {
  const { root, ghCalls, run } = sandbox();
  const grant = { read: true, pushTaskBranches: false };
  const profiles = path.join(root, "profiles.yaml");
  writeFileSync(profiles, YAML.stringify({ read: grant, task: grant }));
  for (const name of [
    "constructor",
    "toString",
    "__proto__",
    "hasOwnProperty",
  ]) {
    const result = run(
      ["grant", "lima:trial", "shields/dotfiles", "--profile", name],
      { VM_ACCESS_PROFILES: profiles },
    );
    assert.equal(result.status, 1, result.stdout);
    assert.equal(result.stderr, `vm-access: unknown profile ${name}\n`);
  }
  assert.equal(existsSync(ghCalls), false);
});

test("edit rejects an invalid policy before it authorizes anything", () => {
  const { root, ghCalls, run } = sandbox();
  const editor = repositoriesEditor(root);
  const valid = "{read: true, pushTaskBranches: false}";
  for (const [repositories, message] of [
    [
      "{shields/new: {read: true, pushTaskBranches: maybe}}",
      "invalid grant for shields/new",
    ],
    [
      `{shields/new: ${valid}, Shields/New: ${valid}}`,
      "duplicate repository Shields/New",
    ],
    [`{"not a repository": ${valid}}`, "repository must be OWNER/REPO"],
    ["null", "repositories must be a mapping"],
    ["5", "repositories must be a mapping"],
    ["[]", "repositories must be a mapping"],
  ] as const) {
    const result = run(["edit", "lima:trial"], {
      EDITOR: editor,
      VISUAL: editor,
      REPOSITORIES: repositories,
    });
    assert.equal(result.status, 1, result.stdout);
    assert.equal(result.stderr, `vm-access: ${message}\n`);
  }
  assert.equal(existsSync(ghCalls), false);
});

test("edit keeps the grants when repositories is a set, an ordered map, or a timestamp", () => {
  const { root, ghCalls, run } = sandbox();
  const read = { read: true, pushTaskBranches: false };
  const seeded = new State(path.join(root, "state"));
  try {
    seeded.update("lima:trial", 0, { "shields/dotfiles": read }, "tester");
  } finally {
    seeded.close();
  }
  const editor = repositoriesEditor(root);
  for (const repositories of [
    "!!set {shields/dotfiles}",
    "!!omap [{shields/dotfiles: {read: true, pushTaskBranches: false}}]",
    "!!timestamp 2001-12-14",
  ]) {
    const result = run(["edit", "lima:trial"], {
      EDITOR: editor,
      VISUAL: editor,
      REPOSITORIES: repositories,
    });
    assert.equal(result.status, 1, result.stdout);
    assert.equal(result.stderr, "vm-access: repositories must be a mapping\n");
  }
  const state = new State(path.join(root, "state"));
  try {
    const instance = state.byName("lima:trial");
    assert.ok(instance);
    assert.equal(instance.revision, 1);
    assert.deepEqual(state.grants(instance.id), { "shields/dotfiles": read });
  } finally {
    state.close();
  }
  assert.equal(existsSync(ghCalls), false);
});

test("edit authorizes a repository it adds", () => {
  const { root, ghCalls, run } = sandbox();
  const editor = repositoriesEditor(root);
  const result = run(["edit", "lima:trial"], {
    EDITOR: editor,
    VISUAL: editor,
    REPOSITORIES: "{shields/new: {read: true, pushTaskBranches: false}}",
  });
  assert.equal(result.status, 1, result.stdout);
  assert.equal(
    result.stderr,
    "vm-access: gh cannot read shields/new; check the repository and your Mac gh login\n",
  );
  assert.equal(
    readFileSync(ghCalls, "utf8"),
    "api repos/shields/new --jq .id\n",
  );
});
