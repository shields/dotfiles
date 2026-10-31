import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
import { once } from "node:events";
import {
  chmodSync,
  mkdirSync,
  mkdtempSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import http from "node:http";
import path from "node:path";
import { test } from "bun:test";
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
