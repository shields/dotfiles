import { spawnSync } from "node:child_process";
import { chmodSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import YAML from "yaml";
import { AppAuth } from "../gateway/auth.ts";
import { normalizeRepository, type Grant } from "../gateway/policy.ts";
import type { CloudInstance } from "./authority.ts";

const root = path.resolve(import.meta.dirname, "..");
const configDir = path.join(os.homedir(), ".config", "vm-access");
const secretDir = path.join(os.homedir(), ".config", "secrets", "vm-access");
const urlFile = path.join(configDir, "cloudflare-url");
const tokenFile = path.join(secretDir, "cloudflare-admin-token");
const wranglerConfig = path.join(root, "cloudflare", "wrangler.jsonc");
const commandName = "bun run cloudflare/admin.ts";

function endpoint(): string {
  const address = readFileSync(urlFile, "utf8").trim();
  const url = new URL(address);
  if (
    url.protocol !== "https:" ||
    url.username ||
    url.password ||
    url.pathname !== "/" ||
    url.search ||
    url.hash
  ) {
    throw new Error("cloudflare-url must contain a bare HTTPS origin");
  }
  return url.origin;
}

function token(): string {
  const value = readFileSync(tokenFile, "utf8").trim();
  if (!/^[0-9a-f]{64}$/.test(value)) {
    throw new Error("Cloudflare admin token file is invalid");
  }
  return value;
}

async function api<T>(
  method: string,
  route: string,
  body?: unknown,
): Promise<T> {
  const response = await fetch(`${endpoint()}${route}`, {
    method,
    headers: {
      authorization: `Bearer ${token()}`,
      ...(body === undefined ? {} : { "content-type": "application/json" }),
    },
    ...(body === undefined ? {} : { body: JSON.stringify(body) }),
    redirect: "error",
    signal: AbortSignal.timeout(30_000),
  });
  if (!response.ok) {
    const payload: unknown = await response.json().catch(() => null);
    const message =
      payload &&
      typeof payload === "object" &&
      "error" in payload &&
      typeof payload.error === "string"
        ? payload.error
        : `request failed with HTTP ${String(response.status)}`;
    throw new Error(message);
  }
  return (response.status === 204 ? undefined : await response.json()) as T;
}

function profile(name: string): Grant {
  const contents: unknown = YAML.parse(
    readFileSync(
      path.join(root, ".config", "vm-access", "profiles.yaml"),
      "utf8",
    ),
  );
  if (!contents || typeof contents !== "object" || Array.isArray(contents)) {
    throw new Error("profile configuration is invalid");
  }
  const selected: unknown = (contents as Record<string, unknown>)[name];
  if (!selected || typeof selected !== "object" || Array.isArray(selected)) {
    throw new Error(`unknown profile ${name}`);
  }
  const grant = selected as Record<string, unknown>;
  if (
    Object.keys(grant).toSorted().join(",") !== "pushTaskBranches,read" ||
    grant.read !== true ||
    typeof grant.pushTaskBranches !== "boolean"
  ) {
    throw new Error(`invalid profile ${name}`);
  }
  return { read: true, pushTaskBranches: grant.pushTaskBranches };
}

function instanceId(value: string | undefined): string {
  if (!value || !/^[0-9a-f]{8}(?:-[0-9a-f]{4}){3}-[0-9a-f]{12}$/.test(value)) {
    throw new Error("expected a VM UUID");
  }
  return value;
}

function print(value: unknown): void {
  process.stdout.write(`${JSON.stringify(value, null, 2)}\n`);
}

async function command(args: string[]): Promise<void> {
  const [action, first, second, third] = args;
  switch (action) {
    case "config": {
      if (args.length !== 2 || !first) {
        throw new Error(`usage: ${commandName} config HTTPS-WORKER-URL`);
      }
      const address = new URL(first);
      if (
        address.protocol !== "https:" ||
        address.username ||
        address.password ||
        address.pathname !== "/" ||
        address.search ||
        address.hash
      ) {
        throw new Error("expected a bare HTTPS origin");
      }
      mkdirSync(configDir, { recursive: true, mode: 0o700 });
      writeFileSync(urlFile, `${address.origin}\n`, { mode: 0o600 });
      chmodSync(urlFile, 0o600);
      print({ url: address.origin });
      return;
    }
    case "authorize": {
      if (args.length !== 2 || !first) {
        throw new Error(`usage: ${commandName} authorize OWNER/REPO`);
      }
      const repository = normalizeRepository(first);
      const auth = new AppAuth(
        path.join(secretDir, "cloudflare-authorization"),
      );
      const issued = await auth.issue(repository);
      await api<undefined>("POST", "/authorizations", {
        repository,
        accessToken: issued.access_token,
        accessExpiresAt: issued.access_expires_at,
        refreshToken: issued.refresh_token,
        refreshExpiresAt: issued.refresh_expires_at,
      });
      print({ authorized: repository });
      return;
    }
    case "new": {
      if (args.length !== 2 || !first) {
        throw new Error(`usage: ${commandName} new NAME`);
      }
      print(await api<CloudInstance>("POST", "/instances", { name: first }));
      return;
    }
    case "show": {
      if (args.length !== 2) {
        throw new Error(`usage: ${commandName} show VM-ID`);
      }
      print(await api<CloudInstance>("GET", `/instances/${instanceId(first)}`));
      return;
    }
    case "list": {
      if (args.length !== 1) {
        throw new Error(`usage: ${commandName} list`);
      }
      print(await api<CloudInstance[]>("GET", "/instances"));
      return;
    }
    case "grant": {
      if (
        args.length !== 4 ||
        (third !== "read" && third !== "task") ||
        !second
      ) {
        throw new Error(
          `usage: ${commandName} grant VM-ID OWNER/REPO read|task`,
        );
      }
      const id = instanceId(first);
      const repository = normalizeRepository(second);
      const vm = await api<CloudInstance>("GET", `/instances/${id}`);
      print(
        await api<CloudInstance>("PUT", `/instances/${id}/grants`, {
          revision: vm.revision,
          grants: { ...vm.grants, [repository]: profile(third) },
        }),
      );
      return;
    }
    case "revoke": {
      if (args.length !== 3 || !second) {
        throw new Error(`usage: ${commandName} revoke VM-ID OWNER/REPO`);
      }
      const id = instanceId(first);
      const repository = normalizeRepository(second);
      const vm = await api<CloudInstance>("GET", `/instances/${id}`);
      if (!Object.hasOwn(vm.grants, repository)) {
        throw new Error(`${repository} is not granted to this VM`);
      }
      const grants = Object.fromEntries(
        Object.entries(vm.grants).filter(([key]) => key !== repository),
      );
      print(
        await api<CloudInstance>("PUT", `/instances/${id}/grants`, {
          revision: vm.revision,
          grants,
        }),
      );
      return;
    }
    case "start": {
      if (args.length !== 2) {
        throw new Error(`usage: ${commandName} start VM-ID`);
      }
      print(
        await api<unknown>("POST", `/instances/${instanceId(first)}/start`),
      );
      return;
    }
    case "delete": {
      if (args.length !== 2) {
        throw new Error(`usage: ${commandName} delete VM-ID`);
      }
      await api<undefined>("DELETE", `/instances/${instanceId(first)}`);
      print({ deleted: first });
      return;
    }
    case "logs":
    case "events": {
      if (args.length !== 2) {
        throw new Error(`usage: ${commandName} ${action} VM-ID`);
      }
      print(
        await api<unknown>("GET", `/instances/${instanceId(first)}/${action}`),
      );
      return;
    }
    case "containers":
    case "instances":
    case "ssh": {
      if (action === "ssh" && (args.length !== 2 || !first)) {
        throw new Error(`usage: ${commandName} ssh CONTAINER-ID`);
      }
      if (action === "instances" && (args.length !== 2 || !first)) {
        throw new Error(`usage: ${commandName} instances APPLICATION-ID`);
      }
      if (action === "containers" && args.length !== 1) {
        throw new Error(`usage: ${commandName} containers`);
      }
      const result = spawnSync(
        "bunx",
        [
          "wrangler",
          "containers",
          action === "containers" ? "list" : action,
          ...(action === "containers" ? [] : args.slice(1)),
          ...(action === "ssh" ? [] : ["--json"]),
          "--config",
          wranglerConfig,
        ],
        { stdio: "inherit" },
      );
      if (result.error) {
        throw result.error;
      }
      if (result.status !== 0) {
        process.exitCode = result.status ?? 1;
      }
      return;
    }
    default:
      throw new Error(
        `usage: ${commandName} {config|authorize|new|list|show|grant|revoke|start|delete|logs|events|containers|instances|ssh}`,
      );
  }
}

if (
  process.argv[1] &&
  path.resolve(process.argv[1]) === path.resolve(import.meta.filename)
) {
  try {
    await command(process.argv.slice(2));
  } catch (error: unknown) {
    process.stderr.write(
      `cloudflare/admin.ts: ${error instanceof Error ? error.message : String(error)}\n`,
    );
    process.exitCode = 1;
  }
}
