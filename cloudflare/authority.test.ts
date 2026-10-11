import assert from "node:assert/strict";
import test from "node:test";
import { CloudAuthority, type CloudAuthorization } from "./authority.ts";

const now = Math.floor(Date.now() / 1000);

class MemoryStorage {
  private values = new Map<string, unknown>();

  get<T>(key: string): Promise<T | undefined> {
    return Promise.resolve(
      structuredClone(this.values.get(key)) as T | undefined,
    );
  }

  put(key: string, value: unknown): Promise<void> {
    this.values.set(key, structuredClone(value));
    return Promise.resolve();
  }

  delete(key: string): Promise<boolean>;
  delete(keys: string[]): Promise<number>;
  delete(input: string | string[]): Promise<boolean | number> {
    if (Array.isArray(input)) {
      return Promise.resolve(
        input.reduce(
          (count, key) => count + Number(this.values.delete(key)),
          0,
        ),
      );
    }
    return Promise.resolve(this.values.delete(input));
  }

  list<T>(options: {
    prefix?: string;
    reverse?: boolean;
    limit?: number;
    startAfter?: string;
  }): Promise<Map<string, T>> {
    const keys = [...this.values.keys()]
      .filter(
        (key) =>
          key.startsWith(options.prefix ?? "") &&
          (options.startAfter === undefined || key > options.startAfter),
      )
      .toSorted();
    if (options.reverse) {
      keys.reverse();
    }
    return Promise.resolve(
      new Map(
        keys
          .slice(0, options.limit)
          .map((key) => [key, structuredClone(this.values.get(key)) as T]),
      ),
    );
  }

  async transaction<T>(
    work: (transaction: DurableObjectTransaction) => Promise<T>,
  ): Promise<T> {
    const snapshot = new MemoryStorage();
    snapshot.values = new Map(this.values);
    const result = await work(snapshot as unknown as DurableObjectTransaction);
    this.values = snapshot.values;
    return result;
  }
}

function authority(
  request: typeof fetch = (() =>
    Promise.resolve(new Response(null, { status: 200 }))) as typeof fetch,
): CloudAuthority {
  return new CloudAuthority(
    new MemoryStorage() as unknown as DurableObjectStorage,
    "Iv23lim5x4MdkNNgv28z",
    request,
  );
}

function authorization(): CloudAuthorization {
  return {
    repository: "shields/dotfiles",
    accessToken: "access-token",
    accessExpiresAt: now + 3600,
    refreshToken: "refresh-token",
    refreshExpiresAt: now + 86_400,
  };
}

void test("live grants and revocations use immutable VM IDs", async () => {
  const state = authority();
  await state.installAuthorization(authorization());
  const instance = await state.register("work", "owner");
  const granted = await state.update(
    instance.id,
    0,
    { "shields/dotfiles": { read: true, pushTaskBranches: true } },
    "owner",
  );
  assert.equal(granted.revision, 1);
  const read = await state.allow(instance.id, {
    repository: "shields/dotfiles",
    operation: "api.read",
    refs: [],
  });
  assert.equal(read.allowed, true);
  assert.ok(read.allowed);
  assert.equal(read.token, "access-token");
  await state.finish(read.requestId, 200, 12, "completed");
  assert.equal((await state.logs(instance.id))[0]?.upstreamStatus, 200);

  await state.update(instance.id, 1, {}, "owner");
  const revoked = await state.allow(instance.id, {
    repository: "shields/dotfiles",
    operation: "git.push",
    refs: [`refs/heads/task/${instance.id}/work`],
  });
  assert.deepEqual(revoked, {
    allowed: false,
    reason: "repository is not granted to this VM",
  });
  await state.disable(instance.id, "owner");
  const replacement = await state.register("work", "owner");
  assert.notEqual(replacement.id, instance.id);
  assert.deepEqual(replacement.grants, {});
  const events = await state.events(instance.id);
  assert.equal(events.length, 4);
  assert.deepEqual(events.find((event) => event.newRevision === 1)?.changes, {
    previous: {},
    current: { "shields/dotfiles": { read: true, pushTaskBranches: true } },
  });
});

void test("refresh is shared across concurrent calls and never appears in logs", async () => {
  let refreshes = 0;
  const request = ((input: RequestInfo | URL) => {
    const url =
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.href
          : input.url;
    if (url.includes("/login/oauth/access_token")) {
      refreshes += 1;
      return Promise.resolve(
        Response.json({
          access_token: "new-access-token",
          expires_in: 28_800,
          refresh_token: "new-refresh-token",
          refresh_token_expires_in: 86_400,
        }),
      );
    }
    return Promise.resolve(new Response(null, { status: 200 }));
  }) as typeof fetch;
  const state = authority(request);
  await state.installAuthorization({
    ...authorization(),
    accessExpiresAt: now + 120,
  });
  const instance = await state.register("work", "owner");
  await state.update(
    instance.id,
    0,
    { "shields/dotfiles": { read: true, pushTaskBranches: false } },
    "owner",
  );
  const classification = {
    repository: "shields/dotfiles",
    operation: "git.read" as const,
    refs: [],
  };
  const results = await Promise.all([
    state.allow(instance.id, classification),
    state.allow(instance.id, classification),
  ]);
  assert.equal(refreshes, 1);
  assert.deepEqual(
    results.map((result) => result.allowed && result.token),
    ["new-access-token", "new-access-token"],
  );
  assert.doesNotMatch(JSON.stringify(await state.logs(instance.id)), /token/);
});

void test("ungranted repository reads are anonymous, audited, and revoked with the VM", async () => {
  const state = authority((() => {
    throw new Error("anonymous access must not request a token");
  }) as typeof fetch);
  const instance = await state.register("work", "owner");
  for (const repository of ["shields/dotfiles", "octocat/hello-world"]) {
    for (const operation of ["git.read", "api.read", "public.read"] as const) {
      const allowed = await state.allow(instance.id, {
        repository,
        operation,
        refs: [],
      });
      assert.ok(allowed.allowed);
      assert.equal(allowed.access, "anonymous");
      assert.equal(allowed.token, null);
      const log = (await state.logs(instance.id)).find(
        (entry) => entry.id === allowed.requestId,
      );
      assert.equal(log?.repository, repository);
      assert.equal(log.operation, operation);
      assert.equal(log.reason, "anonymous read");
    }
    for (const operation of [
      "git.push",
      "graphql.read",
      "identity.read",
    ] as const) {
      assert.deepEqual(
        await state.allow(instance.id, { repository, operation, refs: [] }),
        {
          allowed: false,
          reason: "repository is not granted to this VM",
        },
      );
    }
  }
  await state.disable(instance.id, "owner");
  assert.deepEqual(
    await state.allow(instance.id, {
      repository: "shields/dotfiles",
      operation: "git.read",
      refs: [],
    }),
    { allowed: false, reason: "VM access has been revoked" },
  );
});

void test("revoking a grant removes credentials while preserving anonymous reads", async () => {
  const state = authority();
  await state.installAuthorization(authorization());
  const instance = await state.register("work", "owner");
  await state.update(
    instance.id,
    0,
    {
      "shields/dotfiles": { read: true, pushTaskBranches: true },
    },
    "owner",
  );
  const publicRead = await state.allow(instance.id, {
    repository: "shields/dotfiles",
    operation: "public.read",
    refs: [],
  });
  assert.ok(publicRead.allowed);
  assert.equal(publicRead.access, "anonymous");
  assert.equal(publicRead.token, null);
  await state.update(instance.id, 1, {}, "owner");
  const read = await state.allow(instance.id, {
    repository: "shields/dotfiles",
    operation: "api.read",
    refs: [],
  });
  assert.ok(read.allowed);
  assert.equal(read.access, "anonymous");
  assert.equal(read.token, null);
});

void test("stale policy revisions and missing authorization fail closed", async () => {
  const state = authority();
  const instance = await state.register("work", "owner");
  await assert.rejects(
    state.update(
      instance.id,
      0,
      { "shields/dotfiles": { read: true, pushTaskBranches: false } },
      "owner",
    ),
    /authorization.*missing/,
  );
  await state.installAuthorization(authorization());
  await state.update(
    instance.id,
    0,
    { "shields/dotfiles": { read: true, pushTaskBranches: false } },
    "owner",
  );
  await assert.rejects(
    state.update(instance.id, 0, {}, "owner"),
    /policy changed/,
  );
  assert.equal((await state.instance(instance.id))?.revision, 1);
  await assert.rejects(
    state.installAuthorization(null),
    /invalid GitHub authorization/,
  );
  await assert.rejects(
    state.installAuthorization({ ...authorization(), accessToken: null }),
    /invalid GitHub authorization/,
  );
});

void test("revocation during token refresh prevents forwarding", async () => {
  let release: ((response: Response) => void) | undefined;
  const request = ((input: RequestInfo | URL) => {
    const url =
      typeof input === "string"
        ? input
        : input instanceof URL
          ? input.href
          : input.url;
    if (url.includes("/login/oauth/access_token")) {
      return new Promise<Response>((resolve) => {
        release = resolve;
      });
    }
    return Promise.resolve(new Response(null, { status: 200 }));
  }) as typeof fetch;
  const state = authority(request);
  await state.installAuthorization({
    ...authorization(),
    accessExpiresAt: now + 120,
  });
  const vm = await state.register("work", "owner");
  await state.update(
    vm.id,
    0,
    {
      "shields/dotfiles": { read: true, pushTaskBranches: false },
    },
    "owner",
  );
  const pending = state.allow(vm.id, {
    repository: "shields/dotfiles",
    operation: "git.read",
    refs: [],
  });
  await new Promise<void>((resolve) => setTimeout(resolve, 0));
  assert.ok(release);
  await state.update(vm.id, 1, {}, "owner");
  release(
    Response.json({
      access_token: "new-access-token",
      expires_in: 28_800,
      refresh_token: "new-refresh-token",
      refresh_token_expires_in: 86_400,
    }),
  );
  assert.deepEqual(await pending, {
    allowed: false,
    reason: "repository is not granted to this VM",
  });
  assert.deepEqual(await state.logs(vm.id), []);
});

void test("pruning preserves recent records across storage pages", async () => {
  const storage = new MemoryStorage();
  const state = new CloudAuthority(
    storage as unknown as DurableObjectStorage,
    "Iv23lim5x4MdkNNgv28z",
  );
  const vm = await state.register("work", "owner");
  const current = Date.now();
  for (let index = 0; index < 501; index += 1) {
    await storage.put(`access:${vm.id}:${String(index).padStart(13, "0")}`, {
      createdAt: current - 31 * 86_400_000,
    });
  }
  await storage.put(`access:${vm.id}:recent`, { createdAt: current });
  await state.prune(current);
  assert.equal((await storage.list({ prefix: `access:${vm.id}:` })).size, 1);
  assert.equal((await state.events(vm.id)).length, 1);
});
