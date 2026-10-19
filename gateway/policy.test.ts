import assert from "node:assert/strict";
import test from "node:test";
import {
  authorize,
  classify,
  denial,
  graphqlRepository,
  parsePushCommands,
  rewriteLinks,
  type Classification,
  PushDenied,
} from "./policy.ts";

const id = "01234567-89ab-4cde-8fab-0123456789ab";
const old = "1".repeat(40);
const next = "2".repeat(40);

function packet(line: string): Buffer {
  const body = Buffer.from(line);
  return Buffer.concat([
    Buffer.from((body.length + 4).toString(16).padStart(4, "0")),
    body,
  ]);
}

function push(...lines: string[]): Buffer {
  return Buffer.concat([...lines.map(packet), Buffer.from("0000PACK")]);
}

function pushDenial(body: Uint8Array): PushDenied {
  try {
    parsePushCommands(body, id);
  } catch (error) {
    assert.ok(error instanceof PushDenied);
    return error;
  }
  assert.fail("push was not denied");
}

function sideband(report: Uint8Array): Buffer {
  const bytes = Buffer.from(report);
  const payload: Buffer[] = [];
  let offset = 0;
  for (;;) {
    const size = Number.parseInt(
      bytes.subarray(offset, offset + 4).toString(),
      16,
    );
    if (size === 0) {
      assert.equal(offset + 4, bytes.length);
      return Buffer.concat(payload);
    }
    assert.ok(size <= 65_520);
    assert.equal(bytes[offset + 4], 1);
    payload.push(bytes.subarray(offset + 5, offset + size));
    offset += size;
  }
}

void test("a batch with another destination is denied before any ref can be forwarded", () => {
  const body = push(
    `${old} ${next} refs/heads/task/${id}/one\0report-status\n`,
    `${old} ${next} refs/heads/main\n`,
  );
  assert.throws(() => parsePushCommands(body, id), /outside this VM/);
});

void test("denied ref updates are reported in the status format the client requested", () => {
  const commands = (capabilities: string): Buffer =>
    push(
      `${old} ${next} refs/heads/task/${id}/one\0${capabilities}`,
      `${old} ${next} refs/heads/main`,
      `${old} ${next} refs/tags/v1`,
      `${old} ${"0".repeat(40)} refs/heads/task/${id}/gone`,
    );
  const report = Buffer.concat([
    packet("unpack ok\n"),
    packet(
      `ng refs/heads/task/${id}/one another ref update in this push is denied\n`,
    ),
    packet(
      `ng refs/heads/main push destination is outside this VM's task namespace; allowed namespace: task/${id}/*\n`,
    ),
    packet(
      `ng refs/tags/v1 push destination is outside this VM's task namespace; allowed namespace: task/${id}/*\n`,
    ),
    packet(`ng refs/heads/task/${id}/gone branch deletion is denied\n`),
    Buffer.from("0000"),
  ]);
  for (const capabilities of [
    " report-status agent=git/2.56.0",
    " report-status-v2 quiet object-format=sha1",
  ]) {
    const denied = pushDenial(commands(capabilities));
    assert.equal(
      denied.message,
      `push destination is outside this VM's task namespace; allowed namespace: task/${id}/*`,
    );
    assert.deepEqual(denied.report, new Uint8Array(report));
  }
  const multiplexed = pushDenial(
    commands(" report-status-v2 side-band-64k quiet"),
  ).report;
  assert.ok(multiplexed);
  assert.deepEqual(sideband(multiplexed), report);
  assert.equal(
    pushDenial(commands(" side-band-64k agent=git/2.56.0")).report,
    null,
  );
  assert.equal(
    pushDenial(commands(" report-status-v2side-band-64k")).report,
    null,
  );
});

void test("a sideband report larger than one packet is split across packets", () => {
  const refs = Array.from(
    { length: 1000 },
    (_, index) => `refs/heads/denied/${String(index).padStart(60, "0")}`,
  );
  const report = pushDenial(
    push(
      ...refs.map(
        (ref, index) =>
          `${old} ${next} ${ref}${index === 0 ? "\0report-status side-band-64k" : ""}`,
      ),
    ),
  ).report;
  assert.ok(report);
  const plain = sideband(report).toString();
  assert.ok(plain.length > 65_515);
  assert.ok(plain.startsWith("000eunpack ok\n"));
  assert.ok(
    plain.endsWith(
      `${refs.at(-1) ?? ""} push destination is outside this VM's task namespace; allowed namespace: task/${id}/*\n0000`,
    ),
  );
});

void test("a push naming one destination twice is denied without a report", () => {
  for (const ref of [`refs/heads/task/${id}/one`, "refs/heads/main"]) {
    assert.throws(
      () =>
        parsePushCommands(
          push(
            `${old} ${next} ${ref}\0report-status`,
            `${old} ${next} refs/heads/task/${id}/two`,
            `${next} ${old} ${ref}`,
          ),
          id,
        ),
      (error: unknown) =>
        error instanceof Error &&
        !(error instanceof PushDenied) &&
        error.message === "duplicate Git push destination",
    );
  }
});

void test("a denial waits for the full command list", () => {
  assert.equal(
    parsePushCommands(
      packet(`${old} ${next} refs/heads/main\0report-status\n`),
      id,
    ),
    null,
  );
});

void test("denials reach git as text and gh as a JSON message", () => {
  for (const target of [
    "/shields/dotfiles.git/info/refs?service=git-receive-pack",
    "/shields/dotfiles/info/refs?service=git-upload-pack",
    "/shields/dotfiles.git/git-upload-pack",
    "/shields/dotfiles.git/git-receive-pack",
  ]) {
    assert.deepEqual(denial("github.com", target, "no", "request-1"), {
      contentType: "text/plain; charset=utf-8",
      body: "no\nrequest ID: request-1\n",
    });
  }
  assert.deepEqual(
    denial("github.com", "/shields/dotfiles.git/git-receive-pack", "no", null),
    { contentType: "text/plain; charset=utf-8", body: "no\n" },
  );
  for (const [host, target] of [
    ["api.github.com", "/repos/shields/dotfiles/issues"],
    ["api.github.com", "/repos/shields/dotfiles/info/refs"],
    ["github.com", "/shields/dotfiles/pulls"],
    ["github.com", "/shields/dotfiles/pulls?next=/info/refs"],
  ] as const) {
    const response = denial(host, target, "no", "request-1");
    assert.equal(response.contentType, "application/json");
    assert.deepEqual(JSON.parse(response.body), {
      message: "no",
      request_id: "request-1",
    });
  }
});

void test("task branch creation and force updates are both accepted", () => {
  const created = push(
    `${"0".repeat(40)} ${next} refs/heads/task/${id}/new\0report-status\n`,
  );
  const forced = push(
    `${old} ${next} refs/heads/task/${id}/new\0report-status\n`,
  );
  assert.deepEqual(parsePushCommands(created, id)?.refs, [
    `refs/heads/task/${id}/new`,
  ]);
  assert.deepEqual(parsePushCommands(new Uint8Array(created), id)?.refs, [
    `refs/heads/task/${id}/new`,
  ]);
  assert.deepEqual(parsePushCommands(forced, id)?.refs, [
    `refs/heads/task/${id}/new`,
  ]);
});

void test("branch deletion, push options, and incomplete commands are denied", () => {
  assert.throws(
    () =>
      parsePushCommands(
        push(
          `${old} ${"0".repeat(40)} refs/heads/task/${id}/new\0report-status\n`,
        ),
        id,
      ),
    /deletion/,
  );
  for (const capabilities of ["push-options", "report-status push-options=1"]) {
    assert.throws(
      () =>
        parsePushCommands(
          push(`${old} ${next} refs/heads/task/${id}/new\0${capabilities}\n`),
          id,
        ),
      /options/,
    );
  }
  assert.throws(
    () =>
      parsePushCommands(
        push(
          `${old} ${next} refs/heads/task/${id}/new\0report-status\0 push-options\n`,
        ),
        id,
      ),
    /capabilities/,
  );
  assert.equal(
    parsePushCommands(
      packet(`${old} ${next} refs/heads/task/${id}/new\0report-status\n`),
      id,
    ),
    null,
  );
});

void test("a lone flush packet is classified as a push probe", () => {
  const probe = Buffer.from("0000");
  assert.equal(parsePushCommands(probe, id), null);
  assert.throws(() => parsePushCommands(push(), id), /no ref updates/);
  for (const target of [
    "/shields/dotfiles.git/git-receive-pack",
    "/shields/dotfiles/git-receive-pack",
  ]) {
    assert.deepEqual(
      classify({ host: "github.com", method: "POST", target, body: probe }, id),
      { repository: "shields/dotfiles", operation: "git.push", refs: [] },
    );
  }
  assert.throws(
    () =>
      classify(
        {
          host: "github.com",
          method: "POST",
          target: "/shields/dotfiles.git/git-receive-pack",
          body: push(),
        },
        id,
      ),
    /no ref updates/,
  );
});

void test("a push from a shallow clone lists its shallow commits before the commands", () => {
  const shallow = `shallow ${"3".repeat(40)}\n`;
  const command = `${old} ${next} refs/heads/task/${id}/new\0report-status\n`;
  const receivePack = (body: Buffer): Classification =>
    classify(
      {
        host: "github.com",
        method: "POST",
        target: "/shields/dotfiles.git/git-receive-pack",
        body,
      },
      id,
    );
  assert.deepEqual(receivePack(push(shallow, command)), {
    repository: "shields/dotfiles",
    operation: "git.push",
    refs: [`refs/heads/task/${id}/new`],
  });
  const sha256 = (digit: string): string => digit.repeat(64);
  assert.deepEqual(
    parsePushCommands(
      push(
        `shallow ${sha256("3")}\n`,
        `shallow ${sha256("4")}`,
        `${sha256("0")} ${sha256("2")} refs/heads/task/${id}/new\0report-status\n`,
        `${sha256("1")} ${sha256("2")} refs/heads/task/${id}/other\n`,
      ),
      id,
    )?.refs,
    [`refs/heads/task/${id}/new`, `refs/heads/task/${id}/other`],
  );
  assert.equal(parsePushCommands(packet(shallow), id), null);
  assert.throws(() => receivePack(packet(shallow)), /incomplete/);
  assert.throws(() => receivePack(push(shallow)), /no ref updates/);
  assert.throws(
    () => receivePack(push(shallow, `${old} ${next} refs/heads/main\n`)),
    /outside this VM/,
  );
  assert.throws(
    () =>
      receivePack(
        push(
          shallow,
          `${old} ${next} refs/heads/task/${id}/new\0push-options\n`,
        ),
      ),
    /options/,
  );
  for (const line of [
    `shallow ${"3".repeat(40)}\0report-status\n`,
    `shallow ${"3".repeat(39)}\n`,
    `shallow ${"3".repeat(41)}\n`,
    `shallow ${"A".repeat(40)}\n`,
    `shallow  ${"3".repeat(40)}\n`,
    `shallow ${"3".repeat(40)}\n\n`,
  ]) {
    assert.throws(
      () => receivePack(push(line, command)),
      /unsupported Git push command/,
      JSON.stringify(line),
    );
  }
  assert.throws(
    () => receivePack(push(command, shallow)),
    /unsupported Git push command/,
  );
});

void test("GraphQL allows one scoped query and rejects mutation and cross-repository reads", () => {
  const body = Buffer.from(
    JSON.stringify({
      query:
        "query($o:String!,$n:String!){repository(owner:$o,name:$n){name pullRequests(first:1){nodes{title}}}}",
      variables: { o: "Shields", n: "Dotfiles" },
    }),
  );
  assert.equal(graphqlRepository(body), "shields/dotfiles");
  assert.throws(
    () =>
      graphqlRepository(
        Buffer.from(
          JSON.stringify({
            query: "mutation{createIssue(input:{}){clientMutationId}}",
          }),
        ),
      ),
    /only GraphQL queries/,
  );
  assert.throws(
    () =>
      graphqlRepository(
        Buffer.from(
          JSON.stringify({
            query:
              '{a:repository(owner:"a",name:"x"){name} b:repository(owner:"b",name:"y"){name}}',
          }),
        ),
      ),
    /exactly one repository/,
  );
});

void test("GraphQL accepts gh's interactive repository discovery query", () => {
  const query = `
    fragment repo on Repository {
      id
      databaseId
      name
      owner { login }
      viewerPermission
      defaultBranchRef { name }
      isPrivate
    }
    query RepositoryNetwork {
      viewer { login }
      repo_000: repository(owner: "shields", name: "dotfiles") {
        ...repo
        parent { ...repo }
      }
    }
  `;
  for (const variables of [undefined, null, {}]) {
    assert.equal(
      graphqlRepository(Buffer.from(JSON.stringify({ query, variables }))),
      "shields/dotfiles",
    );
  }
  for (const variables of [[], "", "{}", 0, false]) {
    assert.throws(
      () =>
        graphqlRepository(Buffer.from(JSON.stringify({ query, variables }))),
      /invalid GraphQL variables/,
    );
  }
});

void test("GraphQL viewer reads allow only the login alongside one repository", () => {
  const repository = 'repository(owner:"shields",name:"dotfiles"){name}';
  const body = (query: string): Buffer =>
    Buffer.from(JSON.stringify({ query, variables: null }));
  assert.equal(
    graphqlRepository(
      body(`query { ${repository} me:viewer { ...identity } }
        fragment identity on User { username:login }`),
    ),
    "shields/dotfiles",
  );
  for (const query of [
    `{ ${repository} viewer { login repositories(first:1) { nodes { name } } } }`,
    `{ ${repository} viewer { login:repositories(first:1) { nodes { name } } } }`,
    `{ ${repository} viewer { ... on User { organizations(first:1) { nodes { name } } } } }`,
    `query { ${repository} viewer { ...identity } }
      fragment identity on User { repositories(first:1) { nodes { name } } }`,
    `{ ${repository} user(login:"shields") { login } }`,
  ]) {
    assert.throws(() => graphqlRepository(body(query)), /unsupported GraphQL/);
  }
  assert.throws(
    () => graphqlRepository(body("{ viewer { login } }")),
    /exactly one repository/,
  );
  assert.throws(
    () =>
      graphqlRepository(
        body(`{ ${repository} viewer { login }
          other:repository(owner:"other",name:"repo"){name} }`),
      ),
    /exactly one repository/,
  );
});

void test("REST reads can be anonymous and API writes have no classification", () => {
  const read = classify(
    {
      host: "api.github.com",
      method: "GET",
      target: "/repos/Shields/Dotfiles/pulls/1",
    },
    id,
  );
  assert.equal(read.repository, "shields/dotfiles");
  assert.equal(read.operation, "api.read");
  assert.equal(
    classify(
      {
        host: "api.github.com",
        method: "GET",
        target: "/__vm_access__/health",
      },
      id,
    ).operation,
    "public.read",
  );
  assert.equal(
    classify({ host: "api.github.com", method: "GET", target: "/user" }, id)
      .operation,
    "identity.read",
  );
  assert.doesNotThrow(() => {
    authorize({ read: true, pushTaskBranches: false }, read.operation);
  });
  assert.equal(authorize(null, read.operation), "anonymous");
  assert.throws(() => {
    classify(
      {
        host: "api.github.com",
        method: "POST",
        target: "/repos/shields/dotfiles/git/refs",
      },
      id,
    );
  }, /unsupported/);
  assert.throws(() => {
    classify(
      {
        host: "api.github.com",
        method: "POST",
        target: "/repos/shields/dotfiles/pulls",
      },
      id,
    );
  }, /unsupported/);
  assert.throws(() => {
    authorize({ read: true, pushTaskBranches: false }, "git.push");
  }, /not granted/);
  assert.doesNotThrow(() => {
    authorize({ read: true, pushTaskBranches: true }, "git.push");
  });
});

void test("REST pagination links name the requested repository instead of its ID", () => {
  const read = classify(
    {
      host: "api.github.com",
      method: "GET",
      target: "/repos/Cli/CLI/issues?per_page=1",
    },
    id,
  );
  const link = (repository: string): string =>
    [
      `<https://api.github.com/${repository}/issues?per_page=1&after=Y3Vy%3D&page=2>; rel="next"`,
      `<https://api.github.com/${repository}/issues?per_page=1&page=1&before=Y3Vy%3D>; rel="prev"`,
      `<https://api.github.com/${repository}?page=3>; rel="last"`,
      `<https://api.github.com/${repository}>; rel="first"`,
    ].join(", ");
  const rewritten = rewriteLinks(link("repositories/212613049"), read);
  assert.equal(rewritten, link("repos/cli/cli"));
  const targets = [
    ...rewritten.matchAll(/<https:\/\/api\.github\.com([^>]+)>/g),
  ].map((match) => match[1] ?? "");
  assert.equal(targets.length, 4);
  for (const target of targets) {
    assert.deepEqual(
      classify({ host: "api.github.com", method: "GET", target }, id),
      read,
    );
  }
  for (const unchanged of [
    '<https://api.github.com/repositories?since=369>; rel="next"',
    '<https://api.github.com/repositories/12x/issues?page=2>; rel="next"',
    '<https://uploads.github.com/repositories/1/issues?page=2>; rel="next"',
    '<https://api.github.com/user/repos?page=2>; rel="next"',
  ]) {
    assert.equal(rewriteLinks(unchanged, read), unchanged);
  }
  for (const other of [
    { repository: "cli/cli", operation: "identity.read", refs: [] },
    { repository: "cli/cli", operation: "graphql.read", refs: [] },
    { repository: "cli/cli", operation: "public.read", refs: [] },
    { repository: null, operation: "api.read", refs: [] },
  ] satisfies Classification[]) {
    assert.equal(
      rewriteLinks(link("repositories/212613049"), other),
      link("repositories/212613049"),
    );
  }
  assert.throws(
    () =>
      classify(
        {
          host: "api.github.com",
          method: "GET",
          target: "/repositories/212613049/issues?page=2",
        },
        id,
      ),
    /unsupported/,
  );
});

void test("anonymous access covers repository reads without granting writes or identity", () => {
  for (const operation of ["git.read", "api.read", "public.read"] as const) {
    assert.equal(authorize(null, operation), "anonymous");
    assert.equal(
      authorize({ read: false, pushTaskBranches: true }, operation),
      "anonymous",
    );
  }
  for (const operation of [
    "git.push",
    "graphql.read",
    "identity.read",
  ] as const) {
    assert.throws(() => authorize(null, operation), /not granted/);
  }
  assert.equal(
    authorize({ read: true, pushTaskBranches: false }, "api.read"),
    "authenticated",
  );
  assert.equal(
    authorize({ read: true, pushTaskBranches: true }, "public.read"),
    "anonymous",
  );
});

void test("Git discovery separates fetches from pushes with and without a .git suffix", () => {
  for (const repository of ["shields/dotfiles", "octocat/Hello-World"]) {
    for (const suffix of ["", ".git"]) {
      for (const service of ["git-upload-pack", "git-receive-pack"]) {
        const classification = classify(
          {
            host: "github.com",
            method: "GET",
            target: `/${repository}${suffix}/info/refs?service=${service}`,
          },
          id,
        );
        assert.equal(classification.repository, repository.toLowerCase());
        assert.equal(
          classification.operation,
          service === "git-upload-pack" ? "git.read" : "git.push",
        );
        if (service === "git-upload-pack") {
          assert.equal(authorize(null, classification.operation), "anonymous");
        } else {
          assert.throws(
            () => authorize(null, classification.operation),
            /not granted/,
          );
          assert.throws(
            () =>
              authorize(
                { read: true, pushTaskBranches: false },
                classification.operation,
              ),
            /not granted/,
          );
        }
      }
      assert.equal(
        classify(
          {
            host: "github.com",
            method: "POST",
            target: `/${repository}${suffix}/git-upload-pack`,
          },
          id,
        ).operation,
        "git.read",
      );
    }
  }
  assert.throws(
    () =>
      classify(
        {
          host: "github.com",
          method: "GET",
          target: "/shields/dotfiles.git/info/refs?service=other",
        },
        id,
      ),
    /unsupported/,
  );
});

void test("repository web pages are always anonymous and accept only reads", () => {
  for (const target of [
    "/shields/dotfiles",
    "/shields/dotfiles/blob/main/README.md",
    "/shields/dotfiles/releases/download/v1/archive.zip",
    "/shields/dotfiles/archive/refs/heads/main.zip",
    "/shields/dotfiles/issues/1",
  ]) {
    for (const method of ["GET", "HEAD"]) {
      assert.deepEqual(classify({ host: "github.com", method, target }, id), {
        repository: "shields/dotfiles",
        operation: "public.read",
        refs: [],
      });
    }
    assert.throws(
      () => classify({ host: "github.com", method: "POST", target }, id),
      /unsupported/,
    );
  }
});
