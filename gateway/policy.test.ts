import assert from "node:assert/strict";
import { afterEach, mock, spyOn, test } from "bun:test";
import {
  authorize,
  classify,
  denial,
  graphqlRepository,
  parseGrant,
  parsePushCommands,
  rewriteLinks,
  type Classification,
  PushDenied,
} from "./policy.ts";

afterEach(() => {
  mock.restore();
});

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

test("a batch with another destination is denied before any ref can be forwarded", () => {
  const body = push(
    `${old} ${next} refs/heads/task/${id}/one\0report-status\n`,
    `${old} ${next} refs/heads/main\n`,
  );
  assert.throws(() => parsePushCommands(body, id), /outside this VM/);
});

test("denied ref updates are reported in the status format the client requested", () => {
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

test("a sideband report larger than one packet is split across packets", () => {
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

test("a push naming one destination twice is denied without a report", () => {
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

test("a denial waits for the full command list", () => {
  assert.equal(
    parsePushCommands(
      packet(`${old} ${next} refs/heads/main\0report-status\n`),
      id,
    ),
    null,
  );
});

test("denials reach git as text and gh as a JSON message", () => {
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

test("task branch creation and force updates are both accepted", () => {
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

test("branch deletion, push options, and incomplete commands are denied", () => {
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

test("a lone flush packet is classified as a push probe", () => {
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

test("a push from a shallow clone lists its shallow commits before the commands", () => {
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

test("GraphQL allows one scoped query and rejects mutation and cross-repository reads", () => {
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

test("GraphQL accepts gh's interactive repository discovery query", () => {
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

test("GraphQL viewer reads allow only the login alongside one repository", () => {
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

const repositoryField = 'repository(owner:"shields",name:"dotfiles"){name}';
const graphqlBody = (query: string): Buffer =>
  Buffer.from(JSON.stringify({ query }));

test("classify expands each fragment of a deeply nested chain once", () => {
  const spread = (name: string, count: number): string =>
    `...${name} `.repeat(count);
  const depth = 10;
  const chain = Array.from({ length: depth }, (_, level) =>
    level < depth - 1
      ? `fragment F${String(level)} on Query { ${spread(`F${String(level + 1)}`, 10)} }`
      : `fragment F${String(level)} on Query { ${repositoryField} }`,
  );
  const body = graphqlBody(
    `query { ${spread("F0", 3000)} } ${chain.join("\n")}`,
  );
  const started = performance.now();
  assert.deepEqual(
    classify(
      { host: "api.github.com", method: "POST", target: "/graphql", body },
      id,
    ),
    { repository: "shields/dotfiles", operation: "graphql.read", refs: [] },
  );
  assert.ok(performance.now() - started < 5000, "classification was slow");
});

test("GraphQL accepts a fragment spread on several paths and rejects unknown and recursive ones", () => {
  assert.equal(
    graphqlRepository(
      graphqlBody(`{ ...A ...B viewer { ...C ...C } }
        fragment A on Query { ...D ${repositoryField} }
        fragment B on Query { ...D ...A }
        fragment C on User { login }
        fragment D on Query { __typename }`),
    ),
    "shields/dotfiles",
  );
  assert.equal(
    graphqlRepository(
      graphqlBody(`{ ${repositoryField} viewer { ...A } viewer { ...A ...B } viewer { ...B ...A } }
        fragment A on User { login ...B } fragment B on User { login }`),
    ),
    "shields/dotfiles",
  );
  for (const query of [
    `{ ${repositoryField} ...Missing }`,
    `{ ${repositoryField} ...A } fragment A on Query { ...Missing }`,
    `{ ${repositoryField} ...A } fragment A on Query { ...A }`,
    `{ ${repositoryField} ...A } fragment A on Query { ...B } fragment B on Query { ...A }`,
    `{ ${repositoryField} ...A ...B } fragment A on Query { a } fragment B on Query { ...A ...B }`,
    `{ ${repositoryField} ...A ...B } fragment A on Query { ...B } fragment B on Query { ...A }`,
    `{ ${repositoryField} ...A } fragment A on Query { ... on Query { ...A } }`,
    `{ ${repositoryField} viewer { ...A } } fragment A on User { ...A }`,
    `{ ${repositoryField} viewer { ...A } viewer { ...B } } fragment A on User { login } fragment B on User { ...B }`,
    `{ ${repositoryField} viewer { ...A } viewer { ...C } } fragment A on User { login } fragment C on User { ...A ...D } fragment D on User { ...C }`,
    `{ ${repositoryField} viewer { ...A } viewer { ...A ...Missing } } fragment A on User { login }`,
  ]) {
    assert.throws(
      () => graphqlRepository(graphqlBody(query)),
      /unknown or recursive GraphQL fragment/,
      query,
    );
  }
});

test("GraphQL holds each viewer, and the root, to its own rule for a repeated fragment", () => {
  for (const query of [
    `{ ${repositoryField} ...R viewer { ...R } } fragment R on Query { __typename }`,
    `{ ${repositoryField} viewer { ...A } viewer { ...A name } } fragment A on User { login }`,
    `{ ${repositoryField} viewer { ...A } viewer { ...B } } fragment A on User { login } fragment B on User { ...A name }`,
  ]) {
    assert.throws(
      () => graphqlRepository(graphqlBody(query)),
      /unsupported GraphQL viewer field/,
      query,
    );
  }
  assert.throws(
    () =>
      graphqlRepository(
        graphqlBody(
          `{ ${repositoryField} viewer { ...A } ...A } fragment A on User { login }`,
        ),
      ),
    /unsupported GraphQL root field/,
  );
});

test("GraphQL viewer selections expand a shared fragment once", () => {
  const add = spyOn(Set.prototype, "add");
  const additions = (viewers: number): number => {
    add.mockClear();
    graphqlRepository(
      graphqlBody(`{ ${repositoryField} ${"viewer { ...F } ".repeat(viewers)} }
        fragment F on User { login }`),
    );
    return add.mock.calls.length;
  };
  assert.equal(additions(100), additions(1));
});

test("REST reads can be anonymous and API writes have no classification", () => {
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

test("REST pagination links name the requested repository instead of its ID", () => {
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

test("anonymous access covers repository reads without granting writes or identity", () => {
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

test("Git discovery separates fetches from pushes with and without a .git suffix", () => {
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

test("repository web pages are always anonymous and accept only reads", () => {
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

test("a request target is a path on the requested host and never names an authority", () => {
  for (const [host, target] of [
    [
      "github.com",
      "//attacker:ghp_token@github.com/octocat/Hello-World.git/info/refs?service=git-upload-pack",
    ],
    ["github.com", "//ghp_token@github.com/octocat/Hello-World"],
    ["github.com", "//:ghp_token@github.com/octocat/Hello-World"],
    ["github.com", "//%61ttacker:%74oken@github.com:443/octocat/Hello-World"],
    [
      "github.com",
      String.raw`/\attacker:ghp_token@github.com/octocat/Hello-World`,
    ],
    ["github.com", "///attacker:ghp_token@github.com/octocat/Hello-World"],
    ["github.com", "/\t/attacker:ghp_token@github.com/octocat/Hello-World"],
    ["github.com", "/\r\n/github.com/octocat/Hello-World"],
    [
      "api.github.com",
      "//attacker:ghp_token@api.github.com/repos/octocat/Hello-World/pulls",
    ],
    ["github.com", "//github.com/octocat/Hello-World"],
    ["github.com", "//GitHub.com:443/octocat/Hello-World"],
    ["github.com", String.raw`/\github.com/octocat/Hello-World`],
    ["github.com", "//evil.example/octocat/Hello-World"],
    ["github.com", "//github.com@evil.example/octocat/Hello-World"],
    ["github.com", "https://github.com/octocat/Hello-World"],
    ["github.com", "octocat/Hello-World"],
    ["github.com", ""],
  ] as const) {
    assert.throws(
      () => classify({ host, method: "GET", target }, id),
      /request target must stay on its GitHub host/,
      target,
    );
  }
  for (const target of [
    "/octocat/Hello-World/blob/main//README.md",
    "/octocat/Hello-World/issues/1?q=//a:b@github.com",
    "/octocat/Hello-World#//a:b@github.com",
  ]) {
    assert.equal(
      classify({ host: "github.com", method: "GET", target }, id).repository,
      "octocat/hello-world",
      target,
    );
  }
});

test("a grant has exactly read true and a boolean pushTaskBranches", () => {
  for (const pushTaskBranches of [true, false]) {
    const input = { pushTaskBranches, read: true };
    const grant = parseGrant(input);
    assert.deepEqual(grant, { read: true, pushTaskBranches });
    assert.notEqual(grant, input);
    assert.deepEqual(Object.keys(grant), ["read", "pushTaskBranches"]);
  }
});

test("non-objects, arrays, and functions are not grants", () => {
  for (const input of [
    undefined,
    null,
    true,
    0,
    "",
    "read",
    [],
    [true, true],
    [{ read: true, pushTaskBranches: true }],
    () => ({ read: true, pushTaskBranches: true }),
    Object.assign([], { read: true, pushTaskBranches: true }),
    Object.assign(() => 0, { read: true, pushTaskBranches: true }),
  ]) {
    assert.equal(parseGrant(input), null);
  }
});

test("a grant with a missing, extra, or mistyped field is rejected", () => {
  const rejected: unknown[] = [
    {},
    { read: true },
    { pushTaskBranches: true },
    { read: true, pushTaskBranches: true, extra: true },
    { read: true, pushTaskBranches: true, extra: undefined },
    { read: false, pushTaskBranches: true },
    { read: "true", pushTaskBranches: true },
    { read: 1, pushTaskBranches: true },
    { read: null, pushTaskBranches: true },
    { read: undefined, pushTaskBranches: true },
    { read: true, pushTaskBranches: "true" },
    { read: true, pushTaskBranches: 1 },
    { read: true, pushTaskBranches: 0 },
    { read: true, pushTaskBranches: null },
    { read: true, pushTaskBranches: undefined },
    { read: true, push: true },
    { read: true, pushTaskBranches: [true] },
  ];
  for (const input of rejected) {
    assert.equal(parseGrant(input), null);
  }
});

test("inherited and special keys do not make a grant", () => {
  const inherited = { read: true, pushTaskBranches: true };
  assert.equal(parseGrant(Object.create(inherited)), null);
  assert.equal(
    parseGrant(Object.assign(Object.create(inherited), { read: true })),
    null,
  );
  assert.equal(
    parseGrant(Object.assign(Object.create(inherited), { a: 1, b: 2 })),
    null,
  );
  assert.equal(
    parseGrant(
      Object.assign(Object.create({ pushTaskBranches: true }), {
        read: true,
        extra: 1,
      }),
    ),
    null,
  );
  assert.equal(
    parseGrant(
      JSON.parse('{"read":true,"pushTaskBranches":true,"__proto__":{}}'),
    ),
    null,
  );
  assert.equal(
    parseGrant(
      JSON.parse('{"read":true,"__proto__":{"pushTaskBranches":true}}'),
    ),
    null,
  );
});

test("each field of a grant is read once", () => {
  const reads: string[] = [];
  const input = {
    get read() {
      reads.push("read");
      return true;
    },
    get pushTaskBranches() {
      reads.push("pushTaskBranches");
      return false;
    },
  };
  assert.deepEqual(parseGrant(input), { read: true, pushTaskBranches: false });
  assert.deepEqual(reads, ["read", "pushTaskBranches"]);
});
