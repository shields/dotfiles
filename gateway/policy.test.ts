import assert from "node:assert/strict";
import test from "node:test";
import {
  authorize,
  classify,
  graphqlRepository,
  parsePushCommands,
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

void test("a batch with another destination is denied before any ref can be forwarded", () => {
  const body = push(
    `${old} ${next} refs/heads/task/${id}/one\0report-status\n`,
    `${old} ${next} refs/heads/main\n`,
  );
  assert.throws(() => parsePushCommands(body, id), /outside this VM/);
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
  assert.throws(
    () =>
      parsePushCommands(
        push(`${old} ${next} refs/heads/task/${id}/new\0push-options\n`),
        id,
      ),
    /options/,
  );
  assert.equal(
    parsePushCommands(
      packet(`${old} ${next} refs/heads/task/${id}/new\0report-status\n`),
      id,
    ),
    null,
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
