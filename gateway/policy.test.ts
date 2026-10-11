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

void test("REST reads require a grant and API writes have no classification", () => {
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
  assert.throws(() => {
    authorize(null, read.operation);
  }, /not granted/);
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
    authorize({ read: true, pushTaskBranches: false }, "git.push");
  }, /not granted/);
  assert.doesNotThrow(() => {
    authorize({ read: true, pushTaskBranches: true }, "git.push");
  });
});
