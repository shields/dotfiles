---
name: bughunt
description: Fan out parallel subagents across a codebase to find and fix bugs — security first, then correctness, then anything else (broken docs, dead code, simplifications). Each slice runs in its own git worktree/branch via the Workflow tool; every fix is reviewed with /code-review --fix and committed with lgtmcp. Bugs that reach outside a slice are surfaced to the parent to coordinate; design/behavior changes and unclear items are surfaced to the user, never applied. When done, merge every branch back preserving its original commits, then test the whole stack and fix-forward any breakage. Trigger when the user asks to hunt, sweep, or audit a codebase for bugs, or runs /bughunt.
---

# /bughunt — parallel bug hunt, fix, and integrate

Sweep the codebase (or the area named in `$ARGUMENTS`) for bugs, fix them in parallel
worktrees, review and commit each fix, then merge everything back and test the combined
result. Runs autonomously end to end — **no approval gate once it starts** (it only needs a
clean working tree to begin; see Preconditions). The only things that pause for the user
mid-run are **design/behavior changes** and **anything genuinely unclear**; those land in
the final report, never applied silently.

`$ARGUMENTS` (optional) narrows or directs the hunt:

- paths/globs → restrict the sweep to them (`/bughunt src/api crypto/`)
- a theme → still sweep, but every agent emphasizes it (`/bughunt concurrency & error handling`)
- empty → sweep the entire repo

## Priorities — give these to every agent, in order

1. **Security** — injection, authz/authn, secret handling, unsafe deserialization, path traversal / SSRF, memory safety.
2. **Correctness** — logic errors, wrong edge cases, races, resource leaks, data loss, off-by-one, swallowed/mishandled errors.
3. **Everything else** — wrong or stale docs & comments, dead code, and **simplifications** (prefer the simpler equivalent; deleting code is a valid fix).

Fix clear bugs and safe simplifications **in place**. Do **not** make design, API, or
behavior changes, and do not touch anything ambiguous — **surface those instead**.

## Preconditions (check first)

- Record the starting branch: `git branch --show-current` (this is the **original branch** you merge back into; the main working tree stays on it the whole time).
- Require a **clean working tree** to start. If `git status --porcelain` is non-empty, stop and tell the user to commit or stash first — this is a safety precondition (a dirty base would entangle their uncommitted work in the final merges and test), not an approval gate.
- Generate a **unique run prefix** so this run's branches can't collide with another run's: run `date +%Y%m%d-%H%M%S` and form the prefix `bughunt/<that-timestamp>` (e.g. `bughunt/20260530-153045`). Pass it into the workflow (Step 1) and reuse it for cleanup (Step 5).

## Step 1 — Fan out the hunt (Workflow tool)

Run the script below **inline** via the Workflow tool (`script: …`), passing
`args: { direction: <$ARGUMENTS, verbatim>, prefix: <the unique run prefix from Preconditions> }`.
It scouts the repo into disjoint slices and runs one worktree-isolated agent per slice:
find → fix → `/code-review --fix` → lgtmcp commit. It returns the per-slice branches (all
under your unique prefix) plus everything surfaced (cross-cutting bugs, design changes,
unclear items).

```js
export const meta = {
  name: 'bughunt',
  description: 'Partition a codebase and fan out worktree-isolated agents to find, fix, review, and commit bugs',
  phases: [
    { title: 'Scout', detail: 'partition the repo into disjoint slices' },
    { title: 'Hunt', detail: 'one worktree agent per slice: find -> fix -> /code-review --fix -> lgtmcp commit' },
  ],
}

const PLAN = {
  type: 'object',
  required: ['slices'],
  properties: {
    slices: {
      type: 'array',
      items: {
        type: 'object',
        required: ['id', 'paths', 'focus'],
        properties: {
          id: { type: 'string', description: 'short unique kebab slug' },
          paths: { type: 'array', items: { type: 'string' }, description: 'disjoint file/dir globs this slice owns' },
          focus: { type: 'string', description: 'what to pay special attention to here' },
        },
      },
    },
  },
}

const SLICE = {
  type: 'object',
  required: ['id', 'fixes', 'crossCutting', 'designChanges', 'unclear'],
  properties: {
    id: { type: 'string' },
    branch: { type: 'string', description: 'branch the commits landed on, empty if no commit' },
    headSha: { type: 'string', description: 'HEAD after committing, empty if no commit' },
    fixes: {
      type: 'array',
      items: {
        type: 'object',
        required: ['severity', 'file', 'summary'],
        properties: {
          severity: { type: 'string', enum: ['security', 'correctness', 'other'] },
          file: { type: 'string' },
          summary: { type: 'string' },
        },
      },
    },
    crossCutting: {
      type: 'array',
      description: 'bugs whose fix reaches OUTSIDE this slice — described, not fixed',
      items: {
        type: 'object',
        required: ['severity', 'summary'],
        properties: {
          severity: { type: 'string', enum: ['security', 'correctness', 'other'] },
          area: { type: 'string', description: 'files/modules it spans' },
          summary: { type: 'string' },
          suggestedFix: { type: 'string' },
        },
      },
    },
    designChanges: { type: 'array', items: { type: 'string' }, description: 'changes needing a human decision — described, NOT applied' },
    unclear: { type: 'array', items: { type: 'string' } },
  },
}

function huntPrompt(s, direction, prefix) {
  return [
    `You are hunting bugs in ONE slice of a codebase, working inside your own git worktree. Slice "${s.id}".`,
    `Owned paths — stay within these, do NOT edit files outside them: ${JSON.stringify(s.paths)}.`,
    `Focus: ${s.focus}.` + (direction ? ` User direction: ${direction}.` : ''),
    ``,
    `PRIORITIES, in order: (1) security, (2) correctness, (3) everything else — including wrong/stale docs & comments, dead code, and simplifications. Prefer the simpler equivalent; deleting code is a valid fix.`,
    ``,
    `STEPS:`,
    `1. Read your slice's files and find REAL bugs. Be skeptical: skip nitpicks and false positives, only fix what you can justify.`,
    `2. Create your branch: run "git switch -c ${prefix}/${s.id}"; if that name already exists (git exits 128), use a unique variant like "${prefix}/${s.id}-$RANDOM". Fix the clear bugs and safe simplifications IN PLACE, within your owned paths only.`,
    `3. Review your own diff before committing: invoke "/code-review --fix" to apply its findings. If you cannot invoke that skill from here, instead re-read your full diff critically for security, correctness, and over-complication, and fix what you find.`,
    `4. Commit with the lgtmcp tool "mcp__lgtmcp__review_and_commit" (load its schema via ToolSearch first if needed): directory = your worktree root (run "git rev-parse --show-toplevel"), with a clear commit_message. If it is NOT approved, address the feedback — or, if you genuinely disagree, add a brief code comment explaining why the code is correct — then resubmit. Never bypass the review. Several focused commits are fine.`,
    `5. After committing, set branch to the actual branch name you used and headSha to "git rev-parse HEAD".`,
    ``,
    `SURFACE — do NOT fix these yourself:`,
    `- A bug whose fix reaches OUTSIDE your owned paths -> crossCutting (area + suggestedFix). The parent will coordinate it.`,
    `- Anything needing a design / API / behavior decision -> designChanges (described, not applied).`,
    `- Anything genuinely unclear -> unclear.`,
    ``,
    `If nothing is worth fixing, make no commit, leave branch empty, and return empty fixes. Return the structured SLICE output.`,
  ].join('\n')
}

const direction = typeof args === 'string'
  ? args.trim()
  : (args && typeof args.direction === 'string' ? args.direction.trim() : '')
const rawPrefix = args && typeof args === 'object' && typeof args.prefix === 'string' ? args.prefix.trim() : ''
const prefix = /^[\w.\/-]+$/.test(rawPrefix) ? rawPrefix.replace(/\/+$/, '') : 'bughunt'

phase('Scout')
const plan = await agent(
  `Inventory this git repo and partition it into disjoint slices for a parallel bug hunt.` +
    (direction
      ? ` Direction from the user: "${direction}". If it names paths/globs, restrict the sweep to them; if it names a theme, still cover the repo but record the theme in every slice's focus.`
      : ` Sweep the entire repo.`) +
    ` Group by coherent module/subsystem, balance slices by rough size, and keep the path sets DISJOINT at the FILE level so two worktree agents never edit the same file. Assign shared aggregator files that several areas might touch — package manifests (go.mod, package.json, pyproject.toml), lockfiles, barrel/index files, __init__.py, generated registries — to exactly ONE slice, or leave them out of every slice and note them for the parent. Aim for 3-10 slices (fewer for a small repo; scale up with budget). Skip vendored and generated files and anything in .gitignore. Return the plan.`,
  { label: 'scout', phase: 'Scout', schema: PLAN },
)

log(`Hunting ${plan.slices.length} slice(s)` + (direction ? ` — direction: ${direction}` : ''))

phase('Hunt')
const results = (
  await parallel(
    plan.slices.map((s) => () =>
      agent(huntPrompt(s, direction, prefix), { label: `hunt:${s.id}`, phase: 'Hunt', isolation: 'worktree', schema: SLICE }),
    ),
  )
).filter(Boolean)

return {
  direction,
  prefix,
  results,
  branches: results.filter((r) => r.branch).map((r) => ({ id: r.id, branch: r.branch, headSha: r.headSha })),
  crossCutting: results.flatMap((r) => r.crossCutting || []),
  designChanges: results.flatMap((r) => (r.designChanges || []).map((d) => ({ id: r.id, change: d }))),
  unclear: results.flatMap((r) => (r.unclear || []).map((u) => ({ id: r.id, item: u }))),
}
```

## Step 2 — Merge the reviewed branches (no review needed)

Each run-prefixed (`<prefix>/*`) branch was already reviewed, so the merge itself needs no review. Merge
every `branches[].branch` into the original branch in a deliberate order — shared or
foundational slices first, then dependents (otherwise smallest first) — **preserving each
branch's original commits**:

```sh
git merge --no-ff <branch>   # branches[].branch, e.g. bughunt/20260530-153045/<id>; repeat per branch
```

Skip slices that produced no branch (nothing was committed). Disjoint file ownership means
merges should usually apply cleanly; if one still conflicts, resolve it minimally, or — if
the resolution is non-obvious — leave it and surface it in the report instead of guessing.

## Step 3 — Coordinate cross-cutting bugs (parent)

On the merged tree, work `crossCutting` (deduped, **security → correctness → other**). These
are the "larger bugs" the parent owns. For each that is a genuine **bug**, fix it (spawn a
focused Agent for big ones), then `/code-review --fix` and commit via lgtmcp — this is new
code, so it does get reviewed. Anything that is really a **design/behavior change** or is
unclear goes to the surfaced list instead; do not apply it.

## Step 4 — Test the whole stack, fix-forward on failure

Run the project's full check once, over the integrated result. Detect the command from the
repo: CLAUDE.md "Commands" section first, then a `Makefile` (`make build && make lint && make
test`), then `package.json` scripts, then language defaults (`cargo test`, `go test ./...`,
`uv run pytest`, etc.).

If it **fails**, fix-forward: dispatch fixer Agent(s) to repair the breakage (they may edit
anywhere; each runs `/code-review --fix` + lgtmcp), then re-run the full check. Repeat up to
**3 rounds**. If it still fails, stop and report the failing output — do not keep grinding.

## Step 5 — Clean up and report

Once the merges and tests are done, clean up so reruns start fresh: remove the worktrees the
hunt created (`git worktree list` shows them checked out under your run prefix `<prefix>/*`;
`git worktree remove` each), then delete this run's now-merged branches — `git branch -d <branch>`
for each `branches[].branch` (they all share your `<prefix>/` namespace; `-d` is safe, it
refuses anything not yet merged). Scoping cleanup to your run prefix leaves other runs' branches
untouched.

Then tell the user, concisely:

- **Fixes applied**, grouped by severity (security / correctness / other) and by slice.
- **Cross-cutting** fixes the parent made.
- **Design / behavior changes needing your decision** (from agents + coordination) — described, not applied.
- **Unclear** items.
- **Final test result** and how many fix-forward rounds were used.
- The commits now on the original branch.

## Gotchas (the non-obvious bits)

- **Worktree isolation is mandatory.** `isolation: 'worktree'` is what lets slices edit files in parallel without clobbering each other; disjoint slice paths are a second safety layer. Don't drop it.
- **lgtmcp `directory` must be the worktree root** (`git rev-parse --show-toplevel`, not a bare `pwd`), never the main repo — otherwise it reviews and commits the wrong tree.
- **Branches outlive worktrees.** Worktrees share one `.git`, so a run-prefixed (`<prefix>/*`) branch is visible to the main tree for merging whether or not its worktree still exists — you can even merge a branch that's still checked out in a worktree. Merge by the branch name the agent returned (it disambiguates on collision); fall back to `headSha`.
- **No per-slice full test — only the final stack test.** Slices may individually break the build; that is expected and is what Step 4's fix-forward catches.
- **Agents fix, they don't redesign.** Bugs + safe simplifications + doc fixes only. Design/behavior changes and unclear items are surfaced, never applied.
- **Only new code gets reviewed at the end.** Merges (Step 2) need no review; the cross-cutting (Step 3) and fix-forward (Step 4) changes are new, so they go through `/code-review --fix` + lgtmcp.
- **Never bypass lgtmcp.** On rejection, address the feedback or add a clarifying comment explaining why the code is correct, then resubmit.
