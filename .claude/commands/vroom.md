---
description: "TDD: red → green → gates → LGTM"
argument-hint: "<goal>"
# Runners: no raw Bash; only make/just. Read/search tools from Serena; Context7 docs; LGTM review.
allowed-tools:
  Edit, MultiEdit, Update, Read, Grep, Glob, Bash(make:*), Bash(just:*),
  mcp__lgtmcp__review_only,
  # Serena MCP (read/search only; no editors/shell):
  mcp__serena__find_file,
  mcp__serena__find_symbol,
  mcp__serena__find_referencing_symbols,
  mcp__serena__get_symbols_overview,
  mcp__serena__list_dir,
  mcp__serena__read_file,
  mcp__serena__search_for_pattern,
  mcp__serena__get_current_config,
  mcp__serena__initial_instructions,
  # Context7 MCP (docs lookup):
  mcp__context7__resolve-library-id,
  mcp__context7__get-library-docs
---

Goal: **$ARGUMENTS**.

If no goal specified: Read @CLAUDE.md and find the next TODO item, then use that as the goal.

### Stack discovery

Read **@CLAUDE.md → Commands** and bind to `make`/`just` recipes only:
`TEST`, `TEST_ONE`, `COVERAGE` (+ optional `COVERAGE_TARGET`), `LINT`, `FORMAT`, `TYPECHECK`; also test globs/markers.
Never invoke raw binaries; use `make`/`just` targets.

### Recon (read-only)

Use Serena/Context7 tools to locate relevant files/symbols/fixtures and fetch current docs. Do not modify via these tools.

## Loop

**A) RED (tests-first)**
Repeat until (i) coverage for the goal is comprehensive (≥ `COVERAGE_TARGET` if set, else risk-justified), (ii) ≥1 **new** test fails, (iii) all **preexisting** tests pass:

1. Add or extend tests that encode the goal, edge cases, and regressions. **Never delete tests** (refactors OK).
2. Run `make/just TEST` (or `COVERAGE` if it runs tests).
   - If no **new** test fails → strengthen/expand tests.
   - If **old** tests fail → fix fixtures/setup or minimally refactor seams (no feature logic yet).

**B) GREEN (implement feature)**
Repeat until **all tests pass**:

1. Implement the smallest change to satisfy current failing tests.
2. Run `TEST`; fix failures; refactor only when green.

**C) Coverage gate**
If `COVERAGE_TARGET` is set, run `COVERAGE` and raise tests to meet/exceed it; otherwise add tests for critical paths.

**D) Quality gates**
Run and fix findings via `make`/`just`:
`FORMAT` → `LINT` → `TYPECHECK`.

**E) LGTM loop (MANDATORY - DO NOT SKIP)**
**CRITICAL**: You MUST request review via `mcp__lgtmcp__review_only`. This step is NOT optional.

1. Call `mcp__lgtmcp__review_only` on the working tree/diff.
2. If response ≠ **LGTM**, you MUST:
   - Address ALL review comments (including tests/docs as needed)
   - Re-run **D** (quality gates)
   - Request review again via `mcp__lgtmcp__review_only`
3. **CONTINUE this loop until you receive explicit "LGTM"**. Do not stop at partial approval or suggestions.

### Constraints

- Do **not** remove tests; refactors to improve testability/clarity are allowed.
- Keep prior behavior unless tests document an intentional change.
- Prefer `TEST_ONE` for tight red/green, but run full `TEST` before gates/LGTM.
- Use Context7 docs to avoid stale APIs during test design/implementation.
