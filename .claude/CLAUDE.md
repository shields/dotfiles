<!--
Copyright © 2025-2026 Michael Shields

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Claude Code instructions

Use modern constructs, libraries, and the latest stable versions of languages,
frameworks, and dependencies; don't worry about compatibility with older ones.
Don't trust memory for version numbers—check current versions, as they may have
changed since the knowledge cutoff. Current versions (update as needed):

- Debian: 13 (trixie)
- Python: 3.14
- Go: 1.26

Do not add, remove, or modify license, copyright, or authorship information in
any way unless explicitly told to. Exception: when a file is copyright Michael
Shields and you make substantive changes to it, update the copyright year to
include the current year (e.g. `2025-2026`, or `2024, 2026` if discontiguous).

Add code comments sparingly, explaining _why_ rather than _what_, and only when
they add real value.

## Python

- Use `uv`, never `pip` (even with no pyproject.toml: `uv venv`, `uv pip`).
- Use built-in generics (`dict`, `list`, `tuple`, `set`), not `typing.Dict` etc.
  Reach for `typing` only when needed (e.g. `TypeVar`); prefer `X | None` over `Optional`.
- Always include correct type annotations.

## Testing

- All tests must pass and lint must be clean before you call the work done.
  Don't report partial progress as completion.
- A flaky test is a broken test. Fix all failures, no matter how rare or
  intermittent.
- Get my approval before removing, disabling, or skipping any test.

## Error handling

- Prefer fail-fast over graceful degradation — surface errors loudly unless
  told otherwise.
- Never degrade errors to warnings without asking first.

## CLI tools

- `crane` to inspect container images.
- `hyperfine` (not `time`) for benchmarking commands.
- `go doc` for Go API documentation.

## Git and commits

- Never bypass precommit hooks; respect configured git hooks.
- Don't use Conventional Commits (`feat:`, `fix:`) unless the repo already does;
  match the existing history.
- After a change, run `/code-review max --fix` before review/commit.
- LGTMCP: if you disagree with review feedback, don't bypass and commit—add a
  code comment explaining why, then resubmit.

## Web access

- For JS-heavy pages, use the Playwright MCP tools, not WebFetch.
- For online PDFs, download with `curl` and open with Read.
