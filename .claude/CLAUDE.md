# Claude Code instructions

Use modern constructs, libraries, and the latest stable versions of languages,
frameworks, and dependencies; don't worry about compatibility with older ones.
Don't trust memory for version numbers—check current versions, as they may have
changed since the knowledge cutoff. Current versions (update as needed):

- Debian: 13 (trixie)
- Python: 3.14
- Go: 1.26

Do not add license, copyright, or authorship information unless explicitly told to.

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
- After a change, run `/code-review --fix` before review/commit.
- LGTMCP: if you disagree with review feedback, don't bypass and commit—add a
  code comment explaining why, then resubmit.

## Web access

- For JS-heavy pages, use the Playwright MCP tools, not WebFetch.
- For online PDFs, download with `curl` and open with Read.
