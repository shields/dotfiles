# Claude Code instructions

Do not worry about compatibility with older versions. Use modern constructs and
libraries whenever they are preferred.

## Version guidelines

For new code, always use the latest stable versions of languages, frameworks,
and dependencies. Do not rely on memory for version numbers—always check current
versions as they may have changed since the knowledge cutoff.

Current latest stable versions (update as needed):

- Debian: 13 (trixie)
- Python: 3.14
- Go: 1.26

Be sure lines do not end in whitespace. The last line of a file should include a
newline.

Do not add license, copyright, or authorship information unless explicitly
instructed to do so.

Add code comments sparingly. Focus on _why_ something is done, especially for
complex logic, rather than _what_ is done. Only add high-value comments if
necessary for clarity or if requested by the user.

## Python-specific rules

When setting up a new project, always use ‘uv‘ instead of ‘pip‘.

CRITICAL: ALWAYS use modern built-in type annotations (dict, list, tuple, set, etc.) instead of typing module equivalents
(typing.Dict, typing.List, typing.Tuple, typing.Set, etc.). This is non-negotiable for Python 3.9+ codebases. Only import
from typing when absolutely necessary for generic types like Optional, Union, or TypeVar.

NEVER use: typing.Dict, typing.List, typing.Tuple, typing.Set
ALWAYS use: dict, list, tuple, set

Always include type annotations and verify that they are correct.

## Testing rules

**ABSOLUTE REQUIREMENT: 100% TEST SUCCESS MANDATORY**

CRITICAL: You MUST achieve 100% test pass rate - NO EXCEPTIONS WHATSOEVER. Partial progress is NOT success. Making improvements is NOT completion. 70% passing is NOT acceptable. 90% passing is NOT acceptable. 99% passing is NOT acceptable.

**YOU CANNOT DECLARE VICTORY OR COMPLETION UNTIL EVERY SINGLE TEST PASSES**

- If ANY tests are failing, the job is INCOMPLETE
- If ANY tests are timing out, the job is INCOMPLETE
- If ANY tests are erroring, the job is INCOMPLETE
- If ANY lint errors exist, the job is INCOMPLETE
- Continue working until EVERY test passes without exception and there are NO lint errors
- Do not stop, do not summarize progress, do not declare partial success
- The only acceptable outcome is 100% green tests and zero lint errors

**NO PARTIAL CREDIT. NO PROGRESS REPORTS AS COMPLETION. ALL TESTS MUST PASS AND CODE MUST BE LINT-FREE.**

**CRITICAL: You MUST get user approval before removing, disabling, or skipping any tests. Tests exist for important reasons!**

## CLI tools

- Use `crane` to inspect and check container images.
- Use `hyperfine` instead of `time` for benchmarking CLI commands.
- Use `go doc` for Go API documentation.

## Git hooks

NEVER bypass precommit hooks. Always respect and follow configured git hooks.

## Workflow

After completing a change, run `/code-review max` before review/commit.

## Code review (LGTMCP)

When using LGTMCP for code review, if you disagree with the reviewer's feedback,
do NOT bypass the review and commit directly. Instead, add comments to the code
explaining why the current approach is correct or necessary (e.g., it's temporary
for debugging, follows a specific pattern, or addresses a constraint the reviewer
may not be aware of), then resubmit for review.

## Web access

When you need to access web pages that require JavaScript (e.g., documentation sites with dynamic content), use the Playwright MCP tools instead of WebFetch.

For online PDF documentation, download it with `curl` and read it with the Read tool.
