# Claude Code instructions

Do not worry about compatibility with older versions. Use modern constructs and
libraries whenever they are preferred.

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
- Continue working until EVERY test passes without exception
- Do not stop, do not summarize progress, do not declare partial success
- The only acceptable outcome is 100% green tests

**NO PARTIAL CREDIT. NO PROGRESS REPORTS AS COMPLETION. ALL TESTS MUST PASS.**

**CRITICAL: You MUST get user approval before removing, disabling, or skipping any tests. Tests exist for important reasons!**

## Performance benchmarking

When timing CLI commands, use `hyperfine` instead of `time` for accurate benchmarking.
