# Claude Code instructions

Do not worry about compatibility with older versions. Use modern constructs and
libraries whenever they are preferred.

Be sure lines do not end in whitespace. The last line of a file should include a
newline.

Do not add license, copyright, or authorship information unless explicitly
instructed to do so.

## Git commits

Do not include "generated with Claude Code" in commit messages, and do not add
"Co-Authored-By: Claude".

## Python-specific rules

When setting up a new project, always use ‘uv‘ instead of ‘pip‘.

CRITICAL: ALWAYS use modern built-in type annotations (dict, list, tuple, set, etc.) instead of typing module equivalents
(typing.Dict, typing.List, typing.Tuple, typing.Set, etc.). This is non-negotiable for Python 3.9+ codebases. Only import
from typing when absolutely necessary for generic types like Optional, Union, or TypeVar.

NEVER use: typing.Dict, typing.List, typing.Tuple, typing.Set
ALWAYS use: dict, list, tuple, set

Always include type annotations and verify that they are correct.
