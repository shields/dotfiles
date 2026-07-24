#!/usr/bin/env python3

# Copyright © 2026 Michael Shields
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Exercises .claude/hooks/dependency_guard.py end to end: each case writes a
# manifest, feeds the hook the PreToolUse payload a real tool call would send,
# and checks whether it asked. The point of the hook is precision, so the
# negative cases (routine edits that must stay silent) matter as much as the
# positive ones.

import json
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, cast

if TYPE_CHECKING:
    from collections.abc import Mapping

HOOK = (
    Path(__file__).resolve().parent.parent / ".claude" / "hooks" / "dependency_guard.py"
)

# Marks a structurally wrong hook response, which fails a case either way.
MALFORMED = "malformed response:"

PYPROJECT = """\
[project]
name = "dotfiles"
version = "0.1.0"
dependencies = [
    "pillow>=12.3.0",
]

[dependency-groups]
dev = [
    "ruff>=0.15.7",
]

[tool.ruff]
line-length = 100
"""

PACKAGE_JSON = """\
{
  "type": "module",
  "dependencies": { "colorjs.io": "0.6.1" },
  "devDependencies": { "typescript": "^5.4.3" },
  "scripts": { "build": "tsc --noEmit" }
}
"""

CARGO = """\
[package]
name = "x"
version = "0.1.0"

[dependencies]
serde = "1"

[profile.release]
lto = true
"""

GO_MOD = """\
module example.com/x

go 1.26

require (
\texample.com/a v1.0.0
\texample.com/b v2.0.0 // indirect
)
"""

# Same distribution pinned twice, once per environment marker.
PYPROJECT_DUP = """\
[project]
name = "x"
dependencies = [
    "numpy==1.26; python_version < '3.13'",
    "numpy==2.0; python_version >= '3.13'",
]
"""


class Case(NamedTuple):
    label: str
    filename: str
    before: str
    tool_input: dict[str, object]
    ask: bool
    tool: str = "Edit"


def edit(old: str, new: str, **extra: object) -> dict[str, object]:
    return {"old_string": old, "new_string": new, **extra}


CASES = (
    # pyproject.toml
    Case(
        "pyproject: dependency added",
        "pyproject.toml",
        PYPROJECT,
        edit('    "pillow>=12.3.0",', '    "pillow>=12.3.0",\n    "requests>=2",'),
        ask=True,
    ),
    Case(
        "pyproject: version bumped",
        "pyproject.toml",
        PYPROJECT,
        edit('"pillow>=12.3.0"', '"pillow>=12.4.0"'),
        ask=True,
    ),
    Case(
        "pyproject: dependency group gains an entry",
        "pyproject.toml",
        PYPROJECT,
        edit('    "ruff>=0.15.7",', '    "ruff>=0.15.7",\n    "ty>=0.0.19",'),
        ask=True,
    ),
    Case(
        "pyproject: extra index url added",
        "pyproject.toml",
        PYPROJECT,
        edit(
            "[tool.ruff]",
            '[tool.uv]\nextra-index-url = ["https://example.invalid"]\n\n[tool.ruff]',
        ),
        ask=True,
    ),
    Case(
        "pyproject: version bumped on one of two same-named deps",
        "pyproject.toml",
        PYPROJECT_DUP,
        edit("numpy==1.26", "numpy==1.27"),
        ask=True,
    ),
    Case(
        "pyproject: unrelated tool config edited",
        "pyproject.toml",
        PYPROJECT,
        edit("line-length = 100", "line-length = 88"),
        ask=False,
    ),
    Case(
        "pyproject: dependency removed",
        "pyproject.toml",
        PYPROJECT,
        edit('    "pillow>=12.3.0",\n', ""),
        ask=False,
    ),
    Case(
        "pyproject: comment added",
        "pyproject.toml",
        PYPROJECT,
        edit("[tool.ruff]", "# Formatting.\n[tool.ruff]"),
        ask=False,
    ),
    Case(
        "pyproject: result is malformed toml",
        "pyproject.toml",
        PYPROJECT,
        edit("[tool.ruff]", "[tool.ruff"),
        ask=True,
    ),
    Case(
        "pyproject: old_string not present",
        "pyproject.toml",
        PYPROJECT,
        edit("no such text anywhere", "x"),
        ask=True,
    ),
    Case(
        "pyproject: replace_all across entries",
        "pyproject.toml",
        PYPROJECT,
        edit(">=", "==", replace_all=True),
        ask=True,
    ),
    # package.json
    Case(
        "package.json: devDependency added",
        "package.json",
        PACKAGE_JSON,
        edit('"typescript": "^5.4.3"', '"typescript": "^5.4.3", "vitest": "^2"'),
        ask=True,
    ),
    Case(
        "package.json: dependency version changed",
        "package.json",
        PACKAGE_JSON,
        edit('"colorjs.io": "0.6.1"', '"colorjs.io": "0.7.0"'),
        ask=True,
    ),
    Case(
        "package.json: trustedDependencies added",
        "package.json",
        PACKAGE_JSON,
        edit(
            '"type": "module",',
            '"type": "module",\n  "trustedDependencies": ["esbuild"],',
        ),
        ask=True,
    ),
    Case(
        "package.json: script changed",
        "package.json",
        PACKAGE_JSON,
        edit('"build": "tsc --noEmit"', '"build": "tsc --noEmit --strict"'),
        ask=False,
    ),
    # Cargo.toml
    Case(
        "Cargo.toml: dependency added",
        "Cargo.toml",
        CARGO,
        edit('serde = "1"', 'serde = "1"\ntokio = "1"'),
        ask=True,
    ),
    Case(
        "Cargo.toml: target dependency added",
        "Cargo.toml",
        CARGO,
        edit(
            "[profile.release]",
            "[target.'cfg(unix)'.dependencies]\nlibc = \"0.2\"\n\n[profile.release]",
        ),
        ask=True,
    ),
    Case(
        "Cargo.toml: string spec rewritten as inline table",
        "Cargo.toml",
        CARGO,
        edit('serde = "1"', 'serde = { version = "1" }'),
        ask=False,
    ),
    Case(
        "Cargo.toml: profile edited",
        "Cargo.toml",
        CARGO,
        edit("lto = true", "lto = false"),
        ask=False,
    ),
    # go.mod
    Case(
        "go.mod: require added",
        "go.mod",
        GO_MOD,
        edit(
            "\texample.com/a v1.0.0", "\texample.com/a v1.0.0\n\texample.com/c v3.0.0"
        ),
        ask=True,
    ),
    Case(
        "go.mod: version bumped",
        "go.mod",
        GO_MOD,
        edit("example.com/a v1.0.0", "example.com/a v1.1.0"),
        ask=True,
    ),
    Case(
        "go.mod: tool directive added",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.26\n\ntool golang.org/x/tools/cmd/stringer"),
        ask=True,
    ),
    Case(
        "go.mod: replace added",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.26\n\nreplace example.com/a => example.com/fork v1.0.0"),
        ask=True,
    ),
    Case(
        "go.mod: indirect marker added",
        "go.mod",
        GO_MOD,
        edit("example.com/a v1.0.0", "example.com/a v1.0.0 // indirect"),
        ask=False,
    ),
    Case(
        "go.mod: go directive bumped",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.27"),
        ask=False,
    ),
    Case(
        "go.mod: require removed",
        "go.mod",
        GO_MOD,
        edit("\texample.com/b v2.0.0 // indirect\n", ""),
        ask=False,
    ),
    Case(
        "go.mod: single-line require separated by a tab",
        "go.mod",
        GO_MOD,
        edit("go 1.26", "go 1.26\n\nrequire\texample.com/x v1.0.0"),
        ask=True,
    ),
    # Write, including to a file that does not exist yet.
    Case(
        "write: new manifest declaring a dependency",
        "",
        "",
        {"content": '[project]\nname = "x"\ndependencies = ["requests"]\n'},
        ask=True,
        tool="Write",
    ),
    Case(
        "write: new manifest with no dependencies",
        "",
        "",
        {"content": '[project]\nname = "x"\n\n[tool.ruff]\nline-length = 100\n'},
        ask=False,
        tool="Write",
    ),
    Case(
        "write: overwrite leaving dependencies unchanged",
        "pyproject.toml",
        PYPROJECT,
        {"content": PYPROJECT.replace("line-length = 100", "line-length = 88")},
        ask=False,
        tool="Write",
    ),
    Case(
        "write: manifest with no string content fails closed",
        "pyproject.toml",
        PYPROJECT,
        {},
        ask=True,
        tool="Write",
    ),
    # Files the hook has no opinion about.
    Case(
        "unguarded file is ignored",
        "README.md",
        "# hi\n",
        edit("# hi", "# hello"),
        ask=False,
    ),
)

BASH_CASES = (
    ("bash: append to requirements.txt", "echo 'requests' >> requirements.txt", True),
    (
        "bash: heredoc into pyproject.toml",
        "cat > pyproject.toml <<'EOF'\n[project]\nEOF",
        True,
    ),
    ("bash: sed -i on go.mod", "sed -i '' 's/a/b/' go.mod", True),
    ("bash: tee into Brewfile", "echo 'brew \"jq\"' | tee -a Brewfile", True),
    (
        "bash: python writes go.mod",
        "python3 -c \"open('go.mod','a').write('x')\"",
        True,
    ),
    ("bash: reading a manifest", "cat pyproject.toml", False),
    ("bash: grepping a manifest", "grep pillow pyproject.toml", False),
    ("bash: unrelated redirect", "make lint > /tmp/out.txt", False),
    ("bash: ordinary command", "uv run pytest -q", False),
)


def run_hook(payload: Mapping[str, object]) -> tuple[bool, str]:
    """Run the hook, returning (asked, reason)."""
    result = subprocess.run(
        [sys.executable, str(HOOK)],
        input=json.dumps(payload),
        capture_output=True,
        text=True,
        check=False,
    )
    # The hook only ever exits 0 (it prints an ask decision or nothing), so any
    # non-zero exit is a real failure — a syntax error or crash. Flag it as
    # malformed so it fails the case outright rather than reading as "silent".
    if result.returncode != 0:
        return (
            False,
            f"{MALFORMED} hook exited {result.returncode}: {result.stderr.strip()}",
        )
    out = result.stdout.strip()
    if not out:
        return False, ""
    response = cast("dict[str, dict[str, str]]", json.loads(out))
    decision = response["hookSpecificOutput"]
    if decision.get("hookEventName") != "PreToolUse":
        return False, f"{MALFORMED} hookEventName {decision.get('hookEventName')!r}"
    if not decision.get("permissionDecisionReason"):
        return False, f"{MALFORMED} no permissionDecisionReason"
    return decision["permissionDecision"] == "ask", decision.get(
        "permissionDecisionReason", ""
    )


def check(label: str, *, asked: bool, expected: bool, detail: str) -> bool:
    if detail.startswith(MALFORMED):
        print(f"FAIL {label}: {detail}")
        return False
    if asked == expected:
        print(f"ok   {label}")
        return True
    wanted = "ask" if expected else "silence"
    got = f"ask ({detail})" if asked else "silence"
    print(f"FAIL {label}: expected {wanted}, got {got}")
    return False


def main() -> int:
    failures = 0
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        for index, case in enumerate(CASES):
            # A fresh directory per case keeps Write-to-a-new-file honest.
            workdir = root / str(index)
            workdir.mkdir()
            target = workdir / (case.filename or "pyproject.toml")
            if case.filename:
                _ = target.write_text(case.before, encoding="utf-8")
            payload = {
                "hook_event_name": "PreToolUse",
                "tool_name": case.tool,
                "tool_input": {"file_path": str(target), **case.tool_input},
            }
            asked, detail = run_hook(payload)
            if not check(case.label, asked=asked, expected=case.ask, detail=detail):
                failures += 1

    for label, command, expected in BASH_CASES:
        payload = {
            "hook_event_name": "PreToolUse",
            "tool_name": "Bash",
            "tool_input": {"command": command},
        }
        asked, detail = run_hook(payload)
        if not check(label, asked=asked, expected=expected, detail=detail):
            failures += 1

    total = len(CASES) + len(BASH_CASES)
    if failures:
        print(f"\n{failures} of {total} cases failed")
        return 1
    print(f"\nall {total} cases passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
