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
import resource
import subprocess
import sys
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple, cast

import pytest

if TYPE_CHECKING:
    from collections.abc import Mapping

REPO = Path(__file__).resolve().parents[1]
HOOK = REPO / ".claude" / "hooks" / "dependency_guard.py"

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


class BashCase(NamedTuple):
    label: str
    command: str
    ask: bool


BASH_CASES = (
    BashCase(
        "append to requirements.txt", "echo 'requests' >> requirements.txt", ask=True
    ),
    BashCase(
        "heredoc into pyproject.toml",
        "cat > pyproject.toml <<'EOF'\n[project]\nEOF",
        ask=True,
    ),
    BashCase("sed -i on go.mod", "sed -i '' 's/a/b/' go.mod", ask=True),
    BashCase("tee into Brewfile", "echo 'brew \"jq\"' | tee -a Brewfile", ask=True),
    BashCase(
        "python writes go.mod",
        "python3 -c \"open('go.mod','a').write('x')\"",
        ask=True,
    ),
    # GNU sed spells the in-place flag more ways than BSD sed does.
    BashCase("sed -i.bak on Cargo.toml", "sed -i.bak 's/a/b/' Cargo.toml", ask=True),
    BashCase("sed -Ei on pyproject.toml", "sed -Ei 's/a/b/' pyproject.toml", ask=True),
    BashCase("sed -ni on package.json", "sed -ni 's/a/b/p' package.json", ask=True),
    BashCase(
        "sed -i after other flags",
        "sed -E -n -i 's/a/b/p' pyproject.toml",
        ask=True,
    ),
    BashCase("sed --in-place on go.mod", "sed --in-place 's/a/b/' go.mod", ask=True),
    BashCase(
        "sed --in-place with a suffix on go.sum",
        "sed --in-place=.orig 's/a/b/' go.sum",
        ask=True,
    ),
    BashCase(
        "sed abbreviates --in-place",
        "sed --in-pl 's/a/b/' package-lock.json",
        ask=True,
    ),
    BashCase("sed -i after the file", "sed 's/a/b/' go.mod -i", ask=True),
    BashCase("Homebrew gsed -i on go.mod", "gsed -i 's/a/b/' go.mod", ask=True),
    BashCase("Homebrew gsed -Ei on Cargo.toml", "gsed -Ei s/a/b/ Cargo.toml", ask=True),
    BashCase(
        "sed --in-place after the file", "sed 's/a/b/' go.mod --in-place", ask=True
    ),
    BashCase("BSD sed -I on uv.lock", "sed -I '' 's/a/b/' uv.lock", ask=True),
    BashCase("BSD sed -nI on yarn.lock", "sed -nI '' 's/a/b/p' yarn.lock", ask=True),
    BashCase(
        "tee into a module Brewfile",
        "echo 'brew \"jq\"' | tee brew/dev.Brewfile",
        ask=True,
    ),
    BashCase(
        "redirect into a module Brewfile",
        "echo 'brew \"jq\"' > brew/base.Brewfile",
        ask=True,
    ),
    BashCase(
        "append to a module Brewfile",
        "echo 'brew \"jq\"' >> brew/cloud.Brewfile",
        ask=True,
    ),
    BashCase(
        "heredoc into a module Brewfile",
        "cat > brew/data.Brewfile <<'EOF'\nbrew \"duckdb\"\nEOF",
        ask=True,
    ),
    BashCase(
        "sed -i on a module Brewfile",
        "sed -i s/a/b/ brew/macos.Brewfile",
        ask=True,
    ),
    BashCase(
        "GNU sed -Ei on a module Brewfile",
        "sed -Ei 's/a/b/' brew/linux.Brewfile",
        ask=True,
    ),
    BashCase("reading a manifest", "cat pyproject.toml", ask=False),
    BashCase("grepping a manifest", "grep pillow pyproject.toml", ask=False),
    BashCase("reading a module Brewfile", "cat brew/dev.Brewfile", ask=False),
    BashCase(
        "grepping a module Brewfile", "grep -n ripgrep brew/base.Brewfile", ask=False
    ),
    BashCase(
        "sed without in-place on a manifest",
        "sed -n 's/a/b/p' go.mod",
        ask=False,
    ),
    BashCase(
        "sed -E without in-place on a manifest",
        "sed -E 's/a/b/' pyproject.toml",
        ask=False,
    ),
    BashCase(
        "sed long options without in-place on a manifest",
        "sed --quiet --expression='s/a/b/p' package.json",
        ask=False,
    ),
    BashCase(
        "sed -e script on a module Brewfile",
        "sed -e 's/a/b/' brew/dev.Brewfile",
        ask=False,
    ),
    BashCase("gsed without in-place on a manifest", "gsed -n p go.mod", ask=False),
    BashCase(
        "sed in place on an unguarded file", "sed -Ei 's/a/b/' src/app.py", ask=False
    ),
    BashCase(
        "sed in place elsewhere, manifest read in the next command",
        "sed -i 's/a/b/' notes.txt && cat go.mod",
        ask=False,
    ),
    BashCase("sed over many manifest mentions", "sed go.mod " * 800, ask=False),
    BashCase("sed with a very long token", "sed -n p " + "a" * 64_000, ask=False),
    BashCase("sed -i with a very long token", "sed -i p " + "a" * 64_000, ask=False),
    BashCase("redirect with a very long token", "echo x > " + "a" * 64_000, ask=False),
    BashCase(
        "manifest write after a very long token",
        "sed -i p " + "a" * 64_000 + "\nsed -i s/a/b/ go.mod",
        ask=True,
    ),
    BashCase("unrelated redirect", "make lint > /tmp/out.txt", ask=False),
    BashCase("ordinary command", "uv run pytest -q", ask=False),
)


# The hook has 10 s in settings.json and fails open past that, so a pattern whose
# cost grows faster than its input must fail here. CPU time measures that, and a
# loaded host stretches wall-clock time without changing it.
CPU_LIMIT = 2.0


def run_hook(payload: Mapping[str, object]) -> tuple[bool, str]:
    """Run the hook, returning (asked, reason)."""
    before = resource.getrusage(resource.RUSAGE_CHILDREN)
    result = subprocess.run(
        [sys.executable, str(HOOK)],
        input=json.dumps(payload),
        capture_output=True,
        text=True,
        check=False,
        timeout=120,
    )
    after = resource.getrusage(resource.RUSAGE_CHILDREN)
    cpu = (after.ru_utime - before.ru_utime) + (after.ru_stime - before.ru_stime)
    assert cpu < CPU_LIMIT, f"the hook used {cpu:.1f} s of CPU"
    # The hook only ever exits 0 (it prints an ask decision or nothing), so any
    # non-zero exit is a real failure — a syntax error or crash.
    assert result.returncode == 0, result.stderr.strip()
    out = result.stdout.strip()
    if not out:
        return False, ""
    response = cast("dict[str, dict[str, str]]", json.loads(out))
    decision = response["hookSpecificOutput"]
    assert decision.get("hookEventName") == "PreToolUse", decision
    assert decision.get("permissionDecisionReason"), decision
    return decision["permissionDecision"] == "ask", decision["permissionDecisionReason"]


def check(payload: Mapping[str, object], *, ask: bool) -> None:
    asked, reason = run_hook(payload)
    wanted = "ask" if ask else "silence"
    got = f"ask ({reason})" if asked else "silence"
    assert asked == ask, f"expected {wanted}, got {got}"
    assert "Dependency guard failed" not in reason, reason


@pytest.mark.parametrize("case", CASES, ids=[case.label for case in CASES])
def test_file_edit(case: Case, tmp_path: Path) -> None:
    # tmp_path is fresh per case, which keeps Write-to-a-new-file honest.
    target = tmp_path / (case.filename or "pyproject.toml")
    if case.filename:
        _ = target.write_text(case.before, encoding="utf-8")
    payload = {
        "hook_event_name": "PreToolUse",
        "tool_name": case.tool,
        "tool_input": {"file_path": str(target), **case.tool_input},
    }
    check(payload, ask=case.ask)


@pytest.mark.parametrize("case", BASH_CASES, ids=[case.label for case in BASH_CASES])
def test_bash_command(case: BashCase) -> None:
    payload = {
        "hook_event_name": "PreToolUse",
        "tool_name": "Bash",
        "tool_input": {"command": case.command},
    }
    check(payload, ask=case.ask)
