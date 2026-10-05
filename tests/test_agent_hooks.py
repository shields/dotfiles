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

# The interpreter paths in the registered hook commands are absolute, so the
# lookup is tested on a copy of each command whose candidate paths point at
# fake interpreters, and the real text is checked separately for exactly the
# expected paths.

import json
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, cast, final

import pytest

if TYPE_CHECKING:
    from collections.abc import Mapping

REPO = Path(__file__).resolve().parents[1]
CLAUDE_SETTINGS = REPO / ".claude/settings.json"
CODEX_HOOKS = REPO / ".codex/hooks.json"

INTERPRETERS = (
    "/opt/homebrew/bin/python3.14",
    "/usr/local/bin/python3.14",
    "/home/linuxbrew/.linuxbrew/bin/python3.14",
)

DEPENDENCY_GUARD = ".claude/hooks/dependency_guard.py"
GIT_GUARD = ".codex/hooks/git_guard.py"

# Codex runs hook commands with $SHELL -lc; Claude Code uses sh -c.
SHELLS = (
    pytest.param(["/bin/sh", "-c"], id="sh"),
    pytest.param([shutil.which("zsh") or "zsh", "-lc"], id="zsh"),
)

PYPROJECT = """\
[project]
name = "dotfiles"
version = "0.1.0"
dependencies = [
    "pillow>=12.3.0",
]
"""


@final
@dataclass(frozen=True)
class Hook:
    agent: str
    event: str
    matcher: str
    command: str

    @property
    def script(self) -> str:
        match = re.search(r'"\$HOME/([^"]+)"', self.command)
        assert match is not None, self.command
        return match.group(1)

    @property
    def label(self) -> str:
        return f"{self.agent}-{Path(self.script).stem}"


def registered_hooks(agent: str, path: Path) -> list[Hook]:
    config = cast("dict[str, object]", json.loads(path.read_text()))
    events = cast("dict[str, list[dict[str, object]]]", config["hooks"])
    hooks: list[Hook] = []
    for event, groups in events.items():
        for group in groups:
            for handler in cast("list[dict[str, str]]", group["hooks"]):
                assert handler["type"] == "command"
                matcher = cast("str", group["matcher"])
                hooks.append(Hook(agent, event, matcher, handler["command"]))
    return hooks


HOOKS = [
    *registered_hooks("claude", CLAUDE_SETTINGS),
    *registered_hooks("codex", CODEX_HOOKS),
]
GIT_GUARDS = [hook for hook in HOOKS if hook.script == GIT_GUARD]


def label(hook: object) -> str:
    return cast("Hook", hook).label


def payload(hook: Hook, tool_name: str, tool_input: Mapping[str, object]) -> str:
    common: dict[str, object] = {
        "session_id": "00000000-0000-4000-8000-000000000000",
        "transcript_path": f"{REPO}/transcript.jsonl",
        "cwd": str(REPO),
        "permission_mode": "default",
        "hook_event_name": hook.event,
        "tool_name": tool_name,
        "tool_input": tool_input,
        "tool_use_id": "toolu_01",
    }
    if hook.agent == "codex":
        common |= {"model": "gpt-6-astra", "turn_id": "turn-1"}
    return json.dumps(common)


def decision_of(result: subprocess.CompletedProcess[str]) -> dict[str, str]:
    output = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    return output["hookSpecificOutput"]


def bash(hook: Hook, command: str) -> str:
    return payload(hook, "Bash", {"command": command})


@final
class Sandbox:
    def __init__(self, root: Path) -> None:
        self.root = root
        self.home = root / "home"
        self.log = root / "interpreter.log"
        self.log.touch()
        for script in (DEPENDENCY_GUARD, GIT_GUARD):
            link = self.home / script
            link.parent.mkdir(parents=True, exist_ok=True)
            link.symlink_to(REPO / script)
        decoys = root / "decoys"
        decoys.mkdir()
        for name in ("python3", "python3.14", "python"):
            self.fake(decoys / name, "decoy")
        self.path = f"{decoys}:/usr/bin:/bin"

    def fake(self, path: Path, name: str, *, mode: int = 0o755) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(
            f'#!/bin/sh\necho {name} >> "$FAKE_LOG"\nexec "$REAL_PYTHON" "$@"\n'
        )
        path.chmod(mode)

    def install(self, states: tuple[str, ...]) -> list[str]:
        paths: list[str] = []
        for index, state in enumerate(states):
            path = self.root / f"slot{index}" / "python3.14"
            paths.append(str(path))
            match state:
                case "exec":
                    self.fake(path, f"slot{index}")
                case "not-executable":
                    self.fake(path, f"slot{index}", mode=0o644)
                case "dangling":
                    path.parent.mkdir(parents=True)
                    path.symlink_to(self.root / "nowhere")
                case "missing":
                    pass
                case _:
                    raise AssertionError(state)
        return paths

    def run(
        self, shell: list[str], command: str, stdin: str
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [*shell, command],
            input=stdin,
            text=True,
            capture_output=True,
            check=False,
            timeout=30,
            env={
                "HOME": str(self.home),
                "PATH": self.path,
                "FAKE_LOG": str(self.log),
                "REAL_PYTHON": sys.executable,
            },
        )

    def resolved(self) -> list[str]:
        return self.log.read_text().split()


def substituted(hook: Hook, paths: list[str]) -> str:
    command = hook.command
    for real, fake in zip(INTERPRETERS, paths, strict=True):
        assert command.count(real) == 1
        command = command.replace(real, fake)
    return command


@pytest.fixture
def sandbox(tmp_path: Path) -> Sandbox:
    return Sandbox(tmp_path)


def test_registered_hooks() -> None:
    assert {(hook.agent, hook.event, hook.matcher, hook.script) for hook in HOOKS} == {
        ("claude", "PreToolUse", "Bash|Edit|Write", DEPENDENCY_GUARD),
        ("claude", "PreToolUse", "Bash", GIT_GUARD),
        ("codex", "PreToolUse", "^Bash$", GIT_GUARD),
    }
    assert len(HOOKS) == 3


@pytest.mark.parametrize("hook", HOOKS, ids=label)
def test_command_lists_the_expected_interpreters(hook: Hook) -> None:
    candidates = re.findall(r"/[\w/.+-]*/python3\.14\b", hook.command)
    assert candidates == list(INTERPRETERS)
    assert hook.command.rstrip().endswith("exit 2")
    for lookup in ("/usr/bin/env", "PATH", ".local"):
        assert lookup not in hook.command


@pytest.mark.parametrize("hook", HOOKS, ids=label)
@pytest.mark.parametrize("shell", SHELLS)
@pytest.mark.parametrize(
    ("states", "expected"),
    [
        (("exec", "exec", "exec"), "slot0"),
        (("missing", "exec", "exec"), "slot1"),
        (("not-executable", "exec", "missing"), "slot1"),
        (("dangling", "missing", "exec"), "slot2"),
        (("missing", "missing", "exec"), "slot2"),
        (("missing", "missing", "missing"), None),
    ],
)
def test_first_existing_interpreter_runs(
    sandbox: Sandbox,
    hook: Hook,
    shell: list[str],
    states: tuple[str, ...],
    expected: str | None,
) -> None:
    command = substituted(hook, sandbox.install(states))
    result = sandbox.run(shell, command, bash(hook, "git status"))
    assert result.stdout == ""
    if expected is None:
        assert result.returncode == 2
        assert sandbox.resolved() == []
        assert "Python 3.14 not found" in result.stderr
        assert Path(hook.script).name in result.stderr
    else:
        assert result.returncode == 0, result.stderr
        assert result.stderr == ""
        assert sandbox.resolved() == [expected]


@pytest.mark.parametrize("hook", GIT_GUARDS, ids=label)
@pytest.mark.parametrize(
    "command",
    [
        "git push origin main",
        "/usr/bin/git push origin main",
        "env git push origin main",
        "/usr/bin/env git -C /tmp/repo push",
        "git -C /tmp/repo push",
        "cd /tmp && git push",
        "git commit --no-verify -m message",
    ],
)
def test_git_guard_denies(sandbox: Sandbox, hook: Hook, command: str) -> None:
    result = sandbox.run(
        ["/bin/sh", "-c"],
        substituted(hook, sandbox.install(("exec", "missing", "missing"))),
        bash(hook, command),
    )
    assert result.returncode == 0, result.stderr
    decision = decision_of(result)
    assert decision["hookEventName"] == "PreToolUse"
    assert decision["permissionDecision"] == "deny"


@pytest.mark.parametrize("hook", GIT_GUARDS, ids=label)
@pytest.mark.parametrize(
    "command",
    [
        "git status",
        "git log --oneline -n 5",
        "git -C /tmp/repo diff",
        "env FOO=1 git status",
        "git commit -m 'fix the push handler'",
        "ls -la",
    ],
)
def test_git_guard_stays_silent(sandbox: Sandbox, hook: Hook, command: str) -> None:
    result = sandbox.run(
        ["/bin/sh", "-c"],
        substituted(hook, sandbox.install(("exec", "missing", "missing"))),
        bash(hook, command),
    )
    assert (result.returncode, result.stdout, result.stderr) == (0, "", "")


def test_dependency_guard_asks_about_a_new_dependency(
    sandbox: Sandbox, tmp_path: Path
) -> None:
    (hook,) = [
        hook
        for hook in HOOKS
        if hook.agent == "claude" and hook.script == DEPENDENCY_GUARD
    ]
    manifest = tmp_path / "pyproject.toml"
    _ = manifest.write_text(PYPROJECT)
    edit = {
        "file_path": str(manifest),
        "old_string": '    "pillow>=12.3.0",',
        "new_string": '    "pillow>=12.3.0",\n    "requests>=2",',
    }
    command = substituted(hook, sandbox.install(("exec", "missing", "missing")))
    result = sandbox.run(["/bin/sh", "-c"], command, payload(hook, "Edit", edit))
    assert result.returncode == 0, result.stderr
    decision = decision_of(result)
    assert decision["permissionDecision"] == "ask"
    edit["new_string"] = '    "pillow>=12.3.0",\n    # no new dependency'
    result = sandbox.run(["/bin/sh", "-c"], command, payload(hook, "Edit", edit))
    assert (result.returncode, result.stdout, result.stderr) == (0, "", "")
    assert sandbox.resolved() == ["slot0", "slot0"]
