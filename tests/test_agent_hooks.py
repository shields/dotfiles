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
    "/opt/homebrew/bin/python3",
    "/usr/local/bin/python3",
    "/home/linuxbrew/.linuxbrew/bin/python3",
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
                matcher = cast("str", group.get("matcher", ""))
                hooks.append(Hook(agent, event, matcher, handler["command"]))
    return hooks


HOOKS = [
    *registered_hooks("claude", CLAUDE_SETTINGS),
    *registered_hooks("codex", CODEX_HOOKS),
]
GIT_GUARDS = [hook for hook in HOOKS if hook.script == GIT_GUARD]
CLAUDE_GIT_GUARDS = [hook for hook in GIT_GUARDS if hook.agent == "claude"]


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
        for name in ("python3", "python3.15", "python"):
            self.fake(decoys / name, "decoy")
        self.path = f"{decoys}:/usr/bin:/bin"

    def fake(
        self,
        path: Path,
        name: str,
        *,
        mode: int = 0o755,
        version: tuple[int, int, int] = (3, 15, 0),
    ) -> None:
        path.parent.mkdir(parents=True, exist_ok=True)
        _ = path.write_text(
            f"""#!/bin/sh
if [ "$1" = -c ]; then
  exec "$REAL_PYTHON" -c "import sys; sys.version_info = {version}; $2"
fi
echo {name} >> "$FAKE_LOG"
exec "$REAL_PYTHON" "$@"
"""
        )
        path.chmod(mode)

    def install(self, states: tuple[str, ...]) -> list[str]:
        paths: list[str] = []
        for index, state in enumerate(states):
            path = self.root / f"slot{index}" / "python3"
            paths.append(str(path))
            match state:
                case "exec":
                    self.fake(path, f"slot{index}")
                case "outdated":
                    self.fake(path, f"slot{index}", version=(3, 14, 99))
                case "newer":
                    self.fake(path, f"slot{index}", version=(3, 16, 0))
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
        ("claude", "PreToolUse", "Bash|Edit|Write|Monitor", DEPENDENCY_GUARD),
        ("claude", "PermissionRequest", "Bash|Monitor", DEPENDENCY_GUARD),
        ("claude", "PostToolUse", "Bash|Edit|Write|Monitor", DEPENDENCY_GUARD),
        ("claude", "PostToolUseFailure", "Bash|Edit|Write|Monitor", DEPENDENCY_GUARD),
        ("claude", "Stop", "", DEPENDENCY_GUARD),
        ("claude", "SubagentStop", "", DEPENDENCY_GUARD),
        ("claude", "PostToolBatch", "", DEPENDENCY_GUARD),
        ("claude", "SessionEnd", "", DEPENDENCY_GUARD),
        ("claude", "PreToolUse", "Bash|Monitor", GIT_GUARD),
        ("codex", "PreToolUse", "^Bash$", GIT_GUARD),
    }
    assert len(HOOKS) == 10


def missing_interpreter_exit(hook: Hook) -> int:
    # Exit 2 blocks the tool call on PreToolUse, which a missing interpreter must
    # do. On the other events it would keep Claude from stopping, halt the
    # agentic loop or hand stderr to Claude, so they exit 1, which reports the
    # error to the user.
    return 2 if hook.event == "PreToolUse" else 1


@pytest.mark.parametrize("hook", HOOKS, ids=label)
def test_command_lists_the_expected_interpreters(hook: Hook) -> None:
    candidates = re.findall(r"/[\w/.+-]*/python[\w.]*\b", hook.command)
    assert candidates == list(INTERPRETERS)
    assert hook.command.rstrip().endswith(f"exit {missing_interpreter_exit(hook)}")
    for lookup in ("/usr/bin/env", "PATH", ".local"):
        assert lookup not in hook.command


@pytest.mark.parametrize("hook", HOOKS, ids=label)
@pytest.mark.parametrize("shell", SHELLS)
@pytest.mark.parametrize(
    ("states", "expected"),
    [
        (("exec", "exec", "exec"), "slot0"),
        (("newer", "exec", "exec"), "slot0"),
        (("missing", "exec", "exec"), "slot1"),
        (("outdated", "exec", "exec"), "slot1"),
        (("not-executable", "exec", "missing"), "slot1"),
        (("dangling", "missing", "exec"), "slot2"),
        (("missing", "missing", "exec"), "slot2"),
        (("missing", "missing", "missing"), None),
        (("outdated", "outdated", "outdated"), None),
    ],
)
def test_first_compatible_interpreter_runs(
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
        assert result.returncode == missing_interpreter_exit(hook)
        assert sandbox.resolved() == []
        assert "Python 3.15+ not found" in result.stderr
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
    assert "Git policy check failed" not in decision["permissionDecisionReason"]


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


@pytest.mark.parametrize("hook", CLAUDE_GIT_GUARDS, ids=label)
@pytest.mark.parametrize(
    "tool_input",
    [
        {"command": "git push origin main"},
        {"command": "cd /tmp/repo && git push", "timeout_ms": 60_000},
        {"command": "git commit --no-verify -m message"},
        {},
        {"ws": None, "command": None},
    ],
)
def test_git_guard_denies_monitor(
    sandbox: Sandbox, hook: Hook, tool_input: Mapping[str, object]
) -> None:
    result = sandbox.run(
        ["/bin/sh", "-c"],
        substituted(hook, sandbox.install(("exec", "missing", "missing"))),
        payload(hook, "Monitor", tool_input),
    )
    assert result.returncode == 0, result.stderr
    assert decision_of(result)["permissionDecision"] == "deny"


@pytest.mark.parametrize("hook", CLAUDE_GIT_GUARDS, ids=label)
@pytest.mark.parametrize(
    "tool_input",
    [
        {"command": "git log --oneline -n 5"},
        {"command": "tail -f build.log"},
        {"ws": {"url": "wss://example.com/events"}},
    ],
)
def test_git_guard_allows_monitor(
    sandbox: Sandbox, hook: Hook, tool_input: Mapping[str, object]
) -> None:
    result = sandbox.run(
        ["/bin/sh", "-c"],
        substituted(hook, sandbox.install(("exec", "missing", "missing"))),
        payload(hook, "Monitor", tool_input),
    )
    assert (result.returncode, result.stdout, result.stderr) == (0, "", "")


def test_dependency_guard_asks_about_a_new_dependency(
    sandbox: Sandbox, tmp_path: Path
) -> None:
    (hook,) = [
        hook
        for hook in HOOKS
        if hook.agent == "claude"
        and hook.script == DEPENDENCY_GUARD
        and hook.event == "PreToolUse"
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
    assert "Dependency guard failed" not in decision["permissionDecisionReason"]
    edit["new_string"] = '    "pillow>=12.3.0",\n    # no new dependency'
    result = sandbox.run(["/bin/sh", "-c"], command, payload(hook, "Edit", edit))
    assert (result.returncode, result.stdout, result.stderr) == (0, "", "")
    assert sandbox.resolved() == ["slot0", "slot0"]


def claude_settings() -> dict[str, dict[str, object]]:
    return cast("dict[str, dict[str, object]]", json.loads(CLAUDE_SETTINGS.read_text()))


ALLOW_WRITE = [
    "~/.cache/go-build",
    "~/.cache/golangci-lint",
    "~/.cache/pre-commit",
    "~/.cache/uv",
    "~/.npm",
    "~/.ruff_cache",
    "~/Library/Caches/go-build",
    "~/Library/Caches/golangci-lint",
    "~/go/pkg/mod",
    "~/go/pkg/sumdb",
    "/Volumes/Cache",
    "/Volumes/Data/uv-cache.noindex",
]


def test_claude_sandbox_is_enforced_and_narrow() -> None:
    sandbox = claude_settings()["sandbox"]
    assert sandbox["enabled"] is True
    assert sandbox["failIfUnavailable"] is True
    filesystem = cast("dict[str, list[str]]", sandbox["filesystem"])
    # .zshrc evals files from ~/.cache/zsh, so ~/.cache must not be writable.
    assert sorted(filesystem["allowWrite"]) == sorted(ALLOW_WRITE)
    # The dependency guard's lock lives in its state directory, and a sandboxed
    # command that holds the lock keeps the guard from judging a call.
    assert filesystem["denyRead"] == [
        "~/.config/secrets",
        "~/.config/lgtmcp",
        "~/.local/state/claude-dependency-guard",
    ]
    network = cast("dict[str, object]", sandbox["network"])
    assert network == {
        "allowLocalBinding": True,
        "allowMachLookup": ["org.chromium.Chromium.MachPortRendezvousServer.*"],
    }


def test_claude_subprocess_env_is_not_scrubbed() -> None:
    env = cast("dict[str, str]", claude_settings()["env"])
    # The scrub turns off sandbox auto-allow and forces the default permission
    # mode in every session, including on the Mac.
    assert "CLAUDE_CODE_SUBPROCESS_ENV_SCRUB" not in env


def deny_matches(command: str) -> bool:
    """Whether a deny rule of settings.json matches, with * as any text."""
    deny = cast("list[str]", claude_settings()["permissions"]["deny"])
    return any(
        re.fullmatch(
            ".*".join(map(re.escape, rule[len("Bash(") : -1].split("*"))), command
        )
        for rule in deny
        if rule.startswith("Bash(")
    )


@pytest.mark.parametrize("method", ["POST", "PUT", "PATCH", "DELETE"])
@pytest.mark.parametrize(
    "command",
    [
        "gh api -X {} repos/o/r",
        "gh api repos/o/r -X {}",
        "gh api repos/o/r -X{}",
        "gh api --method {} repos/o/r",
        "gh api repos/o/r --method={}",
        "gh api repos/o/r -f a=b --method {} --jq .id",
    ],
)
def test_claude_denies_gh_api_writes(command: str, method: str) -> None:
    assert deny_matches(command.format(method))


@pytest.mark.parametrize(
    "command",
    [
        "gh api repos/o/r",
        "gh api -X GET repos/o/r -f q=x",
        "gh api repos/o/r --method HEAD",
        "gh api graphql -f query='{ viewer { login } }'",
        "git -c user.name=T -c user.email=t@example.com commit -m message",
        "git -c core.commitGraph=false rev-list HEAD",
    ],
)
def test_claude_allows_gh_api_reads_and_identity_commits(command: str) -> None:
    assert not deny_matches(command)


def test_claude_denies_a_hooks_path_override() -> None:
    assert deny_matches("git -c core.hooksPath=/dev/null commit -m message")
    assert deny_matches("git -C /tmp/repo -c core.hooksPath=/dev/null commit")


def test_claude_blocks_when_the_git_guard_fails() -> None:
    groups = cast(
        "list[dict[str, list[dict[str, object]]]]",
        claude_settings()["hooks"]["PreToolUse"],
    )
    handlers = [
        handler
        for group in groups
        for handler in group["hooks"]
        if GIT_GUARD in cast("str", handler["command"])
    ]
    assert [handler["onFailure"] for handler in handlers] == ["block"]


INSTALL_ASK = {
    "Edit(//**/*.Brewfile)",
    "Edit(//**/Gemfile)",
    "Edit(//**/gems.rb)",
    "Edit(//**/*.gemspec)",
    "Edit(//**/.npmrc)",
    "Edit(//**/bunfig.toml)",
    "Edit(//**/.pnpmfile.cjs)",
    "Edit(//**/pnpm-workspace.yaml)",
    *(
        f"Bash({sudo}{command})"
        for sudo in ("", "sudo ")
        for command in (
            "apt install *",
            "apt * install *",
            "apt-get install *",
            "apt-get * install *",
            "dpkg -i *",
            "dpkg --install *",
        )
    ),
}


def test_claude_asks_before_installing_packages() -> None:
    ask = cast("dict[str, list[str]]", claude_settings()["permissions"])["ask"]
    assert set(ask) >= INSTALL_ASK
