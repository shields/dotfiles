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

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import cast

import pytest

REPO = Path(__file__).resolve().parents[1]
HOOK = REPO / ".codex/hooks/git_guard.py"
RULES = REPO / ".codex/rules"


@pytest.mark.parametrize(
    "command",
    [
        "git push origin main",
        "git -C /tmp/repo push origin main",
        "git -C '/tmp/my repo' -C nested push",
        "git -C/tmp/repo push",
        "git -c color.ui=false push",
        "git --exec-path=/tmp/git-tools push",
        "git --attr-source HEAD push",
        "git --attr-source=HEAD push",
        "git --git-dir=/tmp/repo/.git --work-tree /tmp/repo push",
        "/usr/bin/git -C /tmp/repo push",
        "/opt/homebrew/bin/git push",
        "command git -C /tmp/repo push",
        "env -u GIT_DIR git push",
        "GIT_DIR=/tmp/repo/.git git push",
        "git status && git -C /tmp/repo push",
        "git status\ngit push",
        "if true; then git push; fi",
        "if false; then git status; else git push; fi",
        "while git push; do true; done",
        "for repo in a b; do git -C $repo push; done",
        "bash -lc 'git -C /tmp/repo push'",
        "git commit --no-verify -m message",
        "git commit -m message --no-verify",
        "git -C /tmp/repo commit -n -m message",
        "git commit -an -m message",
        "git commit -nm message",
        "git commit -mm --no-verify",
        "git commit -anmm --no-verify",
        "git -c core.hooksPath=/dev/null commit -m message",
        "git --config-env=core.hooksPath=DISABLED_HOOKS commit",
        "git --attr-source HEAD commit --no-verify",
        "git commit -m message; git push",
        "{ git -C /tmp/repo push; }",
        "time git -C /tmp/repo push",
        "cat <<'EOF'\ngit status\nEOF\ngit push",
        "git \\\n-C /tmp/repo push",
    ],
)
def test_git_hook_denies(command: str) -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    decision = response["hookSpecificOutput"]
    assert decision["permissionDecision"] == "deny"
    assert "failed" not in decision["permissionDecisionReason"]


@pytest.mark.parametrize(
    "command",
    [
        "git status",
        "git -C /tmp/repo status",
        "git -c color.ui=false diff",
        "git --attr-source HEAD diff",
        "git -- push origin main",
        "git -C /tmp/repo -- push",
        "git -- commit --no-verify",
        "git --exec-path /tmp/git-tools push",
        "git --exec-path push",
        "git log --oneline -n 10",
        "git commit -m message",
        "git commit -m 'git push; git commit --no-verify'",
        "git commit -m --no-verify",
        "git commit --message --no-verify",
        "git commit -mn",
        "git commit -F -n",
        "git commit -- -n",
        "printf '%s\\n' 'git push'",
        "printf '%s\\n' ';' git push",
        "# git push\ngit status",
        "rg 'git push' .codex",
        (
            "python3 - <<'PY'\n"
            'print("don\'t parse this as shell")\ngit push\nPY\ngit status'
        ),
        "cat <<-EOF\n\tgit push\n\tEOF\ngit status",
        "cat <<<'git push'\ngit status",
        "command -v git push",
        "command -V git push",
    ],
)
def test_git_hook_allows(command: str) -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    assert result.stdout == ""


def run_hook(payload: dict[str, object]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(HOOK)],
        input=json.dumps(payload),
        text=True,
        capture_output=True,
        check=True,
        timeout=10,
    )


def test_hook_rejects_invalid_input() -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {}})
    assert (
        json.loads(result.stdout)["hookSpecificOutput"]["permissionDecision"] == "deny"
    )


def test_hook_ignores_other_tools() -> None:
    assert run_hook({"tool_name": "apply_patch", "tool_input": {}}).stdout == ""


@pytest.mark.parametrize("command", ["cat << ; git push", "cat <<\ngit push", "cat <<"])
def test_hook_rejects_missing_heredoc_delimiter(command: str) -> None:
    result = run_hook({"tool_name": "Bash", "tool_input": {"command": command}})
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    decision = response["hookSpecificOutput"]
    assert decision["permissionDecision"] == "deny"
    assert "failed" in decision["permissionDecisionReason"]


def test_registered_hook_command() -> None:
    config = cast(
        "dict[str, dict[str, list[dict[str, object]]]]",
        json.loads((REPO / ".codex/hooks.json").read_text()),
    )
    hooks = cast("list[dict[str, str]]", config["hooks"]["PreToolUse"][0]["hooks"])
    command = hooks[0]["command"]
    result = subprocess.run(
        ["/bin/sh", "-c", command],
        input=json.dumps({"tool_name": "Bash", "tool_input": {"command": "git push"}}),
        env={**os.environ, "HOME": str(REPO)},
        text=True,
        capture_output=True,
        check=True,
        timeout=10,
    )
    response = cast("dict[str, dict[str, str]]", json.loads(result.stdout))
    assert response["hookSpecificOutput"]["permissionDecision"] == "deny"


@pytest.mark.parametrize(
    ("command", "expected"),
    [
        (["git", "push", "origin", "main"], "forbidden"),
        (["/usr/bin/git", "push"], "forbidden"),
        (["/usr/local/bin/git", "push"], "forbidden"),
        (["gh", "repo", "delete", "example/repo"], "forbidden"),
        (["/opt/homebrew/bin/gh", "repo", "edit"], "forbidden"),
        (["gh", "repo", "archive"], "forbidden"),
        (["gh", "repo", "create"], "forbidden"),
        (["gh", "repo", "fork"], "forbidden"),
        (["gh", "repo", "rename"], "forbidden"),
        (["gh", "repo", "sync"], "forbidden"),
        (["gh", "repo", "unarchive"], "forbidden"),
        (["gh", "repo", "autolink", "create"], "forbidden"),
        (["gh", "repo", "autolink", "delete"], "forbidden"),
        (["gh", "repo", "deploy-key", "add"], "forbidden"),
        (["gh", "repo", "deploy-key", "delete"], "forbidden"),
        (["lefthook", "uninstall"], "forbidden"),
        (["git", "commit", "--no-verify"], "forbidden"),
        (["git", "commit", "-n"], "forbidden"),
        (["/usr/local/bin/git", "commit", "--no-verify"], "forbidden"),
        (["gh", "repo", "view"], None),
        (["gh", "repo", "autolink", "list"], None),
        (["gh", "repo", "deploy-key", "list"], None),
        (["gh", "pr", "create"], None),
        (["git", "commit", "-m", "message"], None),
        (["lefthook", "run", "pre-commit"], None),
    ],
)
def test_execpolicy(command: list[str], expected: str | None) -> None:
    codex = shutil.which("codex")
    if codex is None:
        pytest.skip("Codex CLI is not installed")
    args = [codex, "execpolicy", "check"]
    for rules in sorted(RULES.glob("*.rules")):
        args.extend(["--rules", str(rules)])
    result = subprocess.run(
        [*args, "--", *command],
        text=True,
        capture_output=True,
        check=True,
        timeout=10,
    )
    response = cast("dict[str, object]", json.loads(result.stdout))
    assert response.get("decision") == expected
