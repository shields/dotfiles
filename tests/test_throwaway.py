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

# provision/throwaway.sh writes the agents' policy for a throwaway environment.
# These tests run it against a temporary etc directory, and with the settings
# file it reads replaced by variants, so nothing here touches /etc.

import copy
import json
import os
import shutil
import subprocess
import tomllib
from pathlib import Path
from typing import cast

import pytest

type Json = dict[str, object]

REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "provision" / "throwaway.sh"
BASH = shutil.which("bash") or "/bin/bash"
HAS_JQ = shutil.which("jq") is not None

pytestmark = pytest.mark.skipif(not HAS_JQ, reason="jq is required")


def load(text: str) -> Json:
    return cast("Json", json.loads(text))


SETTINGS = load((REPO / ".claude/settings.json").read_text())
DENY = cast("list[str]", cast("Json", SETTINGS["permissions"])["deny"])
GUARD_ENTRIES = [
    entry
    for entry in cast("list[Json]", cast("Json", SETTINGS["hooks"])["PreToolUse"])
    if "git_guard.py" in json.dumps(entry)
]


def run(
    etc: Path, *args: str, script: Path = SCRIPT, env: dict[str, str] | None = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [BASH, str(script), "--etc-dir", str(etc), *args],
        capture_output=True,
        text=True,
        check=False,
        env=env,
        timeout=60,
    )


def run_plain(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [BASH, str(SCRIPT), *args],
        capture_output=True,
        text=True,
        check=False,
        timeout=60,
    )


def managed(etc: Path) -> Json:
    return load((etc / "claude-code/managed-settings.json").read_text())


def tree_with_settings(tmp_path: Path, settings: Json) -> Path:
    """A copy of the script beside a .claude/settings.json of the test's choosing."""
    (tmp_path / "provision").mkdir()
    (tmp_path / ".claude").mkdir()
    _ = shutil.copy(SCRIPT, tmp_path / "provision/throwaway.sh")
    _ = (tmp_path / ".claude/settings.json").write_text(json.dumps(settings))
    return tmp_path / "provision/throwaway.sh"


def files_under(root: Path) -> list[str]:
    return sorted(
        str(path.relative_to(root)) for path in root.rglob("*") if path.is_file()
    )


@pytest.fixture
def etc(tmp_path: Path) -> Path:
    path = tmp_path / "etc"
    path.mkdir()
    return path


def test_writes_the_four_files_readable_by_everyone(etc: Path) -> None:
    result = run(etc)
    assert result.returncode == 0, result.stderr
    assert files_under(etc) == [
        "claude-code/managed-settings.json",
        "codex/config.toml",
        "codex/rules/throwaway.rules",
        "dotfiles-throwaway",
    ]
    for name in files_under(etc):
        assert (etc / name).stat().st_mode & 0o777 == 0o644, name
    assert (etc / "dotfiles-throwaway").read_text().strip() != ""
    assert result.stdout == ""


def test_leaves_no_temporary_files(etc: Path) -> None:
    assert run(etc).returncode == 0
    assert sorted(path.name for path in etc.iterdir()) == [
        "claude-code",
        "codex",
        "dotfiles-throwaway",
    ]


def test_managed_settings_remove_the_prompts_and_the_sandbox(etc: Path) -> None:
    assert run(etc).returncode == 0
    settings = managed(etc)
    permissions = cast("Json", settings["permissions"])
    assert permissions["defaultMode"] == "bypassPermissions"
    assert settings["skipDangerousModePermissionPrompt"] is True
    assert settings["sandbox"] == {"enabled": False}


def test_managed_settings_are_the_only_source_of_rules_and_hooks(etc: Path) -> None:
    assert run(etc).returncode == 0
    settings = managed(etc)
    assert settings["allowManagedPermissionRulesOnly"] is True
    assert settings["allowManagedHooksOnly"] is True
    assert set(cast("Json", settings["permissions"])) == {"defaultMode", "deny"}


def test_managed_settings_keep_every_deny_rule(etc: Path) -> None:
    assert run(etc).returncode == 0
    deny = cast("list[str]", cast("Json", managed(etc)["permissions"])["deny"])
    assert deny == DENY
    assert "Bash(git push*)" in deny
    assert "Bash(gh api --method*)" in deny
    assert "Bash(gh repo delete*)" in deny


def test_managed_settings_register_only_the_git_guard_hook(etc: Path) -> None:
    assert run(etc).returncode == 0
    hooks = cast("Json", managed(etc)["hooks"])
    assert set(hooks) == {"PreToolUse"}
    (entry,) = cast("list[Json]", hooks["PreToolUse"])
    assert entry["matcher"] == "Bash"
    (hook,) = cast("list[Json]", entry["hooks"])
    assert hook["type"] == "command"
    assert "$HOME/.codex/hooks/git_guard.py" in cast("str", hook["command"])
    assert "dependency_guard" not in json.dumps(hooks)
    assert hooks["PreToolUse"] == GUARD_ENTRIES


def test_managed_settings_keep_the_status_line(etc: Path) -> None:
    assert run(etc).returncode == 0
    assert managed(etc)["statusLine"] == SETTINGS["statusLine"]


def test_managed_settings_have_nothing_else(etc: Path) -> None:
    assert run(etc).returncode == 0
    assert set(managed(etc)) == {
        "permissions",
        "allowManagedPermissionRulesOnly",
        "allowManagedHooksOnly",
        "skipDangerousModePermissionPrompt",
        "sandbox",
        "hooks",
        "statusLine",
    }


def test_codex_runs_without_approvals_or_a_sandbox(etc: Path) -> None:
    assert run(etc).returncode == 0
    config = tomllib.loads((etc / "codex/config.toml").read_text())
    assert config == {
        "approval_policy": "never",
        "sandbox_mode": "danger-full-access",
    }


@pytest.mark.parametrize(
    ("command", "decision"),
    [
        (["gh", "api", "-X", "POST", "repos/o/r/issues"], "forbidden"),
        (["gh", "api", "--method", "DELETE", "repos/o/r/refs/heads/x"], "forbidden"),
        (["/usr/bin/gh", "api", "-X", "PUT", "repos/o/r/contents/f"], "forbidden"),
        (
            ["/home/linuxbrew/.linuxbrew/bin/gh", "api", "--method", "PATCH", "x"],
            "forbidden",
        ),
        (["gh", "api", "repos/o/r/issues"], None),
        (["gh", "api", "repos/o/r/issues", "-X", "POST"], None),
        (["gh", "pr", "list"], None),
    ],
)
def test_codex_forbids_gh_api_requests_that_name_a_method_first(
    etc: Path, command: list[str], decision: str | None
) -> None:
    codex = shutil.which("codex")
    if codex is None:
        pytest.skip("Codex CLI is not installed")
    assert run(etc).returncode == 0
    result = subprocess.run(
        [
            codex,
            "execpolicy",
            "check",
            "--rules",
            str(etc / "codex/rules/throwaway.rules"),
            "--",
            *command,
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    assert load(result.stdout).get("decision") == decision


def test_the_codex_rule_file_is_one_forbidding_prefix_rule(etc: Path) -> None:
    assert run(etc).returncode == 0
    rules = (etc / "codex/rules/throwaway.rules").read_text()
    assert rules.count("prefix_rule(") == 1
    assert 'decision = "forbidden"' in rules
    assert 'pattern = [GH, "api", ["-X", "--method"]]' in rules


def test_running_again_changes_nothing(etc: Path) -> None:
    assert run(etc).returncode == 0
    first = {name: (etc / name).read_bytes() for name in files_under(etc)}
    assert run(etc).returncode == 0
    second = {name: (etc / name).read_bytes() for name in files_under(etc)}
    assert first == second


def test_replaces_what_an_earlier_run_or_a_person_left(etc: Path) -> None:
    (etc / "claude-code").mkdir()
    _ = (etc / "claude-code/managed-settings.json").write_text('{"old": true}')
    (etc / "codex").mkdir()
    _ = (etc / "codex/config.toml").write_text('approval_policy = "on-request"\n')
    assert run(etc).returncode == 0
    assert "old" not in managed(etc)
    config = tomllib.loads((etc / "codex/config.toml").read_text())
    assert config["approval_policy"] == "never"
    assert (etc / "codex/rules/throwaway.rules").exists()


def test_a_settings_file_without_the_guard_hook_is_refused(tmp_path: Path) -> None:
    settings = copy.deepcopy(SETTINGS)
    hooks = cast("Json", settings["hooks"])
    hooks["PreToolUse"] = [
        entry
        for entry in cast("list[Json]", hooks["PreToolUse"])
        if "git_guard.py" not in json.dumps(entry)
    ]
    script = tree_with_settings(tmp_path, settings)
    etc = tmp_path / "etc"
    etc.mkdir()
    result = run(etc, script=script)
    assert result.returncode != 0
    assert "git_guard.py" in result.stderr
    assert files_under(etc) == []


def test_a_settings_file_without_deny_rules_is_refused(tmp_path: Path) -> None:
    settings = copy.deepcopy(SETTINGS)
    del cast("Json", settings["permissions"])["deny"]
    script = tree_with_settings(tmp_path, settings)
    etc = tmp_path / "etc"
    etc.mkdir()
    result = run(etc, script=script)
    assert result.returncode != 0
    assert "permissions.deny" in result.stderr
    assert files_under(etc) == []


def test_a_missing_settings_file_is_refused(tmp_path: Path) -> None:
    script = tree_with_settings(tmp_path, SETTINGS)
    (tmp_path / ".claude/settings.json").unlink()
    etc = tmp_path / "etc"
    etc.mkdir()
    result = run(etc, script=script)
    assert result.returncode != 0
    assert "settings.json does not exist" in result.stderr


def test_a_status_line_is_optional(tmp_path: Path) -> None:
    settings = copy.deepcopy(SETTINGS)
    del settings["statusLine"]
    script = tree_with_settings(tmp_path, settings)
    etc = tmp_path / "etc"
    etc.mkdir()
    assert run(etc, script=script).returncode == 0
    assert "statusLine" not in managed(etc)


def test_a_missing_jq_is_an_error(etc: Path, tmp_path: Path) -> None:
    empty = tmp_path / "empty"
    empty.mkdir()
    result = run(etc, env={"PATH": str(empty)})
    assert result.returncode != 0
    assert "jq is not installed" in result.stderr
    assert files_under(etc) == []


@pytest.mark.parametrize("args", [["extra"], ["--bogus"], ["--etc-dir"]])
def test_bad_arguments_are_refused(args: list[str]) -> None:
    result = run_plain(*args)
    assert result.returncode != 0
    assert "usage:" in result.stderr


def test_help_succeeds() -> None:
    result = run_plain("--help")
    assert result.returncode == 0
    assert result.stdout.startswith("usage:")


@pytest.mark.skipif(os.geteuid() == 0, reason="root may write /etc")
def test_without_root_the_real_etc_is_refused() -> None:
    result = run_plain()
    assert result.returncode != 0
    assert "must run as root" in result.stderr
