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

# provision/reset-identity.sh runs in a VM before it is cloned. These tests run
# it against a temporary home.

import json
import os
import shutil
import subprocess
from pathlib import Path
from typing import cast

import pytest

type Json = dict[str, object]

REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "provision" / "reset-identity.sh"
BASH = shutil.which("bash") or "/bin/bash"

pytestmark = pytest.mark.skipif(shutil.which("jq") is None, reason="jq is required")

# What Claude Code 2.1.291 writes at its first start, with the entries that
# provisioning wants to keep.
CLAUDE_JSON: Json = {
    "firstStartTime": "2026-10-06T16:14:50.946Z",
    "firstStartVersion": "2.1.291",
    "machineID": "83b0620b389232c76b0b945636492b6290694b023f1a7f0a9d94653bf6970169",
    "userID": "a7d29981084fb094ca580d38f3a4e09b605e468c2e4e323983b88c25d92c44c9",
    "hasCompletedOnboarding": True,
    "migrationVersion": 14,
    "mcpServers": {"lgtmcp": {"command": "/home/user/go/bin/lgtmcp"}},
    "projects": {"/home/user/src/dotfiles": {"hasTrustDialogAccepted": True}},
}
IDENTITY = {"firstStartTime", "firstStartVersion", "machineID", "userID"}


def run(
    home: Path, env: dict[str, str] | None = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [BASH, str(SCRIPT)],
        env={"HOME": str(home), "PATH": os.environ["PATH"]} if env is None else env,
        capture_output=True,
        text=True,
        check=False,
        timeout=60,
    )


@pytest.fixture
def home(tmp_path: Path) -> Path:
    (tmp_path / ".codex").mkdir()
    _ = (tmp_path / ".codex/installation_id").write_text(
        "1b9f6d7c-0000-4000-8000-000000000000\n"
    )
    _ = (tmp_path / ".codex/config.toml").write_text(
        'approvals_reviewer = "auto_review"\n'
    )
    claude_json = tmp_path / ".claude.json"
    _ = claude_json.write_text(json.dumps(CLAUDE_JSON))
    claude_json.chmod(0o600)
    return tmp_path


def test_removes_what_identifies_the_installation(home: Path) -> None:
    result = run(home)
    assert result.returncode == 0, result.stderr
    assert not (home / ".codex/installation_id").exists()
    remaining = claude_json(home)
    assert set(remaining) == set(CLAUDE_JSON) - IDENTITY


def claude_json(home: Path) -> Json:
    return cast("Json", json.loads((home / ".claude.json").read_text()))


def test_keeps_everything_else(home: Path) -> None:
    assert run(home).returncode == 0
    remaining = claude_json(home)
    assert remaining == {k: v for k, v in CLAUDE_JSON.items() if k not in IDENTITY}
    assert (
        home / ".codex/config.toml"
    ).read_text() == 'approvals_reviewer = "auto_review"\n'


def test_keeps_the_mode_and_leaves_no_temporary_file(home: Path) -> None:
    assert run(home).returncode == 0
    assert (home / ".claude.json").stat().st_mode & 0o777 == 0o600
    assert sorted(path.name for path in home.iterdir()) == [".claude.json", ".codex"]


def test_running_again_changes_nothing(home: Path) -> None:
    assert run(home).returncode == 0
    first = (home / ".claude.json").read_bytes()
    assert run(home).returncode == 0
    assert (home / ".claude.json").read_bytes() == first


def test_a_home_without_either_agent_is_fine(tmp_path: Path) -> None:
    result = run(tmp_path)
    assert result.returncode == 0, result.stderr
    assert list(tmp_path.iterdir()) == []


def test_a_broken_claude_json_is_an_error_and_is_left_alone(home: Path) -> None:
    _ = (home / ".claude.json").write_text("{not json")
    result = run(home)
    assert result.returncode != 0
    assert "cannot update" in result.stderr
    assert (home / ".claude.json").read_text() == "{not json"
    assert not any(path.name.startswith(".claude.json.") for path in home.iterdir())


def test_a_missing_jq_is_an_error(home: Path, tmp_path: Path) -> None:
    empty = tmp_path / "empty-path"
    empty.mkdir()
    result = run(home, env={"HOME": str(home), "PATH": str(empty)})
    assert result.returncode != 0
    assert "jq is not installed" in result.stderr
    assert claude_json(home) == CLAUDE_JSON
