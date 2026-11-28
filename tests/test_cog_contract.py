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
import subprocess
from pathlib import Path
from typing import cast

import pytest

REPO = Path(__file__).resolve().parents[1]

THROWAWAY_MARKER = "/etc/cog-throwaway"
GATEWAY_MARKER = "/etc/cog-gateway"

READERS = [
    (".zshrc", THROWAWAY_MARKER),
    (".zshrc", GATEWAY_MARKER),
    (".zshenv", GATEWAY_MARKER),
    (".claude/statusline.sh", GATEWAY_MARKER),
    (".zsh.d/wt.zsh", GATEWAY_MARKER),
    (".codex/hooks/git_guard.py", GATEWAY_MARKER),
    ("tools/configure_codex.py", THROWAWAY_MARKER),
    ("provision.sh", GATEWAY_MARKER),
]

PUSH_RULES = [
    "Bash(git push*)",
    "Bash(git -C * push*)",
    "Bash(git -c * push*)",
    "Bash(git --git-dir* push*)",
    "Bash(git --work-tree* push*)",
]


@pytest.mark.parametrize(("reader", "marker"), READERS)
def test_each_reader_names_the_marker_that_cog_writes(reader: str, marker: str) -> None:
    assert marker in (REPO / reader).read_text()


@pytest.mark.parametrize("reader", [".zshrc", ".claude/statusline.sh"])
def test_the_placeholder_token_file_has_one_path(reader: str) -> None:
    path = "$HOME/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN"
    assert path in (REPO / reader).read_text()


def test_every_marker_path_in_the_tree_is_one_that_cog_writes() -> None:
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    result = subprocess.run(
        ["git", "grep", "-I", "-h", "-o", "-E", "/etc/(cog|dotfiles)-[a-z-]+"],
        cwd=REPO,
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
        check=True,
    )
    assert set(result.stdout.split()) <= {THROWAWAY_MARKER, GATEWAY_MARKER}


def test_settings_carry_the_deny_rules_and_the_hook_that_cog_relies_on() -> None:
    settings = cast(
        "dict[str, dict[str, object]]",
        json.loads((REPO / ".claude/settings.json").read_text()),
    )
    deny = cast("list[str]", settings["permissions"]["deny"])
    assert deny
    assert set(PUSH_RULES) <= set(deny)
    groups = cast(
        "list[dict[str, list[dict[str, str]]]]", settings["hooks"]["PreToolUse"]
    )
    commands = [hook["command"] for group in groups for hook in group["hooks"]]
    assert any("git_guard.py" in command for command in commands)
