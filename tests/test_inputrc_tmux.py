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

import shutil
import subprocess
import tempfile
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
INPUTRC = REPO / ".inputrc"
TMUX_CONF = REPO / ".tmux.conf"
SYSTEM_INPUTRC = "/etc/inputrc"
SYSTEM_SETTINGS = """set bell-style visible
set show-all-if-ambiguous off
set convert-meta on
"""


def tool(name: str) -> str:
    found = shutil.which(name)
    assert found, f"{name} is required"
    return found


def readline_variables(tmp_path: Path, system_inputrc: Path) -> dict[str, str]:
    text = INPUTRC.read_text()
    assert text.count(f"$include {SYSTEM_INPUTRC}\n") == 1
    inputrc = tmp_path / "inputrc"
    _ = inputrc.write_text(text.replace(SYSTEM_INPUTRC, str(system_inputrc)))
    result = subprocess.run(
        [tool("bash"), "--noprofile", "--norc", "-ic", "bind -v"],
        env={"HOME": str(tmp_path), "INPUTRC": str(inputrc), "TERM": "dumb"},
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert str(system_inputrc) not in result.stderr
    variables: dict[str, str] = {}
    for line in result.stdout.splitlines():
        _, name, value = line.split(" ", 2)
        variables[name] = value
    return variables


def tmux_options() -> dict[str, str]:
    # A socket path must fit in about a hundred bytes, which pytest's
    # temporary directories do not.
    socket_dir = Path(tempfile.mkdtemp(prefix="t"))
    try:
        result = subprocess.run(
            [
                tool("tmux"),
                "-S",
                str(socket_dir / "s"),
                "-f",
                str(TMUX_CONF),
                "start-server",
                ";",
                "show-options",
                "-s",
                ";",
                "show-options",
                "-gw",
                "allow-passthrough",
                ";",
                "kill-server",
            ],
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=15,
            check=False,
        )
    finally:
        shutil.rmtree(socket_dir, ignore_errors=True)
    assert result.returncode == 0, result.stderr
    assert result.stderr == ""
    options: dict[str, str] = {}
    for line in result.stdout.splitlines():
        name, _, value = line.partition(" ")
        options[name] = value
    return options


def test_inputrc_includes_the_system_file_before_its_own_settings() -> None:
    directives = [
        line
        for line in INPUTRC.read_text().splitlines()
        if line.strip() and not line.startswith("#")
    ]
    assert directives[0] == f"$include {SYSTEM_INPUTRC}"
    assert len(directives) > 1


def test_inputrc_settings_win_over_the_included_file(tmp_path: Path) -> None:
    system = tmp_path / "system-inputrc"
    _ = system.write_text(SYSTEM_SETTINGS)
    variables = readline_variables(tmp_path, system)
    assert variables["bell-style"] == "visible"
    assert variables["show-all-if-ambiguous"] == "on"
    assert variables["convert-meta"] == "off"
    assert variables["completion-query-items"] == "1638"


def test_inputrc_tolerates_a_missing_system_file(tmp_path: Path) -> None:
    variables = readline_variables(tmp_path, tmp_path / "does-not-exist")
    assert variables["show-all-if-ambiguous"] == "on"
    assert variables["completion-query-items"] == "1638"


def test_tmux_forwards_clipboard_and_keys_but_not_passthrough() -> None:
    options = tmux_options()
    assert options["set-clipboard"] == "on"
    assert options["extended-keys"] == "on"
    assert options["default-terminal"] == "tmux-256color"
    assert "*:RGB" in options.values()
    assert options["allow-passthrough"] == "off"
