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
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
SSH_CONFIG = REPO / ".ssh/config"
LIMA_INCLUDE = "Include ~/.lima/*/ssh.config"

LIMA_HOST = """Host lima-test
    Hostname 127.0.0.1
    Port 60022
    Compression no
    ForwardAgent yes
"""


def resolved_options(tmp_path: Path, host: str) -> dict[str, list[str]]:
    ssh = shutil.which("ssh")
    assert ssh, "ssh is required"
    lima = tmp_path / ".lima/test"
    lima.mkdir(parents=True, exist_ok=True)
    _ = (lima / "ssh.config").write_text(LIMA_HOST)
    # Only HOME is set, so that nothing in the caller's environment reaches
    # ssh; -G starts no other program, so ssh needs no PATH.
    result = subprocess.run(
        [ssh, "-F", str(SSH_CONFIG), "-G", host],
        env={"HOME": str(tmp_path)},
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=True,
    )
    # -G prints a yes as true for AddKeysToAgent, on macOS and Debian alike.
    options: dict[str, list[str]] = {}
    for line in result.stdout.splitlines():
        name, _, value = line.partition(" ")
        options.setdefault(name, []).append(value)
    return options


def test_ssh_config_includes_lima_hosts_before_anything_else() -> None:
    directives = [
        line
        for line in SSH_CONFIG.read_text().splitlines()
        if line.strip() and not line.startswith("#")
    ]
    assert directives[0] == LIMA_INCLUDE


def test_lima_hosts_keep_lima_settings_and_send_the_locale(tmp_path: Path) -> None:
    options = resolved_options(tmp_path, "lima-test")
    assert options["hostname"] == ["127.0.0.1"]
    assert options["port"] == ["60022"]
    assert options["compression"] == ["no"]
    assert options["forwardagent"] == ["yes"]
    assert {"COLORTERM", "LANG"} <= set(options["sendenv"])
    assert options["addkeystoagent"] == ["true"]


def test_other_hosts_get_the_defaults_and_no_locale(tmp_path: Path) -> None:
    options = resolved_options(tmp_path, "otherhost")
    assert options["compression"] == ["yes"]
    assert options["forwardagent"] == ["no"]
    assert options["addkeystoagent"] == ["true"]
    assert "COLORTERM" not in options.get("sendenv", [])
