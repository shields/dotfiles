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

# provision/linux-system.sh installs packages, edits /etc and changes login
# shells, so these tests read it, run its apt update loop against a stub
# apt-get, and run the whole script only with every command that changes the
# machine replaced by a stub.

import os
import re
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "provision" / "linux-system.sh"
TEXT = SCRIPT.read_text()
LINES = TEXT.splitlines()

PACKAGES = {
    "build-essential",
    "procps",
    "curl",
    "file",
    "git",
    "zsh",
    "vim",
    "locales",
    "ca-certificates",
    "bubblewrap",
    "socat",
    "tmux",
    "ncurses-term",
    "unzip",
    "jq",
    "openssh-client",
}


def line_index(pattern: str) -> int:
    matches = [i for i, line in enumerate(LINES) if re.search(pattern, line)]
    assert matches, pattern
    return matches[0]


def apt_commands() -> list[str]:
    joined = TEXT.replace("\\\n", " ")
    commands: list[str] = re.findall(r"apt-get -o [^\n)|]*", joined)
    return [re.sub(r"\s+", " ", command) for command in commands]


def update_loop() -> str:
    start = line_index(r"^deadline=")
    end = next(i for i in range(start, len(LINES)) if LINES[i] == "done")
    return "\n".join(LINES[start : end + 1])


# The stub reports the lock error in English only when LC_ALL=C, as apt does
# under another locale.
UPDATE_HARNESS = r"""
set -euo pipefail
apt-get() {
    local n=0
    if [[ -e $COUNT ]]; then
        n=$(<"$COUNT")
    fi
    echo $((n + 1)) >"$COUNT"
    if [[ $MODE == other ]]; then
        echo "E: Failed to fetch http://deb.debian.org/" >&2
        return 100
    fi
    if [[ $n -lt $LOCKED ]]; then
        if [[ ${LC_ALL-} == C ]]; then
            echo "E: Could not get lock /var/lib/apt/lists/lock" >&2
        else
            echo "E: Impossible de verrouiller /var/lib/apt/lists/lock" >&2
        fi
        return 100
    fi
    echo "Reading package lists..."
}
sleep() { SECONDS=$((SECONDS + $1)); }
"""


def run_update_loop(
    tmp_path: Path,
    mode: str,
    locked: int,
) -> tuple[subprocess.CompletedProcess[str], int]:
    count = tmp_path / "count"
    script = UPDATE_HARNESS + update_loop() + "\necho finished\n"
    result = subprocess.run(
        ["/bin/bash", "-c", script],
        env={
            "PATH": "/usr/bin:/bin",
            "COUNT": str(count),
            "LOCKED": str(locked),
            "MODE": mode,
        },
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )
    return result, int(count.read_text())


# apt-get, chsh and install append their calls to $LOG. id knows root as well
# as alice, so only the script's own guard can refuse root.
SCRIPT_HARNESS = r"""
set -euo pipefail
log() { printf '%s\n' "$*" >>"$LOG"; }
id() {
    case $1,${2-} in
    -u,*) echo 0 ;;
    -gn,root | -gn,alice) echo "$2" ;;
    *) return 1 ;;
    esac
}
apt-get() { log apt-get "$@"; }
locale() { echo en_US.utf8; }
getent() { echo "$2:x:1000:1000::/home/$2:/bin/bash"; }
chsh() { log chsh "$@"; }
install() { log install "$@"; }
"""


def run_script(
    tmp_path: Path, *args: str
) -> tuple[subprocess.CompletedProcess[str], list[str]]:
    log = tmp_path / "log"
    result = subprocess.run(
        ["/bin/bash", "-c", SCRIPT_HARNESS + TEXT, "linux-system.sh", *args],
        env={"PATH": "/usr/bin:/bin", "LOG": str(log)},
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )
    return result, log.read_text().splitlines() if log.exists() else []


def test_script_is_executable_bash() -> None:
    assert os.access(SCRIPT, os.X_OK)
    assert LINES[0] == "#!/bin/bash"
    assert "set -euo pipefail" in LINES


def test_non_root_callers_are_refused_before_anything_changes() -> None:
    root_check = line_index(r"id -u\) -ne 0")
    assert "must run as root" in LINES[root_check + 1]
    assert LINES[root_check + 2].strip() == "exit 1"
    changing = [
        line_index(r"apt-get -o"),
        line_index(r"^\s+locale-gen"),
        line_index(r"chsh "),
        line_index(r"^install -d"),
    ]
    assert all(root_check < i for i in changing)


@pytest.mark.parametrize(
    ("user", "message"),
    [("root", "not root"), ("carol", "no such user: carol")],
)
def test_root_and_missing_users_are_refused(
    tmp_path: Path, user: str, message: str
) -> None:
    result, calls = run_script(tmp_path, user)
    assert result.returncode == 1
    assert message in result.stderr
    assert calls == []


@pytest.mark.parametrize("args", [[], ["alice", "bob"]], ids=["none", "two"])
def test_other_than_one_argument_is_refused(tmp_path: Path, args: list[str]) -> None:
    result, calls = run_script(tmp_path, *args)
    assert result.returncode == 2
    assert "usage: sudo linux-system.sh USER" in result.stderr
    assert calls == []


def test_a_regular_user_is_provisioned_to_the_end(tmp_path: Path) -> None:
    result, calls = run_script(tmp_path, "alice")
    assert result.returncode == 0, result.stderr
    assert [call.split()[0] for call in calls] == [
        "apt-get",
        "apt-get",
        "chsh",
        "install",
        "install",
    ]
    assert calls[-1] == "install -d -m 755 -o alice -g alice /home/linuxbrew/.linuxbrew"


def test_apt_runs_noninteractively_and_waits_for_the_dpkg_lock() -> None:
    assert line_index(r"^export DEBIAN_FRONTEND=noninteractive") < line_index(
        r"apt-get -o"
    )
    update, install = apt_commands()
    assert update == "apt-get -o DPkg::Lock::Timeout=600 update 2>&1"
    assert install.startswith(
        "apt-get -o DPkg::Lock::Timeout=600 install -y --no-install-recommends "
    )


def test_update_succeeds_at_once_without_a_competing_apt(tmp_path: Path) -> None:
    result, calls = run_update_loop(tmp_path, "lock", locked=0)
    assert result.returncode == 0, result.stderr
    assert "Reading package lists..." in result.stdout
    assert result.stdout.endswith("finished\n")
    assert calls == 1
    assert result.stderr == ""


def test_update_waits_out_a_competing_apt(tmp_path: Path) -> None:
    result, calls = run_update_loop(tmp_path, "lock", locked=3)
    assert result.returncode == 0, result.stderr
    assert calls == 4
    assert result.stderr.count("waiting for another apt process") == 3
    assert "Could not get lock" not in result.stderr


def test_update_gives_up_after_ten_minutes_of_lock(tmp_path: Path) -> None:
    result, calls = run_update_loop(tmp_path, "lock", locked=10**6)
    assert result.returncode == 100
    assert "finished" not in result.stdout
    assert "Could not get lock" in result.stderr
    assert 55 <= calls <= 65


def test_update_fails_at_once_on_any_other_error(tmp_path: Path) -> None:
    result, calls = run_update_loop(tmp_path, "other", locked=0)
    assert result.returncode == 100
    assert calls == 1
    assert "Failed to fetch" in result.stderr
    assert "waiting" not in result.stderr


def test_installed_packages_are_exactly_the_expected_set() -> None:
    install = apt_commands()[1]
    packages = install.removeprefix(
        "apt-get -o DPkg::Lock::Timeout=600 install -y --no-install-recommends "
    ).split()
    assert len(packages) == len(set(packages))
    assert set(packages) == PACKAGES


def test_login_shell_is_debians_zsh() -> None:
    assert "chsh -s /usr/bin/zsh" in TEXT
    assert "!= /usr/bin/zsh" in TEXT


def test_homebrew_prefix_is_created_for_the_user_and_group() -> None:
    assert "install -d -m 755 /home/linuxbrew" in LINES
    assert (
        'install -d -m 755 -o "$user" -g "$group" /home/linuxbrew/.linuxbrew' in LINES
    )


def test_locale_is_generated_only_when_missing() -> None:
    check = line_index(r"locale -a")
    generate = line_index(r"^\s+locale-gen")
    assert check < generate
    assert "en_US" in LINES[line_index(r"grep -qix")]


def test_script_never_calls_sudo() -> None:
    commands = [
        line.strip().split()[0]
        for line in LINES
        if line.strip() and not line.strip().startswith("#")
    ]
    assert "sudo" not in commands
