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

import shlex
import shutil
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
ZSHENV = REPO / ".zshenv"
PROFILE = REPO / ".profile"
INHERITED = "/usr/bin:/bin"
LINUXBREW = [
    directory
    for directory in (
        "/home/linuxbrew/.linuxbrew/bin",
        "/home/linuxbrew/.linuxbrew/sbin",
    )
    if Path(directory).is_dir()
]


def tool(name: str) -> str:
    found = shutil.which(name)
    assert found, f"{name} is required"
    return found


def linux_path(home: Path, inherited: str = INHERITED, *, go: bool = False) -> str:
    parts = [f"{home}/bin", f"{home}/.local/bin", *LINUXBREW, *inherited.split(":")]
    if go:
        parts.append(f"{home}/go/bin")
    return ":".join(parts)


def run_zsh(home: Path, command: str, inherited: str = INHERITED) -> str:
    result = subprocess.run(
        [tool("zsh"), "-c", command],
        env={"HOME": str(home), "ZDOTDIR": str(home), "PATH": inherited},
        cwd=home,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=True,
    )
    assert result.stderr == ""
    return result.stdout


def zshenv_path(home: Path, ostype: str, inherited: str = INHERITED) -> str:
    # zsh sets OSTYPE itself, so the test assigns it before the real file runs.
    _ = (home / ".zshenv").write_text(
        f"OSTYPE={ostype}\nsource {shlex.quote(str(ZSHENV))}\n"
    )
    return run_zsh(home, 'print -rn -- "$PATH"', inherited)


def profile_path(home: Path, shell: str, prelude: str, sourced: int = 1) -> str:
    source = f". {shlex.quote(str(PROFILE))}; " * sourced
    result = subprocess.run(
        [tool(shell), "-c", f'{prelude}{source}printf %s "$PATH"'],
        env={
            "HOME": str(home),
            "LANG": "C.UTF-8",
            "PATH": INHERITED,
            "TERM": "dumb",
        },
        cwd=home,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=15,
        check=True,
    )
    assert result.stderr == ""
    return result.stdout


def test_zshenv_wraps_inherited_path_on_linux(tmp_path: Path) -> None:
    (tmp_path / "go/bin").mkdir(parents=True)
    path = zshenv_path(tmp_path, "linux-gnu")
    assert path == linux_path(tmp_path, go=True)


def test_zshenv_skips_missing_go_directory(tmp_path: Path) -> None:
    assert zshenv_path(tmp_path, "linux-gnu") == linux_path(tmp_path)


def test_zshenv_keeps_the_first_occurrence_of_each_directory(tmp_path: Path) -> None:
    inherited = f"/usr/bin:{tmp_path}/.local/bin:/bin:/usr/bin"
    path = zshenv_path(tmp_path, "linux-gnu", inherited)
    expected = [
        f"{tmp_path}/bin",
        *LINUXBREW,
        "/usr/bin",
        f"{tmp_path}/.local/bin",
        "/bin",
    ]
    assert path == ":".join(expected)


def test_zshenv_keeps_directories_the_parent_put_in_front(tmp_path: Path) -> None:
    inherited = f"/venv/bin:{linux_path(tmp_path)}"
    assert zshenv_path(tmp_path, "linux-gnu", inherited) == inherited


@pytest.mark.parametrize("ostype", ["darwin25.4.0", "freebsd14.2", "msys"])
def test_zshenv_leaves_path_alone_off_linux(tmp_path: Path, ostype: str) -> None:
    (tmp_path / "go/bin").mkdir(parents=True)
    assert zshenv_path(tmp_path, ostype) == INHERITED


def test_zshenv_exports_no_secrets(tmp_path: Path) -> None:
    canary = "sk-ant-oat01-never-in-zshenv"
    token = tmp_path / ".config/secrets/CLAUDE_CODE_OAUTH_TOKEN"
    token.parent.mkdir(parents=True)
    _ = token.write_text(canary + "\n")
    _ = zshenv_path(tmp_path, "linux-gnu")
    environment = run_zsh(tmp_path, "env")
    assert "CLAUDE_CODE_OAUTH_TOKEN" not in environment
    assert canary not in environment
    assert "TOKEN" not in ZSHENV.read_text()


def test_zshenv_starts_no_processes() -> None:
    code = [
        line
        for line in ZSHENV.read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    assert code
    assert not [line for line in code if any(s in line for s in ("$(", "`", "<("))]


@pytest.mark.parametrize("shell", ["sh", "bash"])
@pytest.mark.parametrize("prelude", ["", "set -u; "])
def test_profile_wraps_inherited_path_on_linux(
    tmp_path: Path, shell: str, prelude: str
) -> None:
    (tmp_path / "go/bin").mkdir(parents=True)
    path = profile_path(tmp_path, shell, f"{prelude}OSTYPE=linux-gnu; ")
    assert path == linux_path(tmp_path, go=True)


@pytest.mark.parametrize("shell", ["sh", "bash"])
def test_profile_path_is_unchanged_when_sourced_again(
    tmp_path: Path, shell: str
) -> None:
    (tmp_path / "go/bin").mkdir(parents=True)
    path = profile_path(tmp_path, shell, "OSTYPE=linux-gnu; ", sourced=2)
    assert path == linux_path(tmp_path, go=True)


@pytest.mark.parametrize("shell", ["sh", "bash"])
@pytest.mark.parametrize("prelude", ["", "set -u; "])
@pytest.mark.parametrize("ostype", ["OSTYPE=darwin25.4.0; ", "unset OSTYPE; "])
def test_profile_leaves_path_alone_off_linux(
    tmp_path: Path, shell: str, prelude: str, ostype: str
) -> None:
    (tmp_path / "go/bin").mkdir(parents=True)
    assert profile_path(tmp_path, shell, f"{prelude}{ostype}") == INHERITED
