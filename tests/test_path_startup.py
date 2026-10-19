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
DARWIN = "darwin25.4.0"
SANDBOX = {"CLAUDECODE": "1", "SANDBOX_RUNTIME": "1"}
CA_FILE = "/opt/homebrew/etc/ca-certificates/cert.pem"
OVERRIDE = "x509sslcertoverrideplatform=1"
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


def run_zsh(
    home: Path,
    command: str,
    inherited: str = INHERITED,
    environment: dict[str, str] | None = None,
) -> str:
    result = subprocess.run(
        [tool("zsh"), "-c", command],
        env={
            **(environment or {}),
            "HOME": str(home),
            "ZDOTDIR": str(home),
            "PATH": inherited,
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


def write_zshenv(
    home: Path, ostype: str, *, sourced: int = 1, prelude: str = ""
) -> None:
    # zsh sets OSTYPE itself, so the test assigns it before the real file runs.
    source = f"source {shlex.quote(str(ZSHENV))}\n" * sourced
    _ = (home / ".zshenv").write_text(f"OSTYPE={ostype}\n{prelude}{source}")


def zshenv_path(home: Path, ostype: str, inherited: str = INHERITED) -> str:
    write_zshenv(home, ostype)
    return run_zsh(home, 'print -rn -- "$PATH"', inherited)


def zshenv_certificates(
    home: Path,
    ostype: str,
    environment: dict[str, str],
    *,
    sourced: int = 1,
    prelude: str = "",
) -> dict[str, str]:
    write_zshenv(home, ostype, sourced=sourced, prelude=prelude)
    variables = dict(
        record.split("=", 1)
        for record in run_zsh(home, "env -0", environment=environment).split("\0")
        if record
    )
    return {
        name: variables[name]
        for name in ("SSL_CERT_FILE", "GODEBUG")
        if name in variables
    }


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


def test_zshenv_points_go_at_the_ca_file_in_the_macos_sandbox(tmp_path: Path) -> None:
    assert zshenv_certificates(tmp_path, DARWIN, SANDBOX) == {
        "SSL_CERT_FILE": CA_FILE,
        "GODEBUG": OVERRIDE,
    }


@pytest.mark.parametrize(
    ("ostype", "environment"),
    [
        (DARWIN, {}),
        (DARWIN, {"CLAUDECODE": "1"}),
        (DARWIN, {"SANDBOX_RUNTIME": "1"}),
        (DARWIN, {**SANDBOX, "SANDBOX_RUNTIME": "0"}),
        ("freebsd14.2", SANDBOX),
    ],
)
def test_zshenv_leaves_certificates_alone_outside_the_macos_sandbox(
    tmp_path: Path, ostype: str, environment: dict[str, str]
) -> None:
    assert zshenv_certificates(tmp_path, ostype, environment) == {}


def test_zshenv_keeps_the_ca_file_the_sandbox_sets(tmp_path: Path) -> None:
    proxy_ca = str(tmp_path / "proxy-ca.pem")
    environment = {**SANDBOX, "SSL_CERT_FILE": proxy_ca}
    assert zshenv_certificates(tmp_path, DARWIN, environment) == {
        "SSL_CERT_FILE": proxy_ca,
        "GODEBUG": OVERRIDE,
    }


def test_zshenv_replaces_an_empty_ca_file_setting(tmp_path: Path) -> None:
    environment = {**SANDBOX, "SSL_CERT_FILE": ""}
    assert zshenv_certificates(tmp_path, DARWIN, environment) == {
        "SSL_CERT_FILE": CA_FILE,
        "GODEBUG": OVERRIDE,
    }


@pytest.mark.parametrize(
    ("inherited", "expected"),
    [
        ("", OVERRIDE),
        ("http2client=0", f"http2client=0,{OVERRIDE}"),
        (f"not{OVERRIDE}", f"not{OVERRIDE},{OVERRIDE}"),
        ("x509sslcertoverrideplatform=0", "x509sslcertoverrideplatform=0"),
        (
            "http2client=0,x509sslcertoverrideplatform=0,gctrace=1",
            "http2client=0,x509sslcertoverrideplatform=0,gctrace=1",
        ),
    ],
)
def test_zshenv_adds_the_override_to_godebug_once_unless_set(
    tmp_path: Path, inherited: str, expected: str
) -> None:
    environment = {**SANDBOX, "GODEBUG": inherited}
    certificates = zshenv_certificates(tmp_path, DARWIN, environment, sourced=2)
    assert certificates == {"SSL_CERT_FILE": CA_FILE, "GODEBUG": expected}


@pytest.mark.parametrize(
    ("environment", "expected"),
    [({}, {}), (SANDBOX, {"SSL_CERT_FILE": CA_FILE, "GODEBUG": OVERRIDE})],
)
def test_zshenv_reads_no_unset_variable(
    tmp_path: Path, environment: dict[str, str], expected: dict[str, str]
) -> None:
    # zsh -u applies nounset before any startup file runs; run_zsh requires an
    # empty stderr, so a read of an unset variable fails the test.
    certificates = zshenv_certificates(
        tmp_path, DARWIN, environment, prelude="setopt nounset\n"
    )
    assert certificates == expected


@pytest.mark.parametrize(
    ("ostype", "environment"), [("linux-gnu", {}), (DARWIN, SANDBOX)]
)
def test_zshenv_exports_no_secrets(
    tmp_path: Path, ostype: str, environment: dict[str, str]
) -> None:
    canary = "sk-ant-oat01-never-in-zshenv"
    token = tmp_path / ".config/secrets/CLAUDE_CODE_OAUTH_TOKEN"
    token.parent.mkdir(parents=True)
    _ = token.write_text(canary + "\n")
    write_zshenv(tmp_path, ostype)
    exported = run_zsh(tmp_path, "env", environment=environment)
    assert "CLAUDE_CODE_OAUTH_TOKEN" not in exported
    assert canary not in exported
    assert "TOKEN" not in ZSHENV.read_text()


def test_zshenv_runs_no_external_command(tmp_path: Path) -> None:
    # With no program on PATH, any command that .zshenv ran would be reported
    # on stderr, which run_zsh requires to be empty.
    nowhere = tmp_path / "nowhere"
    nowhere.mkdir()
    assert zshenv_path(tmp_path, "linux-gnu", str(nowhere)).endswith(f":{nowhere}")


def test_zshenv_runs_no_external_command_in_the_macos_sandbox(tmp_path: Path) -> None:
    nowhere = tmp_path / "nowhere"
    nowhere.mkdir()
    write_zshenv(tmp_path, DARWIN)
    exported = run_zsh(
        tmp_path, 'print -rn -- "$SSL_CERT_FILE $GODEBUG"', str(nowhere), SANDBOX
    )
    assert exported == f"{CA_FILE} {OVERRIDE}"


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
