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

# These tests also run in the Linux image, which has gh and openssl but no
# limactl.

import ast
import importlib.util
import json
import os
import shutil
import socket
import stat
import subprocess
import sys
import time
from pathlib import Path
from typing import TYPE_CHECKING, cast, final

import pytest

from tests.fake_github import FakeGitHub, make_certificate

if TYPE_CHECKING:
    from collections.abc import Callable, Iterator
    from types import ModuleType

type Json = dict[str, object]

REPO = Path(__file__).resolve().parents[1]
TOOL = REPO / "bin" / "github_app_token.py"
CLIENT_ID = "Iv-test-client"
REPOSITORIES = {"owner/repo": 101, "owner/other": 102}
GH_STUB = """\
#!/bin/sh
n=$(ls "$GH_STUB_DIR" | grep -c '^argv\\.')
n=$((n + 1))
printf '%s\\n' "$@" >"$GH_STUB_DIR/argv.$n"
cat >"$GH_STUB_DIR/stdin.$n"
printf '%s\\n' "$GH_HOST" >"$GH_STUB_DIR/host.$n"
env >"$GH_STUB_DIR/env.$n"
if [ -e "$GH_STUB_DIR/fail" ]; then
    echo "gh: stub failure for $(cat "$GH_STUB_DIR/stdin.$n")" >&2
    exit 1
fi
"""


@pytest.fixture
def fake() -> Iterator[FakeGitHub]:
    with FakeGitHub(client_id=CLIENT_ID, repo_ids=REPOSITORIES) as server:
        yield server


@final
class Machine:
    """A home directory, a stub gh, and the way to run the tool in them."""

    def __init__(self, root: Path) -> None:
        self.home = root / "home"
        self.home.mkdir()
        self.stub = root / "stub"
        self.stub.mkdir()
        gh = self.stub / "bin" / "gh"
        gh.parent.mkdir()
        _ = gh.write_text(GH_STUB)
        gh.chmod(0o755)
        self.env = {
            "PATH": f"{gh.parent}{os.pathsep}{os.environ['PATH']}",
            "HOME": str(self.home),
            "GH_STUB_DIR": str(self.stub),
            "GIT_CONFIG_NOSYSTEM": "1",
            "GIT_TERMINAL_PROMPT": "0",
            "LC_ALL": "C",
        }

    @property
    def directory(self) -> Path:
        return self.home / ".config" / "github-app"

    @property
    def record_path(self) -> Path:
        return self.directory / "auth.json"

    def record(self) -> Json:
        return cast("Json", json.loads(self.record_path.read_text()))

    def gh_calls(self) -> int:
        return len(list(self.stub.glob("argv.*")))

    def run(
        self, *args: str, stdin: str = "", env: dict[str, str] | None = None
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(TOOL), *args],
            input=stdin,
            capture_output=True,
            text=True,
            check=False,
            env={**self.env, **(env or {})},
            timeout=120,
        )

    def install(
        self, record: Json, *, env: dict[str, str] | None = None
    ) -> subprocess.CompletedProcess[str]:
        return self.run("install", stdin=json.dumps(record), env=env)


@pytest.fixture
def machine(tmp_path: Path) -> Machine:
    return Machine(tmp_path)


def make_record(
    fake: FakeGitHub,
    repository: str = "owner/repo",
    *,
    access_ttl: int = 28800,
    web_url: str | None = None,
) -> Json:
    """A record as limavm builds it from a device-flow token."""
    issued = fake.issue(repository, access_ttl=access_ttl)
    now = int(time.time())
    return {
        "client_id": CLIENT_ID,
        "repository": repository,
        "access_token": access(issued),
        "access_expires_at": now + access_ttl,
        "refresh_token": refresh(issued),
        "refresh_expires_at": now + cast("int", issued["refresh_token_expires_in"]),
        "web_url": web_url or fake.url,
        "api_url": f"{fake.url}/api/v3",
    }


def access(record: Json) -> str:
    return cast("str", record["access_token"])


def refresh(record: Json) -> str:
    return cast("str", record["refresh_token"])


def tokens(record: Json) -> list[str]:
    return [access(record), refresh(record)]


def assert_no_token(result: subprocess.CompletedProcess[str], *secrets: str) -> None:
    for secret in secrets:
        assert secret not in result.stdout
        assert secret not in result.stderr


def installed(machine: Machine, fake: FakeGitHub, *, access_ttl: int = 28800) -> Json:
    record = make_record(fake, access_ttl=access_ttl)
    result = machine.install(record)
    assert result.returncode == 0, result.stderr
    return record


def age(machine: Machine, seconds_left: int) -> None:
    """Make the saved access token look as if it expires in seconds_left."""
    record = machine.record()
    record["access_expires_at"] = int(time.time()) + seconds_left
    _ = machine.record_path.write_text(json.dumps(record))


def credential(
    machine: Machine, fake: FakeGitHub, path: str, *, host: str | None = None
) -> subprocess.CompletedProcess[str]:
    request = f"protocol=http\nhost={host or fake.host}\npath={path}\n\n"
    return machine.run("credential", "get", stdin=request)


def parsed(output: str) -> dict[str, str]:
    return dict(line.split("=", 1) for line in output.splitlines())


def test_install_writes_the_record_privately_and_logs_gh_in(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = make_record(fake)
    result = machine.install(record)
    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    assert result.stderr == ""
    assert stat.S_IMODE(machine.directory.stat().st_mode) == 0o700
    assert stat.S_IMODE(machine.record_path.stat().st_mode) == 0o600
    assert machine.record() == record
    assert fake.refresh_requests() == []
    assert machine.gh_calls() == 1
    assert (machine.stub / "argv.1").read_text().split() == [
        "auth",
        "login",
        "--with-token",
        "--insecure-storage",
    ]
    assert (machine.stub / "stdin.1").read_text() == f"{access(record)}\n"
    assert (machine.stub / "host.1").read_text() == f"{fake.host}\n"


def test_gh_gets_the_token_only_on_stdin_and_no_token_variable(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = make_record(fake)
    env = {
        "GH_TOKEN": "ghp_other",
        "GITHUB_TOKEN": "ghp_other2",
        "GH_HOST": "elsewhere",
    }
    result = machine.install(record, env=env)
    assert result.returncode == 0, result.stderr
    argv = (machine.stub / "argv.1").read_text()
    environment = (machine.stub / "env.1").read_text()
    for secret in tokens(record):
        assert secret not in argv
        assert secret not in environment
    names = {line.split("=", 1)[0] for line in environment.splitlines()}
    assert not {"GH_TOKEN", "GITHUB_TOKEN"} & names
    assert f"GH_HOST={fake.host}" in environment


def test_install_configures_git_for_the_one_host_and_its_paths(
    machine: Machine, fake: FakeGitHub
) -> None:
    _ = installed(machine, fake)
    config = machine.home / ".config" / "git" / "config"
    result = subprocess.run(
        ["git", "config", "--file", str(config), "--list"],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    )
    entries = dict(line.split("=", 1) for line in result.stdout.splitlines())
    assert entries[f"credential.{fake.url}.usehttppath"] == "true"
    helper = entries[f"credential.{fake.url}.helper"]
    assert helper.startswith("!")
    assert helper.endswith("github_app_token.py credential")


def test_install_replaces_the_gh_helper_for_the_host(
    machine: Machine, fake: FakeGitHub
) -> None:
    config = machine.home / ".config" / "git" / "config"
    config.parent.mkdir(parents=True)
    for url in (fake.url, "https://gist.github.com"):
        _ = subprocess.run(
            [
                "git",
                "config",
                "--file",
                str(config),
                f"credential.{url}.helper",
                "!gh auth git-credential",
            ],
            check=True,
            timeout=30,
        )
    _ = installed(machine, fake)
    listing = subprocess.run(
        [
            "git",
            "config",
            "--file",
            str(config),
            "--get-regexp",
            r"credential\..*helper",
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=30,
    ).stdout
    assert listing.count("gh auth git-credential") == 1
    assert (
        "gist.github.com" in listing.split("gh auth git-credential")[0].splitlines()[-1]
    )


def test_install_renews_a_token_that_is_nearly_expired(
    machine: Machine, fake: FakeGitHub
) -> None:
    old = make_record(fake, access_ttl=120)
    result = machine.install(old)
    assert result.returncode == 0, result.stderr
    assert len(fake.refresh_requests()) == 1
    new = machine.record()
    assert access(new) != access(old)
    assert refresh(new) != refresh(old)
    assert cast("int", new["access_expires_at"]) > time.time() + 28000
    assert (machine.stub / "stdin.1").read_text() == f"{access(new)}\n"
    assert_no_token(result, *tokens(old), *tokens(new))


def test_refresh_if_needed_does_nothing_for_a_fresh_token(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    before = machine.record_path.read_bytes()
    result = machine.run("refresh-if-needed")
    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    assert result.stderr == ""
    assert fake.refresh_requests() == []
    assert machine.record_path.read_bytes() == before
    assert machine.gh_calls() == 1
    assert machine.record() == record


def test_refresh_if_needed_renews_inside_the_margin_and_not_outside(
    machine: Machine, fake: FakeGitHub
) -> None:
    _ = installed(machine, fake, access_ttl=900)
    assert machine.run("refresh-if-needed").returncode == 0
    assert fake.refresh_requests() == []
    result = machine.run("refresh-if-needed", "--margin", "1000")
    assert result.returncode == 0, result.stderr
    assert len(fake.refresh_requests()) == 1
    assert "renewed the token for owner/repo" in result.stdout


def test_refresh_rotates_and_saves_both_tokens_before_gh_is_touched(
    machine: Machine, fake: FakeGitHub
) -> None:
    old = installed(machine, fake)
    (machine.stub / "fail").touch()
    result = machine.run("refresh-if-needed", "--force")
    assert result.returncode == 1
    assert "gh auth login failed" in result.stderr
    saved = machine.record()
    assert refresh(saved) != refresh(old)
    assert access(saved) != access(old)
    assert_no_token(result, *tokens(old), *tokens(saved))
    assert fake.valid_access_tokens() == [access(saved)]
    (machine.stub / "fail").unlink()
    result = machine.run("refresh-if-needed")
    assert result.returncode == 0, result.stderr
    assert len(fake.refresh_requests()) == 1
    assert (machine.stub / "stdin.3").read_text() == f"{access(saved)}\n"


def test_two_refreshes_in_a_row_follow_the_rotation(
    machine: Machine, fake: FakeGitHub
) -> None:
    first = installed(machine, fake)
    assert machine.run("refresh-if-needed", "--force").returncode == 0
    second = machine.record()
    assert machine.run("refresh-if-needed", "--force").returncode == 0
    third = machine.record()
    assert len({refresh(first), refresh(second), refresh(third)}) == 3
    assert len(fake.refresh_requests()) == 2
    assert fake.valid_access_tokens() == [access(third)]


def test_concurrent_refreshes_spend_the_refresh_token_once(
    machine: Machine, fake: FakeGitHub
) -> None:
    old = installed(machine, fake)
    age(machine, 60)
    fake.refresh_delay = 0.5
    callers = [
        subprocess.Popen(
            [sys.executable, str(TOOL), "refresh-if-needed"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env=machine.env,
        )
        for _ in range(8)
    ]
    outputs = [caller.communicate(timeout=120) for caller in callers]
    assert [caller.returncode for caller in callers] == [0] * 8, outputs
    assert len(fake.refresh_requests()) == 1
    assert sum("renewed the token" in out for out, _ in outputs) == 1
    assert refresh(machine.record()) != refresh(old)
    assert fake.valid_access_tokens() == [access(machine.record())]


def test_concurrent_credential_requests_all_get_the_one_new_token(
    machine: Machine, fake: FakeGitHub
) -> None:
    _ = installed(machine, fake)
    age(machine, 60)
    fake.refresh_delay = 0.5
    request = f"protocol=http\nhost={fake.host}\npath=owner/repo.git\n\n"
    callers = [
        subprocess.Popen(
            [sys.executable, str(TOOL), "credential", "get"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env=machine.env,
        )
        for _ in range(6)
    ]
    outputs = [caller.communicate(request, timeout=120) for caller in callers]
    assert [caller.returncode for caller in callers] == [0] * 6, outputs
    assert len(fake.refresh_requests()) == 1
    passwords = {parsed(out)["password"] for out, _ in outputs}
    assert passwords == {access(machine.record())}


def test_a_clock_that_jumped_past_the_access_expiry_still_refreshes(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = make_record(fake)
    record["access_expires_at"] = int(time.time()) - 7200
    machine.record_path.parent.mkdir(parents=True, mode=0o700)
    result = machine.install(record)
    assert result.returncode == 0, result.stderr
    assert len(fake.refresh_requests()) == 1
    assert cast("int", machine.record()["access_expires_at"]) > time.time()
    assert (machine.stub / "stdin.1").read_text() == f"{access(machine.record())}\n"


def test_an_expired_refresh_token_is_refused_without_asking_github(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = make_record(fake, access_ttl=60)
    record["refresh_expires_at"] = int(time.time()) - 5
    result = machine.install(record)
    assert result.returncode == 1
    assert "expired" in result.stderr
    assert "limavm github" in result.stderr
    assert "owner/repo" in result.stderr
    assert fake.refresh_requests() == []
    assert_no_token(result, *tokens(record))


def test_bad_refresh_token_says_to_reauthorize_and_to_suspect_a_leak(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    _ = fake.rotate(refresh(record))
    before = machine.record_path.read_bytes()
    result = machine.run("refresh-if-needed", "--force")
    assert result.returncode == 1
    assert result.stdout == ""
    assert "bad_refresh_token" in result.stderr
    assert "limavm github" in result.stderr
    assert "owner/repo" in result.stderr
    assert "de-authorize" in result.stderr
    assert "github.com/settings/apps/authorizations" in result.stderr
    assert "used elsewhere" in result.stderr
    assert machine.record_path.read_bytes() == before
    assert_no_token(result, *tokens(record))


def load_tool() -> ModuleType:
    spec = importlib.util.spec_from_file_location("github_app_token", TOOL)
    assert spec is not None
    assert spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_the_reauthorize_command_names_the_vm_after_lima_(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    reauthorize = cast("Callable[[str], str]", load_tool().reauthorize)
    monkeypatch.setattr(socket, "gethostname", lambda: "lima-w3b-final")
    assert reauthorize("owner/repo") == "limavm github w3b-final owner/repo"
    monkeypatch.setattr(socket, "gethostname", lambda: "debian")
    assert reauthorize("owner/repo") == "limavm github NAME owner/repo"


def test_the_tool_parses_as_python_3_13() -> None:
    """Homebrew's python3 may not be first on a PATH, and Debian's is 3.13."""
    _ = ast.parse(TOOL.read_text(), feature_version=(3, 13))
    python = shutil.which("python3.13")
    if python is not None:
        result = subprocess.run(
            [python, str(TOOL), "--help"],
            capture_output=True,
            text=True,
            check=False,
            timeout=60,
        )
        assert result.returncode == 0, result.stderr


def test_a_server_error_fails_and_leaves_the_record(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    before = machine.record_path.read_bytes()
    fake.refresh_status = 500
    result = machine.run("refresh-if-needed", "--force")
    assert result.returncode == 1
    assert "HTTP 500" in result.stderr
    assert machine.record_path.read_bytes() == before
    assert_no_token(result, *tokens(record))


def test_an_unreachable_github_fails_and_leaves_the_record(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    before = machine.record_path.read_bytes()
    fake.stop()
    result = machine.run("refresh-if-needed", "--force")
    assert result.returncode == 1
    assert "cannot reach" in result.stderr
    assert machine.record_path.read_bytes() == before
    assert_no_token(result, *tokens(record))


def test_without_a_record_every_command_says_to_authorize(machine: Machine) -> None:
    for args in (("refresh-if-needed",), ("status",)):
        result = machine.run(*args)
        assert result.returncode == 1
        assert "limavm github" in result.stderr
    result = machine.run(
        "credential", "get", stdin="protocol=https\nhost=github.com\n\n"
    )
    assert result.returncode == 1
    assert result.stdout == ""


def test_credential_get_answers_for_the_configured_repository(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    for path in ("owner/repo.git", "owner/repo", "Owner/Repo.git", "/owner/repo.git/"):
        result = credential(machine, fake, path)
        assert result.returncode == 0, result.stderr
        assert parsed(result.stdout) == {
            "username": "x-access-token",
            "password": access(record),
            "password_expiry_utc": str(record["access_expires_at"]),
        }
        assert result.stderr == ""


@pytest.mark.parametrize(
    "path",
    [
        "owner/other.git",
        "owner/repo-two.git",
        "owner/repo/extra.git",
        "other/repo.git",
        "repo.git",
        "",
    ],
)
def test_credential_get_gives_nothing_for_another_repository(
    machine: Machine, fake: FakeGitHub, path: str
) -> None:
    _ = installed(machine, fake)
    result = credential(machine, fake, path)
    assert result.returncode == 0
    assert result.stdout == ""


def test_credential_get_gives_nothing_without_a_path_or_for_another_host(
    machine: Machine, fake: FakeGitHub
) -> None:
    _ = installed(machine, fake)
    no_path = machine.run(
        "credential", "get", stdin=f"protocol=http\nhost={fake.host}\n\n"
    )
    assert (no_path.returncode, no_path.stdout) == (0, "")
    other_host = credential(machine, fake, "owner/repo.git", host="evil.example")
    assert (other_host.returncode, other_host.stdout) == (0, "")
    other_protocol = machine.run(
        "credential",
        "get",
        stdin=f"protocol=https\nhost={fake.host}\npath=owner/repo.git\n\n",
    )
    assert (other_protocol.returncode, other_protocol.stdout) == (0, "")


def test_credential_store_and_erase_do_nothing(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    before = machine.record_path.read_bytes()
    request = (
        f"protocol=http\nhost={fake.host}\npath=owner/repo.git\n"
        "username=x\npassword=y\n\n"
    )
    for operation in ("store", "erase"):
        result = machine.run("credential", operation, stdin=request)
        assert (result.returncode, result.stdout, result.stderr) == (0, "", "")
    assert machine.record_path.read_bytes() == before
    assert machine.record() == record


def test_credential_get_refreshes_first(machine: Machine, fake: FakeGitHub) -> None:
    old = installed(machine, fake)
    age(machine, 120)
    result = credential(machine, fake, "owner/repo.git")
    assert result.returncode == 0, result.stderr
    assert len(fake.refresh_requests()) == 1
    answer = parsed(result.stdout)
    assert answer["password"] != access(old)
    assert answer["password"] == access(machine.record())
    assert answer["password"] in fake.valid_access_tokens()
    assert machine.gh_calls() == 2
    assert (machine.stub / "stdin.2").read_text() == f"{answer['password']}\n"


def test_credential_get_fails_loudly_when_the_refresh_token_is_bad(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    age(machine, 120)
    _ = fake.rotate(refresh(record))
    result = credential(machine, fake, "owner/repo.git")
    assert result.returncode == 1
    assert result.stdout == ""
    assert "bad_refresh_token" in result.stderr
    assert_no_token(result, *tokens(record))


def test_git_credential_fill_gives_the_token_for_the_repository_only(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)

    def fill(path: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["git", "credential", "fill"],
            input=f"protocol=http\nhost={fake.host}\npath={path}\n\n",
            capture_output=True,
            text=True,
            check=False,
            env=machine.env,
            timeout=60,
        )

    right = fill("owner/repo.git")
    assert right.returncode == 0, right.stderr
    assert parsed(right.stdout)["password"] == access(record)
    assert parsed(right.stdout)["username"] == "x-access-token"
    wrong = fill("owner/other.git")
    assert wrong.returncode != 0
    assert access(record) not in wrong.stdout
    assert access(record) not in wrong.stderr


def git_ls_remote(
    machine: Machine, fake: FakeGitHub, repository: str
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["git", "ls-remote", f"{fake.url}/{repository}.git"],
        capture_output=True,
        text=True,
        check=False,
        env=machine.env,
        timeout=60,
    )


def test_git_ls_remote_authenticates_through_the_helper(
    machine: Machine, fake: FakeGitHub
) -> None:
    _ = installed(machine, fake)
    result = git_ls_remote(machine, fake, "owner/repo")
    assert result.returncode == 0, result.stderr
    assert "refs/heads/main" in result.stdout
    served = [r for r in fake.requests if r.path == "/owner/repo.git/info/refs"]
    assert [r.status for r in served] == [401, 200]
    assert "authorization" not in served[0].headers
    assert "authorization" in served[1].headers


def test_git_never_sends_the_token_for_another_repository(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    result = git_ls_remote(machine, fake, "owner/other")
    assert result.returncode != 0
    assert result.stdout == ""
    sent = [
        r
        for r in fake.requests
        if r.path.startswith("/owner/other") and "authorization" in r.headers
    ]
    assert sent == []
    assert access(record) not in result.stderr


def test_git_ls_remote_after_the_token_was_renewed_uses_the_new_one(
    machine: Machine, fake: FakeGitHub
) -> None:
    old = installed(machine, fake)
    assert machine.run("refresh-if-needed", "--force").returncode == 0
    result = git_ls_remote(machine, fake, "owner/repo")
    assert result.returncode == 0, result.stderr
    assert access(machine.record()) != access(old)


def test_status_shows_expiry_and_no_token(machine: Machine, fake: FakeGitHub) -> None:
    record = installed(machine, fake)
    result = machine.run("status")
    assert result.returncode == 0, result.stderr
    assert "repository: owner/repo" in result.stdout
    assert "access token expires" in result.stdout
    assert "refresh token expires" in result.stdout
    assert "logged in with the current access token: yes" in result.stdout
    assert_no_token(result, *tokens(record))


def test_status_check_asks_the_api(machine: Machine, fake: FakeGitHub) -> None:
    _ = installed(machine, fake)
    result = machine.run("status", "--check")
    assert result.returncode == 0, result.stderr
    assert "api: HTTP 200, push=True" in result.stdout


def test_status_check_fails_when_the_token_is_rejected(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = installed(machine, fake)
    fake.expire_access(access(record))
    result = machine.run("status", "--check")
    assert result.returncode == 1
    assert "api: HTTP 401" in result.stdout


def test_status_check_fails_for_a_token_that_cannot_reach_the_repository(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = make_record(fake, "owner/other")
    record["repository"] = "owner/repo"
    assert machine.install(record).returncode == 0
    result = machine.run("status", "--check")
    assert result.returncode == 1
    assert "api: HTTP 403" in result.stdout


BAD_RECORDS = {
    "not json": "ghu_secretvalue",
    "an array": "[1, 2]",
    "a string": '"ghu_secretvalue"',
    "empty": "",
}


@pytest.mark.parametrize("text", list(BAD_RECORDS.values()), ids=list(BAD_RECORDS))
def test_malformed_records_are_refused_without_echo(
    machine: Machine, text: str
) -> None:
    result = machine.run("install", stdin=text)
    assert result.returncode == 1
    assert "ghu_secretvalue" not in result.stdout + result.stderr
    assert not machine.directory.exists()
    assert machine.gh_calls() == 0


def bad(fake: FakeGitHub, **change: object) -> Json:
    record = make_record(fake)
    record.update(change)
    return record


@pytest.mark.parametrize(
    ("change", "message"),
    [
        ({"repository": "owner"}, "repository"),
        ({"repository": "../repo"}, "repository"),
        ({"repository": "owner/repo/x"}, "repository"),
        ({"repository": "owner/rep o"}, "repository"),
        ({"access_token": "ghu_bad value"}, "access_token"),
        ({"access_token": "ghu_x\nusername=y"}, "access_token"),
        ({"access_token": ""}, "access_token"),
        ({"refresh_token": 5}, "refresh_token"),
        ({"access_expires_at": -1}, "access_expires_at"),
        ({"access_expires_at": "soon"}, "access_expires_at"),
        ({"refresh_expires_at": True}, "refresh_expires_at"),
        ({"web_url": "ftp://github.com"}, "web_url"),
        ({"web_url": "https://user:pw@github.com"}, "web_url"),
        ({"web_url": "https://github.com/x?y=1"}, "web_url"),
        ({"api_url": "javascript:alert(1)"}, "api_url"),
        ({"client_id": "has space"}, "client_id"),
        ({"extra": "field"}, "unknown fields: extra"),
    ],
)
def test_bad_fields_are_refused_by_name_and_nothing_is_written(
    machine: Machine, fake: FakeGitHub, change: dict[str, object], message: str
) -> None:
    record = bad(fake, **change)
    result = machine.install(record)
    assert result.returncode == 1
    assert message in result.stderr
    for value in (record["access_token"], record["refresh_token"]):
        if isinstance(value, str) and len(value) > 8:
            assert value not in result.stdout + result.stderr
    assert not machine.directory.exists()
    assert not (machine.home / ".config" / "git").exists()
    assert machine.gh_calls() == 0


@pytest.mark.parametrize(
    "field",
    [
        "client_id",
        "repository",
        "access_token",
        "access_expires_at",
        "refresh_token",
        "refresh_expires_at",
    ],
)
def test_a_missing_field_is_refused_by_name(
    machine: Machine, fake: FakeGitHub, field: str
) -> None:
    record = make_record(fake)
    del record[field]
    result = machine.install(record)
    assert result.returncode == 1
    assert f"lacks {field}" in result.stderr
    assert not machine.directory.exists()


def test_the_urls_default_to_github(machine: Machine, fake: FakeGitHub) -> None:
    record = make_record(fake)
    del record["web_url"]
    del record["api_url"]
    record["access_expires_at"] = int(time.time()) + 28800
    # Nothing is fresh enough to need github.com, so nothing is contacted.
    result = machine.install(record, env={"HOME": str(machine.home)})
    assert result.returncode == 0, result.stderr
    saved = machine.record()
    assert saved["web_url"] == "https://github.com"
    assert saved["api_url"] == "https://api.github.com"
    assert (machine.stub / "host.1").read_text() == "github.com\n"


def test_a_reinstall_replaces_the_record_and_the_login(
    machine: Machine, fake: FakeGitHub
) -> None:
    first = installed(machine, fake)
    second = installed(machine, fake)
    assert machine.record() == second
    assert machine.record() != first
    assert machine.gh_calls() == 2
    assert (machine.stub / "stdin.2").read_text() == f"{access(second)}\n"


def test_no_temporary_files_are_left_behind(machine: Machine, fake: FakeGitHub) -> None:
    _ = installed(machine, fake)
    assert machine.run("refresh-if-needed", "--force").returncode == 0
    assert sorted(path.name for path in machine.directory.iterdir()) == [
        "auth.json",
        "gh-synced",
        "lock",
    ]
    assert stat.S_IMODE(machine.record_path.stat().st_mode) == 0o600
    assert stat.S_IMODE(machine.directory.stat().st_mode) == 0o700


def test_no_command_but_credential_get_prints_a_token(
    machine: Machine, fake: FakeGitHub
) -> None:
    record = make_record(fake, access_ttl=60)
    outputs = [machine.install(record)]
    outputs.append(machine.run("refresh-if-needed", "--force"))
    outputs.append(machine.run("status"))
    outputs.append(machine.run("status", "--check"))
    for result in outputs:
        assert_no_token(result, *tokens(record), *tokens(machine.record()))
        assert "ghu_" not in result.stdout + result.stderr
        assert "ghr_" not in result.stdout + result.stderr


@pytest.fixture
def tls(tmp_path: Path) -> tuple[Path, Path]:
    if shutil.which("openssl") is None or shutil.which("gh") is None:
        pytest.fail("these tests need openssl and gh")
    return make_certificate(tmp_path)


def real_gh(machine: Machine, tmp_path: Path, tls: tuple[Path, Path]) -> Path:
    """Replace the stub gh with the real one, which keeps its files in tmp_path."""
    real = shutil.which("gh", path=os.environ["PATH"])
    assert real is not None
    stub = machine.stub / "bin" / "gh"
    stub.unlink()
    stub.symlink_to(real)
    machine.env["SSL_CERT_FILE"] = str(tls[0])
    machine.env["GH_CONFIG_DIR"] = str(tmp_path / "gh")
    return tmp_path / "gh" / "hosts.yml"


# gh sends no credentials to a host that has a port, so the fake answers the
# requests of "gh auth login" without them.
@pytest.mark.parametrize(
    "scopes",
    [None, "", "repo, read:org"],
    ids=["no header", "empty header", "classic scopes"],
)
def test_the_real_gh_accepts_the_token_with_or_without_a_scopes_header(
    tmp_path: Path, tls: tuple[Path, Path], scopes: str | None
) -> None:
    with FakeGitHub(
        client_id=CLIENT_ID,
        repo_ids=REPOSITORIES,
        tls=tls,
        oauth_scopes=scopes,
        public_viewer=True,
    ) as server:
        machine = Machine(tmp_path)
        hosts = real_gh(machine, tmp_path, tls)
        record = make_record(server)
        result = machine.install(record)
        assert result.returncode == 0, result.stderr
        text = hosts.read_text()
        assert access(record) in text
        assert server.host in text
        assert (machine.directory / "gh-synced").exists()
        assert_no_token(result, *tokens(record))


def test_the_real_gh_follows_the_rotation(
    tmp_path: Path, tls: tuple[Path, Path]
) -> None:
    with FakeGitHub(
        client_id=CLIENT_ID, repo_ids=REPOSITORIES, tls=tls, public_viewer=True
    ) as server:
        machine = Machine(tmp_path)
        hosts = real_gh(machine, tmp_path, tls)
        old = make_record(server)
        assert machine.install(old).returncode == 0
        assert machine.run("refresh-if-needed", "--force").returncode == 0
        new = machine.record()
        text = hosts.read_text()
        assert access(new) in text
        assert access(old) not in text
