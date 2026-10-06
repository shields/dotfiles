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

# `make test-linux` builds a Docker image, which takes tens of minutes, so these
# tests run the real target in a copy of the repository against a stub docker
# that records its arguments.

import hashlib
import os
import select
import shutil
import signal
import stat
import subprocess
from dataclasses import dataclass
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
STAGED = ["Makefile", "tools/stage_tree.sh", "cloudflare/Dockerfile"]
TEST_COMMAND = "bun install --frozen-lockfile && make test lint"
TIMEOUT = 120

# One argument per line is enough: none of the arguments contains a newline.
# `$STUB/<subcommand>.status` sets the exit status; `$STUB/hold` makes `build`
# report its parent shell's pid on `$STUB/ready` and wait for `$STUB/release`.
STUB_DOCKER = r"""#!/bin/sh
set -eu
sub=$1
echo "$sub" >>"$STUB/calls"
printf '%s\n' "$@" >"$STUB/$sub.args"
if [ "$sub" = build ]; then
    for context; do :; done
    echo "$context" >"$STUB/context.path"
    ls -A "$context" >"$STUB/context.ls"
    if [ -e "$STUB/hold" ]; then
        echo "$PPID" >"$STUB/ready"
        read -r _ <"$STUB/release"
    fi
fi
if [ -e "$STUB/$sub.status" ]; then
    exit "$(cat "$STUB/$sub.status")"
fi
"""

GIT_CONFIG = """\
[user]
    name = Test User
    email = test@example.com
[init]
    defaultBranch = main
[maintenance]
    auto = false
[gc]
    auto = 0
"""


@dataclass(frozen=True)
class Layout:
    repo: Path
    tmp: Path
    stub: Path
    env: dict[str, str]

    def make(self, *args: str) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            ["make", "test-linux", *args],
            cwd=self.repo,
            env=self.env,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=TIMEOUT,
            check=False,
        )

    def calls(self) -> list[str]:
        calls = self.stub / "calls"
        return calls.read_text().split() if calls.exists() else []

    def args(self, subcommand: str) -> list[str]:
        return (self.stub / f"{subcommand}.args").read_text().splitlines()

    def stage_directories(self) -> list[str]:
        return sorted(path.name for path in self.tmp.iterdir())

    def snapshot(self) -> dict[str, bytes]:
        return {
            path.relative_to(self.repo).as_posix(): path.read_bytes()
            for path in sorted(self.repo.rglob("*"))
            if path.is_file() and not path.is_relative_to(self.repo / ".git")
        }

    def status(self) -> str:
        return subprocess.run(
            ["git", "status", "--porcelain", "--ignored"],
            cwd=self.repo,
            env=self.env,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=TIMEOUT,
            check=True,
        ).stdout


@pytest.fixture
def layout(tmp_path: Path) -> Layout:
    base = tmp_path.resolve()
    repo = base / "work/repo"
    tmp = base / "tmp"
    stub = base / "stub"
    bin_dir = base / "bin"
    for directory in (repo, tmp, stub, bin_dir):
        directory.mkdir(parents=True)
    for name in STAGED:
        destination = repo / name
        destination.parent.mkdir(parents=True, exist_ok=True)
        _ = shutil.copy2(REPO / name, destination)
    _ = (repo / "README.md").write_text("readme\n")
    _ = (repo / ".gitignore").write_text("ignored.txt\n")
    config = base / "gitconfig"
    _ = config.write_text(GIT_CONFIG)
    docker = bin_dir / "docker"
    _ = docker.write_text(STUB_DOCKER)
    docker.chmod(docker.stat().st_mode | stat.S_IXUSR)
    env = {
        "PATH": f"{bin_dir}{os.pathsep}{os.environ['PATH']}",
        "HOME": str(base),
        "TMPDIR": str(tmp),
        "STUB": str(stub),
        "GIT_CONFIG_GLOBAL": str(config),
        "GIT_CONFIG_NOSYSTEM": "1",
    }
    for command in (["init", "-q"], ["add", "-A"], ["commit", "-q", "-m", "initial"]):
        _ = subprocess.run(
            ["git", *command],
            cwd=repo,
            env=env,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            timeout=TIMEOUT,
            check=True,
        )
    _ = (repo / "uncommitted.txt").write_text("not committed yet\n")
    _ = (repo / "ignored.txt").write_text("ignored\n")
    return Layout(repo=repo, tmp=tmp, stub=stub, env=env)


def test_the_staged_tree_is_built_and_tested_in_the_image(layout: Layout) -> None:
    before = layout.snapshot()
    status = layout.status()
    result = layout.make()
    assert result.returncode == 0, result.stderr
    assert layout.calls() == ["build", "run"]
    context = Path((layout.stub / "context.path").read_text().strip())
    assert layout.args("build") == [
        "build",
        "--build-arg",
        "MODULES=dev",
        "-t",
        "dotfiles-linux-test",
        "-f",
        "cloudflare/Dockerfile",
        str(context),
    ]
    assert layout.args("run") == [
        "run",
        "--rm",
        "dotfiles-linux-test",
        "zsh",
        "-c",
        TEST_COMMAND,
    ]
    assert context.is_relative_to(layout.tmp)
    assert not context.is_relative_to(layout.repo)
    listing = (layout.stub / "context.ls").read_text().split()
    assert {".git", "Makefile", "README.md", "cloudflare", "uncommitted.txt"} <= set(
        listing
    )
    assert "ignored.txt" not in listing
    assert layout.stage_directories() == []
    assert layout.snapshot() == before
    assert layout.status() == status


def test_modules_and_the_image_name_can_be_overridden(layout: Layout) -> None:
    result = layout.make("MODULES=dev cloud", "LINUX_IMAGE=example/dotfiles:tag")
    assert result.returncode == 0, result.stderr
    assert "MODULES=dev cloud" in layout.args("build")
    assert "example/dotfiles:tag" in layout.args("build")
    assert "example/dotfiles:tag" in layout.args("run")


def test_a_failed_build_fails_the_target_without_running_the_tests(
    layout: Layout,
) -> None:
    _ = (layout.stub / "build.status").write_text("3\n")
    result = layout.make()
    assert result.returncode != 0
    assert layout.calls() == ["build"]
    assert layout.stage_directories() == []


def test_failed_tests_fail_the_target(layout: Layout) -> None:
    _ = (layout.stub / "run.status").write_text("4\n")
    result = layout.make()
    assert result.returncode != 0
    assert layout.calls() == ["build", "run"]
    assert layout.stage_directories() == []


def test_a_secret_in_the_tree_stops_the_target_before_docker_runs(
    layout: Layout,
) -> None:
    secret = "ghp_" + hashlib.sha256(b"test-linux secret").hexdigest()[:36]
    _ = (layout.repo / "uncommitted.txt").write_text(f"token = {secret}\n")
    result = layout.make()
    assert result.returncode != 0
    assert layout.calls() == []
    assert layout.stage_directories() == []
    assert secret not in result.stdout + result.stderr


def test_a_terminated_target_removes_the_stage_directory(layout: Layout) -> None:
    ready = layout.stub / "ready"
    release = layout.stub / "release"
    os.mkfifo(ready)
    os.mkfifo(release)
    _ = (layout.stub / "hold").write_text("")
    process = subprocess.Popen(
        ["make", "test-linux"],
        cwd=layout.repo,
        env=layout.env,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        start_new_session=True,
    )
    try:
        descriptor = os.open(ready, os.O_RDONLY | os.O_NONBLOCK)
        try:
            readable, _, _ = select.select([descriptor], [], [], TIMEOUT)
            assert readable, "docker build was never reached"
            shell_pid = int(os.read(descriptor, 64))
        finally:
            os.close(descriptor)
        assert layout.stage_directories() != []
        os.kill(shell_pid, signal.SIGTERM)
        with release.open("w") as stream:
            _ = stream.write("go\n")
        assert process.wait(timeout=TIMEOUT) != 0
    finally:
        if process.poll() is None:
            os.killpg(process.pid, signal.SIGKILL)
            _ = process.wait()
    assert layout.calls() == ["build"]
    assert layout.stage_directories() == []
