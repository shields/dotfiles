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

# The version ordering is npm's own semver, found through the host's real npm.

import json
import shutil
import subprocess
from datetime import UTC, datetime, timedelta
from pathlib import Path
from typing import final

import pytest

REPO = Path(__file__).resolve().parents[1]
CLI_TOOLS_SH = REPO / "provision" / "cli-tools.sh"

DRIVER = """
set -euo pipefail
source "$1"
shift
"$@"
"""

FAKE_NPM = """#!/bin/sh
case "$1 $2" in
"root -g") echo "$FAKE_ROOT" ;;
"view $FAKE_PACKAGE")
    [ "$3 $4 $5 $6" = "time dist-tags versions --json" ] &&
        [ -z "${FAKE_NPM_FAILS-}" ] &&
        cat "$FAKE_VIEW" ;;
"install -g")
    printf '%s\\n' "$*" >> "$FAKE_ROOT/installs"
    [ -z "${FAKE_NPM_INSTALL_FAILS-}" ] ;;
*) exit 64 ;;
esac
"""


def bash(env: dict[str, str], *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["/bin/bash", "-c", DRIVER, "bash", str(CLI_TOOLS_SH), *args],
        env=env,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )


def hours_ago(hours: float) -> str:
    moment = datetime.now(UTC) - timedelta(hours=hours)
    return moment.strftime("%Y-%m-%dT%H:%M:%S.000Z")


DAYS = 24


@final
class Npm:
    def __init__(self, tmp_path: Path, host_root: Path, package: str) -> None:
        self.package = package
        self.root = tmp_path / "root"
        self.root.mkdir()
        (self.root / "npm").symlink_to(host_root / "npm")
        self.view = tmp_path / "view.json"
        fakebin = tmp_path / "fakebin"
        fakebin.mkdir()
        npm = fakebin / "npm"
        _ = npm.write_text(FAKE_NPM)
        npm.chmod(0o755)
        node = shutil.which("node")
        assert node
        self.env = {
            "PATH": f"{fakebin}:{Path(node).parent}:/usr/bin:/bin",
            "FAKE_ROOT": str(self.root),
            "FAKE_VIEW": str(self.view),
            "FAKE_PACKAGE": package,
        }

    def publish(
        self,
        versions: dict[str, float],
        latest: str,
        unpublished: tuple[str, ...] = (),
    ) -> None:
        """Publish VERSIONS, each that many days ago, with LATEST as the tag.

        The registry keeps the time of an UNPUBLISHED version but lists no more.
        """
        times = {"created": hours_ago(9999 * DAYS), "modified": hours_ago(0)}
        times.update({v: hours_ago(days * DAYS) for v, days in versions.items()})
        listed = [v for v in versions if v not in unpublished]
        view = {"time": times, "dist-tags": {"latest": latest}, "versions": listed}
        _ = self.view.write_text(json.dumps(view))

    def install_version(self, version: str) -> None:
        (self.root / self.package).mkdir(parents=True)
        _ = (self.root / self.package / "package.json").write_text(
            json.dumps({"name": self.package, "version": version})
        )

    def run(self) -> subprocess.CompletedProcess[str]:
        return bash(self.env, "npm_install_global", self.package)

    def install(self, version: str) -> str:
        return (
            f"install -g --ignore-scripts --min-release-age=3 {self.package}@{version}"
        )

    @property
    def installs(self) -> list[str]:
        log = self.root / "installs"
        return log.read_text().splitlines() if log.exists() else []


@pytest.fixture
def cf(tmp_path: Path, npm_global_root: Path) -> Npm:
    return Npm(tmp_path, npm_global_root, "cf")


def test_the_newest_beta_wins_over_an_older_stable(cf: Npm) -> None:
    cf.publish(
        {"0.15.0": 80, "1.0.0-beta.9": 5, "1.0.0-beta.10": 4, "1.0.0-beta.2": 20},
        latest="1.0.0-beta.10",
    )
    result = cf.run()
    assert result.returncode == 0, result.stderr
    assert cf.installs == [cf.install("1.0.0-beta.10")]


def test_a_stable_release_wins_over_its_betas(cf: Npm) -> None:
    cf.publish({"1.0.0-beta.10": 12, "1.0.0": 4, "0.15.0": 80}, latest="1.0.0")
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("1.0.0")]


def test_a_version_inside_the_cooldown_waits(cf: Npm) -> None:
    cf.publish(
        {"0.15.0": 80, "1.0.0-beta.9": 4, "1.0.0-beta.10": 2.9},
        latest="1.0.0-beta.10",
    )
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("1.0.0-beta.9")]


def test_a_version_above_the_latest_tag_is_not_installed(cf: Npm) -> None:
    cf.publish({"0.15.0": 80, "1.0.0-rc.1": 10}, latest="0.15.0")
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("0.15.0")]


def test_an_unpublished_version_is_skipped(cf: Npm) -> None:
    cf.publish(
        {"0.15.0": 80, "1.0.1": 5, "1.0.2": 1},
        latest="1.0.2",
        unpublished=("1.0.1",),
    )
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("0.15.0")]


def test_the_chosen_version_already_installed_is_kept(cf: Npm) -> None:
    cf.publish(
        {"0.15.0": 80, "1.0.0-beta.9": 4, "1.0.0-beta.10": 2},
        latest="1.0.0-beta.10",
    )
    cf.install_version("1.0.0-beta.9")
    assert cf.run().returncode == 0
    assert cf.installs == []


def test_an_installed_version_inside_the_cooldown_is_replaced(cf: Npm) -> None:
    cf.publish({"1.0.0-beta.9": 4, "1.0.0-beta.10": 2}, latest="1.0.0-beta.10")
    cf.install_version("1.0.0-beta.10")
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("1.0.0-beta.9")]


def test_an_older_installed_version_is_replaced(cf: Npm) -> None:
    cf.publish({"0.15.0": 80, "1.0.0-beta.9": 4}, latest="1.0.0-beta.9")
    cf.install_version("0.15.0")
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("1.0.0-beta.9")]


def test_a_scoped_package_is_found_under_its_scope(
    tmp_path: Path, npm_global_root: Path
) -> None:
    npm = Npm(tmp_path, npm_global_root, "@scope/tool")
    npm.publish({"0.6.2": 30, "0.6.3": 1}, latest="0.6.3")
    npm.install_version("0.6.2")
    assert npm.run().returncode == 0
    assert npm.installs == []


def test_nothing_old_enough_fails(cf: Npm) -> None:
    cf.publish({"1.0.0-beta.10": 2}, latest="1.0.0-beta.10")
    result = cf.run()
    assert result.returncode != 0
    assert "cf: no release up to its latest tag is 3 days old" in result.stderr
    assert cf.installs == []


def test_a_failed_registry_query_fails(cf: Npm) -> None:
    cf.publish({"0.15.0": 80}, latest="0.15.0")
    cf.env["FAKE_NPM_FAILS"] = "1"
    assert cf.run().returncode != 0
    assert cf.installs == []


def test_a_failed_install_fails(cf: Npm) -> None:
    cf.publish({"0.15.0": 80}, latest="0.15.0")
    cf.env["FAKE_NPM_INSTALL_FAILS"] = "1"
    assert cf.run().returncode != 0
    assert cf.installs == [cf.install("0.15.0")]


@pytest.mark.parametrize(
    "view",
    [
        {},
        {"time": {"0.15.0": "2020-01-01T00:00:00.000Z"}},
        {"dist-tags": {}},
        {
            "time": {"0.15.0": "2020-01-01T00:00:00.000Z"},
            "dist-tags": {"latest": "0.15.0"},
        },
    ],
)
def test_a_registry_answer_without_times_tag_or_versions_fails(
    cf: Npm, view: dict[str, object]
) -> None:
    _ = cf.view.write_text(json.dumps(view))
    assert cf.run().returncode != 0
    assert cf.installs == []


@final
class Completions:
    def __init__(self, tmp_path: Path) -> None:
        self.cache = tmp_path / "cache"
        self.dir = self.cache / "oh-my-zsh/completions"
        self.startup_cache = self.cache / "zsh"
        self.bin = tmp_path / "bin"
        self.bin.mkdir()
        self.env = {
            "PATH": f"{self.bin}:/usr/bin:/bin",
            "HOME": str(tmp_path / "home"),
            "XDG_CACHE_HOME": str(self.cache),
        }

    def tool(self, name: str, body: str) -> None:
        path = self.bin / name
        _ = path.write_text(f"#!/bin/sh\n{body}\n")
        path.chmod(0o755)

    def install(self, name: str, *command: str) -> subprocess.CompletedProcess[str]:
        return bash(self.env, "completion_install", name, *command)

    def remove(self, name: str) -> subprocess.CompletedProcess[str]:
        return bash(self.env, "completion_remove", name)

    def seed_startup_cache(self, name: str) -> Path:
        self.startup_cache.mkdir(parents=True, exist_ok=True)
        path = self.startup_cache / f"{name}-completion"
        _ = path.write_text("stale\n")
        return path


@pytest.fixture
def completions(tmp_path: Path) -> Completions:
    return Completions(tmp_path)


def test_completion_install_writes_the_generated_script_atomically(
    completions: Completions,
) -> None:
    completions.tool("tool", 'echo "#compdef tool"; echo "args: $*"')
    result = completions.install("tool", "tool", "complete", "zsh")
    assert result.returncode == 0, result.stderr
    target = completions.dir / "_tool"
    assert target.read_text() == "#compdef tool\nargs: complete zsh\n"
    assert target.stat().st_mode & 0o777 == 0o644
    assert [path.name for path in completions.dir.iterdir()] == ["_tool"]


def test_completion_install_replaces_the_previous_file(
    completions: Completions,
) -> None:
    completions.tool("tool", "echo new")
    completions.dir.mkdir(parents=True)
    _ = (completions.dir / "_tool").write_text("old\n")
    assert completions.install("tool", "tool").returncode == 0
    assert (completions.dir / "_tool").read_text() == "new\n"


def test_completion_install_removes_the_startup_cache(
    completions: Completions,
) -> None:
    completions.tool("tool", "echo new")
    stale = completions.seed_startup_cache("tool")
    assert completions.install("tool", "tool").returncode == 0
    assert not stale.exists()


@pytest.mark.parametrize("body", ["echo partial; exit 3", "exit 0"])
def test_a_failing_generator_fails_and_leaves_no_file(
    completions: Completions, body: str
) -> None:
    completions.tool("tool", body)
    result = completions.install("tool", "tool", "complete")
    assert result.returncode != 0
    assert "cannot generate the tool completion" in result.stderr
    assert list(completions.dir.iterdir()) == []


def test_a_failing_generator_keeps_the_previous_file(
    completions: Completions,
) -> None:
    completions.tool("tool", "exit 3")
    completions.dir.mkdir(parents=True)
    _ = (completions.dir / "_tool").write_text("old\n")
    assert completions.install("tool", "tool").returncode != 0
    assert [path.name for path in completions.dir.iterdir()] == ["_tool"]
    assert (completions.dir / "_tool").read_text() == "old\n"


def test_a_missing_command_fails(completions: Completions) -> None:
    result = completions.install("tool", str(completions.bin / "absent"))
    assert result.returncode != 0
    assert list(completions.dir.iterdir()) == []


@pytest.mark.parametrize(
    ("step", "body"),
    [
        ("chmod", '[ "$1" = go-w ] && exit 1; exec /bin/chmod "$@"'),
        ("chmod", '[ "$1" = 644 ] && exit 1; exec /bin/chmod "$@"'),
        ("mv", "exit 1"),
    ],
    ids=["chmod-directory", "chmod-file", "mv"],
)
def test_a_failing_install_step_fails_and_leaves_no_temporary_file(
    completions: Completions, step: str, body: str
) -> None:
    completions.tool("tool", "echo new")
    completions.tool(step, body)
    assert completions.install("tool", "tool").returncode != 0
    assert list(completions.dir.iterdir()) == []


def test_the_file_is_renamed_from_inside_its_directory(
    completions: Completions,
) -> None:
    completions.tool("tool", "echo new")
    log = completions.bin / "mv.log"
    completions.tool("mv", f'printf "%s\\n" "$@" >> "{log}"; exec /bin/mv "$@"')
    assert completions.install("tool", "tool").returncode == 0
    *_, source, target = log.read_text().splitlines()
    assert Path(source).parent == Path(target).parent == completions.dir


def test_an_existing_group_writable_directory_is_made_safe(
    completions: Completions,
) -> None:
    completions.tool("tool", "echo new")
    completions.dir.mkdir(parents=True)
    completions.dir.chmod(0o775)
    assert completions.install("tool", "tool").returncode == 0
    assert completions.dir.stat().st_mode & 0o022 == 0


def test_the_directory_is_not_group_writable_under_umask_002(
    completions: Completions,
) -> None:
    # compinit ignores a group-writable completions directory.
    completions.tool("tool", "echo new")
    result = bash(completions.env, "eval", "umask 002; completion_install tool tool")
    assert result.returncode == 0, result.stderr
    assert completions.dir.stat().st_mode & 0o022 == 0
    assert completions.dir.parent.stat().st_mode & 0o022 == 0


def test_completion_remove_deletes_the_file_and_the_startup_cache(
    completions: Completions,
) -> None:
    completions.dir.mkdir(parents=True)
    _ = (completions.dir / "_tool").write_text("old\n")
    _ = (completions.dir / "_other").write_text("other\n")
    stale = completions.seed_startup_cache("tool")
    assert completions.remove("tool").returncode == 0
    assert [path.name for path in completions.dir.iterdir()] == ["_other"]
    assert not stale.exists()


def test_completion_remove_succeeds_when_nothing_exists(
    completions: Completions,
) -> None:
    assert completions.remove("tool").returncode == 0


def test_the_directory_follows_zsh_cache_dir_then_xdg_then_home(
    completions: Completions,
) -> None:
    def directory(env: dict[str, str]) -> str:
        return bash(env, "completion_dir").stdout.strip()

    home = completions.env["HOME"]
    assert directory(completions.env) == f"{completions.cache}/oh-my-zsh/completions"
    without_xdg = {k: v for k, v in completions.env.items() if k != "XDG_CACHE_HOME"}
    assert directory(without_xdg) == f"{home}/.cache/oh-my-zsh/completions"
    explicit = {**completions.env, "ZSH_CACHE_DIR": "/somewhere"}
    assert directory(explicit) == "/somewhere/completions"
