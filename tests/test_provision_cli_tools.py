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
import sys
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
        """The registry keeps the time of an UNPUBLISHED version but lists no more."""
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


def test_a_prerelease_waits_behind_a_new_stable_latest_tag(cf: Npm) -> None:
    cf.publish({"0.15.0": 80, "1.0.0-rc.1": 10, "1.0.0": 1}, latest="1.0.0")
    assert cf.run().returncode == 0
    assert cf.installs == [cf.install("0.15.0")]


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


FAKE_CARGO = """#!/bin/sh
case "$1 $2" in
"install --list")
    [ -z "${FAKE_CARGO_LIST_FAILS-}" ] && cat "$FAKE_LIST" ;;
"install --locked")
    printf '%s\\n' "$*" >> "$FAKE_LOG"
    [ -z "${FAKE_CARGO_INSTALL_FAILS-}" ] ;;
*) exit 64 ;;
esac
"""

FAKE_CURL = """#!/bin/sh
printf 'curl %s\\n' "$*" >> "$FAKE_LOG"
[ "$*" = "-fsSL --proto =https https://index.crates.io/$FAKE_INDEX_PATH" ] &&
    [ -z "${FAKE_CURL_FAILS-}" ] &&
    cat "$FAKE_INDEX"
"""


@final
class Cargo:
    def __init__(
        self, tmp_path: Path, host_root: Path, crate: str, index_path: str
    ) -> None:
        self.crate = crate
        root = tmp_path / "root"
        root.mkdir()
        (root / "npm").symlink_to(host_root / "npm")
        self.list = tmp_path / "list"
        _ = self.list.write_text("")
        self.index = tmp_path / "index"
        self.log = tmp_path / "log"
        fakebin = tmp_path / "fakebin"
        fakebin.mkdir()
        for name, body in [
            ("npm", FAKE_NPM),
            ("cargo", FAKE_CARGO),
            ("curl", FAKE_CURL),
        ]:
            _ = (fakebin / name).write_text(body)
            (fakebin / name).chmod(0o755)
        node = shutil.which("node")
        assert node
        self.env = {
            "PATH": f"{fakebin}:{Path(node).parent}:/usr/bin:/bin",
            "FAKE_ROOT": str(root),
            "FAKE_LIST": str(self.list),
            "FAKE_INDEX": str(self.index),
            "FAKE_INDEX_PATH": index_path,
            "FAKE_LOG": str(self.log),
        }

    def publish(
        self, versions: dict[str, float | None], yanked: tuple[str, ...] = ()
    ) -> None:
        lines: list[str] = []
        for version, days in versions.items():
            entry: dict[str, object] = {
                "name": self.crate,
                "vers": version,
                "deps": [],
                "yanked": version in yanked,
            }
            if days is not None:
                entry["pubtime"] = hours_ago(days * DAYS).replace(".000", "")
            lines.append(json.dumps(entry))
        _ = self.index.write_text("".join(f"{line}\n" for line in lines))

    def run(self) -> subprocess.CompletedProcess[str]:
        return bash(self.env, "cargo_install_global", self.crate)

    def install(self, version: str) -> str:
        return f"install --locked {self.crate} --version ={version}"

    @property
    def calls(self) -> list[str]:
        return self.log.read_text().splitlines() if self.log.exists() else []

    @property
    def installs(self) -> list[str]:
        return [call for call in self.calls if not call.startswith("curl ")]


@pytest.fixture
def ghist(tmp_path: Path, npm_global_root: Path) -> Cargo:
    return Cargo(tmp_path, npm_global_root, "ghist", "gh/is/ghist")


def test_the_newest_old_enough_stable_crate_is_installed(ghist: Cargo) -> None:
    ghist.publish(
        {"0.9.0": 80, "0.10.0": 10, "0.11.0": 5, "0.12.0-rc.1": 4, "0.12.0": 1}
    )
    result = ghist.run()
    assert result.returncode == 0, result.stderr
    assert ghist.installs == [ghist.install("0.11.0")]


def test_a_yanked_crate_version_is_skipped(ghist: Cargo) -> None:
    ghist.publish({"0.10.0": 10, "0.11.0": 5}, yanked=("0.11.0",))
    assert ghist.run().returncode == 0
    assert ghist.installs == [ghist.install("0.10.0")]


def test_a_crate_version_without_a_publish_time_counts_as_new(ghist: Cargo) -> None:
    ghist.publish({"0.10.0": 10, "0.11.0": None})
    assert ghist.run().returncode == 0
    assert ghist.installs == [ghist.install("0.10.0")]


def test_the_chosen_crate_version_already_installed_is_kept(ghist: Cargo) -> None:
    ghist.publish({"0.10.0": 10, "0.11.0": 1})
    _ = ghist.list.write_text(
        "ghist-extra v9.0.0:\n    ghist-extra\nghist v0.10.0:\n    ghist\n"
    )
    assert ghist.run().returncode == 0
    assert ghist.installs == []


@pytest.mark.parametrize("installed", ["0.9.0", "0.11.0"])
def test_another_installed_crate_version_is_replaced(
    ghist: Cargo, installed: str
) -> None:
    ghist.publish({"0.9.0": 80, "0.10.0": 10, "0.11.0": 1})
    _ = ghist.list.write_text(f"ghist v{installed}:\n    ghist\n")
    assert ghist.run().returncode == 0
    assert ghist.installs == [ghist.install("0.10.0")]


def test_a_crate_installed_from_a_path_is_left_alone(ghist: Cargo) -> None:
    _ = ghist.list.write_text("ghist v0.0.0 (/home/me/src/ghist):\n    ghist\n")
    ghist.env["FAKE_CURL_FAILS"] = "1"
    assert ghist.run().returncode == 0
    assert ghist.calls == []


def test_a_crate_installed_from_git_is_replaced(ghist: Cargo) -> None:
    ghist.publish({"0.10.0": 10})
    origin = "https://github.com/shields/ghist#0123456789abcdef"
    _ = ghist.list.write_text(f"ghist v0.10.0 ({origin}):\n    ghist\n")
    assert ghist.run().returncode == 0
    assert ghist.installs == [ghist.install("0.10.0")]


def test_another_crates_binary_of_the_same_name_is_not_the_crate(
    ghist: Cargo,
) -> None:
    ghist.publish({"0.10.0": 10})
    _ = ghist.list.write_text("other v1.0.0 (/home/me/src/other):\n    ghist\n")
    assert ghist.run().returncode == 0
    assert ghist.installs == [ghist.install("0.10.0")]


@pytest.mark.parametrize(
    ("crate", "index_path"),
    [
        ("a", "1/a"),
        ("ab", "2/ab"),
        ("abc", "3/a/abc"),
        ("cargo-fuzz", "ca/rg/cargo-fuzz"),
        ("Serde_JSON", "se/rd/serde_json"),
    ],
)
def test_the_index_path_follows_the_crate_names_length(
    tmp_path: Path, npm_global_root: Path, crate: str, index_path: str
) -> None:
    cargo = Cargo(tmp_path, npm_global_root, crate, index_path)
    cargo.publish({"1.0.0": 10})
    result = cargo.run()
    assert result.returncode == 0, result.stderr
    assert cargo.installs == [cargo.install("1.0.0")]


def test_no_crate_version_old_enough_fails(ghist: Cargo) -> None:
    ghist.publish({"0.11.0-rc.1": 10, "0.11.0": 1})
    result = ghist.run()
    assert result.returncode != 0
    assert "ghist: no release is 3 days old" in result.stderr
    assert ghist.installs == []


@pytest.mark.parametrize(
    "failure", ["FAKE_CARGO_LIST_FAILS", "FAKE_CURL_FAILS", "FAKE_CARGO_INSTALL_FAILS"]
)
def test_a_failing_cargo_step_fails(ghist: Cargo, failure: str) -> None:
    ghist.publish({"0.10.0": 10})
    ghist.env[failure] = "1"
    assert ghist.run().returncode != 0


def test_an_unreadable_index_fails(ghist: Cargo) -> None:
    _ = ghist.index.write_text("not json\n")
    assert ghist.run().returncode != 0
    assert ghist.installs == []


FAKE_GO = """#!{python}
import json, os, sys

args = sys.argv[1:]
with open(os.environ["FAKE_LOG"], "a") as log:
    log.write(" ".join(args) + "\\n")
with open(os.environ["FAKE_SPEC"]) as spec_file:
    spec = json.load(spec_file)
if args[0] == "env":
    print(os.environ.get(args[1]) or spec["env"].get(args[1], ""))
elif args[:2] == ["version", "-m"]:
    with open(args[2]) as binary:
        info = binary.read()
    if not info.startswith("x: go"):
        sys.exit(info)
    sys.stdout.write(info)
elif args[0] == "install":
    with open(os.environ["FAKE_LOG"] + ".env", "a") as log:
        log.write(" ".join(os.environ.get(k, "") for k in ["GOBIN", "GOPATH"]) + "\\n")
    sys.exit(1 if spec.get("install_fails") else 0)
elif args[:2] == ["list", "-m"]:
    module, _, query = args[-1].partition("@")
    if module in spec.get("errors", {{}}):
        sys.exit(spec["errors"][module])
    found = spec["modules"].get(module)
    if found is None:
        sys.exit(f'go: module {{module}}: no matching versions for query "latest"')
    template = args[-2]
    version = found["latest"] if query == "latest" else query
    if "-versions" in args:
        sys.stdout.write("".join(v + "\\n" for v in found["versions"]))
    elif template == "{{{{.Version}}}}":
        print(version)
    elif template == "{{{{.Path}}}}":
        print(module)
    elif template == "{{{{.Version}}}} {{{{.Time.Unix}}}}":
        print(version, found["times"][version])
    elif template == "{{{{.Time.Unix}}}}":
        print(found["times"][version])
    else:
        sys.exit(64)
else:
    sys.exit(64)
"""

GOIMPORTS = "golang.org/x/tools/cmd/goimports"
TOOLS = "golang.org/x/tools"


def days_ago(days: float) -> int:
    return int((datetime.now(UTC) - timedelta(days=days)).timestamp())


@final
class Go:
    def __init__(self, tmp_path: Path, package: str) -> None:
        self.package = package
        self.gobin = tmp_path / "gobin"
        self.gobin.mkdir()
        self.spec_file = tmp_path / "spec.json"
        self.log = tmp_path / "log"
        self.spec: dict[str, object] = {"env": {"GOBIN": str(self.gobin)}}
        self.modules: dict[str, dict[str, object]] = {}
        fakebin = tmp_path / "fakebin"
        fakebin.mkdir()
        go = fakebin / "go"
        _ = go.write_text(FAKE_GO.format(python=sys.executable))
        go.chmod(0o755)
        self.env = {
            "PATH": f"{fakebin}:/usr/bin:/bin",
            "FAKE_SPEC": str(self.spec_file),
            "FAKE_LOG": str(self.log),
        }

    def publish(
        self,
        module: str,
        versions: dict[str, float],
        commit: tuple[str, float] | None = None,
        untagged: dict[str, float] | None = None,
    ) -> None:
        """VERSIONS are oldest first.

        @latest resolves as Go resolves it: the highest release, else the
        highest prerelease, else COMMIT, the pseudo-version of the newest commit.
        UNTAGGED are older commits that resolve by pseudo-version.
        """
        tags = list(versions)
        releases = [v for v in tags if "-" not in v]
        times = {
            v: days_ago(days) for v, days in {**versions, **(untagged or {})}.items()
        }
        if commit:
            times[commit[0]] = days_ago(commit[1])
        latest = (releases or tags or [commit[0] if commit else None])[-1]
        self.modules[module] = {"versions": tags, "times": times, "latest": latest}

    def install_binary(self, name: str, path: str, mod: str) -> None:
        binary = self.gobin / name
        _ = binary.write_text(f"x: go1.27\n\tpath\t{path}\n\tmod\t{mod}\n")
        binary.chmod(0o755)

    def install_other_file(self, name: str, diagnostic: str) -> None:
        binary = self.gobin / name
        _ = binary.write_text(
            f"{binary}: could not read Go build info from {binary}: {diagnostic}"
        )
        binary.chmod(0o755)

    def run(self) -> subprocess.CompletedProcess[str]:
        _ = self.spec_file.write_text(
            json.dumps({**self.spec, "modules": self.modules})
        )
        return bash(self.env, "go_install_global", self.package)

    @property
    def calls(self) -> list[str]:
        return self.log.read_text().splitlines() if self.log.exists() else []

    @property
    def installs(self) -> list[str]:
        return [call for call in self.calls if call.startswith("install ")]

    @property
    def install_environments(self) -> list[str]:
        log = self.log.with_name(self.log.name + ".env")
        return log.read_text().splitlines() if log.exists() else []


@pytest.fixture
def goimports(tmp_path: Path) -> Go:
    return Go(tmp_path, GOIMPORTS)


def test_the_newest_old_enough_release_of_the_packages_module_is_installed(
    goimports: Go,
) -> None:
    goimports.publish(
        TOOLS, {"v0.20.0": 80, "v0.21.0": 10, "v0.22.0-pre.1": 5, "v0.22.0": 1}
    )
    result = goimports.run()
    assert result.returncode == 0, result.stderr
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


def test_a_go_prerelease_is_installed_only_without_a_release(goimports: Go) -> None:
    goimports.publish(TOOLS, {"v0.1.0-pre.1": 10, "v0.1.0-pre.2": 5, "v0.1.0-pre.3": 1})
    assert goimports.run().returncode == 0
    assert goimports.installs == [f"install {GOIMPORTS}@v0.1.0-pre.2"]


def test_an_untagged_modules_old_enough_commit_is_installed(goimports: Go) -> None:
    goimports.publish(TOOLS, {}, commit=("v0.0.0-20261120000000-abcdef123456", 7))
    assert goimports.run().returncode == 0
    assert goimports.installs == [
        f"install {GOIMPORTS}@v0.0.0-20261120000000-abcdef123456"
    ]


def test_an_installed_old_enough_version_stays_while_the_newest_is_new(
    goimports: Go,
) -> None:
    old = "v0.0.0-20261001000000-000000000001"
    goimports.publish(
        TOOLS,
        {},
        commit=("v0.0.0-20261126000000-000000000002", 1),
        untagged={old: 50},
    )
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\t{old}\th1:sum=")
    assert goimports.run().returncode == 0
    assert goimports.installs == []


@pytest.mark.parametrize("installed", ["", "v0.0.0-20261126000000-000000000002"])
def test_no_go_version_old_enough_fails(goimports: Go, installed: str) -> None:
    goimports.publish(TOOLS, {}, commit=("v0.0.0-20261126000000-000000000002", 1))
    if installed:
        goimports.install_binary(
            "goimports", GOIMPORTS, f"{TOOLS}\t{installed}\th1:sum="
        )
    result = goimports.run()
    assert result.returncode != 0
    assert f"{GOIMPORTS}: no version is 3 days old" in result.stderr
    assert goimports.installs == []


def test_the_chosen_go_version_already_installed_is_kept(goimports: Go) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10, "v0.22.0": 1})
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\tv0.21.0\th1:sum=")
    assert goimports.run().returncode == 0
    assert goimports.installs == []


def test_another_installed_go_version_is_replaced(goimports: Go) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10, "v0.22.0": 1})
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\tv0.22.0\th1:sum=")
    assert goimports.run().returncode == 0
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


@pytest.mark.parametrize("version", ["v0.0.0-20261126000000-000000000002", "(devel)"])
def test_a_binary_built_from_a_checkout_is_left_alone(
    goimports: Go, version: str
) -> None:
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\t{version}\t")
    assert goimports.run().returncode == 0
    assert not [call for call in goimports.calls if call.startswith("list ")]
    assert goimports.installs == []


def test_another_packages_binary_of_the_same_name_is_replaced(goimports: Go) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10})
    goimports.install_binary("goimports", "example.com/goimports", "example.com\t\t")
    assert goimports.run().returncode == 0
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


@pytest.mark.parametrize(
    ("package", "binary"),
    [
        ("github.com/evilmartians/lefthook/v2", "lefthook"),
        ("example.com/tool/v10", "tool"),
        ("example.com/tool/v1", "v1"),
        ("example.com/tool/v0", "v0"),
    ],
)
def test_the_binary_is_named_without_a_major_version_suffix(
    tmp_path: Path, package: str, binary: str
) -> None:
    go = Go(tmp_path, package)
    go.publish(package, {"v2.1.0": 10})
    go.install_binary(binary, package, f"{package}\tv2.1.0\th1:sum=")
    assert go.run().returncode == 0
    assert go.installs == []


def test_without_gobin_the_first_gopath_entry_holds_the_binary(
    tmp_path: Path, goimports: Go
) -> None:
    gopath = tmp_path / "gopath"
    (gopath / "bin").mkdir(parents=True)
    goimports.spec["env"] = {"GOBIN": "", "GOPATH": f"{gopath}:{tmp_path}/other"}
    goimports.gobin = gopath / "bin"
    goimports.publish(TOOLS, {"v0.21.0": 10})
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\tv0.21.0\th1:sum=")
    assert goimports.run().returncode == 0
    assert goimports.installs == []


def test_go_runs_with_brew_bundles_gobin_and_gopath(
    tmp_path: Path, goimports: Go
) -> None:
    gobin = tmp_path / "homebrew-gobin"
    gobin.mkdir()
    goimports.env |= {
        "GOBIN": str(goimports.gobin),
        "GOPATH": str(tmp_path / "gopath"),
        "HOMEBREW_GOBIN": str(gobin),
        "HOMEBREW_GOPATH": str(tmp_path / "homebrew-gopath"),
    }
    goimports.publish(TOOLS, {"v0.21.0": 10, "v0.22.0": 1})
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\tv0.21.0\th1:sum=")
    assert goimports.run().returncode == 0
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]
    assert goimports.install_environments == [f"{gobin} {tmp_path}/homebrew-gopath"]


def test_go_ignores_gobin_and_gopath_without_their_homebrew_settings(
    tmp_path: Path, goimports: Go
) -> None:
    goimports.env |= {"GOBIN": str(tmp_path / "elsewhere"), "GOPATH": "/nowhere"}
    goimports.publish(TOOLS, {"v0.21.0": 10, "v0.22.0": 1})
    goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\tv0.22.0\th1:sum=")
    assert goimports.run().returncode == 0
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]
    assert goimports.install_environments == [" "]


@pytest.mark.parametrize(
    "diagnostic", ["not a Go executable", "unrecognized file format"]
)
def test_a_file_that_is_not_a_go_executable_is_replaced(
    goimports: Go, diagnostic: str
) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10})
    goimports.install_other_file("goimports", diagnostic)
    result = goimports.run()
    assert result.returncode == 0, result.stderr
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


def test_an_unreadable_binary_fails(goimports: Go) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10})
    goimports.install_other_file("goimports", "permission denied")
    result = goimports.run()
    assert result.returncode != 0
    assert "goimports: permission denied" in result.stderr
    assert goimports.installs == []


@pytest.mark.parametrize("kind", ["symlink", "directory", "not executable"])
def test_only_an_executable_regular_file_counts_as_installed(
    tmp_path: Path, goimports: Go, kind: str
) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10})
    binary = goimports.gobin / "goimports"
    if kind == "directory":
        binary.mkdir()
    else:
        goimports.install_binary("goimports", GOIMPORTS, f"{TOOLS}\tv0.21.0\th1:sum=")
        if kind == "symlink":
            _ = binary.rename(tmp_path / "target")
            binary.symlink_to(tmp_path / "target")
        else:
            binary.chmod(0o644)
    assert goimports.run().returncode == 0
    assert not [call for call in goimports.calls if call.startswith("version ")]
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


CMD = f"{TOOLS}/cmd"


@pytest.mark.parametrize(
    ("subject", "reading"),
    [
        (
            f"module {CMD}",
            f"https://proxy.golang.org/{CMD}/@v/list: 404 Not Found\n\tserver response",
        ),
        (f"module {CMD}", f"https://proxy.example/{CMD}/@v/list: 410 Gone"),
        (
            f'{CMD}@latest: unrecognized import path "{CMD}"',
            f"https://{CMD}?go-get=1: 404 Not Found",
        ),
    ],
)
def test_the_module_search_passes_a_path_the_proxy_or_host_does_not_find(
    goimports: Go, subject: str, reading: str
) -> None:
    goimports.spec["errors"] = {CMD: f"go: {subject}: reading {reading}"}
    goimports.publish(TOOLS, {"v0.21.0": 10})
    result = goimports.run()
    assert result.returncode == 0, result.stderr
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


def test_a_failed_module_query_stops_the_search(goimports: Go) -> None:
    proxy = "proxy.golang.org"
    error = f'go: module {CMD}: Get "https://{proxy}/{CMD}/@v/list": dial tcp: {proxy}'
    goimports.spec["errors"] = {CMD: error}
    goimports.publish(TOOLS, {"v0.21.0": 10})
    result = goimports.run()
    assert result.returncode != 0
    assert error in result.stderr
    assert f"list -m -f {{{{.Path}}}} {TOOLS}@latest" not in goimports.calls
    assert goimports.installs == []


@pytest.mark.parametrize(
    ("versions", "commit", "latest"),
    [
        (
            {},
            ("v0.0.0-20261126000000-000000000002", 0.5),
            "v0.0.0-20261126000000-000000000002",
        ),
        ({"v1.0.0": 10, "v1.1.0": 0.5}, None, "v1.1.0"),
    ],
)
def test_an_msrl_dev_module_installs_its_latest_version_without_waiting(
    tmp_path: Path,
    versions: dict[str, float],
    commit: tuple[str, float] | None,
    latest: str,
) -> None:
    go = Go(tmp_path, "msrl.dev/trackage/cmd/trackage")
    go.publish("msrl.dev/trackage", versions, commit=commit)
    result = go.run()
    assert result.returncode == 0, result.stderr
    assert go.installs == [f"install msrl.dev/trackage/cmd/trackage@{latest}"]


def test_a_lookalike_of_msrl_dev_waits(tmp_path: Path) -> None:
    go = Go(tmp_path, "msrl.dev.example.com/tool")
    go.publish("msrl.dev.example.com/tool", {"v1.0.0": 10, "v1.1.0": 0.5})
    assert go.run().returncode == 0
    assert go.installs == ["install msrl.dev.example.com/tool@v1.0.0"]


def test_a_package_that_no_module_provides_fails(goimports: Go) -> None:
    result = goimports.run()
    assert result.returncode != 0
    assert (
        f"provision: could not find the module that provides {GOIMPORTS}"
        in result.stderr
    )
    assert goimports.installs == []


def test_a_failing_go_install_fails(goimports: Go) -> None:
    goimports.publish(TOOLS, {"v0.21.0": 10})
    goimports.spec["install_fails"] = True
    assert goimports.run().returncode != 0
    assert goimports.installs == [f"install {GOIMPORTS}@v0.21.0"]


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
