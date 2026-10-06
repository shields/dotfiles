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

# provision/modules.sh is only function definitions, so it is sourced into a
# fresh bash here and driven directly. /bin/bash is the shell provision.sh
# runs under: 3.2 on macOS, which is the oldest the script must support.

import subprocess
from dataclasses import dataclass
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[1]
MODULES_SH = REPO / "provision" / "modules.sh"
RESERVED = ("base", "macos", "linux")
FAKE_MODULES = ("base", "macos", "linux", "dev", "cloud", "data", "media")

DRIVER = """
set -euo pipefail
source "$1"
shift
"$@"
"""

RESOLVE = """
set -euo pipefail
source "$1"
status=0
modules_resolve "${@:2}" || status=$?
printf 'status=%s\\nselection=%s\\ndropped=%s\\nremove=%s\\n' \\
    "$status" "${modules_selection-}" "${modules_dropped-}" "${modules_remove-}"
"""


@dataclass(frozen=True)
class Resolution:
    status: int
    selection: str
    dropped: str
    remove: str
    stderr: str


def bash(
    script: str,
    *args: str,
    cwd: Path | None = None,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        ["/bin/bash", "-c", script, "bash", *args],
        cwd=cwd,
        env={"PATH": "/usr/bin:/bin"},
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=30,
        check=False,
    )


def call(function: str, *args: str) -> subprocess.CompletedProcess[str]:
    return bash(DRIVER, str(MODULES_SH), function, *args)


def make_brew_dir(tmp_path: Path, names: tuple[str, ...] = FAKE_MODULES) -> Path:
    brew_dir = tmp_path / "brew"
    brew_dir.mkdir(exist_ok=True)
    for name in names:
        _ = (brew_dir / f"{name}.Brewfile").write_text("")
    # Only *.Brewfile names are modules.
    _ = (brew_dir / "README.md").write_text("")
    return brew_dir


def resolve(
    tmp_path: Path,
    os_name: str,
    *args: str,
    persisted: str | None = None,
    have_brew: bool = False,
    names: tuple[str, ...] = FAKE_MODULES,
    cwd: Path | None = None,
) -> Resolution:
    brew_dir = make_brew_dir(tmp_path, names)
    selection_file = tmp_path / "config" / "brew-modules"
    if persisted is not None:
        selection_file.parent.mkdir(exist_ok=True)
        _ = selection_file.write_text(persisted)
    result = bash(
        RESOLVE,
        str(MODULES_SH),
        os_name,
        str(brew_dir),
        str(selection_file),
        "1" if have_brew else "0",
        *args,
        cwd=cwd,
    )
    assert result.returncode == 0, result.stderr
    fields = dict(line.split("=", 1) for line in result.stdout.splitlines())
    return Resolution(
        status=int(fields["status"]),
        selection=fields["selection"],
        dropped=fields["dropped"],
        remove=fields["remove"],
        stderr=result.stderr,
    )


def test_available_lists_optional_modules_sorted(tmp_path: Path) -> None:
    result = call("modules_available", str(make_brew_dir(tmp_path)))
    assert result.returncode == 0
    assert result.stdout == "cloud data dev media\n"


def test_available_is_empty_without_optional_modules(tmp_path: Path) -> None:
    result = call("modules_available", str(make_brew_dir(tmp_path, RESERVED)))
    assert result.returncode == 0
    assert result.stdout.strip() == ""


def test_available_fails_on_a_missing_directory(tmp_path: Path) -> None:
    result = call("modules_available", str(tmp_path / "missing"))
    assert result.returncode == 1
    assert f"no such directory: {tmp_path / 'missing'}" in result.stderr


def test_available_matches_the_repository() -> None:
    expected = sorted(
        p.name.removesuffix(".Brewfile")
        for p in (REPO / "brew").glob("*.Brewfile")
        if p.name.removesuffix(".Brewfile") not in RESERVED
    )
    result = call("modules_available", str(REPO / "brew"))
    assert result.stdout.split() == expected
    assert "dev" in expected


@pytest.mark.parametrize(
    ("modules", "name", "status"),
    [
        ("cloud dev", "dev", 0),
        ("cloud dev", "cloud", 0),
        ("devtools", "dev", 1),
        ("cloud-dev", "dev", 1),
        ("", "dev", 1),
        ("cloud data dev", "cloud data", 1),
        ("cloud dev", "", 1),
    ],
)
def test_has_matches_whole_names(modules: str, name: str, status: int) -> None:
    assert call("modules_has", modules, name).returncode == status


def test_default_is_every_optional_module_on_macos() -> None:
    result = call("modules_default", "macos", "cloud data dev media")
    assert result.stdout == "cloud data dev media\n"


def test_default_is_dev_on_linux() -> None:
    result = call("modules_default", "linux", "cloud data dev media")
    assert result.stdout == "dev\n"


def test_default_fails_on_linux_without_a_dev_module() -> None:
    result = call("modules_default", "linux", "cloud data")
    assert result.returncode == 1
    assert "unknown modules dev" in result.stderr


def test_default_rejects_other_operating_systems() -> None:
    result = call("modules_default", "plan9", "dev")
    assert result.returncode == 1
    assert "plan9" in result.stderr


def test_minus_lists_what_the_second_list_lacks() -> None:
    result = call("modules_minus", "cloud data dev", "dev media")
    assert result.stdout == "cloud data\n"


def test_minus_of_an_empty_list_is_empty() -> None:
    assert call("modules_minus", "", "dev").stdout.strip() == ""


def test_macos_defaults_to_every_optional_module(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "macos")
    assert resolved.status == 0
    assert resolved.selection == "cloud data dev media"
    assert resolved.dropped == ""
    assert resolved.remove == "0"


def test_linux_defaults_to_dev(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux")
    assert resolved.status == 0
    assert resolved.selection == "dev"


def test_persisted_selection_applies_without_arguments(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "macos", persisted="media cloud\n", have_brew=True)
    assert resolved.status == 0
    assert resolved.selection == "cloud media"
    assert resolved.dropped == ""


def test_persisted_selection_may_be_empty(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "macos", persisted="", have_brew=True)
    assert resolved.status == 0
    assert resolved.selection == ""


def test_persisted_selection_may_lack_a_trailing_newline(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", persisted="dev  data", have_brew=True)
    assert resolved.selection == "data dev"


def test_persisted_none_is_the_empty_selection(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", persisted="none\n", have_brew=True)
    assert resolved.status == 0
    assert resolved.selection == ""


def test_persisted_selection_spanning_lines_is_read_whole(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", persisted="dev\ndata\n")
    assert resolved.selection == "data dev"


@pytest.mark.parametrize("persisted", ["dev bogus\n", "dev base\n", "nonsense"])
def test_invalid_persisted_selection_fails_naming_the_file(
    tmp_path: Path,
    persisted: str,
) -> None:
    resolved = resolve(tmp_path, "linux", persisted=persisted)
    assert resolved.status == 1
    assert str(tmp_path / "config" / "brew-modules") in resolved.stderr


def test_arguments_override_the_persisted_selection(tmp_path: Path) -> None:
    resolved = resolve(
        tmp_path,
        "linux",
        "data",
        "cloud",
        persisted="data cloud dev",
        have_brew=True,
    )
    assert resolved.status == 3
    assert resolved.selection == "cloud data"
    assert resolved.dropped == "dev"


def test_arguments_are_sorted_and_deduplicated(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "media", "dev", "media")
    assert resolved.status == 0
    assert resolved.selection == "dev media"


def test_none_selects_no_optional_modules(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "none")
    assert resolved.status == 0
    assert resolved.selection == ""


def test_none_cannot_be_combined_with_modules(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "dev", "none")
    assert resolved.status == 2
    assert "none cannot be combined" in resolved.stderr
    assert "usage: ./provision.sh" in resolved.stderr


def test_a_module_name_with_a_space_is_not_two_names(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "cloud data")
    assert resolved.status == 1
    assert "unknown modules cloud data;" in resolved.stderr


def test_failed_resolution_leaves_no_stale_results(tmp_path: Path) -> None:
    script = (
        'source "$1"; modules_selection=stale modules_dropped=stale; '
        'modules_resolve linux "$2" "$3" 0 --bogus || true; '
        'printf "[%s][%s]" "$modules_selection" "$modules_dropped"'
    )
    result = bash(
        script, str(MODULES_SH), str(make_brew_dir(tmp_path)), str(tmp_path / "none")
    )
    assert result.stdout == "[][]"


def test_unknown_options_are_usage_errors(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "--force")
    assert resolved.status == 2
    assert "unknown option --force" in resolved.stderr


@pytest.mark.parametrize("args", [("",), ("dev", ""), ("--remove-modules", "")])
def test_empty_module_names_are_usage_errors(
    tmp_path: Path,
    args: tuple[str, ...],
) -> None:
    resolved = resolve(tmp_path, "linux", *args, persisted="dev data", have_brew=True)
    assert resolved.status == 2
    assert "empty module name" in resolved.stderr
    assert "usage: ./provision.sh" in resolved.stderr


@pytest.mark.parametrize("reserved", RESERVED)
def test_reserved_modules_cannot_be_selected(tmp_path: Path, reserved: str) -> None:
    resolved = resolve(tmp_path, "linux", "dev", reserved)
    assert resolved.status == 1
    assert f"reserved modules {reserved}" in resolved.stderr


def test_unknown_modules_are_all_reported(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "nope", "dev", "nada")
    assert resolved.status == 1
    assert "unknown modules nope nada" in resolved.stderr
    assert "available modules: cloud data dev media" in resolved.stderr


def test_a_glob_is_not_expanded_into_module_names(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "linux", "*", cwd=tmp_path)
    assert resolved.status == 1
    assert "unknown modules *;" in resolved.stderr


def test_dropping_a_persisted_module_needs_remove_modules(tmp_path: Path) -> None:
    resolved = resolve(tmp_path, "macos", "dev", persisted="cloud dev", have_brew=True)
    assert resolved.status == 3
    assert resolved.dropped == "cloud"
    assert "drops installed modules: cloud" in resolved.stderr
    assert "--remove-modules" in resolved.stderr


def test_remove_modules_allows_dropping(tmp_path: Path) -> None:
    resolved = resolve(
        tmp_path,
        "macos",
        "--remove-modules",
        "dev",
        persisted="cloud dev",
        have_brew=True,
    )
    assert resolved.status == 0
    assert resolved.selection == "dev"
    assert resolved.dropped == "cloud"
    assert resolved.remove == "1"


def test_remove_modules_may_follow_the_modules(tmp_path: Path) -> None:
    resolved = resolve(
        tmp_path,
        "macos",
        "dev",
        "--remove-modules",
        persisted="cloud dev",
        have_brew=True,
    )
    assert resolved.status == 0
    assert resolved.selection == "dev"


def test_remove_modules_with_none_drops_everything(tmp_path: Path) -> None:
    resolved = resolve(
        tmp_path,
        "macos",
        "--remove-modules",
        "none",
        persisted="cloud dev",
        have_brew=True,
    )
    assert resolved.status == 0
    assert resolved.selection == ""
    assert resolved.dropped == "cloud dev"


def test_remove_modules_alone_changes_nothing(tmp_path: Path) -> None:
    resolved = resolve(
        tmp_path,
        "macos",
        "--remove-modules",
        persisted="cloud dev",
        have_brew=True,
    )
    assert resolved.status == 0
    assert resolved.selection == "cloud dev"
    assert resolved.dropped == ""


def test_adding_modules_needs_no_flag(tmp_path: Path) -> None:
    resolved = resolve(
        tmp_path,
        "macos",
        "cloud",
        "data",
        "dev",
        persisted="dev",
        have_brew=True,
    )
    assert resolved.status == 0
    assert resolved.selection == "cloud data dev"
    assert resolved.dropped == ""


def test_unpersisted_macos_with_homebrew_counts_the_default_as_installed(
    tmp_path: Path,
) -> None:
    resolved = resolve(tmp_path, "macos", "dev", have_brew=True)
    assert resolved.status == 3
    assert resolved.dropped == "cloud data media"


def test_unpersisted_linux_with_homebrew_counts_dev_as_installed(
    tmp_path: Path,
) -> None:
    resolved = resolve(tmp_path, "linux", "cloud", have_brew=True)
    assert resolved.status == 3
    assert resolved.dropped == "dev"


@pytest.mark.parametrize("os_name", ["macos", "linux"])
def test_without_homebrew_nothing_is_installed_to_drop(
    tmp_path: Path,
    os_name: str,
) -> None:
    resolved = resolve(tmp_path, os_name, "media", have_brew=False)
    assert resolved.status == 0
    assert resolved.selection == "media"
    assert resolved.dropped == ""


def test_a_persisted_selection_without_homebrew_has_nothing_installed(
    tmp_path: Path,
) -> None:
    resolved = resolve(
        tmp_path,
        "linux",
        "dev",
        persisted="cloud data dev",
        have_brew=False,
    )
    assert resolved.status == 0
    assert resolved.selection == "dev"
    assert resolved.dropped == ""


def test_resolve_fails_on_a_brew_dir_without_optional_modules(
    tmp_path: Path,
) -> None:
    resolved = resolve(tmp_path, "linux", names=RESERVED)
    assert resolved.status == 1
    assert "unknown modules dev" in resolved.stderr


def test_persist_writes_the_selection_and_creates_directories(
    tmp_path: Path,
) -> None:
    target = tmp_path / "a" / "b" / "brew-modules"
    result = call("modules_persist", str(target), "cloud dev")
    assert result.returncode == 0
    assert target.read_text() == "cloud dev\n"


def test_persist_accepts_a_path_without_directories(tmp_path: Path) -> None:
    result = bash(
        DRIVER, str(MODULES_SH), "modules_persist", "brew-modules", "dev", cwd=tmp_path
    )
    assert result.returncode == 0, result.stderr
    assert (tmp_path / "brew-modules").read_text() == "dev\n"


def test_persist_writes_an_empty_file_for_no_modules(tmp_path: Path) -> None:
    target = tmp_path / "brew-modules"
    _ = target.write_text("dev\n")
    result = call("modules_persist", str(target), "")
    assert result.returncode == 0
    assert target.read_text() == ""


def test_persist_keeps_the_old_selection_when_the_write_fails(
    tmp_path: Path,
) -> None:
    target = tmp_path / "brew-modules"
    _ = target.write_text("cloud dev\n")
    # A zero file-size limit with SIGXFSZ ignored makes every write fail.
    result = bash(
        'source "$1"; trap "" XFSZ; ulimit -f 0; modules_persist "$2" dev',
        str(MODULES_SH),
        str(target),
    )
    assert result.returncode != 0
    assert target.read_text() == "cloud dev\n"
    assert list(tmp_path.iterdir()) == [target]


def test_persisted_selection_round_trips_through_resolve(tmp_path: Path) -> None:
    target = tmp_path / "config" / "brew-modules"
    assert call("modules_persist", str(target), "data dev").returncode == 0
    resolved = resolve(tmp_path, "linux", have_brew=True)
    assert resolved.selection == "data dev"
    assert target.read_text() == "data dev\n"
