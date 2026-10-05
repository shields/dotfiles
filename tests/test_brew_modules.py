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

# The loader's OS check is stubbed inside `brew ruby` so both of its branches run
# on either host. `brew bundle list` covers the real command line, including
# brew's environment filtering, for the host OS only.

import json
import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from itertools import chain, repeat
from pathlib import Path
from typing import cast

import pytest

REPO = Path(__file__).resolve().parents[1]
BREW_DIR = REPO / "brew"
LOADER = REPO / "Brewfile"
RESERVED = ("base", "macos", "linux")
OS_NAMES = ("macos", "linux")
HOST_OS = "macos" if sys.platform == "darwin" else "linux"
DSL_KINDS = frozenset({"tap", "brew", "cask", "mas", "go", "cargo", "npm"})
MODULES = tuple(
    sorted(p.name.removesuffix(".Brewfile") for p in BREW_DIR.glob("*.Brewfile")),
)
OPTIONAL = tuple(m for m in MODULES if m not in RESERVED)
OTHER = next(m for m in OPTIONAL if m != "dev")

LICENSE_LINES = [
    "#",
    '# Licensed under the Apache License, Version 2.0 (the "License");',
    "# you may not use this file except in compliance with the License.",
    "# You may obtain a copy of the License at",
    "#",
    "#     http://www.apache.org/licenses/LICENSE-2.0",
    "#",
    "# Unless required by applicable law or agreed to in writing, software",
    '# distributed under the License is distributed on an "AS IS" BASIS,',
    "# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.",
    "# See the License for the specific language governing permissions and",
    "# limitations under the License.",
]

EVALUATE = """
require "bundle/dsl"
require "json"

results = JSON.parse(ARGV.fetch(0)).map do |job|
  ENV.delete("HOMEBREW_DOTFILES_BREW_MODULES")
  ENV["HOMEBREW_DOTFILES_BREW_MODULES"] = job["modules"] if job["modules"]
  ENV["HOME"] = job["home"]
  mac = job["mac"]
  OS.define_singleton_method(:mac?) { mac }
  begin
    dsl = Homebrew::Bundle::Dsl.new(Pathname.new(job["file"]))
    { "entries" => dsl.entries.map { |e| [e.type.to_s, e.name, e.options] } }
  rescue RuntimeError => e
    { "error" => e.message }
  end
end
puts JSON.generate(results)
"""


@dataclass(frozen=True, slots=True)
class Entry:
    kind: str
    name: str
    options: dict[str, object]

    @property
    def full_name(self) -> str:
        return cast("str", self.options.get("full_name", self.name))

    @property
    def key(self) -> tuple[str, str]:
        return (self.kind, self.full_name)

    @property
    def tap(self) -> str | None:
        parts = self.full_name.split("/")
        if self.kind not in {"brew", "cask"} or len(parts) != 3:
            return None
        return f"{parts[0]}/{parts[1].removeprefix('homebrew-')}".lower()


@dataclass(frozen=True, slots=True)
class Result:
    entries: list[Entry]
    error: str | None


@dataclass(frozen=True, slots=True)
class Run:
    returncode: int
    names: list[str]
    stderr: str


@dataclass(frozen=True, slots=True)
class Scenario:
    modules: str | None = None
    config: str | None = None
    symlink: bool = False


@dataclass(frozen=True, slots=True)
class Case:
    file: Path
    os_name: str
    modules: str | None = None
    config: str | None = None


SCENARIOS = {
    "selected": Scenario(modules=f"dev {OTHER}"),
    "none": Scenario(modules="none"),
    "duplicated": Scenario(modules="dev dev"),
    "default": Scenario(),
    "config": Scenario(config=f"{OTHER}\n"),
    "empty-config": Scenario(config=""),
    "env-over-config": Scenario(modules="dev", config=OTHER),
    "symlink": Scenario(modules="none", symlink=True),
    **{f"reserved-{r}": Scenario(modules=f"dev {r}") for r in RESERVED},
    "unknown": Scenario(modules="dev nosuch"),
    "traversal": Scenario(modules="../brew/dev"),
    "traversal-config": Scenario(config="../brew/dev\n"),
}
CLI_CASES = [
    *(f"module:{m}" for m in MODULES),
    *(f"{s}:{HOST_OS}" for s in ("selected", "config", "unknown", "traversal")),
]


def brew_env() -> dict[str, str]:
    env = {
        k: v
        for k, v in os.environ.items()
        if k
        not in {
            "HOMEBREW_DOTFILES_BREW_MODULES",
            "HOMEBREW_BUNDLE_FILE",
            "HOMEBREW_BUNDLE_FILE_GLOBAL",
        }
    }
    return env | {
        # `brew ruby` is a dev-cmd; without this it writes homebrew.devcmdrun
        # into the host Homebrew's git config and switches on developer mode.
        "HOMEBREW_DEV_CMD_RUN": "1",
        "HOMEBREW_NO_ANALYTICS": "1",
        "HOMEBREW_NO_AUTO_UPDATE": "1",
        "HOMEBREW_NO_ENV_HINTS": "1",
        # Without this, a cold cache makes every brew command download its API index.
        "HOMEBREW_NO_INSTALL_FROM_API": "1",
    }


def brew_cache() -> str:
    return subprocess.run(
        ["brew", "--cache"],
        env=brew_env(),
        check=True,
        capture_output=True,
        text=True,
        timeout=300,
    ).stdout.strip()


def make_home(root: Path, name: str, config: str | None) -> Path:
    home = root / name.replace(":", "-")
    home.mkdir()
    if config is not None:
        (home / ".config/dotfiles").mkdir(parents=True)
        _ = (home / ".config/dotfiles/brew-modules").write_text(config)
    return home


def build_cases(link: Path) -> dict[str, Case]:
    cases = {f"module:{m}": Case(BREW_DIR / f"{m}.Brewfile", HOST_OS) for m in MODULES}
    for os_name in OS_NAMES:
        for name, s in SCENARIOS.items():
            cases[f"{name}:{os_name}"] = Case(
                link if s.symlink else LOADER, os_name, s.modules, s.config
            )
    return cases


def evaluate(cases: dict[str, Case], root: Path) -> dict[str, Result]:
    homes = {n: make_home(root, n, c.config) for n, c in cases.items()}
    jobs = [
        {
            "file": str(c.file),
            "home": str(homes[n]),
            "mac": c.os_name == "macos",
            "modules": c.modules,
        }
        for n, c in cases.items()
    ]
    stdout = subprocess.run(
        ["brew", "ruby", "-e", EVALUATE, "--", json.dumps(jobs)],
        env=brew_env(),
        check=True,
        capture_output=True,
        text=True,
        timeout=300,
    ).stdout
    raw = cast("list[dict[str, list[list[object]] | str]]", json.loads(stdout))
    return {
        name: Result(
            [
                Entry(cast("str", k), cast("str", n), cast("dict[str, object]", o))
                for k, n, o in cast("list[list[object]]", r.get("entries", []))
            ],
            cast("str | None", r.get("error")),
        )
        for name, r in zip(cases, raw, strict=True)
    }


def list_with_brew(case: Case, home: Path, cache: str) -> Run:
    env = brew_env() | {"HOME": str(home), "HOMEBREW_CACHE": cache}
    if case.modules is not None:
        env["HOMEBREW_DOTFILES_BREW_MODULES"] = case.modules
    proc = subprocess.run(
        ["brew", "bundle", "list", "--all", f"--file={case.file}"],
        env=env,
        check=False,
        capture_output=True,
        text=True,
        timeout=300,
    )
    return Run(proc.returncode, proc.stdout.splitlines(), proc.stderr)


@pytest.fixture(scope="module")
def cases(tmp_path_factory: pytest.TempPathFactory) -> dict[str, Case]:
    assert shutil.which("brew"), "brew must be installed to evaluate Brewfiles"
    link = tmp_path_factory.mktemp("link") / "Brewfile"
    link.symlink_to(LOADER)
    return build_cases(link)


@pytest.fixture(scope="module")
def evaluated(
    cases: dict[str, Case], tmp_path_factory: pytest.TempPathFactory
) -> dict[str, Result]:
    return evaluate(cases, tmp_path_factory.mktemp("evaluate"))


@pytest.fixture(scope="module")
def listed(
    cases: dict[str, Case], tmp_path_factory: pytest.TempPathFactory
) -> dict[str, Run]:
    root = tmp_path_factory.mktemp("list")
    names = [n for n in cases if n in CLI_CASES]
    homes = [make_home(root, n, cases[n].config) for n in names]
    with ThreadPoolExecutor() as pool:
        runs = pool.map(
            list_with_brew,
            (cases[n] for n in names),
            homes,
            repeat(brew_cache()),
        )
        return dict(zip(names, runs, strict=True))


def entry_lines(path: Path) -> list[str]:
    return [
        line
        for line in path.read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]


def module_entries(evaluated: dict[str, Result], module: str) -> list[Entry]:
    result = evaluated[f"module:{module}"]
    assert result.error is None
    return result.entries


def loaded(evaluated: dict[str, Result], name: str, os_name: str) -> list[Entry]:
    result = evaluated[f"{name}:{os_name}"]
    assert result.error is None
    return result.entries


def expected(
    evaluated: dict[str, Result], os_name: str, modules: tuple[str, ...]
) -> list[Entry]:
    return list(
        chain.from_iterable(
            module_entries(evaluated, m) for m in ("base", os_name, *modules)
        )
    )


def test_expected_modules_exist() -> None:
    assert set(RESERVED) <= set(MODULES)
    assert "dev" in OPTIONAL
    assert OTHER in OPTIONAL


@pytest.mark.parametrize("module", MODULES)
def test_module_has_license_header(module: str) -> None:
    lines = (BREW_DIR / f"{module}.Brewfile").read_text().splitlines()
    assert re.fullmatch(
        r"# Copyright © (\d{4}(-\d{4}|, \d{4})?) Michael Shields", lines[0]
    )
    assert lines[1 : len(LICENSE_LINES) + 1] == LICENSE_LINES


@pytest.mark.parametrize("module", MODULES)
def test_module_evaluates_on_its_own(evaluated: dict[str, Result], module: str) -> None:
    assert module_entries(evaluated, module)


@pytest.mark.parametrize("module", MODULES)
def test_every_non_comment_line_is_one_dsl_entry(
    evaluated: dict[str, Result], module: str
) -> None:
    lines = entry_lines(BREW_DIR / f"{module}.Brewfile")
    entries = module_entries(evaluated, module)
    assert len(lines) == len(entries)
    for line, entry in zip(lines, entries, strict=True):
        assert entry.kind in DSL_KINDS
        assert re.match(rf'{entry.kind} "[^"]+"(, |\s*(#.*)?$)', line), line


def test_no_entry_appears_in_two_modules(evaluated: dict[str, Result]) -> None:
    seen: dict[tuple[str, str], str] = {}
    for module in MODULES:
        for entry in module_entries(evaluated, module):
            assert entry.key not in seen, (
                f"{entry.key} is in {seen[entry.key]} and {module}"
            )
            seen[entry.key] = module


@pytest.mark.parametrize("module", MODULES)
def test_every_tap_is_declared_in_the_module_that_uses_it(
    evaluated: dict[str, Result], module: str
) -> None:
    entries = module_entries(evaluated, module)
    declared = {e.name for e in entries if e.kind == "tap"}
    used = {t for e in entries if (t := e.tap) and not t.startswith("homebrew/")}
    assert used == declared


def is_trusted(entry: Entry, taps: dict[str, Entry]) -> bool:
    if entry.options.get("trusted") is True:
        return True
    tap = taps.get(entry.tap or "")
    trusted = tap.options.get("trusted") if tap else None
    if trusted is True:
        return True
    if not isinstance(trusted, dict):
        return False
    items = cast("dict[str, list[str]]", trusted)
    keys = ("formula", "formulae") if entry.kind == "brew" else ("cask", "casks")
    item = entry.full_name.rsplit("/", 1)[1]
    return any(item in items.get(k, []) for k in keys)


@pytest.mark.parametrize("module", MODULES)
def test_every_third_party_tap_item_is_trusted(
    evaluated: dict[str, Result], module: str
) -> None:
    entries = module_entries(evaluated, module)
    taps = {e.name: e for e in entries if e.kind == "tap"}
    for e in entries:
        if e.tap and not e.tap.startswith("homebrew/"):
            assert is_trusted(e, taps), e.full_name


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_selects_base_os_and_requested_modules(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "selected", os_name) == expected(
        evaluated, os_name, ("dev", OTHER)
    )


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_none_selects_base_and_os_only(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "none", os_name) == expected(evaluated, os_name, ())


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_loads_a_repeated_module_once(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "duplicated", os_name) == expected(
        evaluated, os_name, ("dev",)
    )


def test_loader_default_is_every_optional_module_on_macos(
    evaluated: dict[str, Result],
) -> None:
    assert loaded(evaluated, "default", "macos") == expected(
        evaluated, "macos", OPTIONAL
    )


def test_loader_default_is_dev_on_linux(evaluated: dict[str, Result]) -> None:
    assert loaded(evaluated, "default", "linux") == expected(
        evaluated, "linux", ("dev",)
    )


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_reads_the_selection_file(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "config", os_name) == expected(
        evaluated, os_name, (OTHER,)
    )


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_empty_selection_file_means_no_optional_modules(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "empty-config", os_name) == expected(
        evaluated, os_name, ()
    )


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_environment_beats_selection_file(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "env-over-config", os_name) == expected(
        evaluated, os_name, ("dev",)
    )


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_finds_modules_through_a_symlink(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "symlink", os_name) == expected(evaluated, os_name, ())


@pytest.mark.parametrize("os_name", OS_NAMES)
@pytest.mark.parametrize("reserved", RESERVED)
def test_loader_rejects_reserved_module_names(
    evaluated: dict[str, Result], os_name: str, reserved: str
) -> None:
    error = evaluated[f"reserved-{reserved}:{os_name}"].error
    assert error is not None
    assert reserved in error
    assert "cannot be selected" in error


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_loader_rejects_unknown_module_names(
    evaluated: dict[str, Result], os_name: str
) -> None:
    error = evaluated[f"unknown:{os_name}"].error
    assert error is not None
    assert str(BREW_DIR / "nosuch.Brewfile") in error


@pytest.mark.parametrize("os_name", OS_NAMES)
@pytest.mark.parametrize("scenario", ["traversal", "traversal-config"])
def test_loader_rejects_names_that_are_not_files_in_the_module_directory(
    evaluated: dict[str, Result], os_name: str, scenario: str
) -> None:
    error = evaluated[f"{scenario}:{os_name}"].error
    assert error is not None
    assert "No such Brewfile module" in error


@pytest.mark.parametrize("os_name", OS_NAMES)
def test_root_brewfile_contributes_no_entries_of_its_own(
    evaluated: dict[str, Result], os_name: str
) -> None:
    assert loaded(evaluated, "none", os_name) == [
        *module_entries(evaluated, "base"),
        *module_entries(evaluated, os_name),
    ]


@pytest.mark.parametrize("case", CLI_CASES)
def test_brew_bundle_list_agrees_with_the_parser(
    evaluated: dict[str, Result], listed: dict[str, Run], case: str
) -> None:
    result = evaluated[case]
    run = listed[case]
    if result.error is None:
        assert run.returncode == 0, run.stderr
        assert run.names == [e.name for e in result.entries]
    else:
        assert run.returncode != 0
        assert result.error in run.stderr
