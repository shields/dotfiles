#!/usr/bin/env python3

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

# PreToolUse hook: prompt before a dependency is added to a manifest.
#
# The permissions.ask rules in settings.json match Bash command text, so they
# cannot fire when a manifest is edited directly with Edit or Write. This hook
# closes that gap for the manifests that are *also* edited for unrelated
# reasons: it applies the pending edit in memory, compares the declared
# dependency set before and after, and stays silent unless that set actually
# gains an entry or changes one. Manifests where every edit is a dependency
# change (lockfiles, Brewfile, requirements.txt) need no parsing and are
# covered by plain Edit(...) ask rules instead.
#
# Shell commands get two checks: a write to one of those manifests, and a package
# manager's install, which an ask rule cannot split into a bare install and one
# that names a package (see Installer).
#
# Silence is the "no opinion" pass-through; anything it cannot analyze asks.
#
# permissionDecisionReason is shown to the user, not to the model, so it is
# phrased for a human. The rule against routing around a refusal lives in
# CLAUDE.md: a hook cannot deliver it on the path that needs it, because on a
# denial the tool call short-circuits before hook output reaches the model.
#
# A PreToolUse hook that exits 1 is a non-blocking error, so an incompatible
# interpreter would let edits proceed unguarded. settings.json checks the
# version of Homebrew's absolute python3 paths and exits 2 if none meets the
# project minimum; a PATH lookup could run an older or untrusted interpreter.

import json
import re
import signal
import sys
import tomllib
from pathlib import Path
from typing import TYPE_CHECKING, Any, Literal, NamedTuple, cast

if TYPE_CHECKING:
    from collections.abc import Callable
    from types import FrameType

type Specs = dict[str, str]

# Refuse to parse anything implausibly large rather than stalling a tool call.
MAX_BYTES = 2_000_000

# Cap on how many names a prompt lists before eliding.
MAX_LISTED = 5

# Seconds the hook may run before it gives up and asks. settings.json gives it
# 10, and a hook that outlasts that fails open.
WALL_BUDGET = 5.0

EDIT_TOOLS = frozenset({"Edit", "Write"})

# Monitor runs its command under the Bash permission rules.
SHELL_TOOLS = frozenset({"Bash", "Monitor"})


def _canon(value: object) -> str:
    return json.dumps(value, sort_keys=True, default=str)


_NAME_RE = re.compile(r"^\s*([A-Za-z0-9][A-Za-z0-9._-]*)")


def _pep503(name: str) -> str:
    """Normalize a Python distribution name per PEP 503."""
    return re.sub(r"[-_.]+", "-", name).lower()


def _req_name(requirement: str) -> str:
    """Key a PEP 508 requirement by its distribution name.

    Keying by name rather than by the whole string is what lets a version bump
    register as a *change* to an existing entry instead of an add plus a remove.
    """
    match = _NAME_RE.match(requirement)
    if match is None:
        return requirement.strip().lower()
    return _pep503(match.group(1))


def _table(parent: object, key: str) -> dict[str, Any]:
    """Fetch a sub-table, tolerating a malformed manifest that puts a scalar there."""
    if isinstance(parent, dict):
        value = cast("dict[str, Any]", parent).get(key)
        if isinstance(value, dict):
            return cast("dict[str, Any]", value)
    return {}


def _add_requirements(specs: Specs, prefix: str, items: object) -> None:
    if not isinstance(items, list):
        return
    seen: dict[str, int] = {}
    for item in items:
        if isinstance(item, str):
            # A distribution can legitimately appear more than once in one list
            # (e.g. the same name pinned per environment marker). Keying every
            # occurrence by bare name alone would let a later entry clobber an
            # earlier one, so a version bump to the earlier could go unnoticed.
            # The first occurrence keeps the bare-name key — so a bump to an
            # ordinary single entry still reads as a change — and duplicates get
            # a positional suffix.
            name = _req_name(item)
            count = seen.get(name, 0)
            seen[name] = count + 1
            key = name if count == 0 else f"{name}#{count}"
            specs[f"{prefix}:{key}"] = item.strip()
        else:
            # e.g. a PEP 735 {include-group = "..."} table: no name to key on,
            # so key by content and let any change read as add plus remove.
            specs[f"{prefix}:{_canon(item)}"] = _canon(item)


def _add_mapping(
    specs: Specs,
    prefix: str,
    table: object,
    *,
    pep503: bool = False,
    value_fn: Callable[[object], str] | None = None,
) -> None:
    if not isinstance(table, dict):
        return
    canon = value_fn or _canon
    for name, spec in cast("dict[str, Any]", table).items():
        key = _pep503(str(name)) if pep503 else str(name)
        specs[f"{prefix}:{key}"] = canon(spec)


def _pyproject_specs(text: str) -> Specs:
    data = tomllib.loads(text)
    specs: Specs = {}
    _add_requirements(
        specs, "build-system.requires", _table(data, "build-system").get("requires")
    )

    project = _table(data, "project")
    _add_requirements(specs, "project.dependencies", project.get("dependencies"))
    for extra, deps in _table(project, "optional-dependencies").items():
        _add_requirements(specs, f"project.optional-dependencies.{extra}", deps)

    for group, deps in _table(data, "dependency-groups").items():
        _add_requirements(specs, f"dependency-groups.{group}", deps)

    uv = _table(_table(data, "tool"), "uv")
    for key in (
        "dev-dependencies",
        "constraint-dependencies",
        "override-dependencies",
        "build-constraint-dependencies",
    ):
        _add_requirements(specs, f"tool.uv.{key}", uv.get(key))
    _add_mapping(specs, "tool.uv.sources", uv.get("sources"), pep503=True)
    # A new package index is a supply-chain change even when no name moves.
    for key in ("index", "extra-index-url", "find-links"):
        if key in uv:
            specs[f"tool.uv.{key}"] = _canon(uv[key])

    poetry = _table(_table(data, "tool"), "poetry")
    _add_mapping(
        specs, "tool.poetry.dependencies", poetry.get("dependencies"), pep503=True
    )
    for group, table in _table(poetry, "group").items():
        _add_mapping(
            specs,
            f"tool.poetry.group.{group}.dependencies",
            _table(table, "dependencies"),
            pep503=True,
        )
    return specs


PACKAGE_JSON_MAPS = (
    "dependencies",
    "devDependencies",
    "peerDependencies",
    "optionalDependencies",
    "overrides",
    "resolutions",
    "patchedDependencies",
)

PACKAGE_JSON_LISTS = (
    "bundledDependencies",
    "bundleDependencies",
    "trustedDependencies",
)


def _package_json_specs(text: str) -> Specs:
    data = json.loads(text) if text.strip() else {}
    specs: Specs = {}
    for key in PACKAGE_JSON_MAPS:
        _add_mapping(specs, key, data.get(key) if isinstance(data, dict) else None)
    if not isinstance(data, dict):
        return specs
    for key in PACKAGE_JSON_LISTS:
        value = data.get(key)
        if isinstance(value, list):
            for name in value:
                specs[f"{key}:{name}"] = ""
        elif value is not None:
            specs[key] = _canon(value)
    if "packageManager" in data:
        specs["packageManager"] = _canon(data["packageManager"])
    # Bun/pnpm version catalogs, at the top level or under workspaces.
    for key in ("catalog", "catalogs"):
        if key in data:
            specs[key] = _canon(data[key])
        workspaces = _table(data, "workspaces")
        if key in workspaces:
            specs[f"workspaces.{key}"] = _canon(workspaces[key])
    return specs


CARGO_TABLES = ("dependencies", "dev-dependencies", "build-dependencies")


def _cargo_spec(value: object) -> str:
    # `serde = "1"` and `serde = { version = "1" }` mean the same thing; without
    # this, rewriting one as the other would prompt for no reason.
    if isinstance(value, str):
        return _canon({"version": value})
    return _canon(value)


def _cargo_specs(text: str) -> Specs:
    data = tomllib.loads(text)
    specs: Specs = {}
    for key in CARGO_TABLES:
        _add_mapping(specs, key, data.get(key), value_fn=_cargo_spec)
    for target, table in _table(data, "target").items():
        for key in CARGO_TABLES:
            _add_mapping(
                specs,
                f"target.{target}.{key}",
                _table(table, key),
                value_fn=_cargo_spec,
            )
    _add_mapping(
        specs,
        "workspace.dependencies",
        _table(data, "workspace").get("dependencies"),
        value_fn=_cargo_spec,
    )
    for source, table in _table(data, "patch").items():
        _add_mapping(specs, f"patch.{source}", table, value_fn=_cargo_spec)
    _add_mapping(specs, "replace", data.get("replace"), value_fn=_cargo_spec)
    return specs


# Directives that introduce or redirect code. `go`, `module` and `godebug` are
# deliberately absent: they are edited routinely and pull in nothing.
GO_DIRECTIVES = frozenset({"require", "replace", "exclude", "tool", "toolchain"})


def _go_entry(specs: Specs, directive: str, entry: str) -> None:
    entry = entry.strip()
    if not entry:
        return
    if directive == "toolchain":
        specs["toolchain"] = entry
        return
    if directive == "replace":
        left, sep, right = entry.partition("=>")
        if sep:
            specs[f"replace:{left.split()[0]}"] = right.strip()
            return
    parts = entry.split()
    specs[f"{directive}:{parts[0]}"] = " ".join(parts[1:])


def _go_mod_specs(text: str) -> Specs:
    specs: Specs = {}
    block: str | None = None
    for raw in text.splitlines():
        # Module paths cannot contain "//", so this only ever strips a comment
        # — importantly the `// indirect` marker, which is not a dependency change.
        line = raw.split("//", 1)[0].strip()
        if not line:
            continue
        if block is not None:
            if line.startswith(")"):
                block = None
            else:
                _go_entry(specs, block, line)
            continue
        # split() over partition(" ") so a directive separated from its argument
        # by a tab or run of spaces still parses — hand edits, which this hook
        # exists to catch, don't always use gofmt's single space.
        directive, _, rest = line.replace("\t", " ").partition(" ")
        if directive not in GO_DIRECTIVES:
            continue
        rest = rest.strip()
        if rest.startswith("("):
            block = directive
            _go_entry(specs, directive, rest[1:])
            continue
        _go_entry(specs, directive, rest)
    return specs


EXTRACTORS = {
    "pyproject.toml": _pyproject_specs,
    "package.json": _package_json_specs,
    "Cargo.toml": _cargo_specs,
    "go.mod": _go_mod_specs,
}


def _read(path: Path) -> str | None:
    """Current contents, "" if the file is new, or None if it cannot be analyzed."""
    try:
        if not path.exists():
            return ""
        if path.stat().st_size > MAX_BYTES:
            return None
        return path.read_text(encoding="utf-8")
    # Parenthesis-free multi-except: valid since Python 3.14 (PEP 758), which
    # this file requires. Ruff's formatter keeps this form.
    except OSError, UnicodeDecodeError, ValueError:
        return None


def _apply_edit(before: str, tool_input: dict[str, Any]) -> str | None:
    old = tool_input.get("old_string")
    new = tool_input.get("new_string")
    if not isinstance(old, str) or not isinstance(new, str):
        return None
    if old == "":
        # Empty old_string only makes sense as file creation.
        return new if before == "" else None
    if old not in before:
        return None
    if tool_input.get("replace_all"):
        return before.replace(old, new)
    return before.replace(old, new, 1)


def _names(keys: list[str]) -> str:
    shown = sorted({key.rpartition(":")[2] or key for key in keys})
    if len(shown) > MAX_LISTED:
        return f"{', '.join(shown[:MAX_LISTED])} and {len(shown) - MAX_LISTED} more"
    return ", ".join(shown)


def _describe(filename: str, added: list[str], changed: list[str]) -> str:
    parts = []
    if added:
        parts.append(f"adds {_names(added)}")
    if changed:
        parts.append(f"changes {_names(changed)}")
    return f"This edit to {filename} {' and '.join(parts)}."


def _edit_reason(tool: str, tool_input: dict[str, Any]) -> str | None:
    file_path = tool_input.get("file_path")
    if not isinstance(file_path, str):
        return None
    path = Path(file_path)
    extract = EXTRACTORS.get(path.name)
    if extract is None:
        return None

    before = _read(path)
    if before is None:
        return f"Could not read {path.name} to check its dependencies."

    if tool == "Write":
        after = tool_input.get("content")
        if not isinstance(after, str):
            # A real Write always carries string content; if it somehow does
            # not, fail closed rather than let a manifest write through unseen.
            return f"Could not check whether this changes dependencies in {path.name}."
    else:
        after = _apply_edit(before, tool_input)
        if after is None:
            return f"Could not check whether this changes dependencies in {path.name}."

    if len(after) > MAX_BYTES:
        return f"{path.name} is too large to check for dependency changes."

    try:
        old_specs = extract(before)
        new_specs = extract(after)
    except ValueError:
        # Covers TOMLDecodeError and JSONDecodeError, both ValueError subclasses.
        return f"Could not parse {path.name} to check its dependencies."

    added = [key for key in new_specs if key not in old_specs]
    changed = [
        key
        for key in new_specs
        if key in old_specs and new_specs[key] != old_specs[key]
    ]
    if not added and not changed:
        return None
    return _describe(path.name, added, changed)


# Manifests worth guarding against a raw shell write. Unlike the Edit path this
# cannot parse anything, so any write to one of these asks. Each is matched
# after \S* or _SEG, which already cover a leading run of filename characters,
# so no alternative may begin with a loop over those characters: the overlap
# makes a long token cost quadratic time, and a hook that outlasts its timeout
# fails open.
BASH_GUARDED = (
    r"pyproject\.toml",
    r"package\.json",
    r"Cargo\.toml",
    r"go\.mod",
    r"go\.sum",
    r"Brewfile",
    r"Gemfile",
    r"gems\.rb",
    r"\.gemspec",
    r"\.npmrc",
    r"bunfig\.toml",
    r"\.pnpmfile\.cjs",
    r"pnpm-workspace\.yaml",
    r"Package\.swift",
    r"\.pre-commit-config\.yaml",
    r"requirements[\w.-]*\.txt",
    r"constraints\.txt",
    r"\.lockb?",
    r"package-lock\.json",
    r"npm-shrinkwrap\.json",
    r"pnpm-lock\.yaml",
    r"yarn\.lock",
    r"Package\.resolved",
)

_MANIFEST = f"(?:{'|'.join(BASH_GUARDED)})"
# Stop each pattern at a shell separator so a write later in the line is not
# attributed to a manifest merely read earlier in it. The split is not
# quote-aware, so a ; | or & inside a quoted sed script also ends the segment.
_SEG = r"[^|;&\n]*?"
# The in-place flag of BSD (-i, -I) and GNU sed. It may end a cluster of
# argument-less flags (-Ei, -ni), carry a suffix (-i.bak) or be spelled out; GNU
# accepts any prefix of --in-place (--in-pl, --i) and, unlike BSD, the flag
# after the file operands.
_SED_IN_PLACE = r"(?:-[abEnrsuz]*[iI]|--i[a-z-]*)"

BASH_WRITE_RES = tuple(
    re.compile(pattern)
    for pattern in (
        rf">>?\s*[^\s>]*{_MANIFEST}\b",
        rf"\btee\b{_SEG}\s\S*{_MANIFEST}\b",
        # Two independent lookaheads, flag first, so that the flag may sit on
        # either side of the file operands and each lookahead is one scan. gsed
        # is Homebrew's GNU sed.
        rf"\bg?sed\b(?={_SEG}\s{_SED_IN_PLACE})(?={_SEG}\s\S*{_MANIFEST}\b)",
        rf"\bdd\b{_SEG}\bof=\S*{_MANIFEST}\b",
        rf"\b(?:python3?|perl|ruby|node|bun)\b{_SEG}\s-[ce]\b{_SEG}{_MANIFEST}",
        # The destination of a copy is its last operand, so copying a manifest
        # elsewhere does not match.
        rf"\b(?:cp|mv|rsync|ln)\b{_SEG}\s\S*{_MANIFEST}\b(?=\s*(?:[|;&\n]|$))",
        rf"\b(?:curl|wget)\b{_SEG}\s(?:-[A-Za-z]*[oO]|--output(?:-document)?)[\s=]\s*\S*{_MANIFEST}\b",
        rf"\b(?:patch|truncate)\b{_SEG}\s\S*{_MANIFEST}\b",
        rf"\b(?:npm|pnpm|bun)\b{_SEG}\bpkg\s+(?:set|delete|fix)\b",
    )
)


class Installer(NamedTuple):
    subcommands: frozenset[str]
    flags: frozenset[str]
    value_flags: tuple[str, ...] = ()
    adds: frozenset[str] = frozenset()
    manifest_env: tuple[re.Pattern[str], ...] = ()
    bare: bool = False

    @property
    def value_names(self) -> frozenset[str]:
        return frozenset(flag.rstrip("=") for flag in self.value_flags)


# A package manager's install command with no package names installs what the
# manifest already declares, which the checks above vet. A permissions rule
# cannot express that: `npm install:*` also matches `npm install left-pad`, and
# an allow rule cannot carve `--no-audit` out of an ask rule. So only the listed
# flags (`--name=` prefixes for those that take a value) and bare words may appear
# around the subcommand; anything else, a package name included, asks. The `adds`
# subcommands always name a package. Ask rules cover them, but a rule matches from
# the first word, so the hook asks for them behind a flag, as in
# `pnpm --filter web add zod`. `manifest_env` matches the variables that select a
# different manifest or package source, as `--file`, `--gemfile` and `--registry`
# do. `bare` marks a manager whose command with no subcommand installs.
_HELP = frozenset({"--help", "-h", "--version", "-v"})

# Every `npm_config_` variable is the flag of the same name, and npm reads the
# prefix in any case. `npm_config_cache` only moves the cache.
_JS_ENV = (re.compile(r"(?:npm|pnpm|bun)_config_(?!cache=)", re.IGNORECASE),)

_PNPM = Installer(
    subcommands=frozenset({"install", "i"}),
    flags=_HELP
    | {
        "--dry-run",
        "--frozen-lockfile",
        "--ignore-scripts",
        "--no-progress",
        "--offline",
        "--prefer-frozen-lockfile",
        "--prefer-offline",
        "--prod",
        "-P",
        "--recursive",
        "-r",
    },
    value_flags=("--dir=", "--filter=", "--loglevel=", "--reporter="),
    adds=frozenset({"add", "it", "install-test"}),
    manifest_env=_JS_ENV,
)

_BUNDLE = Installer(
    subcommands=frozenset({"install", "i", "ins", "inst", "insta", "instal"}),
    flags=_HELP | {"--deployment", "--frozen", "--local", "--quiet"},
    value_flags=("--jobs=", "--retry=", "--with=", "--without="),
    adds=frozenset({"add", "ad", "a", "inject", "inj", "inje", "injec"}),
    manifest_env=(
        re.compile(
            r"BUNDLE_(?:(?:GEMFILE|APP_CONFIG|USER_CONFIG)=|(?:MIRROR|LOCAL)__)"
        ),
    ),
    bare=True,
)

INSTALLERS = {
    "bun": Installer(
        subcommands=frozenset({"install", "i"}),
        flags=_HELP
        | {
            "--dry-run",
            "--frozen-lockfile",
            "--ignore-scripts",
            "--no-progress",
            "--no-summary",
            "--production",
            "-p",
            "--silent",
            "--verbose",
        },
        value_flags=("--cwd=", "--filter=", "--omit="),
        adds=frozenset({"add", "a"}),
        manifest_env=_JS_ENV,
    ),
    # npm resolves any unambiguous prefix of "install" and its misspellings.
    "npm": Installer(
        subcommands=frozenset(
            {
                "i",
                "in",
                "ins",
                "inst",
                "insta",
                "instal",
                "install",
                "isnt",
                "isnta",
                "isntal",
                "isntall",
            }
        ),
        flags=_HELP
        | {
            "--dry-run",
            "--foreground-scripts",
            "--ignore-scripts",
            "--no-audit",
            "--no-fund",
            "--no-progress",
            "--offline",
            "--prefer-offline",
            "--production",
            "--silent",
        },
        value_flags=("--include=", "--loglevel=", "--omit=", "--prefix="),
        adds=frozenset(
            {
                "add",
                "it",
                "install-t",
                "install-te",
                "install-tes",
                "install-test",
                "cit",
                "install-ci-test",
            }
        ),
        manifest_env=_JS_ENV,
    ),
    "pnpm": _PNPM,
    "pn": _PNPM,
    "bundle": _BUNDLE,
    "bundler": _BUNDLE,
    # Only a bare `brew bundle` or `brew bundle install` installs from the
    # Brewfile. The other subcommands (add, dump, cleanup, exec) and `--file`
    # and `--global`, which name a different Brewfile, ask.
    "brew": Installer(
        subcommands=frozenset({"bundle"}),
        flags=_HELP
        | {
            "--cask",
            "--force",
            "--formula",
            "--no-upgrade",
            "-q",
            "--quiet",
            "install",
            "--verbose",
        },
        value_flags=("--upgrade-formulae=",),
        manifest_env=(
            re.compile(
                r"HOMEBREW_(?:BUNDLE_FILE|API_DOMAIN|ARTIFACT_DOMAIN|BOTTLE_DOMAIN"
                r"|BREW_GIT_REMOTE|CORE_GIT_REMOTE)="
            ),
        ),
    ),
}

# These only print or search their arguments, so a package manager named in one
# is text. rg is absent because `rg --pre COMMAND` runs COMMAND.
TEXT_COMMANDS = frozenset({"echo", "printf", "grep", "egrep", "fgrep"})

# Commands that pass their standard input on as arguments.
FEEDERS = frozenset({"xargs", "gxargs", "parallel"})

# Shell syntax that can precede the command it governs.
_KEYWORDS = frozenset({"if", "then", "else", "elif", "do", "while", "until", "!", "{"})

# Words that run the command after them unchanged, as an assignment does.
_PREFIXES = frozenset({"time", "env", "command"})
_ASSIGNMENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")

# A flag value containing one of these can expand to several words.
_SHELL_META = frozenset("'\"$\\")

# Words read after a manager's name before the scan gives up and asks, which keeps
# a run of flags linear.
MAX_LOOKAHEAD = 64

# A lone & ends a command, but the & of 2>&1 and &> does not, and neither does
# the | of >|. The delimiter is captured so a following substitution is visible.
_COMMAND_END_RE = re.compile(r"(&&|\|\||[;\n`()]|(?<!>)\||(?<![<>&])&(?![>&]))")
_REDIRECT_RE = re.compile(r"(?:\d*|&)(?:>[>|]?|<)&?")
_COMMENT_RE = re.compile(r"(?:^|\s)#")
_UNQUOTE = str.maketrans("", "", "'\"\\")


def _cut(raw: str) -> bool:
    """Whether a separator inside quotes ended this word early."""
    return raw[0] in "'\"" and raw.count(raw[0]) % 2 == 1


def _plain(raw: str) -> str:
    """A word as the shell hands it on, without quotes or a redirection glued to it."""
    word = raw.translate(_UNQUOTE)
    ends = [index for index in (word.find("<"), word.find(">")) if index >= 0]
    return word[: min(ends)] if ends else word


def _program(word: str) -> str:
    """The command a word names, as the shell would look it up."""
    return word.rpartition("/")[2].lower()


type Verdict = Literal["ask", "vetted", "open"]


def _classify(
    installer: Installer, words: list[str], start: int, *, fed: bool
) -> Verdict | None:
    """How the words after a package manager's name read.

    "ask" if they could add a dependency, "vetted" if they install only what the
    manifest declares, "open" if the command ends before any subcommand, and None
    if the manager runs something else. Before the subcommand only flags, their
    values and redirections may appear. A flag the tables do not list taints the
    install, as does `fed`, input that arrives on standard input.
    """
    names = installer.value_names
    taint = fed
    maybe_value = False
    target = False
    for index in range(start, len(words)):
        raw = words[index]
        if raw.startswith("#"):
            break
        if index - start >= MAX_LOOKAHEAD or _cut(raw):
            return "ask"
        if target:
            target = False
            continue
        word = _plain(raw)
        if word in installer.subcommands:
            if taint or _has_unvetted_arg(installer, words, index + 1):
                return "ask"
            return "vetted"
        if word in installer.adds:
            return "ask"
        redirect = _REDIRECT_RE.match(raw)
        if redirect:
            target = redirect.end() == len(raw)
            continue
        if not word.startswith("-"):
            if "$" in word:
                return "ask"
            if not maybe_value:
                return None
            maybe_value = False
            continue
        if word in installer.flags or (
            word.startswith(installer.value_flags) and not _SHELL_META.intersection(raw)
        ):
            maybe_value = False
            continue
        taint = taint or word not in names
        maybe_value = "=" not in word
    if not installer.bare:
        return "open"
    return "ask" if taint else "vetted"


def _has_unvetted_arg(installer: Installer, words: list[str], start: int) -> bool:
    skip = False
    for index in range(start, len(words)):
        raw = words[index]
        if skip:
            skip = False
            continue
        if raw.startswith("#"):
            break
        redirect = _REDIRECT_RE.match(raw)
        if redirect:
            # A bare operator such as `>` or `2>` takes the next word as its target.
            skip = redirect.end() == len(raw)
            continue
        if _cut(raw):
            return True
        arg = _plain(raw)
        if arg in installer.flags:
            continue
        if arg.startswith(installer.value_flags) and not _SHELL_META.intersection(raw):
            continue
        return True
    return False


def _is_text(words: list[str]) -> bool:
    """Whether the command only prints or searches its arguments."""
    for index, raw in enumerate(words):
        word = raw.translate(_UNQUOTE)
        if word in _KEYWORDS or _ASSIGNMENT_RE.match(word):
            continue
        if word in _PREFIXES:
            # A flag can change what runs, as `env -S` does.
            if words[index + 1 : index + 2] and words[index + 1].startswith("-"):
                return False
            continue
        return _program(word) in TEXT_COMMANDS
    return False


def _fold(command: str, *, comments: bool) -> str:
    """Join each line ending in an odd number of backslashes to the next.

    An even number is an escaped backslash, so the newline still ends the
    command. With `comments` set, a line that may hold a comment is not joined,
    because a comment ends at its newline.
    """
    lines = command.split("\n")
    parts: list[str] = []
    for index, line in enumerate(lines):
        odd = (len(line) - len(line.rstrip("\\"))) % 2 == 1
        joins = odd and index < len(lines) - 1
        if joins and not (comments and _COMMENT_RE.search(line)):
            parts.append(line[:-1])
        else:
            parts.append(line + "\n")
    return "".join(parts)[:-1]


def _segments_add(command: str) -> bool:
    parts = _COMMAND_END_RE.split(command)
    plain = command.translate(_UNQUOTE)
    selects: dict[str, bool] = {}
    ticks = 0
    for index in range(0, len(parts), 2):
        delimiter = parts[index + 1] if index + 1 < len(parts) else ""
        words = parts[index].split()
        # An opening backtick, `$(`, `<(` or `>(` can supply packages or the rest
        # of the command.
        substituted = (delimiter == "`" and ticks % 2 == 0) or (
            delimiter == "(" and bool(words) and words[-1].endswith(("$", "<", ">"))
        )
        if delimiter == "`":
            ticks += 1
        if not words or words[0].startswith("#") or _is_text(words):
            continue
        fed = False
        for position, raw in enumerate(words):
            if raw.startswith("#"):
                break
            word = _plain(raw)
            if word.startswith("-"):
                continue
            program = _program(word)
            fed = fed or program in FEEDERS
            installer = INSTALLERS.get(program)
            if installer is None:
                continue
            verdict = _classify(installer, words, position + 1, fed=fed)
            if verdict is None:
                continue
            if verdict == "ask" or substituted:
                return True
            if verdict == "vetted":
                if program not in selects:
                    selects[program] = any(
                        pattern.search(plain) for pattern in installer.manifest_env
                    )
                if selects[program]:
                    return True
            break
    return False


def _install_adds(command: str) -> bool:
    # Whether a backslash line ending continues the line depends on whether it
    # sits in a comment, which this cannot tell from a quoted #. Either reading
    # is enough to ask.
    readings = {_fold(command, comments=comments) for comments in (False, True)}
    return any(map(_segments_add, readings))


def _bash_reason(command: str) -> str | None:
    if _install_adds(command):
        return "This package install has arguments that could add a dependency."
    if any(pattern.search(command) for pattern in BASH_WRITE_RES):
        return "This shell command appears to write a dependency manifest directly."
    return None


def _reason(payload: dict[str, Any]) -> str | None:
    tool = payload.get("tool_name")
    tool_input = payload.get("tool_input")
    if not isinstance(tool_input, dict):
        return None
    if tool in SHELL_TOOLS:
        command = tool_input.get("command")
        return _bash_reason(command) if isinstance(command, str) else None
    if tool in EDIT_TOOLS:
        return _edit_reason(tool, tool_input)
    return None


class OutOfTime(Exception):  # noqa: N818 - not an error condition in the usual sense
    pass


def _out_of_time(_signum: int, _frame: FrameType | None) -> None:
    raise OutOfTime


def main() -> None:
    signal.signal(signal.SIGALRM, _out_of_time)
    try:
        signal.setitimer(signal.ITIMER_REAL, WALL_BUDGET)
        try:
            payload = json.load(sys.stdin)
            reason = _reason(payload) if isinstance(payload, dict) else None
        finally:
            signal.setitimer(signal.ITIMER_REAL, 0)
    except Exception as exc:  # noqa: BLE001 - a crash here would fail open
        reason = (
            f"Dependency guard failed ({type(exc).__name__}), so asking to be safe."
        )

    if reason is None:
        return

    json.dump(
        {
            "hookSpecificOutput": {
                "hookEventName": "PreToolUse",
                "permissionDecision": "ask",
                "permissionDecisionReason": reason,
            }
        },
        sys.stdout,
    )
    print()


if __name__ == "__main__":
    main()
