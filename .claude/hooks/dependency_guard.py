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
    code_flags: frozenset[str] = frozenset()

    @property
    def value_names(self) -> frozenset[str]:
        return frozenset(flag.rstrip("=") for flag in self.value_flags)


# A package manager's install command with no package names installs what the
# manifest already declares, which the checks above vet. A permissions rule
# cannot express that: `npm install:*` also matches `npm install left-pad`, and
# an allow rule cannot carve `--no-audit` out of an ask rule. So only the listed
# flags (`--name=` prefixes for those that take a value, which may instead be the
# next word) and bare words may appear
# around the subcommand; anything else, a package name included, asks. The `adds`
# subcommands always name a package. Ask rules cover them, but a rule matches from
# the first word, so the hook asks for them behind a flag, as in
# `pnpm --filter web add zod`. `manifest_env` matches the variables that select a
# different manifest or package source, as `--file`, `--gemfile` and `--registry`
# do. `bare` marks a manager whose command with no subcommand installs.
# `code_flags` are the flags that make the manager run the next word as code, so
# that no later word is a subcommand.
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
        "--lockfile-only",
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
            "--bun",
            "--dry-run",
            "--frozen-lockfile",
            "--ignore-scripts",
            "--lockfile-only",
            "--no-progress",
            "--no-summary",
            "--production",
            "-p",
            "--silent",
            "--verbose",
        },
        value_flags=(
            "--cache-dir=",
            "--cpu=",
            "--cwd=",
            "--filter=",
            "--omit=",
            "--os=",
        ),
        adds=frozenset({"add", "a"}),
        manifest_env=_JS_ENV,
        # Only this spelling: bun skips `-p`, `--eval` and `--print` as flags and
        # still runs a subcommand that follows them.
        code_flags=frozenset({"-e"}),
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
            "--package-lock-only",
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

# A quoted pattern, unlike printed text, never reaches a shell through a pipe, so
# a separator inside one, as in `grep "a\|bun add"`, does not end the command.
PATTERN_COMMANDS = frozenset({"grep", "egrep", "fgrep"})

# Commands that pass their standard input on as arguments.
FEEDERS = frozenset({"xargs", "gxargs", "parallel"})

# Shell syntax that can precede the command it governs.
_KEYWORDS = frozenset({"if", "then", "else", "elif", "do", "while", "until", "!", "{"})

# Words that run the command after them unchanged, as an assignment does.
_PREFIXES = frozenset({"time", "env", "command"})
_ASSIGNMENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*=")

# A flag value containing one of these can expand to several words.
_SHELL_META = frozenset("'\"$\\")
_PATTERN_CHARS = frozenset("*?[]{}")

# Words read after a manager's name before the scan gives up and asks, which keeps
# a run of flags linear.
MAX_LOOKAHEAD = 64

# A lone & ends a command, but the & of 2>&1 and &> does not, and neither does
# the | of >|. The delimiter is captured so a following substitution is visible.
_COMMAND_END_RE = re.compile(r"(&&|\|\||[;\n`()]|(?<!>)\||(?<![<>&])&(?![>&]))")
_REDIRECT_RE = re.compile(r"(?:\d*|&)(?:>[>|]?|<)&?")
_COMMENT_RE = re.compile(r"(?:^|\s)#")
_UNQUOTE = str.maketrans("", "", "'\"\\")

type Quote = Literal["'", '"', "$'"] | None

_HEREDOC_RE = re.compile(
    r"<<(-?)[ \t]*(?:'([^'\n]*)'|\"([^\"\n]*)\"|\\?([^\s;&|()<>'\"]+))"
)


class Segment(NamedTuple):
    """The words of one command, up to any comment.

    `nested` marks the words that begin inside a quote, as the words of
    `sh -c '...'` do. `cut` is the index of the word that opens a quote still
    open at the separator that ends the segment, so the rest of the word is out
    of sight, or -1.
    """

    words: list[str]
    nested: list[bool]
    cut: int
    trusted: frozenset[str]


def _scan(raw: str, quote: Quote) -> tuple[Quote, bool]:
    """The quote state after a word, and whether the word opened that quote."""
    opened = False
    index = 0
    while index < len(raw):
        char = raw[index]
        if quote is None:
            if char == "\\":
                index += 1
            elif char in {"'", '"'}:
                quote, opened = char, True
            elif char == "$" and raw.startswith("'", index + 1):
                quote, opened = "$'", True
                index += 1
        elif quote == "'":
            if char == "'":
                quote, opened = None, False
        elif char == "\\":
            index += 1
        elif char == quote[-1]:
            quote, opened = None, False
        index += 1
    return quote, opened


def _segment(
    raw_words: list[str], quote: Quote, trusted: frozenset[str]
) -> tuple[Segment, Quote]:
    words: list[str] = []
    nested: list[bool] = []
    cut = -1
    for raw in raw_words:
        if quote is None and raw.startswith("#"):
            break
        words.append(raw)
        nested.append(quote is not None)
        quote, opened = _scan(raw, quote)
        if opened:
            cut = len(words) - 1
        elif quote is None:
            cut = -1
    return Segment(words, nested, cut, trusted), quote


# A variable expands to one word that cannot be a subcommand when this command
# sets it to a literal without spaces or patterns, or when it is one of these,
# which hold an absolute path or nothing. Only uppercase names count: zsh ties
# lowercase arrays such as `path` to other variables. The shell itself sets the
# excluded names, as `read` sets REPLY, so a literal assignment does not fix
# their value.
_ENV_PATHS = frozenset({"CLAUDE_PROJECT_DIR", "HOME", "OLDPWD", "PWD", "TMPDIR"})
_SHELL_SET = frozenset(
    {
        "COPROC",
        "IFS",
        "MAPFILE",
        "MATCH",
        "MBEGIN",
        "MEND",
        "OPTARG",
        "OPTIND",
        "PIPESTATUS",
        "REPLY",
    }
)
_TRUSTABLE_RE = re.compile(r"[A-Z][A-Z0-9_]*")
_IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_REF_RE = re.compile(r"\$(?:\{([A-Za-z_][A-Za-z0-9_]*)\}|([A-Za-z_][A-Za-z0-9_]*))")
# Builtins that can assign a variable without naming it in the command text, and
# IFS, which splits even a literal without spaces.
_DYNAMIC_RE = re.compile(
    r"(?:^|(?<=[\s;&|({]))(?:declare|eval|local|readonly|source|typeset|\.)(?=\s|$)"
    r"|IFS"
)
_LITERAL = frozenset(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_./:@%+,=~-"
)
# Characters that can precede a word that names a variable to a builtin, as in
# `read S`, `for S in` or `export S=...`.
_WORD_START = frozenset(" \t\n;&|()'\"`{!")


def _literal_value(command: str, index: int) -> set[str] | None:
    """The names an assignment's value refers to, or None if it is not a literal.

    Plain characters, names in `$NAME` or `${NAME}`, and quotes around them make
    a literal. The value ends at a space, a separator or the end of the command.
    A value that can start with `-` is not a literal, so that no trusted name
    expands to a flag, and neither is an array, `S=(...)`, which zsh expands
    to several words.
    """
    names: set[str] = set()
    quote: str | None = None
    leading = True
    while index < len(command):
        char = command[index]
        if char == "$" and quote != "'":
            match = _REF_RE.match(command, index)
            if match is None:
                return None
            names.add(match.group(1) or match.group(2))
            index = match.end()
            continue
        if char in {"'", '"'}:
            if quote is None:
                quote = char
            elif quote == char:
                quote = None
            else:
                return None
        elif char not in _LITERAL:
            if quote is not None or char not in " \t\n;&|)<>":
                return None
            break
        elif leading:
            if char == "-":
                return None
            leading = False
        index += 1
    return names if quote is None else None


def _heredoc_bodies(command: str) -> list[tuple[int, int]]:
    """The spans of heredoc bodies, each with its delimiter line."""
    spans: list[tuple[int, int]] = []
    pending: list[tuple[str, bool]] = []
    quote: Quote = None
    # A `$(` inside double quotes starts unquoted code up to its matching `)`, so
    # a heredoc there has a body. Each entry is the quote to resume and the
    # parentheses still open.
    substitutions: list[tuple[Quote, int]] = []
    arithmetic = 0
    index = 0
    while index < len(command):
        char = command[index]
        if quote is None:
            if char == "\\":
                index += 1
            elif substitutions and char in "()":
                resume, depth = substitutions.pop()
                depth += 1 if char == "(" else -1
                if depth:
                    substitutions.append((resume, depth))
                else:
                    quote = resume
            elif command.startswith("((", index):
                arithmetic += 1
                index += 2
                continue
            elif arithmetic and command.startswith("))", index):
                arithmetic -= 1
                index += 2
                continue
            elif char in {"'", '"'}:
                quote = char
            elif char == "$" and command.startswith("'", index + 1):
                quote = "$'"
                index += 1
            elif char == "#" and (index == 0 or command[index - 1] in " \t\n;&|()"):
                newline = command.find("\n", index)
                index = len(command) if newline < 0 else newline
                continue
            elif command.startswith("<<<", index):
                index += 3
                continue
            elif not arithmetic and (match := _HEREDOC_RE.match(command, index)):
                delimiter = match.group(2) or match.group(3) or match.group(4) or ""
                pending.append((delimiter, match.group(1) == "-"))
                index = match.end()
                continue
            elif char == "\n" and pending:
                index += 1
                for delimiter, dash in pending:
                    start = index
                    while index < len(command):
                        newline = command.find("\n", index)
                        end = len(command) if newline < 0 else newline
                        line = command[index:end]
                        index = end + 1
                        if (line.lstrip("\t") if dash else line) == delimiter:
                            spans.append((start, min(index, len(command))))
                            break
                    else:
                        # Without its delimiter line the text may not be a
                        # heredoc at all, so it stays shell.
                        return spans
                pending = []
                continue
        elif quote == "'":
            if char == "'":
                quote = None
        elif char == "\\":
            index += 1
        elif quote == '"' and char == "$" and command.startswith("(", index + 1):
            substitutions.append((quote, 1))
            quote = None
            index += 1
        elif char == quote[-1]:
            quote = None
        index += 1
    return spans


def _trusted_names(command: str) -> frozenset[str]:
    """The variables that expand to one literal word wherever this command uses them.

    The command must exclude heredoc bodies: they cannot assign the outer
    shell's variables, and code in them would read as shell.
    """
    if _DYNAMIC_RE.search(command.translate(_UNQUOTE)):
        return frozenset()
    deps: dict[str, set[str] | None] = {}
    for match in _IDENT_RE.finditer(command):
        name = match.group()
        start, end = match.span()
        before = command[start - 1] if start else " "
        after = command[end] if end < len(command) else " "
        if before == "$":
            continue
        if before == "{" and command[start - 2 : start - 1] == "$":
            if after != "}":
                deps[name] = None
            continue
        if before in "-\\":
            # A name glued to an option or escaped, as in `printf -vS` or
            # `read \S`, still receives the value.
            for offset, letter in enumerate(name):
                if letter.isupper():
                    deps[name[offset:]] = None
            continue
        if before not in _WORD_START:
            continue
        current = deps.get(name, set())
        if current is None:
            continue
        value = _literal_value(command, end + 1) if after == "=" else None
        deps[name] = None if value is None else current | value
    trusted = {name for name in _ENV_PATHS if name not in deps}
    candidates = {
        name: names
        for name, names in deps.items()
        if names is not None
        and _TRUSTABLE_RE.fullmatch(name)
        and name not in _SHELL_SET
        and not name.startswith(("BASH", "ZSH"))
    }
    waiting: dict[str, set[str]] = {}
    dependents: dict[str, list[str]] = {}
    ready: list[str] = []
    for name, names in candidates.items():
        waiting[name] = names - trusted - {name}
        for dep in waiting[name]:
            dependents.setdefault(dep, []).append(name)
        if not waiting[name]:
            ready.append(name)
    while ready:
        name = ready.pop()
        trusted.add(name)
        for dependent in dependents.get(name, ()):
            waiting[dependent].discard(name)
            if not waiting[dependent] and dependent not in trusted:
                ready.append(dependent)
    return frozenset(trusted)


def _one_field(raw: str, *, nested: bool, trusted: frozenset[str]) -> bool:
    """Whether the shell expands a word to exactly one field.

    At the top level a double-quoted expansion stays one field. Inside a quote,
    as in `sh -c '...'`, the inner shell splits the word again, so only trusted
    names qualify.
    """
    quote: str | None = None
    index = 0
    while index < len(raw):
        char = raw[index]
        if quote == "'":
            if char == "'":
                quote = None
        elif char == "\\" or char in _PATTERN_CHARS:
            return False
        elif char == "$":
            match = _REF_RE.match(raw, index)
            if match is None:
                return False
            name = match.group(1) or match.group(2)
            if name not in trusted and (nested or quote != '"'):
                return False
            index = match.end()
            continue
        elif (char in {"'", '"'}) and not nested:
            quote = None if quote == char else char
        index += 1
    return quote is None


def _may_be_flag(raw: str, trusted: frozenset[str]) -> bool:
    """Whether a one-field word can expand to a flag.

    Trusted names never start with `-`, but they can be empty, so a word that
    starts with them takes its first character from what follows.
    """
    text = raw.translate(_UNQUOTE)
    index = 0
    while match := _REF_RE.match(text, index):
        if (match.group(1) or match.group(2)) not in trusted:
            return True
        index = match.end()
    return text.startswith("-", index)


def _literal_prefix(installer: Installer, raw: str) -> bool:
    """Whether the literal text before a word's first expansion rules out a subcommand.

    However the expansion splits, the first word it yields starts with that
    text, so it is a positional argument unless the text begins a subcommand or
    a flag.
    """
    prefix = raw[: raw.find("$")]
    plain = prefix.translate(_UNQUOTE)
    return (
        bool(plain)
        and _LITERAL.union("'\"").issuperset(prefix)
        and not plain.startswith("-")
        and not any(
            name.startswith(plain) for name in installer.subcommands | installer.adds
        )
    )


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
    installer: Installer, segment: Segment, start: int, *, fed: bool
) -> Verdict | None:
    """How the words after a package manager's name read.

    "ask" if they could add a dependency, "vetted" if they install only what the
    manifest declares, "open" if the command ends before any subcommand, and None
    if the manager runs something else. Before the subcommand only flags, their
    values and redirections may appear. A flag the tables do not list taints the
    install, as does `fed`, input that arrives on standard input.
    """
    words = segment.words
    names = installer.value_names
    taint = fed
    maybe_value = False
    target = False
    for index in range(start, len(words)):
        raw = words[index]
        if index - start >= MAX_LOOKAHEAD or index == segment.cut:
            return "ask"
        if target:
            target = False
            continue
        word = _plain(raw)
        if word in installer.subcommands:
            if taint or _has_unvetted_arg(installer, segment, index + 1):
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
                nested = segment.nested[index]
                if "/" in _REF_RE.sub("", word) and _one_field(
                    raw, nested=nested, trusted=segment.trusted
                ):
                    if _may_be_flag(raw, segment.trusted):
                        taint = taint or not maybe_value
                        maybe_value = False
                        continue
                elif maybe_value or not _literal_prefix(installer, raw):
                    return "ask"
            if not maybe_value:
                return None
            maybe_value = False
            continue
        if word in installer.code_flags and not maybe_value:
            return None
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


def _has_unvetted_arg(installer: Installer, segment: Segment, start: int) -> bool:
    words = segment.words
    skip = False
    for index in range(start, len(words)):
        raw = words[index]
        if skip:
            skip = False
            continue
        redirect = _REDIRECT_RE.match(raw)
        if redirect:
            # A bare operator such as `>` or `2>` takes the next word as its target.
            skip = redirect.end() == len(raw)
            continue
        if index == segment.cut:
            return True
        arg = _plain(raw)
        if arg in installer.flags:
            continue
        if arg.startswith(installer.value_flags) and not _SHELL_META.intersection(raw):
            continue
        if arg in installer.value_names and _is_value(segment, index + 1):
            skip = True
            continue
        return True
    return False


def _is_value(segment: Segment, index: int) -> bool:
    """Whether a word is a plain value for the flag before it."""
    if index >= len(segment.words) or index == segment.cut:
        return False
    raw = segment.words[index]
    return not (
        raw.startswith("-")
        or _REDIRECT_RE.match(raw)
        or _SHELL_META.intersection(raw)
        or _PATTERN_CHARS.intersection(raw)
    )


def _text_program(words: list[str]) -> str | None:
    """The command, if it only prints or searches its arguments."""
    for index, raw in enumerate(words):
        word = raw.translate(_UNQUOTE)
        if word in _KEYWORDS or _ASSIGNMENT_RE.match(word):
            continue
        if word in _PREFIXES:
            # A flag can change what runs, as `env -S` does.
            if words[index + 1 : index + 2] and words[index + 1].startswith("-"):
                return None
            continue
        program = _program(word)
        return program if program in TEXT_COMMANDS else None
    return None


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


def _enter(
    delimiter: str,
    part: str,
    state: tuple[Quote, bool],
    stack: list[tuple[Quote, bool, str]],
) -> tuple[Quote, bool]:
    """The quote state and pattern flag after a delimiter.

    A substitution or subshell starts unquoted and ends where its context
    resumes.
    """
    quote = state[0]
    if quote in {"'", "$'"}:
        return state
    # An odd run of backslashes ends in one that escapes the delimiter.
    if (len(part) - len(part.rstrip("\\"))) % 2:
        return state
    if delimiter == "(" and (quote is None or part.endswith("$")):
        stack.append((*state, "("))
        return None, False
    if (
        delimiter in {")", "`"}
        and quote is None
        and stack
        and stack[-1][2] == delimiter.replace(")", "(")
    ):
        saved = stack.pop()
        return saved[0], saved[1]
    if delimiter == "`":
        stack.append((*state, "`"))
        return None, False
    return state


def _segments_add(command: str) -> bool:
    parts = _COMMAND_END_RE.split(command)
    plain = command.translate(_UNQUOTE)
    bodies = _heredoc_bodies(command)
    outer = command
    for start, end in reversed(bodies):
        outer = outer[:start] + outer[end:]
    trusted = _trusted_names(outer)
    selects: dict[str, bool] = {}
    ticks = 0
    quote: Quote = None
    pattern_open = False
    stack: list[tuple[Quote, bool, str]] = []
    # Each heredoc line is lexed on its own, and the shell's state resumes
    # after the body: quotes in a body are text to the outer shell.
    outside: tuple[Quote, bool, list[tuple[Quote, bool, str]]] | None = None
    line_start = True
    commented = False
    body = 0
    offset = 0
    for index in range(0, len(parts), 2):
        delimiter = parts[index + 1] if index + 1 < len(parts) else ""
        while body < len(bodies) and bodies[body][1] <= offset:
            body += 1
        in_body = body < len(bodies) and bodies[body][0] <= offset
        offset += len(parts[index]) + len(delimiter)
        if in_body and outside is None:
            outside = (quote, pattern_open, stack)
        elif not in_body and outside is not None:
            quote, pattern_open, stack = outside
            outside = None
        if in_body and line_start:
            quote, pattern_open, stack = None, False, []
        line_start = delimiter == "\n"
        in_pattern = pattern_open and quote is not None
        raw_words = parts[index].split()
        if commented:
            segment, _ = _segment(raw_words, None, trusted)
        else:
            segment, quote = _segment(
                raw_words, quote, frozenset() if in_body else trusted
            )
            commented = len(segment.words) < len(raw_words)
        if line_start:
            commented = False
        words = segment.words
        # A part that begins inside a quote continues the command that opened it,
        # so its first word is not a command name.
        text = _text_program(words) if words and not segment.nested[0] else None
        pattern_open = (
            in_pattern or (text in PATTERN_COMMANDS and segment.cut >= 0)
        ) and quote is not None
        quote, pattern_open = _enter(
            delimiter, parts[index], (quote, pattern_open), stack
        )
        # An opening backtick, `$(`, `<(` or `>(` can supply packages or the rest
        # of the command.
        substituted = (delimiter == "`" and ticks % 2 == 0) or (
            delimiter == "(" and bool(words) and words[-1].endswith(("$", "<", ">"))
        )
        if delimiter == "`":
            ticks += 1
        if not words or text is not None or in_pattern:
            continue
        fed = False
        for position, raw in enumerate(words):
            word = _plain(raw)
            if word.startswith("-"):
                continue
            program = _program(word)
            fed = fed or program in FEEDERS
            installer = INSTALLERS.get(program)
            if installer is None:
                continue
            verdict = _classify(installer, segment, position + 1, fed=fed)
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
