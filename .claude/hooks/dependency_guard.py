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

# Claude Code hook: keep the agent from changing a project's declared
# dependencies without the user's approval.
#
# Edit and Write: the permissions.ask rules in settings.json match Bash command
# text, so they cannot fire when a manifest is edited directly. PreToolUse
# applies the pending edit in memory, compares the declared dependency set
# before and after, and asks only when that set gains an entry or changes one.
# Manifests where every edit is a dependency change (lockfiles, Brewfile,
# requirements.txt) are covered by plain Edit(...) ask rules instead.
#
# Shell commands: a command's text cannot reliably say what it will change, so
# the hook compares what it did change. PreToolUse snapshots the guarded files
# of the git repository of the working directory, and PostToolUse compares. An
# unapproved change to the declared dependencies is undone and reported to
# Claude, and running the same command again asks first. A change is approved
# when the user was asked about the call and it ran, or when git history before
# the call already held it (checkout, rebase, pull). Background commands are
# judged at Stop, once they have finished. When another command ran at the same
# time, or git is mid-merge, the change cannot be pinned on one command and
# undoing it could lose work, so it is reported but left in place. A call that
# gets no PostToolUse, because its prompt was denied or another hook refused it,
# ends with its PostToolBatch, and a session's open calls end with it.
#
# Lockfiles are not compared: they resolve what the manifests declare. They are
# restored together with a manifest that is undone.
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

import fcntl
import hashlib
import json
import os
import re
import shutil
import signal
import stat
import subprocess
import sys
import time
import tomllib
from pathlib import Path, PurePosixPath
from typing import TYPE_CHECKING, Any, NamedTuple, Self, cast

if TYPE_CHECKING:
    from collections.abc import Callable
    from types import FrameType, TracebackType

type Specs = dict[str, str]

# Refuse to parse anything implausibly large rather than stalling a tool call.
MAX_BYTES = 2_000_000

# Lockfiles are only copied, never parsed, and a monorepo's outgrow MAX_BYTES.
MAX_COPY_BYTES = 64_000_000

# Cap on how many names a prompt lists before eliding.
MAX_LISTED = 5

# Seconds the hook may run before it gives up. settings.json gives it 10, and a
# hook that outlasts that fails open.
WALL_BUDGET = 5.0

# Seconds a finished call stays on record, so that a call that overlapped it can
# still see it, and a background call may stay open before it is dropped.
KEEP_CLOSED = 3600
KEEP_OPEN = 86400

# The version of what state.json holds. A file written under another one is
# discarded, since its specs were extracted by other code.
STATE_VERSION = 1

KEEP_SAVED = 30 * 86400

MAX_SHOWN = 160

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
        if sep and left.strip():
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


# A line manifest such as .npmrc or a Gemfile can hold credentials, and the
# messages quote its lines.
_SECRET_RE = re.compile(
    r"(?<=://)[^/\s@]+@|(?<![\w.-])(?P<key>[\w.-]*(?:_auth|_password|token|secret)[\w.-]*)=\S+",
    re.IGNORECASE,
)


def _name(key: str, *, whole: bool) -> str:
    key = _SECRET_RE.sub(
        lambda match: f"{match['key']}=<redacted>" if match["key"] else "", key
    )
    return key if whole else key.rpartition(":")[2] or key


def _names(keys: list[str], *, whole: bool = False) -> str:
    shown = sorted({_name(key, whole=whole) for key in keys})
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


# What a shell command may not change unasked. Parsed manifests compare their
# declared specs. Line manifests compare their set of non-comment lines, so that
# reordering or reformatting one is not a change; the rest compare their text
# without whitespace or quotes.
LINE_MANIFEST_RE = re.compile(
    r"Brewfile|.+\.Brewfile|requirements[\w.-]*\.txt|constraints\.txt"
    r"|Gemfile|gems\.rb|\.npmrc"
)
TEXT_MANIFEST_RE = re.compile(
    r"Package\.swift|.+\.gemspec|\.pre-commit-config\.yaml|bunfig\.toml"
    r"|\.pnpmfile\.cjs|pnpm-workspace\.yaml"
)
LOCKFILES = frozenset(
    {
        "uv.lock",
        "bun.lock",
        "bun.lockb",
        "Cargo.lock",
        "go.sum",
        "package-lock.json",
        "npm-shrinkwrap.json",
        "pnpm-lock.yaml",
        "yarn.lock",
        "Package.resolved",
        "Gemfile.lock",
        "poetry.lock",
        "composer.lock",
        "Pipfile.lock",
        "flake.lock",
    }
)
PATHSPECS = tuple(
    f":(glob)**/{name}"
    for name in (
        *EXTRACTORS,
        *sorted(LOCKFILES),
        "Brewfile",
        "*.Brewfile",
        "requirements*.txt",
        "constraints.txt",
        "Gemfile",
        "gems.rb",
        ".npmrc",
        "Package.swift",
        "*.gemspec",
        ".pre-commit-config.yaml",
        "bunfig.toml",
        ".pnpmfile.cjs",
        "pnpm-workspace.yaml",
    )
)


def _kind(name: str) -> str | None:
    if name in EXTRACTORS:
        return "parsed"
    if name in LOCKFILES:
        return "lock"
    if LINE_MANIFEST_RE.fullmatch(name):
        return "lines"
    if TEXT_MANIFEST_RE.fullmatch(name):
        return "text"
    return None


# `go mod tidy` rewrites indirect requirements and the toolchain line the way a
# package manager rewrites its lockfile.
_GO_RESOLVED_RE = re.compile(r"^\s*toolchain\b|//\s*indirect\b")


def _line_specs(text: str, *, semicolon: bool) -> Specs:
    comment = re.compile(r"(?:^|\s)[#;].*" if semicolon else r"(?:^|\s)#.*")
    specs: Specs = {}
    for raw in text.splitlines():
        line = " ".join(comment.sub("", raw).replace("'", '"').split())
        if line:
            specs[line] = ""
    return specs


def _shell_specs(name: str, text: str) -> Specs:
    match _kind(name):
        case "parsed" if name == "go.mod":
            lines = text.splitlines()
            return _go_mod_specs(
                "\n".join(line for line in lines if not _GO_RESOLVED_RE.search(line))
            )
        case "parsed":
            return EXTRACTORS[name](text)
        case "lines":
            return _line_specs(text, semicolon=name == ".npmrc")
        case _:
            # Specs are cached by content alone, so an empty file must have none
            # under every kind.
            content = re.sub(r"[\s'\"]+", "", text)
            return {"content": content} if content else {}


class Busy(Exception):  # noqa: N818 - not an error condition in the usual sense
    pass


DEADLINE = [time.monotonic() + WALL_BUDGET]


def _remaining() -> float:
    return max(0.1, DEADLINE[0] - time.monotonic())


# A variable such as GIT_DIR or GIT_INDEX_FILE would point git at another
# repository than the one the hook names with -C.
_GIT_ENV = {
    key: value for key, value in os.environ.items() if not key.startswith("GIT_")
}


def _git(
    root: Path, *args: str, stdin: bytes | None = None, literal: bool = False
) -> bytes:
    result = subprocess.run(
        ["git", "-C", str(root), *(["--literal-pathspecs"] if literal else []), *args],
        input=stdin,
        capture_output=True,
        env=_GIT_ENV,
        check=False,
        timeout=_remaining(),
    )
    if result.returncode != 0:
        error = result.stderr.decode(errors="replace").strip()
        message = f"git {args[0]} failed in {root}: {error}"
        raise RuntimeError(message)
    return result.stdout


def _git_or_none(root: Path, *args: str) -> bytes | None:
    try:
        return _git(root, *args)
    except RuntimeError:
        return None


def _toplevel(directory: str) -> Path | None:
    path = Path(os.path.realpath(directory))
    for candidate in (path, *path.parents):
        if (candidate / ".git").exists():
            return candidate
    return None


def _state_home() -> Path:
    base = os.environ.get("XDG_STATE_HOME") or str(Path.home() / ".local" / "state")
    return Path(base) / "claude-dependency-guard"


def _safe(name: str) -> str:
    return re.sub(r"[^A-Za-z0-9_.-]", "_", name)[:120] or "_"


def _write_atomic(path: Path, data: bytes, mode: int = 0o600) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.{os.getpid()}.tmp")
    # A sandboxed command can plant a link at this name in a repository.
    temporary.unlink(missing_ok=True)
    fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, mode)
    try:
        with os.fdopen(fd, "wb") as handle:
            _ = handle.write(data)
        temporary.chmod(mode)
        _ = temporary.replace(path)
    except BaseException:
        temporary.unlink(missing_ok=True)
        raise


def _short(command: str) -> str:
    flat = " ".join(command.split())
    if len(flat) <= MAX_SHOWN:
        return flat
    return f"{flat[: MAX_SHOWN - 3]}..."


type Snapshot = dict[str, str | None]
type Pair = tuple[str, str]

MISSING = "\0missing"

# A guarded name that links to a file outside the repository. The hook runs
# unsandboxed, so it must not read what a sandboxed command could not.
OUTSIDE = "\0outside"

# A git operation that leaves the working tree half done: undoing a file then
# would drop the other side's changes when the agent resolves it.
_IN_PROGRESS = (
    "MERGE_HEAD",
    "CHERRY_PICK_HEAD",
    "REVERT_HEAD",
    "rebase-merge",
    "rebase-apply",
    "sequencer",
)


_STAGED_RE = re.compile(r"([0-7]{6}) ([0-9a-f]{40,64}) ([0-3])\t(.*)", re.DOTALL)


class Change(NamedTuple):
    rel: str
    pairs: list[Pair]
    created: bool
    parsed: bool


class Scan(NamedTuple):
    files: Snapshot
    index: dict[str, list[str]]
    unmerged: frozenset[str]
    links: frozenset[str]


def _pair_key(rel: str, pair: Pair) -> str:
    return json.dumps([rel, *pair])


def _describe_pairs(rel: str, pairs: list[Pair], before: Specs) -> str:
    added = [key for key, _ in pairs if key not in before]
    changed = [key for key, _ in pairs if key in before]
    whole = _kind(PurePosixPath(rel).name) == "lines"
    parts = []
    if added:
        parts.append(f"adds {_names(added, whole=whole)}")
    if changed:
        parts.append(f"changes {_names(changed, whole=whole)}")
    return f"{rel} {' and '.join(parts)}"


class Root:
    """One repository's guard state, locked for the duration of a `with`."""

    def __init__(self, path: Path) -> None:
        self.path = path
        key = hashlib.sha256(str(path).encode()).hexdigest()[:20]
        self.dir = _state_home() / "roots" / key
        self.state: dict[str, Any] = {}
        self._lock = -1

    def __enter__(self) -> Self:
        self.dir.mkdir(parents=True, exist_ok=True, mode=0o700)
        self._lock = os.open(self.dir / "lock", os.O_RDWR | os.O_CREAT, 0o600)
        while True:
            try:
                fcntl.flock(self._lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
                break
            except BlockingIOError:
                if time.monotonic() > DEADLINE[0] - 1:
                    os.close(self._lock)
                    message = f"{self.dir} is locked by another hook run"
                    raise Busy(message) from None
                time.sleep(0.02)
        state_file = self.dir / "state.json"
        if state_file.exists():
            try:
                self.state = cast("dict[str, Any]", json.loads(state_file.read_bytes()))
            except ValueError as exc:
                message = f"cannot read {state_file}: {exc}"
                raise RuntimeError(message) from exc
        if self.state.get("version") != STATE_VERSION:
            self.state = {"version": STATE_VERSION}
        for key, default in (
            ("calls", {}),
            ("stat", {}),
            ("specs", {}),
            ("taint", {}),
            ("handled", {}),
            ("reverted", {}),
        ):
            _ = self.state.setdefault(key, default)
        self.state["root"] = str(self.path)
        self._finish_journal()
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc: BaseException | None,
        tb: TracebackType | None,
    ) -> None:
        try:
            if exc_type is None:
                self._prune()
                data = json.dumps(self.state, separators=(",", ":")).encode()
                _write_atomic(self.dir / "state.json", data)
        finally:
            os.close(self._lock)

    def _blob(self, sha: str) -> Path:
        return self.dir / "blobs" / sha

    def _hash(self, rel: str) -> str | None:
        """The content hash of a guarded file, None if too large, MISSING or OUTSIDE."""
        path = self.path / rel
        try:
            info = path.stat()
        except FileNotFoundError, NotADirectoryError:
            return MISSING
        if not stat.S_ISREG(info.st_mode):
            return MISSING
        sig = [info.st_size, info.st_mtime_ns, info.st_ctime_ns, info.st_ino]
        cached = cast("dict[str, list[Any]]", self.state["stat"]).get(rel)
        if cached is not None and cached[:4] == sig:
            return cast("str | None", cached[4])
        if not Path(os.path.realpath(path)).is_relative_to(self.path):
            return OUTSIDE
        sha: str | None = None
        if info.st_size <= MAX_COPY_BYTES:
            data = path.read_bytes()
            sha = hashlib.sha256(data).hexdigest()
            if not self._blob(sha).exists():
                _write_atomic(self._blob(sha), data)
            name = PurePosixPath(rel).name
            if _kind(name) != "lock" and sha not in self.state["specs"]:
                specs: Specs | None = None
                if len(data) <= MAX_BYTES:
                    try:
                        specs = _shell_specs(name, data.decode("utf-8"))
                    except UnicodeDecodeError, ValueError, RecursionError:
                        specs = None
                self.state["specs"][sha] = specs
        self.state["stat"][rel] = [*sig, sha]
        return sha

    def scan(self) -> Scan:
        listing = _git(
            self.path,
            "ls-files",
            "-z",
            "-s",
            "-o",
            "--exclude-standard",
            "--",
            *PATHSPECS,
        )
        index: dict[str, list[str]] = {}
        unmerged: set[str] = set()
        rels: set[str] = set()
        for entry in listing.decode("utf-8", "surrogateescape").split("\0"):
            if not entry:
                continue
            staged = _STAGED_RE.match(entry)
            if staged is None:
                # Untracked files are listed by path alone.
                rels.add(entry)
                continue
            mode, blob, stage, rel = staged.groups()
            if stage != "0":
                unmerged.add(rel)
            index[rel] = [mode, blob]
            rels.add(rel)
        files: Snapshot = {}
        links: set[str] = set()
        for rel in sorted(rels):
            if _kind(PurePosixPath(rel).name) is None:
                continue
            sha = self._hash(rel)
            if sha != MISSING:
                files[rel] = sha
                if (self.path / rel).is_symlink():
                    links.add(rel)
        cache = cast("dict[str, list[Any]]", self.state["stat"])
        for rel in set(cache) - rels:
            del cache[rel]
        return Scan(files, index, frozenset(unmerged), frozenset(links))

    def _specs(self, sha: str | None) -> Specs | None:
        if sha is None:
            return None
        return cast("Specs | None", self.state["specs"].get(sha))

    def _changes(self, before: Snapshot, after: Snapshot) -> list[Change]:
        changes: list[Change] = []
        for rel, sha in after.items():
            if _kind(PurePosixPath(rel).name) == "lock":
                continue
            old = before.get(rel, MISSING)
            if old == sha and sha is not None:
                continue
            new_specs = self._specs(sha)
            old_specs: Specs | None = {} if old == MISSING else self._specs(old)
            if new_specs is None or old_specs is None:
                changes.append(
                    Change(rel, [("", str(sha))], created=old == MISSING, parsed=False)
                )
                continue
            pairs = [
                (key, value)
                for key, value in new_specs.items()
                if old_specs.get(key) != value
            ]
            if pairs:
                changes.append(Change(rel, pairs, created=old == MISSING, parsed=True))
        return changes

    def stashes(self) -> list[str]:
        """The stash entries, which `git stash pop` removes from history."""
        return _git(self.path, "stash", "list", "--format=%H").decode().split()

    def _historical(self, rel: str, before: float, stashes: list[str]) -> list[Specs]:
        """The spec sets this path had in commits made before the call."""
        revisions = ["--all", "--reflog", *stashes]
        # Only a FETCH_HEAD that names a commit can go on the command line: git
        # log fails on an empty one.
        if (
            _git_or_none(
                self.path, "rev-parse", "-q", "--verify", "FETCH_HEAD^{commit}"
            )
            is not None
        ):
            revisions.append("FETCH_HEAD")
        log = _git(
            self.path,
            "log",
            *revisions,
            "--no-abbrev",
            "--no-renames",
            # A merge's result, including a stash entry's, shows only in a diff.
            "--diff-merges=first-parent",
            "--format=",
            "--raw",
            f"--before=@{int(before) - 1}",
            "--",
            rel,
            literal=True,
        )
        blobs: set[str] = set()
        for line in log.decode().splitlines():
            if line.startswith(":"):
                fields = line.split("\t", 1)[0].split()
                blobs.update(blob for blob in fields[2:4] if blob.strip("0"))
        if not blobs:
            return []
        out = _git(
            self.path,
            "cat-file",
            "--batch",
            stdin="".join(f"{blob}\n" for blob in sorted(blobs)).encode(),
        )
        name = PurePosixPath(rel).name
        versions: list[Specs] = []
        position = 0
        while position < len(out):
            header_end = out.index(b"\n", position)
            header = out[position:header_end].split()
            position = header_end + 1
            if header[1:2] != [b"blob"]:
                continue
            size = int(header[2])
            content = out[position : position + size]
            position += size + 1
            try:
                versions.append(_shell_specs(name, content.decode("utf-8")))
            except UnicodeDecodeError, ValueError, RecursionError:
                continue
        return versions

    def _in_progress(self) -> str | None:
        git_dir = Path(
            _git(self.path, "rev-parse", "--absolute-git-dir").decode().strip()
        )
        for name in _IN_PROGRESS:
            if (git_dir / name).exists():
                return name
        return None

    def accept(self, current: Scan) -> None:
        """Release the taint of pairs that the tree already holds."""
        taint = cast("dict[str, float]", self.state["taint"])
        for key in list(taint):
            rel, name, value = cast("list[str]", json.loads(key))
            specs = self._specs(current.files.get(rel))
            if specs is not None and specs.get(name) == value:
                del taint[key]

    def register(self, call_id: str, call: dict[str, Any]) -> None:
        self.state["calls"][call_id] = call

    def call(self, call_id: str) -> dict[str, Any] | None:
        return cast("dict[str, Any] | None", self.state["calls"].get(call_id))

    def undone(self, session: str, command: str) -> str | None:
        """What the hook undid the last time this session ran the command."""
        commands = cast("dict[str, str]", self.state["reverted"].get(session, {}))
        return commands.get(command)

    def _forget(self, session: str, command: str) -> None:
        commands = cast("dict[str, str]", self.state["reverted"].get(session, {}))
        _ = commands.pop(command, None)
        if not commands:
            _ = self.state["reverted"].pop(session, None)

    def _overlapping(
        self, call_id: str, call: dict[str, Any], now: float
    ) -> list[dict[str, Any]]:
        start = cast("float", call["pre"])
        return [
            other
            for other_id, other in cast(
                "dict[str, dict[str, Any]]", self.state["calls"]
            ).items()
            if other_id != call_id
            and other["pre"] < now
            and (other["end"] is None or other["end"] > start)
        ]

    def judge(self, call_id: str, call: dict[str, Any], now: float) -> str | None:
        """Check what a finished shell call changed, undoing what it may not."""
        command = cast("str", call["command"])
        before = cast("Snapshot", call["snapshot"])
        current = self.scan()
        changes = self._changes(before, current.files)
        taint = cast("dict[str, float]", self.state["taint"])
        if call["asked"]:
            self._forget(call["session"], command)
            for change in changes:
                for pair in change.pairs:
                    _ = taint.pop(_pair_key(change.rel, pair), None)
            return None
        if not changes:
            return None

        overlapping = self._overlapping(call_id, call, now)
        if any(
            other["tool"] in SHELL_TOOLS and other["asked"] and other["ran"]
            for other in overlapping
        ):
            return None
        # An agent never edits a file while its own shell command runs, so an
        # edit of the same agent that has not run was refused.
        owner = (call["session"], call["agent"])
        edited = {
            other["file"]
            for other in overlapping
            if other["tool"] in EDIT_TOOLS
            and (other["ran"] or (other["session"], other["agent"]) != owner)
        }
        # A manifest that has the content of one the call removed was moved.
        vanished = {
            sha
            for rel, sha in before.items()
            if sha is not None and rel not in current.files
        }
        handled = cast("dict[str, float]", self.state["handled"])
        start = cast("float", call["pre"])

        unexplained: list[Change] = []
        for change in changes:
            if change.rel in edited:
                continue
            if change.created and current.files.get(change.rel) in vanished:
                continue
            pairs = [
                pair
                for pair in change.pairs
                if handled.get(_pair_key(change.rel, pair), 0.0) < start
            ]
            if pairs and change.parsed:
                versions = self._historical(change.rel, start, call.get("stashes", []))
                pairs = [
                    pair
                    for pair in pairs
                    if _pair_key(change.rel, pair) in taint
                    or not any(version.get(pair[0]) == pair[1] for version in versions)
                ]
            if pairs:
                unexplained.append(change._replace(pairs=pairs))
        if not unexplained:
            return None

        for change in unexplained:
            for pair in change.pairs:
                key = _pair_key(change.rel, pair)
                handled[key] = now
                if change.parsed:
                    taint[key] = now

        blocker = self._blocker(unexplained, overlapping, current)
        if blocker is not None:
            return self._report(command, unexplained, before, blocker)
        return self._undo(call, unexplained, before, current)

    def _blocker(
        self, changes: list[Change], overlapping: list[dict[str, Any]], current: Scan
    ) -> str | None:
        if any(other["tool"] in SHELL_TOOLS for other in overlapping):
            return (
                "another command ran at the same time, so it may have made the change"
            )
        operation = self._in_progress()
        if operation is not None:
            return f"git is in the middle of an operation ({operation})"
        for change in changes:
            if not change.parsed:
                return f"the hook could not parse {change.rel}"
            if change.rel in current.unmerged:
                return f"{change.rel} has merge conflicts"
            if change.rel in current.links:
                return f"{change.rel} is a symbolic link"
        return None

    def _report(
        self, command: str, changes: list[Change], before: Snapshot, blocker: str
    ) -> str:
        what = "; ".join(self._what(change, before) for change in changes)
        return (
            f"While `{_short(command)}` ran, declared dependencies changed: {what}. "
            f"The change was left in place because {blocker}. Dependency changes "
            "need the user's approval: undo this change, or ask the user before "
            "keeping it."
        )

    def _what(self, change: Change, before: Snapshot) -> str:
        if not change.parsed:
            return f"{change.rel} changed in a way the hook could not parse"
        old = before.get(change.rel, MISSING)
        old_specs = {} if old == MISSING else (self._specs(old) or {})
        description = _describe_pairs(change.rel, change.pairs, old_specs)
        return f"{description} (new file)" if change.created else description

    def _undo(
        self,
        call: dict[str, Any],
        changes: list[Change],
        before: Snapshot,
        current: Scan,
    ) -> str:
        command = cast("str", call["command"])
        # Each path's content to put back: a blob hash, MISSING to delete a file
        # the command created, or None when the old copy was too large to keep.
        restore: dict[str, str | None] = {}
        for change in changes:
            restore[change.rel] = before.get(change.rel, MISSING)
            directory = PurePosixPath(change.rel).parent
            for rel in set(current.files) | set(before):
                path = PurePosixPath(rel)
                if _kind(path.name) != "lock":
                    continue
                if path.parent != directory and path.parent not in directory.parents:
                    continue
                old = before.get(rel, MISSING)
                if current.files.get(rel, MISSING) != old:
                    restore[rel] = old
        too_large = [rel for rel, sha in restore.items() if sha is None]
        if too_large:
            return self._report(
                command,
                changes,
                before,
                f"{', '.join(too_large)} is too large for the hook to keep a copy",
            )
        linked = sorted(
            {rel for rel, sha in restore.items() if sha == OUTSIDE}
            | {rel for rel in restore if rel in current.links}
        )
        if linked:
            return self._report(
                command, changes, before, f"{', '.join(linked)} is a symbolic link"
            )

        commits = _git(
            self.path,
            "log",
            "--all",
            f"--since=@{int(call['pre'])}",
            "--format=%h",
            "--",
            *(change.rel for change in changes),
            literal=True,
        ).split()

        stamp = time.strftime("%Y%m%dT%H%M%S")
        saved = self.dir / "reverted" / f"{stamp}-{os.getpid()}"
        for rel in restore:
            path = self.path / rel
            if path.exists():
                target = saved / rel
                target.parent.mkdir(parents=True, exist_ok=True)
                _ = shutil.copy2(path, target)
        journal = [[rel, sha] for rel, sha in restore.items()]
        _write_atomic(self.dir / "journal.json", json.dumps(journal).encode())
        self._finish_journal()

        staged: list[str] = []
        stuck: list[str] = []
        for rel in restore:
            old_index = cast("list[str] | None", call["index"].get(rel))
            if current.index.get(rel) == old_index:
                continue
            try:
                if old_index is None:
                    _ = _git(self.path, "update-index", "--force-remove", "--", rel)
                else:
                    mode, blob = old_index
                    _ = _git(
                        self.path,
                        "update-index",
                        "--add",
                        "--cacheinfo",
                        f"{mode},{blob},{rel}",
                    )
            except RuntimeError as exc:
                stuck.append(f"{rel} ({exc})")
                continue
            staged.append(rel)

        what = "; ".join(self._what(change, before) for change in changes)
        deleted = [rel for rel, sha in restore.items() if sha == MISSING]
        restored = [rel for rel, sha in restore.items() if sha != MISSING]
        undone = []
        if restored:
            undone.append(f"restored {', '.join(restored)}")
        if deleted:
            undone.append(f"deleted {', '.join(deleted)}, which the command created")
        if staged:
            undone.append(f"reset the git index for {', '.join(staged)}")
        if stuck:
            undone.append(f"could not reset the git index for {', '.join(stuck)}")
        description = what
        self.state["reverted"].setdefault(call["session"], {})[command] = description
        message = (
            f"`{_short(command)}` changed declared dependencies without the user's "
            f"approval: {what}. The hook undid that: it {'; '.join(undone)}. The "
            f"changed files are saved in {saved}. Packages the command installed, "
            "such as in node_modules, are still there, and any install scripts "
            "have already run."
        )
        if commits:
            message += (
                f" Commit {', '.join(c.decode() for c in commits)} made during the "
                "command also contains the change."
            )
        return (
            f"{message} Dependency changes need the user's approval. To ask for it, "
            "run exactly the same command again: that run asks the user first."
        )

    def _finish_journal(self) -> None:
        journal = self.dir / "journal.json"
        if not journal.exists():
            return
        for rel, sha in cast(
            "list[list[str | None]]", json.loads(journal.read_bytes())
        ):
            path = self.path / cast("str", rel)
            if sha == MISSING:
                path.unlink(missing_ok=True)
                continue
            mode = stat.S_IMODE(path.stat().st_mode) if path.exists() else 0o644
            _write_atomic(path, self._blob(cast("str", sha)).read_bytes(), mode)
        journal.unlink()

    def _prune(self) -> None:
        now = time.time()
        calls = cast("dict[str, dict[str, Any]]", self.state["calls"])
        for call in calls.values():
            if call["end"] is None and call.get("background") is None:
                limit = call.get("timeout", 600.0) + 60.0
                if now - call["pre"] > limit:
                    _close(call, call["pre"] + limit)
        # A closed call stays on record while an older one is open, which still
        # needs it to tell what ran alongside.
        open_since = min(
            (call["pre"] for call in calls.values() if call["end"] is None),
            default=None,
        )
        for call_id, call in list(calls.items()):
            if call["end"] is None:
                if now - call["pre"] > KEEP_OPEN:
                    del calls[call_id]
            elif now - call["end"] > KEEP_CLOSED and (
                open_since is None or call["end"] <= open_since
            ):
                del calls[call_id]
        handled = cast("dict[str, float]", self.state["handled"])
        for key, when in list(handled.items()):
            if now - when > KEEP_OPEN:
                del handled[key]
        taint = cast("dict[str, float]", self.state["taint"])
        for key, when in list(taint.items()):
            if now - when > KEEP_OPEN:
                del taint[key]
        live = {
            sha
            for call in calls.values()
            for sha in cast("Snapshot", call.get("snapshot", {})).values()
            if sha is not None
        }
        live.update(
            entry[4] for entry in self.state["stat"].values() if entry[4] is not None
        )
        specs = cast("dict[str, Any]", self.state["specs"])
        for sha in list(specs):
            if sha not in live:
                del specs[sha]
        saved = self.dir / "reverted"
        if saved.is_dir():
            for directory in saved.iterdir():
                if now - directory.stat().st_mtime > KEEP_SAVED:
                    shutil.rmtree(directory)
        blobs = self.dir / "blobs"
        if blobs.is_dir():
            for blob in blobs.iterdir():
                if blob.name not in live and now - blob.stat().st_mtime > KEEP_CLOSED:
                    blob.unlink(missing_ok=True)


def _close(call: dict[str, Any], when: float) -> None:
    call["end"] = when
    call["snapshot"] = {}
    call["index"] = {}


def _index_file(kind: str, name: str) -> Path:
    return _state_home() / kind / _safe(name)


def _roots(payload: dict[str, Any]) -> list[Path]:
    # The project directory stays at the main checkout once Claude is in a
    # worktree, where the user may be working, so it only stands in for a
    # working directory outside any repository.
    for candidate in (payload.get("cwd"), os.environ.get("CLAUDE_PROJECT_DIR")):
        if isinstance(candidate, str) and candidate:
            root = _toplevel(candidate)
            if root is not None:
                return [root]
    return []


def _remember(payload: dict[str, Any], call_id: str, roots: list[Path]) -> None:
    _write_atomic(
        _index_file("calls", call_id), json.dumps([str(r) for r in roots]).encode()
    )
    sessions = _index_file("sessions", str(payload.get("session_id")))
    known = set(sessions.read_text().splitlines()) if sessions.exists() else set()
    new = [str(root) for root in roots if str(root) not in known]
    if new:
        # Appending, unlike a rewrite, cannot drop a root another hook run adds
        # at the same moment.
        sessions.parent.mkdir(parents=True, exist_ok=True)
        with sessions.open("a") as handle:
            _ = handle.write("".join(f"{root}\n" for root in new))


def _recalled(call_id: str) -> list[Path]:
    path = _index_file("calls", call_id)
    if not path.exists():
        return []
    return [Path(root) for root in cast("list[str]", json.loads(path.read_bytes()))]


def _ask_marker(payload: dict[str, Any]) -> Path:
    tool_input = payload.get("tool_input")
    identity = json.dumps(
        [
            payload.get("session_id"),
            payload.get("agent_id"),
            payload.get("tool_name"),
            tool_input.get("command") if isinstance(tool_input, dict) else None,
        ]
    )
    return _index_file("asks", hashlib.sha256(identity.encode()).hexdigest())


def _call_record(payload: dict[str, Any]) -> dict[str, Any]:
    tool_input = cast("dict[str, Any]", payload.get("tool_input") or {})
    timeout = tool_input.get("timeout") or tool_input.get("timeout_ms")
    return {
        "session": str(payload.get("session_id")),
        "agent": payload.get("agent_id"),
        "tool": payload.get("tool_name"),
        "command": tool_input.get("command"),
        "file": None,
        "pre": time.time(),
        "end": None,
        "asked": False,
        "ran": False,
        "background": None,
        "timeout": float(timeout) / 1000 if isinstance(timeout, int | float) else 600.0,
        "snapshot": {},
        "index": {},
    }


def _ask(reason: str) -> dict[str, Any]:
    return {
        "hookSpecificOutput": {
            "hookEventName": "PreToolUse",
            "permissionDecision": "ask",
            "permissionDecisionReason": reason,
        }
    }


def _pre_shell(payload: dict[str, Any]) -> dict[str, Any] | None:
    call_id = str(payload.get("tool_use_id"))
    record = _call_record(payload)
    if not isinstance(record["command"], str):
        return None
    # A prompt for this call comes after this hook, so a marker left by an
    # earlier identical call, whose prompt the user denied, must not approve it.
    _ask_marker(payload).unlink(missing_ok=True)
    roots = _roots(payload)
    _remember(payload, call_id, roots)
    descriptions: list[str] = []
    for root in roots:
        with Root(root) as guard:
            description = guard.undone(record["session"], record["command"])
            if description is not None:
                descriptions.append(description)
            current = guard.scan()
            guard.accept(current)
            guard.register(
                call_id,
                record
                | {
                    "snapshot": current.files,
                    "index": current.index,
                    "stashes": guard.stashes(),
                },
            )
    reason: str | None = None
    if descriptions:
        reason = (
            f"The last time this command ran, it changed dependencies "
            f"({'; '.join(descriptions)}), and the hook undid that. Approve to let "
            "it change them."
        )
        for root in roots:
            with Root(root) as guard:
                call = guard.call(call_id)
                if call is not None:
                    call["asked"] = True
    return _ask(reason) if reason is not None else None


def _pre_edit(payload: dict[str, Any]) -> dict[str, Any] | None:
    tool_input = cast("dict[str, Any]", payload.get("tool_input") or {})
    reason = _edit_reason(cast("str", payload.get("tool_name")), tool_input)
    file_path = tool_input.get("file_path")
    if isinstance(file_path, str) and _kind(Path(file_path).name) is not None:
        call_id = str(payload.get("tool_use_id"))
        root = _toplevel(str(Path(file_path).parent))
        if root is not None:
            _remember(payload, call_id, [root])
            with Root(root) as guard:
                rel = os.path.relpath(os.path.realpath(file_path), root)
                guard.register(
                    call_id, _call_record(payload) | {"file": rel, "timeout": 300.0}
                )
    return _ask(reason) if reason is not None else None


def _post(payload: dict[str, Any]) -> dict[str, Any] | None:
    event = payload.get("hook_event_name")
    call_id = str(payload.get("tool_use_id"))
    roots = _recalled(call_id)
    response = payload.get("tool_response")
    background = None
    if isinstance(response, dict):
        response = cast("dict[str, Any]", response)
        background = response.get("backgroundTaskId") or response.get("taskId")
    marker = _ask_marker(payload)
    asked = marker.exists()
    now = time.time()
    messages: list[str] = []
    for root in roots:
        if not root.is_dir():
            continue
        with Root(root) as guard:
            call = guard.call(call_id)
            if call is None:
                continue
            if asked:
                call["asked"] = True
            call["ran"] = True
            if call["tool"] in EDIT_TOOLS:
                _close(call, now)
                continue
            if isinstance(background, str) and event == "PostToolUse":
                call["background"] = background
                continue
            message = guard.judge(call_id, call, now)
            _close(call, now)
            if message is not None:
                messages.append(message)
    marker.unlink(missing_ok=True)
    if not (isinstance(background, str) and event == "PostToolUse"):
        _index_file("calls", call_id).unlink(missing_ok=True)
    if not messages:
        return None
    text = "\n\n".join(messages)
    if event == "PostToolUse":
        return {"decision": "block", "reason": text}
    return {"hookSpecificOutput": {"hookEventName": event, "additionalContext": text}}


def _permission_request(payload: dict[str, Any]) -> None:
    _write_atomic(_ask_marker(payload), b"")


def _batch(payload: dict[str, Any]) -> None:
    """End the calls of a resolved batch, including those that never ran.

    A call whose prompt was denied, or that another hook refused, gets no
    PostToolUse, and left open it would count as running beside every later call.
    """
    now = time.time()
    batch = payload.get("tool_calls")
    for item in batch if isinstance(batch, list) else []:
        if not isinstance(item, dict):
            continue
        entry = cast("dict[str, Any]", item)
        call_id = str(entry.get("tool_use_id"))
        background = False
        for root in _recalled(call_id):
            if not root.is_dir():
                continue
            with Root(root) as guard:
                call = guard.call(call_id)
                if call is None:
                    continue
                if call["background"] is not None:
                    background = True
                elif call["end"] is None:
                    _close(call, now)
        if not background:
            _index_file("calls", call_id).unlink(missing_ok=True)
        _ask_marker(
            payload
            | {
                "tool_name": entry.get("tool_name"),
                "tool_input": entry.get("tool_input"),
            }
        ).unlink(missing_ok=True)


def _end(payload: dict[str, Any]) -> None:
    """End every call of a finished session, background ones included."""
    session = str(payload.get("session_id"))
    sessions = _index_file("sessions", session)
    if not sessions.exists():
        return
    now = time.time()
    for root in sessions.read_text().splitlines():
        if not Path(root).is_dir():
            continue
        with Root(Path(root)) as guard:
            calls = cast("dict[str, dict[str, Any]]", guard.state["calls"])
            for call_id, call in calls.items():
                if call["session"] == session and call["end"] is None:
                    _close(call, now)
                    _index_file("calls", call_id).unlink(missing_ok=True)
    sessions.unlink(missing_ok=True)


def _stop(payload: dict[str, Any]) -> dict[str, Any] | None:
    session = str(payload.get("session_id"))
    agent = payload.get("agent_id")
    tasks = payload.get("background_tasks")
    known = isinstance(tasks, list)
    running = {
        cast("dict[str, Any]", task).get("id")
        for task in (tasks if isinstance(tasks, list) else [])
        if isinstance(task, dict)
    }
    sessions = _index_file("sessions", session)
    if not sessions.exists():
        return None
    now = time.time()
    messages: list[str] = []
    for root in sessions.read_text().splitlines():
        if not Path(root).is_dir():
            continue
        with Root(Path(root)) as guard:
            calls = cast("dict[str, dict[str, Any]]", guard.state["calls"])
            for call_id, call in list(calls.items()):
                if call["session"] != session or call["end"] is not None:
                    continue
                if call["background"] is None:
                    if call["agent"] == agent:
                        _close(call, now)
                    continue
                if (
                    payload.get("hook_event_name") != "Stop"
                    or not known
                    or call["background"] in running
                ):
                    continue
                _index_file("calls", call_id).unlink(missing_ok=True)
                message = guard.judge(call_id, call, now)
                _close(call, now)
                if message is not None:
                    messages.append(message)
    if not messages:
        return None
    return {"decision": "block", "reason": "\n\n".join(messages)}


def _handle(payload: dict[str, Any]) -> dict[str, Any] | None:
    event = payload.get("hook_event_name")
    tool = payload.get("tool_name")
    if event == "PreToolUse":
        if tool in SHELL_TOOLS:
            return _pre_shell(payload)
        if tool in EDIT_TOOLS:
            return _pre_edit(payload)
        return None
    if event in ("PostToolUse", "PostToolUseFailure"):
        return _post(payload) if tool in SHELL_TOOLS | EDIT_TOOLS else None
    if event == "PermissionRequest":
        if tool in SHELL_TOOLS:
            _permission_request(payload)
        return None
    if event in ("Stop", "SubagentStop"):
        return _stop(payload)
    if event == "PostToolBatch":
        _batch(payload)
    elif event == "SessionEnd":
        _end(payload)
    return None


def _failure(payload: object, exc: Exception) -> dict[str, Any]:
    name = type(exc).__name__
    event = payload.get("hook_event_name") if isinstance(payload, dict) else None
    detail = f"Dependency guard failed ({name}: {exc})"
    match event:
        case "PreToolUse" | None:
            return _ask(f"{detail}, so asking to be safe.")
        case "PostToolUse":
            return {
                "decision": "block",
                "reason": f"{detail}, so this command's dependency changes were "
                "not checked.",
            }
        case "PostToolUseFailure":
            return {
                "hookSpecificOutput": {
                    "hookEventName": event,
                    "additionalContext": f"{detail}, so this command's dependency "
                    "changes were not checked.",
                }
            }
        case _:
            return {"systemMessage": f"{detail}."}


class OutOfTime(Exception):  # noqa: N818 - not an error condition in the usual sense
    pass


def _out_of_time(_signum: int, _frame: FrameType | None) -> None:
    raise OutOfTime


def main() -> None:
    DEADLINE[0] = time.monotonic() + WALL_BUDGET
    signal.signal(signal.SIGALRM, _out_of_time)
    payload: object = None
    try:
        signal.setitimer(signal.ITIMER_REAL, WALL_BUDGET)
        try:
            payload = json.load(sys.stdin)
            output = (
                _handle(cast("dict[str, Any]", payload))
                if isinstance(payload, dict)
                else None
            )
        finally:
            signal.setitimer(signal.ITIMER_REAL, 0)
    except Exception as exc:  # noqa: BLE001 - a crash here would fail open
        output = _failure(payload, exc)

    if output is None:
        return
    json.dump(output, sys.stdout)
    print()


if __name__ == "__main__":
    main()
