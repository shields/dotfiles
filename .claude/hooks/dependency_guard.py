#!/opt/homebrew/bin/python3

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
# Silence is the "no opinion" pass-through; anything it cannot analyze asks.
#
# permissionDecisionReason is shown to the user, not to the model, so it is
# phrased for a human. The rule against routing around a refusal lives in
# CLAUDE.md: a hook cannot deliver it on the path that needs it, because on a
# denial the tool call short-circuits before hook output reaches the model.
#
# The interpreter is pinned rather than /usr/bin/env python3 on purpose: macOS
# ships 3.9 at /usr/bin/python3, which has no tomllib. An ImportError here would
# exit 1, and a PreToolUse hook that exits 1 is a *non-blocking* error — the
# edit would proceed unguarded. Pinning keeps the failure mode closed.

import json
import re
import sys
import tomllib
from pathlib import Path
from typing import TYPE_CHECKING, Any, cast

if TYPE_CHECKING:
    from collections.abc import Callable

type Specs = dict[str, str]

# Refuse to parse anything implausibly large rather than stalling a tool call.
MAX_BYTES = 2_000_000

# Cap on how many names a prompt lists before eliding.
MAX_LISTED = 5

EDIT_TOOLS = frozenset({"Edit", "Write"})


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
    # Parenthesis-free multi-except: valid since Python 3.14 (PEP 758). The
    # interpreter is pinned to 3.14 above, and ruff's formatter keeps this form.
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
# cannot parse anything, so any write to one of these asks.
BASH_GUARDED = (
    r"pyproject\.toml",
    r"package\.json",
    r"Cargo\.toml",
    r"go\.mod",
    r"go\.sum",
    r"Brewfile",
    r"Package\.swift",
    r"\.pre-commit-config\.yaml",
    r"requirements[\w.-]*\.txt",
    r"constraints\.txt",
    r"[\w.-]*\.lockb?",
    r"package-lock\.json",
    r"npm-shrinkwrap\.json",
    r"pnpm-lock\.yaml",
    r"yarn\.lock",
    r"Package\.resolved",
)

_MANIFEST = f"(?:{'|'.join(BASH_GUARDED)})"
# Stop each pattern at a shell separator so a write later in the line is not
# attributed to a manifest merely read earlier in it.
_SEG = r"[^|;&\n]*?"

BASH_WRITE_RES = tuple(
    re.compile(pattern)
    for pattern in (
        rf">>?\s*\S*{_MANIFEST}\b",
        rf"\btee\b{_SEG}\s\S*{_MANIFEST}\b",
        rf"\bsed\b{_SEG}\s-i{_SEG}\s\S*{_MANIFEST}\b",
        rf"\bdd\b{_SEG}\bof=\S*{_MANIFEST}\b",
        rf"\b(?:python3?|perl|ruby|node|bun)\b{_SEG}\s-[ce]\b{_SEG}{_MANIFEST}",
    )
)


def _bash_reason(command: str) -> str | None:
    if any(pattern.search(command) for pattern in BASH_WRITE_RES):
        return "This shell command appears to write a dependency manifest directly."
    return None


def _reason(payload: dict[str, Any]) -> str | None:
    tool = payload.get("tool_name")
    tool_input = payload.get("tool_input")
    if not isinstance(tool_input, dict):
        return None
    if tool == "Bash":
        command = tool_input.get("command")
        return _bash_reason(command) if isinstance(command, str) else None
    if tool in EDIT_TOOLS:
        return _edit_reason(tool, tool_input)
    return None


def main() -> None:
    try:
        payload = json.load(sys.stdin)
        reason = _reason(payload) if isinstance(payload, dict) else None
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
