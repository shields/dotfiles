#!/usr/bin/env python3

# Copyright © 2020, 2022-2026 Michael Shields
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

# Takes a baseline of macOS defaults(1), then continually diffs it.
# This allows you to change settings in System Preferences and find
# the commands to create the same changes.

import datetime
import plistlib
import re
import shlex
import subprocess
import time
from typing import cast

type PlistValue = (
    bool
    | int
    | float
    | str
    | bytes
    | datetime.datetime
    | list[PlistValue]
    | dict[str, PlistValue]
)


def get_defaults(domain: str) -> dict[str, PlistValue]:
    plist = subprocess.check_output(["defaults", "export", domain, "-"])
    # Some domains store sentinel <date>0000-12-30T00:00:00Z</date> values
    # that plistlib rejects (year must be >= 1). Remap year 0 to year 1.
    plist = re.sub(rb"<date>0000-", rb"<date>0001-", plist)
    return cast("dict[str, PlistValue]", plistlib.loads(plist, fmt=plistlib.FMT_XML))


def _plist_typed(x: PlistValue) -> tuple[str, ...]:
    match x:
        case bool():
            return "-bool", "true" if x else "false"
        case bytes():
            return "-data", x.hex()
        case datetime.datetime():
            # The ISO date contains a space, so quote it as a single shell
            # argument; print_diff emits these values without re-quoting.
            return "-date", shlex.quote(x.isoformat(sep=" "))
        case float():
            return "-float", str(x)
        case int():
            return "-int", str(x)
        case str():
            # Pass -string explicitly: without a type flag `defaults write`
            # infers the type from the value, so a string that looks like a
            # plist ("(1, 2)", "{a=1;}") or a flag ("-int") would be mis-typed.
            return "-string", shlex.quote(x)
        case list() | dict():
            # `-array`/`-dict` can only express a flat sequence of bare,
            # untyped tokens — there is no syntax for typed or nested elements
            # — so collections that nest (most Dock/Finder/Safari prefs) would
            # be silently corrupted. Emit the whole value as a single
            # property-list argument, which round-trips any depth and type.
            return (shlex.quote(plistlib.dumps(x, fmt=plistlib.FMT_XML).decode()),)


def is_boring_domain(domain: str) -> bool:
    return "Cache" in domain


def print_diff(
    domain: str,
    old: dict[str, PlistValue],
    new: dict[str, PlistValue],
) -> None:
    for k, v in new.items():
        if k in old and old[k] == v:
            continue

        value = " ".join(_plist_typed(v))

        print(f"defaults write {shlex.quote(domain)} {shlex.quote(k)} {value}")

    for k in old:
        if k in new:
            continue

        print(f"defaults delete {shlex.quote(domain)} {shlex.quote(k)}")


if __name__ == "__main__":
    domains = set(
        subprocess.check_output(["defaults", "domains"], encoding="ascii")
        .rstrip("\n")
        .split(", "),
    )
    domains.add("NSGlobalDomain")

    # Remove some domains that are frequently-updated state, not preferences.
    # Use discard, not remove: these vary across macOS versions, and a domain
    # being absent just means there is nothing to exclude.
    domains.discard("ContextStoreAgent")
    domains.discard("com.apple.spaces")
    domains.discard("com.apple.systempreferences")  # ironically
    domains.discard("com.apple.xpc.activity2")
    domains.discard("knowledge-agent")

    # https://bugs.python.org/issue41083
    domains.discard("com.apple.security.KCN")

    # Cache domains are high-churn noise, not preferences; drop them so we
    # neither baseline nor repeatedly re-export them in the diff loop.
    domains -= {domain for domain in domains if is_boring_domain(domain)}

    print("Baselining...", end="", flush=True)
    defaults = {domain: get_defaults(domain) for domain in domains}
    print("ready")

    while True:
        time.sleep(1)
        for domain in domains:
            print("Diffing...", end="", flush=True)
            new = get_defaults(domain)
            print("\r          \r", end="", flush=True)
            print_diff(domain, defaults[domain], new)
            defaults[domain] = new
