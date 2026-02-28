#!/usr/bin/env python3
#
# Takes a baseline of macOS defaults(1), then continually diffs it.
# This allows you to change settings in System Preferences and find
# the commands to create the same changes.

import datetime
import plistlib
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
    return cast(dict[str, PlistValue], plistlib.loads(plist, fmt=plistlib.FMT_XML))


def _plist_typed(x: PlistValue) -> tuple[str, ...]:
    match x:
        case bool():
            return "-bool", "true" if x else "false"
        case bytes():
            return "-data", x.hex()
        case datetime.datetime():
            return "-date", x.isoformat(sep=" ")
        case float():
            return "-float", str(x)
        case int():
            return "-int", str(x)
        case str():
            return (shlex.quote(x),)
        case list():
            return "-array", " ".join(_plist_str(elt) for elt in x)
        case dict():
            return "-dict", " ".join(
                s for k, v in x.items() for s in (_plist_str(k), _plist_str(v))
            )


def _plist_str(x: PlistValue) -> str:
    return _plist_typed(x)[-1]


def is_boring_domain(domain: str) -> bool:
    return "Cache" in domain


def print_diff(
    domain: str, old: dict[str, PlistValue], new: dict[str, PlistValue]
) -> None:
    for k, v in new.items():
        if k in old and old[k] == v:
            continue

        if is_boring_domain(k):
            continue

        value = " ".join(_plist_typed(v))

        print(f"defaults write {shlex.quote(domain)} {shlex.quote(k)} {value}")

    for k in old:
        if k in new:
            continue

        if is_boring_domain(k):
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
    domains.remove("ContextStoreAgent")
    domains.remove("com.apple.spaces")
    domains.remove("com.apple.systempreferences")  # ironically
    domains.remove("com.apple.xpc.activity2")
    domains.remove("knowledge-agent")

    # https://bugs.python.org/issue41083
    domains.remove("com.apple.security.KCN")

    # Sometimes includes "<date>0000-12-30T00:00:00Z</date>" which plistlib
    # chokes on; also, is unimportant.
    domains.remove("com.apple.stocks.detailintents")
    domains.remove("com.apple.stocks.widget")

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
