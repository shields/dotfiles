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


def get_defaults(domain: str):
    plist = subprocess.check_output(["defaults", "export", domain, "-"])
    return plistlib.loads(plist, fmt=plistlib.FMT_XML)


def plist_string(x) -> str:
    if isinstance(x, bool):
        return "true" if x else "false"
    if isinstance(x, bytes):
        return x.hex()
    if isinstance(x, datetime.datetime):
        return x.isoformat(sep=" ")
    if isinstance(x, int) or isinstance(x, float):
        return str(x)
    if isinstance(x, str):
        return shlex.quote(x)

    if isinstance(x, list) or isinstance(x, tuple):
        return " ".join(plist_string(elt) for elt in x)
    if isinstance(x, dict):
        return " ".join(plist_string(elt) for elt in zip(x.keys(), x.values()))

    raise TypeError(f"Unknown type {type(x)}")


def print_diff(domain: str, old, new) -> None:
    for k, v in new.items():
        if k in old and old[k] == v:
            continue

        if k == "CloudKitAccountInfoCache":
            continue

        if type(v) == bool:
            value = f"-bool {plist_string(v)}"
        if type(v) == bytes:
            value = f"-data {plist_string(v)}"
        elif type(v) == datetime.datetime:
            value = f"-date {plist_string(v)}"
        elif type(v) == dict:
            value = f"-dict {plist_string(v)}"
        elif type(v) == float:
            value = f"-float {plist_string(v)}"
        elif type(v) == int:
            value = f"-int {plist_string(v)}"
        elif type(v) == list:
            value = f"-array {plist_string(v)}"
        else:
            value = plist_string(v)

        print(f"defaults write {shlex.quote(domain)} {shlex.quote(k)} {value}")

    for k in old.keys():
        if k in new:
            continue

        print(f"defaults delete {shlex.quote(domain)} {shlex.quote(k)}")


if __name__ == "__main__":
    domains = set(
        subprocess.check_output(["defaults", "domains"], encoding="ascii")
        .rstrip("\n")
        .split(", ")
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
