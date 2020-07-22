#!/usr/bin/env python3
#
# Takes a baseline of macOS defaults(1), then continually diffs it.
# This allows you to change settings in System Preferences and find
# the commands to create the same changes.

import plistlib
import shlex
import subprocess
import sys
import time
from typing import Any, Dict


def get_defaults(domain: str):
    plist = subprocess.check_output(["defaults", "export", domain, "-"])
    return plistlib.loads(plist, fmt=plistlib.FMT_XML)


def print_diff(domain: str, old, new) -> None:
    for k, v in new.items():
        if k not in old or old[k] != v:
            if type(v) == bool:
                value = "-bool true" if v else "-bool false"
            elif type(v) == int:
                value = f"-int {v}"
            elif type(v) == float:
                value = f"-float {v}"
            else:
                value = shlex.quote(v)
            print(f"defaults write {shlex.quote(domain)} {shlex.quote(k)} {value}")
    for k in old.keys():
        if k not in new:
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

    print("Baselining...", end="", flush=True)
    defaults = {domain: get_defaults(domain) for domain in domains}
    print("ready")

    while True:
        time.sleep(1)
        for domain in domains:
            new = get_defaults(domain)
            print_diff(domain, defaults[domain], new)
            defaults[domain] = new
