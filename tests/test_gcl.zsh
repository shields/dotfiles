#!/bin/zsh
set -euo pipefail

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

PATH="${0:A:h}/../bin:$PATH"
source "${0:A:h}/../.zsh.d/gcl.zsh"

GCL_ROOT="$(mktemp -d)"
export GCL_ROOT
trap 'rm -rf "$GCL_ROOT"' EXIT

pass=0
fail=0

assert_eq() {
    local desc="$1" expected="$2" actual="$3"
    if [[ "$expected" == "$actual" ]]; then
        echo "PASS: $desc"
        (( ++pass ))
    else
        echo "FAIL: $desc (expected=$expected actual=$actual)"
        (( ++fail ))
    fi
}

# --- no args ---
out="$(gcl 2>&1)" && rc=$? || rc=$?
assert_eq "no args returns 1" 1 "$rc"

# --- bad format ---
out="$(gcl foo 2>&1)" && rc=$? || rc=$?
assert_eq "bad format returns 1" 1 "$rc"

# --- nonexistent repo ---
out="$(gcl github.com/shields/does-not-exist 2>&1)" && rc=$? || rc=$?
assert_eq "nonexistent repo returns nonzero" 1 "$([[ $rc -ne 0 ]] && echo 1 || echo 0)"
assert_eq "nonexistent repo creates no dir" 0 "$(ls "$GCL_ROOT" 2>/dev/null | wc -l | tr -d ' ')"

# --- clone with 2-component url (host/repo) ---
out="$(gcl https://go.googlesource.com/net 2>&1)" && rc=$? || rc=$?
assert_eq "2-component url clones successfully" 0 "$rc"
assert_eq "2-component url creates .git dir" 1 "$([[ -d $GCL_ROOT/go.googlesource.com/net/.git ]] && echo 1 || echo 0)"

# --- clone with bare host/owner/repo ---
out="$(gcl github.com/octocat/Hello-World 2>&1)" && rc=$? || rc=$?
assert_eq "bare url clones successfully" 0 "$rc"
assert_eq "bare url creates .git dir" 1 "$([[ -d $GCL_ROOT/github.com/octocat/Hello-World/.git ]] && echo 1 || echo 0)"

# --- already cloned (https:// url) ---
out="$(gcl https://github.com/octocat/Hello-World 2>&1)" && rc=$? || rc=$?
assert_eq "https url returns 0 (already cloned)" 0 "$rc"
assert_eq "https url prints already cloned" 1 "$([[ "$out" == *"already cloned"* ]] && echo 1 || echo 0)"

# --- already cloned (with .git suffix) ---
out="$(gcl https://github.com/octocat/Hello-World.git 2>&1)" && rc=$? || rc=$?
assert_eq "url with .git returns 0 (already cloned)" 0 "$rc"
assert_eq "url with .git prints already cloned" 1 "$([[ "$out" == *"already cloned"* ]] && echo 1 || echo 0)"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
