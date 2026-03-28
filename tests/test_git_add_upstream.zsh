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

SCRIPT="${0:A:h}/../bin/git-add-upstream"
TMPBASE="$(mktemp -d)"
trap 'rm -rf "$TMPBASE"' EXIT

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

make_repo() {
    local d="$TMPBASE/$1"
    mkdir -p "$d"
    git -C "$d" init -q
    if [[ -n "${2:-}" ]]; then
        git -C "$d" remote add origin "$2"
    fi
    echo "$d"
}

mock_gh() {
    local dir="$1" body="$2" rc="${3:-0}"
    mkdir -p "$dir"
    cat > "$dir/gh" <<SCRIPT
#!/bin/bash
if [[ "\$1" == "repo" && "\$2" == "view" ]]; then
    [[ -n '$body' ]] && printf '%s\n' '$body'
    exit $rc
fi
exit 1
SCRIPT
    chmod +x "$dir/gh"
}

# --- 1. Not a git repo ---
out="$(cd "$TMPBASE" && bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "not a git repo returns 1" 1 "$rc"

# --- 2. gh not on PATH ---
repo="$(make_repo no-gh https://github.com/octocat/Hello-World.git)"
out="$(cd "$repo" && PATH=/usr/bin:/bin bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "no gh returns 0" 0 "$rc"

# --- 3. Non-github.com origin ---
repo="$(make_repo non-gh https://gitlab.com/owner/repo.git)"
mock_gh "$TMPBASE/mock3" ""
out="$(cd "$repo" && PATH="$TMPBASE/mock3:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "non-github origin returns 0" 0 "$rc"
assert_eq "non-github origin has no upstream" "" "$(git -C "$repo" remote get-url upstream 2>/dev/null || true)"

# --- 4. GitHub repo, not a fork ---
repo="$(make_repo not-fork https://github.com/octocat/Hello-World.git)"
mock_gh "$TMPBASE/mock4" ""
out="$(cd "$repo" && PATH="$TMPBASE/mock4:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "not a fork returns 0" 0 "$rc"
assert_eq "not a fork has no upstream" "" "$(git -C "$repo" remote get-url upstream 2>/dev/null || true)"

# --- 5. GitHub fork → upstream added ---
repo="$(make_repo fork https://github.com/myuser/Hello-World.git)"
mock_gh "$TMPBASE/mock5" "https://github.com/octocat/Hello-World.git"
out="$(cd "$repo" && PATH="$TMPBASE/mock5:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "fork returns 0" 0 "$rc"
assert_eq "fork adds upstream" "https://github.com/octocat/Hello-World.git" "$(git -C "$repo" remote get-url upstream)"

# --- 6. Idempotent: upstream already correct ---
out="$(cd "$repo" && PATH="$TMPBASE/mock5:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "idempotent returns 0" 0 "$rc"
assert_eq "idempotent upstream unchanged" "https://github.com/octocat/Hello-World.git" "$(git -C "$repo" remote get-url upstream)"

# --- 7. Upstream exists with wrong URL ---
repo="$(make_repo wrong-upstream https://github.com/myuser/Hello-World.git)"
git -C "$repo" remote add upstream https://github.com/other/Hello-World.git
mock_gh "$TMPBASE/mock7" "https://github.com/octocat/Hello-World.git"
out="$(cd "$repo" && PATH="$TMPBASE/mock7:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "wrong upstream returns 1" 1 "$rc"
assert_eq "wrong upstream prints warning" 1 "$([[ "$out" == *"upstream already set"* ]] && echo 1 || echo 0)"

# --- 8. gh API failure ---
repo="$(make_repo gh-fail https://github.com/myuser/Hello-World.git)"
mock_gh "$TMPBASE/mock8" "error" 1
out="$(cd "$repo" && PATH="$TMPBASE/mock8:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "gh failure returns 1" 1 "$rc"
assert_eq "gh failure prints warning" 1 "$([[ "$out" == *"gh api call failed"* ]] && echo 1 || echo 0)"

# --- 9. Dotted repo name (e.g. github.io sites) ---
repo="$(make_repo dotted https://github.com/myuser/myuser.github.io.git)"
mock_gh "$TMPBASE/mock9dot" "https://github.com/octocat/myuser.github.io.git"
out="$(cd "$repo" && PATH="$TMPBASE/mock9dot:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "dotted repo name returns 0" 0 "$rc"
assert_eq "dotted repo name adds upstream" "https://github.com/octocat/myuser.github.io.git" "$(git -C "$repo" remote get-url upstream)"

# --- 10. SSH origin URL ---
repo="$(make_repo ssh-origin git@github.com:myuser/Hello-World.git)"
mock_gh "$TMPBASE/mock9" "https://github.com/octocat/Hello-World.git"
out="$(cd "$repo" && PATH="$TMPBASE/mock9:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "ssh origin returns 0" 0 "$rc"
assert_eq "ssh origin adds upstream" "https://github.com/octocat/Hello-World.git" "$(git -C "$repo" remote get-url upstream)"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
