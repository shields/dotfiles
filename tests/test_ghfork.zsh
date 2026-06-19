#!/bin/zsh
#
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

set -euo pipefail

SCRIPT="${0:A:h}/../bin/ghfork"
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
    local dir="$1"
    local fork_rc="${2:-0}"
    local api_user="${3:-testuser}"
    local api_rc="${4:-0}"
    mkdir -p "$dir"
    cat > "$dir/gh" <<SCRIPT
#!/bin/bash
if [[ "\$1" == "repo" && "\$2" == "fork" ]]; then
    exit $fork_rc
fi
if [[ "\$1" == "api" && "\$2" == "user" ]]; then
    if [[ $api_rc -ne 0 ]]; then
        exit $api_rc
    fi
    printf '%s\n' '$api_user'
    exit 0
fi
if [[ "\$1" == "repo" && "\$2" == "set-default" ]]; then
    exit 0
fi
exit 1
SCRIPT
    chmod +x "$dir/gh"
}

# --- 1. Not a git repo ---
out="$(cd "$TMPBASE" && bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "not a git repo returns 1" 1 "$rc"
assert_eq "not a git repo prints message" 1 "$([[ "$out" == *"not a git repository"* ]] && echo 1 || echo 0)"

# --- 2. No origin remote ---
repo="$(make_repo no-origin)"
out="$(cd "$repo" && bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "no origin returns 1" 1 "$rc"
assert_eq "no origin prints message" 1 "$([[ "$out" == *"no origin remote"* ]] && echo 1 || echo 0)"

# --- 3. Non-GitHub origin ---
repo="$(make_repo non-gh https://gitlab.com/owner/repo.git)"
out="$(cd "$repo" && bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "non-github origin returns 1" 1 "$rc"
assert_eq "non-github origin prints message" 1 "$([[ "$out" == *"not a GitHub URL"* ]] && echo 1 || echo 0)"

# --- 4. Upstream already exists ---
repo="$(make_repo has-upstream https://github.com/someowner/somerepo.git)"
git -C "$repo" remote add upstream https://github.com/other/somerepo.git
mock_gh "$TMPBASE/mock4" 0 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock4:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "upstream exists returns 1" 1 "$rc"
assert_eq "upstream exists prints message" 1 "$([[ "$out" == *"upstream remote already exists"* ]] && echo 1 || echo 0)"
assert_eq "upstream exists leaves origin unchanged" "https://github.com/someowner/somerepo.git" "$(git -C "$repo" remote get-url origin)"

# --- 5. gh not on PATH ---
repo="$(make_repo no-gh https://github.com/someowner/somerepo.git)"
out="$(cd "$repo" && PATH=/usr/bin:/bin bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "no gh returns 1" 1 "$rc"
assert_eq "no gh prints message" 1 "$([[ "$out" == *"gh CLI not found"* ]] && echo 1 || echo 0)"

# --- 6. HTTPS origin, successful fork ---
repo="$(make_repo https-fork https://github.com/someowner/somerepo.git)"
mock_gh "$TMPBASE/mock6" 0 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock6:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "https fork returns 0" 0 "$rc"
assert_eq "https fork sets origin to fork" "https://github.com/testuser/somerepo.git" "$(git -C "$repo" remote get-url origin)"
assert_eq "https fork sets upstream to original" "https://github.com/someowner/somerepo.git" "$(git -C "$repo" remote get-url upstream)"

# --- 7. SSH origin, successful fork ---
repo="$(make_repo ssh-fork git@github.com:someowner/somerepo.git)"
mock_gh "$TMPBASE/mock7" 0 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock7:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "ssh fork returns 0" 0 "$rc"
assert_eq "ssh fork sets origin to fork (ssh)" "git@github.com:testuser/somerepo.git" "$(git -C "$repo" remote get-url origin)"
assert_eq "ssh fork sets upstream to original (ssh)" "git@github.com:someowner/somerepo.git" "$(git -C "$repo" remote get-url upstream)"

# --- 8. gh repo fork fails ---
repo="$(make_repo fork-fail https://github.com/someowner/somerepo.git)"
mock_gh "$TMPBASE/mock8" 1 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock8:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "fork failure returns 1" 1 "$rc"
assert_eq "fork failure leaves origin unchanged" "https://github.com/someowner/somerepo.git" "$(git -C "$repo" remote get-url origin)"
assert_eq "fork failure has no upstream" "" "$(git -C "$repo" remote get-url upstream 2>/dev/null || true)"

# --- 9. gh api user fails ---
repo="$(make_repo api-fail https://github.com/someowner/somerepo.git)"
mock_gh "$TMPBASE/mock9" 0 "" 1
out="$(cd "$repo" && PATH="$TMPBASE/mock9:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "api user failure returns 1" 1 "$rc"
assert_eq "api user failure leaves origin unchanged" "https://github.com/someowner/somerepo.git" "$(git -C "$repo" remote get-url origin)"
assert_eq "api user failure has no upstream" "" "$(git -C "$repo" remote get-url upstream 2>/dev/null || true)"

# --- 10. Origin without .git suffix ---
repo="$(make_repo no-dotgit https://github.com/someowner/somerepo)"
mock_gh "$TMPBASE/mock10" 0 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock10:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "no .git suffix returns 0" 0 "$rc"
assert_eq "no .git suffix sets origin to fork" "https://github.com/testuser/somerepo.git" "$(git -C "$repo" remote get-url origin)"
assert_eq "no .git suffix sets upstream to original" "https://github.com/someowner/somerepo" "$(git -C "$repo" remote get-url upstream)"

# --- 11. Dotted repo name ---
repo="$(make_repo dotted https://github.com/myuser/myuser.github.io.git)"
mock_gh "$TMPBASE/mock11" 0 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock11:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "dotted repo name returns 0" 0 "$rc"
assert_eq "dotted repo sets origin to fork" "https://github.com/testuser/myuser.github.io.git" "$(git -C "$repo" remote get-url origin)"
assert_eq "dotted repo sets upstream to original" "https://github.com/myuser/myuser.github.io.git" "$(git -C "$repo" remote get-url upstream)"

# --- 12. Branch tracking origin is re-pointed at the new origin (the fork) ---
# `git remote rename origin upstream` rewrites branch.<name>.remote to upstream;
# ghfork must flip it back to origin. Covers the get-regexp/awk flip loop, which
# is otherwise exercised only by fixtures that have no tracking branches.
repo="$(make_repo tracking https://github.com/someowner/somerepo.git)"
git -C "$repo" -c user.email=test@example.com -c user.name=Test \
    -c commit.gpgsign=false commit -q --allow-empty -m init
git -C "$repo" config branch.main.remote origin
git -C "$repo" config branch.main.merge refs/heads/main
mock_gh "$TMPBASE/mock12" 0 "testuser" 0
out="$(cd "$repo" && PATH="$TMPBASE/mock12:$PATH" bash "$SCRIPT" 2>&1)" && rc=$? || rc=$?
assert_eq "tracking branch fork returns 0" 0 "$rc"
assert_eq "tracking branch remote flipped back to origin" "origin" "$(git -C "$repo" config branch.main.remote)"
assert_eq "tracking branch merge ref unchanged" "refs/heads/main" "$(git -C "$repo" config branch.main.merge)"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
