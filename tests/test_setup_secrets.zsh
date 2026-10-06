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
umask 022
zmodload zsh/stat

SCRIPT="${0:A:h}/../bin/setup-secrets"
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

assert_absent() {
    local desc="$1" needle="$2" haystack="$3"
    if [[ "$haystack" != *"$needle"* ]]; then
        echo "PASS: $desc"
        (( ++pass ))
    else
        echo "FAIL: $desc ($needle appears)"
        (( ++fail ))
    fi
}

mode_of() {
    local -a mode
    zstat -A mode +mode "$1"
    printf '%o' $(( mode[1] & 8#7777 ))
}

SECRET='ghp_FAKEsecretValue0123456789abcdef'
STUBS="$TMPBASE/stubs"
mkdir -p "$STUBS"

cat > "$STUBS/gh" <<'STUB'
#!/bin/bash
printf '%s\n' "$@" > "$GH_STUB_DIR/argv"
cat > "$GH_STUB_DIR/stdin"
env > "$GH_STUB_DIR/env"
if [[ ${GH_STUB_RC:-0} -ne 0 ]]; then
    echo "gh: stub failure" >&2
fi
exit "${GH_STUB_RC:-0}"
STUB
chmod +x "$STUBS/gh"

new_home() {
    HOME_DIR="$TMPBASE/home-$1"
    GH_STUB_DIR="$TMPBASE/gh-$1"
    mkdir -p "$HOME_DIR" "$GH_STUB_DIR"
}

run() {
    local name="$1" input="$2"
    RC=0
    OUT="$(printf '%s' "$input" | env HOME="$HOME_DIR" GH_STUB_DIR="$GH_STUB_DIR" \
        PATH="$STUBS:$PATH" bash "$SCRIPT" "$name" 2>&1)" || RC=$?
}

# --- 1. Arguments ---
new_home args
RC=0
OUT="$(printf '%s' "$SECRET" | env HOME="$HOME_DIR" bash "$SCRIPT" 2>&1)" || RC=$?
assert_eq "no argument fails" 1 "$RC"
assert_eq "no argument prints usage" 1 "$([[ "$OUT" == *usage:* ]] && echo 1 || echo 0)"
RC=0
OUT="$(printf '%s' "$SECRET" | env HOME="$HOME_DIR" bash "$SCRIPT" GH_TOKEN extra 2>&1)" || RC=$?
assert_eq "extra argument fails" 1 "$RC"
RC=0
OUT="$(env HOME="$HOME_DIR" bash "$SCRIPT" --help 2>&1)" || RC=$?
assert_eq "--help succeeds" 0 "$RC"
assert_eq "--help names the secrets" 1 "$([[ "$OUT" == *CLAUDE_CODE_OAUTH_TOKEN* ]] && echo 1 || echo 0)"

# --- 2. Unknown names ---
new_home unknown
for name in BOGUS gh_token '' 'GH_TOKEN;id' '../../x'; do
    run "$name" "$SECRET"
    assert_eq "unknown name '$name' fails" 1 "$RC"
    assert_eq "unknown name '$name' says so" 1 "$([[ "$OUT" == *"unknown secret"* ]] && echo 1 || echo 0)"
    assert_absent "unknown name '$name' does not echo the value" "$SECRET" "$OUT"
done
assert_eq "unknown names write nothing" "" "$(ls -A "$HOME_DIR")"

# --- 3. Empty input ---
new_home empty
for name in GH_TOKEN CLAUDE_CODE_OAUTH_TOKEN LGTMCP_CONFIG; do
    for input in '' $'\n' $'  \n\t\n'; do
        run "$name" "$input"
        assert_eq "$name with empty input fails" 1 "$RC"
        assert_eq "$name with empty input says so" 1 "$([[ "$OUT" == *empty* ]] && echo 1 || echo 0)"
    done
done
assert_eq "empty input writes nothing" "" "$(ls -A "$HOME_DIR")"
assert_eq "empty input never runs gh" 0 "$([[ -e "$GH_STUB_DIR/argv" ]] && echo 1 || echo 0)"

# --- 4. A terminal on stdin is refused, so a typed secret is never echoed ---
new_home tty
RC=0
OUT="$(python3 - "$SCRIPT" "$HOME_DIR" <<'PY' 2>&1
import os
import pty
import subprocess
import sys

script, home = sys.argv[1:]
master, slave = pty.openpty()
result = subprocess.run(
    ["bash", script, "CLAUDE_CODE_OAUTH_TOKEN"],
    stdin=slave,
    capture_output=True,
    text=True,
    env={**os.environ, "HOME": home},
    check=False,
)
print(result.returncode, result.stderr.strip())
PY
)" || RC=$?
assert_eq "terminal stdin: python ran" 0 "$RC"
assert_eq "terminal stdin is refused" 1 "$([[ "$OUT" == "1 setup-secrets: CLAUDE_CODE_OAUTH_TOKEN is read from stdin; pipe it in" ]] && echo 1 || echo 0)"
assert_eq "terminal stdin writes nothing" "" "$(ls -A "$HOME_DIR")"

# --- 5. CLAUDE_CODE_OAUTH_TOKEN ---
new_home claude
run CLAUDE_CODE_OAUTH_TOKEN "$SECRET"$'\n'
file="$HOME_DIR/.config/secrets/CLAUDE_CODE_OAUTH_TOKEN"
assert_eq "claude token succeeds" 0 "$RC"
assert_eq "claude token prints nothing" "" "$OUT"
assert_eq "claude token content" "$SECRET" "$(<"$file")"
assert_eq "claude token has one trailing newline" "$(( ${#SECRET} + 1 ))" "$(wc -c < "$file" | tr -d ' ')"
assert_eq "claude token file mode" 600 "$(mode_of "$file")"
assert_eq "claude token directory mode" 700 "$(mode_of "$HOME_DIR/.config/secrets")"
assert_eq "claude token leaves no temporary file" 1 "$(ls -A "$HOME_DIR/.config/secrets" | wc -l | tr -d ' ')"
assert_eq "claude token never runs gh" 0 "$([[ -e "$GH_STUB_DIR/argv" ]] && echo 1 || echo 0)"

chmod 755 "$HOME_DIR/.config/secrets"
chmod 644 "$file"
run CLAUDE_CODE_OAUTH_TOKEN "second-value"
assert_eq "claude token replacement succeeds" 0 "$RC"
assert_eq "claude token replacement content" "second-value" "$(<"$file")"
assert_eq "claude token replacement file mode" 600 "$(mode_of "$file")"
assert_eq "claude token replacement directory mode" 700 "$(mode_of "$HOME_DIR/.config/secrets")"

# --- 6. LGTMCP_CONFIG ---
new_home lgtmcp
config=$'gemini_api_key: FAKE-KEY-123\nreview:\n  model: x\n'
run LGTMCP_CONFIG "$config"
file="$HOME_DIR/.config/lgtmcp/config.yaml"
assert_eq "lgtmcp config succeeds" 0 "$RC"
assert_eq "lgtmcp config prints nothing" "" "$OUT"
assert_eq "lgtmcp config content is verbatim" same \
    "$(printf '%s' "$config" | cmp -s - "$file" && echo same || echo different)"
assert_eq "lgtmcp config file mode" 600 "$(mode_of "$file")"
assert_eq "lgtmcp config directory mode" 700 "$(mode_of "$HOME_DIR/.config/lgtmcp")"

# --- 7. GH_TOKEN ---
new_home gh
run GH_TOKEN "$SECRET"$'\n'
assert_eq "gh token succeeds" 0 "$RC"
assert_eq "gh token prints nothing" "" "$OUT"
assert_eq "gh is run to log in" \
    "auth login --hostname github.com --with-token --insecure-storage" \
    "$(tr '\n' ' ' < "$GH_STUB_DIR/argv" | sed 's/ $//')"
assert_eq "gh receives the token on stdin" "$SECRET" "$(<"$GH_STUB_DIR/stdin")"
assert_absent "gh token is not in gh's argv" "$SECRET" "$(<"$GH_STUB_DIR/argv")"
assert_absent "gh token is not in gh's environment" "$SECRET" "$(<"$GH_STUB_DIR/env")"
assert_eq "gh token does not export GH_TOKEN" 0 "$(grep -c '^GH_TOKEN=' "$GH_STUB_DIR/env" || true)"
assert_eq "gh token writes no file itself" "" "$(ls -A "$HOME_DIR")"

# --- 8. gh failure ---
new_home ghfail
RC=0
OUT="$(printf '%s' "$SECRET" | env HOME="$HOME_DIR" GH_STUB_DIR="$GH_STUB_DIR" GH_STUB_RC=1 \
    PATH="$STUBS:$PATH" bash "$SCRIPT" GH_TOKEN 2>&1)" || RC=$?
assert_eq "gh failure fails" 1 "$RC"
assert_eq "gh failure says so" 1 "$([[ "$OUT" == *"gh auth login failed"* ]] && echo 1 || echo 0)"
assert_absent "gh failure does not echo the token" "$SECRET" "$OUT"

# --- 9. No HOME ---
RC=0
OUT="$(printf '%s' "$SECRET" | env -u HOME bash "$SCRIPT" CLAUDE_CODE_OAUTH_TOKEN 2>&1)" || RC=$?
assert_eq "no HOME fails" 1 "$RC"
assert_eq "no HOME says so" 1 "$([[ "$OUT" == *"HOME is not set"* ]] && echo 1 || echo 0)"

# --- 10. Nothing under any HOME holds the gh token or the unknown-name input ---
assert_eq "no secret is stored where it should not be" 0 \
    "$(grep -rl --exclude=CLAUDE_CODE_OAUTH_TOKEN "$SECRET" "$TMPBASE"/home-* 2>/dev/null | wc -l | tr -d ' ')"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
