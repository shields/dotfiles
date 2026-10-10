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

PYTHON="$(uv run --project "${0:A:h}/.." python -c 'import sys; print(sys.executable)')"
export PATH="${PYTHON:h}:$PATH"
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
assert_eq "--help names the GitHub App authorization" 1 "$([[ "$OUT" == *GITHUB_APP_AUTH* ]] && echo 1 || echo 0)"
assert_eq "--help names the Codex login" 1 "$([[ "$OUT" == *CODEX_AUTH* ]] && echo 1 || echo 0)"

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
for name in GH_TOKEN GITHUB_APP_AUTH CLAUDE_CODE_OAUTH_TOKEN CODEX_AUTH LGTMCP_CONFIG; do
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

# --- 5a. CODEX_AUTH ---
CODEX_ACCESS='FAKE-codex-access-token-4c8e'
CODEX_JSON=$'{\n  "OPENAI_API_KEY": null,\n  "tokens": {\n    "access_token": "'"$CODEX_ACCESS"$'",\n    "refresh_token": "FAKE-codex-refresh-9d1a"\n  },\n  "last_refresh": "2026-10-08T00:00:00Z"\n}'
new_home codex
run CODEX_AUTH "$CODEX_JSON"$'\n'
file="$HOME_DIR/.codex/auth.json"
assert_eq "codex auth succeeds" 0 "$RC"
assert_eq "codex auth prints nothing" "" "$OUT"
assert_eq "codex auth keeps the JSON intact, line by line" same \
    "$(printf '%s\n' "$CODEX_JSON" | cmp -s - "$file" && echo same || echo different)"
assert_eq "codex auth file mode" 600 "$(mode_of "$file")"
assert_eq "codex auth directory mode" 700 "$(mode_of "$HOME_DIR/.codex")"
assert_eq "codex auth leaves no temporary file" auth.json "$(ls -A "$HOME_DIR/.codex")"
assert_eq "codex auth never runs gh" 0 "$([[ -e "$GH_STUB_DIR/argv" ]] && echo 1 || echo 0)"

new_home codexreplace
mkdir -p "$HOME_DIR/.codex"
printf '{"old": true}\n' > "$HOME_DIR/.codex/auth.json"
printf 'model = "x"\n' > "$HOME_DIR/.codex/config.toml"
chmod 755 "$HOME_DIR/.codex"
run CODEX_AUTH '{"new": true}'
assert_eq "codex auth replacement succeeds" 0 "$RC"
assert_eq "codex auth replacement content" '{"new": true}' "$(<"$HOME_DIR/.codex/auth.json")"
assert_eq "codex auth replacement directory mode" 700 "$(mode_of "$HOME_DIR/.codex")"
assert_eq "codex auth leaves the other Codex files alone" 'model = "x"' "$(<"$HOME_DIR/.codex/config.toml")"

new_home codexbad
for input in "\"$CODEX_ACCESS\"" '[]' "[\"$CODEX_ACCESS\"]" 'null' '42' 'not json' "{\"tokens\": \"$CODEX_ACCESS\"" '{} {}'; do
    run CODEX_AUTH "$input"
    assert_eq "a Codex value that is not an object fails: $input" 1 "$RC"
    assert_eq "a Codex value that is not an object says so: $input" 1 "$([[ "$OUT" == *"not a JSON object"* ]] && echo 1 || echo 0)"
    assert_absent "a Codex value that is not an object is not echoed: $input" "$CODEX_ACCESS" "$OUT"
    assert_eq "a Codex value that is not an object writes nothing: $input" "" "$(ls -A "$HOME_DIR")"
done

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

# --- 8a. GITHUB_APP_AUTH ---
# Every record here expires in the future, so the token manager never asks a
# GitHub server to refresh it.
ACCESS='ghu_FAKEaccessToken0123456789abcdef'
REFRESH='ghr_FAKErefreshToken0123456789abcdef'
NOW=$(date +%s)
APP_RECORD="$(printf '{"client_id":"Iv-fake","repository":"shields/dotfiles","access_token":"%s","access_expires_at":%d,"refresh_token":"%s","refresh_expires_at":%d}' \
    "$ACCESS" $(( NOW + 28800 )) "$REFRESH" $(( NOW + 15811200 )))"

new_home app
run GITHUB_APP_AUTH "$APP_RECORD"$'\n'
auth="$HOME_DIR/.config/github-app/auth.json"
assert_eq "app auth succeeds" 0 "$RC"
assert_eq "app auth prints nothing" "" "$OUT"
assert_eq "app auth file mode" 600 "$(mode_of "$auth")"
assert_eq "app auth directory mode" 700 "$(mode_of "$HOME_DIR/.config/github-app")"
assert_eq "app auth keeps the repository" 1 "$(grep -c '"repository": "shields/dotfiles"' "$auth")"
assert_eq "app auth keeps the access token" 1 "$(grep -c "\"access_token\": \"$ACCESS\"" "$auth")"
assert_eq "app auth keeps the refresh token" 1 "$(grep -c "\"refresh_token\": \"$REFRESH\"" "$auth")"
assert_eq "app auth logs gh in" \
    "auth login --with-token --insecure-storage" \
    "$(tr '\n' ' ' < "$GH_STUB_DIR/argv" | sed 's/ $//')"
assert_eq "gh receives the access token on stdin" "$ACCESS" "$(<"$GH_STUB_DIR/stdin")"
assert_eq "gh is aimed at github.com" 1 "$(grep -c '^GH_HOST=github.com$' "$GH_STUB_DIR/env")"
assert_absent "app auth keeps the access token out of gh's argv" "$ACCESS" "$(<"$GH_STUB_DIR/argv")"
assert_absent "app auth keeps the access token out of gh's environment" "$ACCESS" "$(<"$GH_STUB_DIR/env")"
assert_absent "app auth keeps the refresh token away from gh" "$REFRESH" "$(cat "$GH_STUB_DIR"/*)"
assert_eq "only the record holds a token" "$auth" \
    "$(grep -rl -e "$ACCESS" -e "$REFRESH" "$HOME_DIR")"
git_config="$HOME_DIR/.config/git/config"
helper="$(git config --file "$git_config" --get credential.https://github.com.helper)"
assert_eq "git asks the token manager for github.com credentials" 1 \
    "$([[ "$helper" == '!'*/github_app_token.py\ credential ]] && echo 1 || echo 0)"
assert_eq "git sends the repository path to the helper" true \
    "$(git config --file "$git_config" --get credential.https://github.com.useHttpPath)"
assert_eq "the helper is the only one for github.com" 1 \
    "$(git config --file "$git_config" --get-all credential.https://github.com.helper | wc -l | tr -d ' ')"

new_home appreplace
mkdir -p "$HOME_DIR/.config/git"
git config --file "$HOME_DIR/.config/git/config" credential.https://github.com.helper '!gh auth git-credential'
run GITHUB_APP_AUTH "$APP_RECORD"
assert_eq "app auth replaces gh's credential helper" 1 \
    "$(git config --file "$HOME_DIR/.config/git/config" --get-all credential.https://github.com.helper | grep -c 'github_app_token.py')"
assert_eq "app auth leaves no other github.com helper" 1 \
    "$(git config --file "$HOME_DIR/.config/git/config" --get-all credential.https://github.com.helper | wc -l | tr -d ' ')"

new_home appbad
bad_inputs=(
    'not json'
    '[]'
    '{"client_id":"Iv-fake"}'
    "{\"client_id\":\"Iv-fake\",\"repository\":\"$ACCESS\",\"access_token\":\"$ACCESS\",\"access_expires_at\":$(( NOW + 28800 )),\"refresh_token\":\"$REFRESH\",\"refresh_expires_at\":$(( NOW + 15811200 ))}"
    "{\"client_id\":\"Iv-fake\",\"repository\":\"shields/dotfiles\",\"access_token\":\"$ACCESS\",\"access_expires_at\":$(( NOW + 28800 )),\"refresh_token\":\"$REFRESH\",\"refresh_expires_at\":$(( NOW + 15811200 )),\"extra\":\"$ACCESS\"}"
    "{\"client_id\":\"Iv-fake\",\"repository\":\"shields/dotfiles\",\"access_token\":\"$ACCESS \$(id)\",\"access_expires_at\":$(( NOW + 28800 )),\"refresh_token\":\"$REFRESH\",\"refresh_expires_at\":$(( NOW + 15811200 ))}"
    "{\"client_id\":\"Iv-fake\",\"repository\":\"shields/dotfiles\",\"access_token\":\"$ACCESS\",\"access_expires_at\":\"soon\",\"refresh_token\":\"$REFRESH\",\"refresh_expires_at\":$(( NOW + 15811200 ))}"
    "{\"client_id\":\"Iv-fake\",\"repository\":\"shields/dotfiles\",\"access_token\":\"$ACCESS\",\"access_expires_at\":$(( NOW + 28800 )),\"refresh_token\":\"$REFRESH\",\"refresh_expires_at\":$(( NOW + 15811200 )),\"web_url\":\"https://user:$ACCESS@github.com\"}"
)
for input in "${bad_inputs[@]}"; do
    run GITHUB_APP_AUTH "$input"
    assert_eq "a bad app record fails: ${input:0:30}" 1 "$RC"
    assert_absent "a bad app record is not echoed: ${input:0:30}" "$ACCESS" "$OUT"
    assert_absent "a bad app record's refresh token is not echoed: ${input:0:30}" "$REFRESH" "$OUT"
    assert_eq "a bad app record writes nothing: ${input:0:30}" "" "$(ls -A "$HOME_DIR")"
done
assert_eq "a bad app record never runs gh" 0 "$([[ -e "$GH_STUB_DIR/argv" ]] && echo 1 || echo 0)"

new_home appghfail
RC=0
OUT="$(printf '%s' "$APP_RECORD" | env HOME="$HOME_DIR" GH_STUB_DIR="$GH_STUB_DIR" GH_STUB_RC=1 \
    PATH="$STUBS:$PATH" bash "$SCRIPT" GITHUB_APP_AUTH 2>&1)" || RC=$?
assert_eq "an app auth gh failure fails" 1 "$RC"
assert_eq "an app auth gh failure says so" 1 "$([[ "$OUT" == *"gh auth login failed"* ]] && echo 1 || echo 0)"
assert_absent "an app auth gh failure does not echo the access token" "$ACCESS" "$OUT"
assert_absent "an app auth gh failure does not echo the refresh token" "$REFRESH" "$OUT"

new_home ghgit
run GH_TOKEN "$SECRET"
assert_eq "gh token leaves git's configuration alone" "" "$(ls -A "$HOME_DIR")"

# --- 9. No HOME ---
RC=0
OUT="$(printf '%s' "$SECRET" | env -u HOME bash "$SCRIPT" CLAUDE_CODE_OAUTH_TOKEN 2>&1)" || RC=$?
assert_eq "no HOME fails" 1 "$RC"
assert_eq "no HOME says so" 1 "$([[ "$OUT" == *"HOME is not set"* ]] && echo 1 || echo 0)"

# --- 10. Nothing under any HOME holds the gh token or the unknown-name input ---
assert_eq "no secret is stored where it should not be" 0 \
    "$(grep -rl --exclude=CLAUDE_CODE_OAUTH_TOKEN "$SECRET" "$TMPBASE"/home-* 2>/dev/null | wc -l | tr -d ' ')"
assert_eq "only the Codex record holds the Codex token" "$TMPBASE/home-codex/.codex/auth.json" \
    "$(grep -rl "$CODEX_ACCESS" "$TMPBASE"/home-* 2>/dev/null)"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
