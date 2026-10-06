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

# A git hook's environment would aim the fixture's git at the hook's repository.
for var in $(git rev-parse --local-env-vars); do
    unset "$var"
done

LIMAVM="${0:A:h}/../bin/limavm"
TMPBASE="${$(mktemp -d):A}"
trap 'rm -rf "$TMPBASE"' EXIT
export TMPDIR="$TMPBASE/tmp"
mkdir -p "$TMPDIR"

pass=0
fail=0

assert_eq() {
    local desc="$1" expected="$2" actual="$3"
    if [[ "$expected" == "$actual" ]]; then
        echo "PASS: $desc"
        (( ++pass ))
    else
        echo "FAIL: $desc"
        echo "  expected: $expected"
        echo "  actual:   $actual"
        (( ++fail ))
    fi
}

assert_contains() {
    local desc="$1" needle="$2" haystack="$3"
    if [[ "$haystack" == *"$needle"* ]]; then
        echo "PASS: $desc"
        (( ++pass ))
    else
        echo "FAIL: $desc"
        echo "  missing:  $needle"
        echo "  in:       $haystack"
        (( ++fail ))
    fi
}

STUBS="$TMPBASE/stubs"
mkdir -p "$STUBS"

# limactl keeps its instances and its calls under $LIMA_STUB_STATE, and a call
# whose arguments contain $LIMA_STUB_FAIL fails.
cat > "$STUBS/limactl" <<'STUB'
#!/bin/bash
state=$LIMA_STUB_STATE
printf '%s\n' "$*" >> "$state/limactl.log"
call=$(wc -l < "$state/limactl.log" | tr -d ' ')
printf '%s\n' "$@" > "$state/calls/$call.argv"
env | sort > "$state/calls/$call.env"
if [[ ! -t 0 ]]; then
    cat > "$state/calls/$call.stdin"
fi
if [[ -n ${LIMA_STUB_FAIL:-} && $* == *"$LIMA_STUB_FAIL"* ]]; then
    echo "limactl: injected failure" >&2
    exit 1
fi
sub=$1
shift
case $sub in
list)
    case ${1:-} in
    -q)
        grep -qx -- "$2" "$state/instances" || exit 1
        echo "$2"
        ;;
    --format)
        case $2 in
        '{{.Protected}}') [[ -e "$state/protected-$3" ]] && echo true || echo false ;;
        '{{len .Config.Mounts}}') cat "$state/mounts" 2>/dev/null || echo 0 ;;
        esac
        ;;
    *) cat "$state/instances" ;;
    esac
    ;;
start)
    while [[ $# -gt 0 ]]; do
        case $1 in
        --name=*) echo "${1#--name=}" >> "$state/instances" ;;
        --cpus | --memory) shift ;;
        --tty=false) ;;
        -*)
            echo "Error: unknown flag: $1" >&2
            exit 1
            ;;
        esac
        shift
    done
    ;;
clone)
    while [[ $# -gt 0 ]]; do
        case $1 in
        --cpus | --memory) shift ;;
        --tty=false | --start) ;;
        -*)
            echo "Error: unknown flag: $1" >&2
            exit 1
            ;;
        *) last=$1 ;;
        esac
        shift
    done
    echo "$last" >> "$state/instances"
    ;;
edit)
    while [[ $# -gt 0 ]]; do
        case $1 in
        --shell) shift ;;
        --tty=false) ;;
        -*)
            echo "Error: unknown flag: $1" >&2
            exit 1
            ;;
        esac
        shift
    done
    ;;
delete)
    for last in "$@"; do :; done
    if [[ -e "$state/protected-$last" ]]; then
        echo "limactl: instance is protected" >&2
        exit 1
    fi
    grep -vx -- "$last" "$state/instances" > "$state/instances.new" || true
    mv "$state/instances.new" "$state/instances"
    ;;
protect) : > "$state/protected-$1" ;;
unprotect) rm -f "$state/protected-$1" ;;
shell)
    if [[ $1 == --workdir ]]; then
        shift 2
    fi
    shift
    case $* in
    'sh -c printf %s "$HOME"') printf '/home/guest' ;;
    esac
    ;;
esac
exit 0
STUB

# security knows the items listed in $LIMA_STUB_STATE/keychain, and prints a
# fake secret that names its item for -w.
cat > "$STUBS/security" <<'STUB'
#!/bin/bash
state=$LIMA_STUB_STATE
call=$(mktemp "$state/calls/security.XXXXXX")
printf '%s\n' "$@" > "$call.argv"
env | sort > "$call.env"
item=
want_value=0
while [[ $# -gt 0 ]]; do
    case $1 in
    -s)
        item=$2
        shift
        ;;
    -w) want_value=1 ;;
    esac
    shift
done
if ! grep -qx -- "$item" "$state/keychain"; then
    echo "security: SecKeychainSearchCopyNext: The specified item could not be found in the keychain." >&2
    exit 44
fi
if ((want_value)); then
    echo "FAKE-SECRET-${item#limavm-}-7f3c9a"
else
    echo 'keychain: "login.keychain-db"'
fi
STUB

# stage_tree.sh stages one file into DEST, as the real script stages the tree.
cat > "$TMPBASE/stage_tree.sh" <<'STUB'
#!/bin/bash
printf '%s\n' "$@" > "$STAGE_ARGS_LOG"
if [[ -n ${STAGE_STUB_FAIL:-} ]]; then
    echo "stage_tree: injected failure" >&2
    exit 1
fi
mkdir -p "$1/provision"
echo staged > "$1/provision.sh"
STUB
chmod +x "$STUBS/limactl" "$STUBS/security"

CHECKOUT="$TMPBASE/checkout"
mkdir -p "$CHECKOUT/lima" "$CHECKOUT/provision" "$CHECKOUT/tools"
git -C "$CHECKOUT" init -q
: > "$CHECKOUT/lima/dev.yaml"
printf '#!/bin/bash\n' > "$CHECKOUT/provision.sh"
printf '#!/bin/bash\n' > "$CHECKOUT/provision/throwaway.sh"
printf '#!/bin/bash\n' > "$CHECKOUT/provision/reset-identity.sh"
cp "$TMPBASE/stage_tree.sh" "$CHECKOUT/tools/stage_tree.sh"
chmod +x "$CHECKOUT/provision.sh" "$CHECKOUT/tools/stage_tree.sh"

GUEST_DIR=/home/guest/src/github.com/shields/dotfiles
FAKE_GH='FAKE-SECRET-GH_TOKEN-7f3c9a'
FAKE_CLAUDE='FAKE-SECRET-CLAUDE_CODE_OAUTH_TOKEN-7f3c9a'
FAKE_LGTMCP='FAKE-LGTMCP-KEY-55'
LGTMCP_YAML="gemini_api_key: $FAKE_LGTMCP"$'\n'

CASE_N=0
EXTRA_ENV=()

# new_case [INSTANCE...]: a fresh state with a home, a Keychain and the
# instances that exist.
new_case() {
    (( ++CASE_N ))
    STATE="$TMPBASE/state-$CASE_N"
    HOME_DIR="$TMPBASE/home-$CASE_N"
    mkdir -p "$STATE/calls" "$HOME_DIR/.config/lgtmcp"
    print -rn -- "$LGTMCP_YAML" > "$HOME_DIR/.config/lgtmcp/config.yaml"
    : > "$STATE/instances"
    (( $# == 0 )) || printf '%s\n' "$@" >> "$STATE/instances"
    printf '%s\n' limavm-GH_TOKEN limavm-CLAUDE_CODE_OAUTH_TOKEN > "$STATE/keychain"
    : > "$STATE/limactl.log"
    STAGE_ARGS_LOG="$STATE/stage-args"
    EXTRA_ENV=()
}

# run_limavm ARGS...: sets OUT (stdout and stderr) and RC.
run_limavm() {
    RC=0
    OUT="$(cd "$CHECKOUT" && env PATH="$STUBS:$PATH" HOME="$HOME_DIR" USER=shields \
        LIMA_STUB_STATE="$STATE" STAGE_ARGS_LOG="$STAGE_ARGS_LOG" LIMAVM_BASE=test-base \
        "${EXTRA_ENV[@]}" /bin/bash "$LIMAVM" "$@" 2>&1 </dev/null)" || RC=$?
}

limactl_log() {
    sed "s|$CHECKOUT|CHECKOUT|g; s|$TMPBASE|TMP|g" "$STATE/limactl.log"
}

stdin_of() {
    cat "$STATE/calls/$1.stdin"
}

count_in_log() {
    grep -c -- "$1" "$STATE/limactl.log" || true
}

staging_dirs_left() {
    ls "$TMPDIR" | grep -c '^limavm\.' || true
}

# No fake secret may show in what limactl or security was given as arguments
# or environment, in limactl's log, or in what limavm printed.
assert_no_secret_leak() {
    local desc="$1" leaks
    leaks="$(cat "$STATE"/calls/*.argv "$STATE"/calls/*.env "$STATE/limactl.log" 2>/dev/null |
        grep -c -e "$FAKE_GH" -e "$FAKE_CLAUDE" -e "$FAKE_LGTMCP" || true)"
    assert_eq "$desc: no secret in any recorded argv or environment" 0 "$leaks"
    assert_eq "$desc: no secret in limavm's output" 0 \
        "$(printf '%s' "$OUT" | grep -c -e "$FAKE_GH" -e "$FAKE_CLAUDE" -e "$FAKE_LGTMCP" || true)"
}

# --- 1. Static checks ---
assert_eq "no bash 4 constructs" "" \
    "$(grep -vE '^[[:space:]]*#' "$LIMAVM" | grep -nE 'mapfile|readarray|declare +-A|local +-n|\$\{[A-Za-z_0-9]+(,,|\^\^|@[LUu])' || true)"
assert_eq "limavm is run by the system bash" "#!/bin/bash" "$(head -1 "$LIMAVM")"
assert_eq "no eval" 0 "$(grep -cE '(^|[^a-z_])eval ' "$LIMAVM" || true)"
assert_eq "limavm is executable" yes "$([[ -x $LIMAVM ]] && echo yes || echo no)"

# --- 2. help and bad invocations ---
new_case
run_limavm help
assert_eq "help succeeds" 0 "$RC"
assert_contains "help lists base" "limavm base" "$OUT"
assert_contains "help lists new" "limavm new" "$OUT"
run_limavm
assert_eq "no command fails" 2 "$RC"
run_limavm frobnicate
assert_eq "unknown command fails" 2 "$RC"
run_limavm new a b
assert_eq "new with two names fails" 1 "$RC"
run_limavm list
assert_eq "list succeeds" 0 "$RC"
assert_eq "list is limactl list" "list" "$(limactl_log)"

# --- 3. base on a fresh machine ---
new_case
run_limavm base none
assert_eq "base succeeds" 0 "$RC"
assert_eq "base call order" "list -q test-base
start --tty=false --name=test-base CHECKOUT/lima/dev.yaml
list --format {{len .Config.Mounts}} test-base
shell test-base sh -c printf %s \"\$HOME\"
shell test-base sh -c set -eu; rm -rf \"\$1\"; mkdir -p \"\$1\"; tar -xf - --no-same-owner -C \"\$1\" _ $GUEST_DIR
shell --workdir $GUEST_DIR test-base ./provision.sh none
shell test-base sudo $GUEST_DIR/provision/throwaway.sh
shell test-base sudo truncate -s 0 /etc/machine-id
shell test-base $GUEST_DIR/provision/reset-identity.sh
stop --tty=false test-base
edit --tty=false test-base --shell /usr/bin/zsh
protect test-base" "$(limactl_log)"
assert_eq "base stages into a directory outside the checkout" 1 \
    "$([[ "$(<"$STAGE_ARGS_LOG")" == "$TMPDIR"/limavm.*/tree ]] && echo 1 || echo 0)"
assert_eq "base streams the staged tree as a tar archive" "./provision.sh" \
    "$(tar -tf "$STATE/calls/5.stdin" | grep -x './provision.sh')"
assert_eq "base removes its staging directory" 0 "$(staging_dirs_left)"
assert_eq "base leaves the VM protected" 1 "$([[ -e "$STATE/protected-test-base" ]] && echo 1 || echo 0)"
assert_contains "base says what to do next" "limavm new" "$OUT"

# --- 4. base passes modules and sizes through ---
new_case
run_limavm base dev data
assert_contains "base passes the modules" "./provision.sh dev data" "$(limactl_log)"
assert_eq "base without a size passes none" 0 "$(count_in_log '--cpus')"
new_case
EXTRA_ENV=(LIMAVM_CPUS=4 LIMAVM_MEMORY=8)
run_limavm base none
assert_contains "base passes the size to start" \
    "start --tty=false --name=test-base --cpus 4 --memory 8 CHECKOUT/lima/dev.yaml" "$(limactl_log)"
new_case
EXTRA_ENV=(LIMAVM_CPUS=many)
run_limavm base none
assert_eq "a bad LIMAVM_CPUS fails" 1 "$RC"
assert_eq "a bad LIMAVM_CPUS touches no VM" "" "$(limactl_log)"
new_case
EXTRA_ENV=(LIMAVM_MEMORY=8GiB)
run_limavm base none
assert_eq "a bad LIMAVM_MEMORY fails" 1 "$RC"

# --- 5. base replaces an existing protected base ---
new_case test-base other
: > "$STATE/protected-test-base"
run_limavm base none
assert_eq "base over a protected base succeeds" 0 "$RC"
assert_eq "base unprotects before deleting" "list -q test-base
list --format {{.Protected}} test-base
unprotect test-base
delete --tty=false --force test-base" "$(limactl_log | sed -n 1,4p)"
assert_eq "base leaves other VMs alone" 1 "$(grep -cx other "$STATE/instances")"

# --- 6. base outside a dotfiles checkout ---
new_case
RC=0
OUT="$(cd "$TMPBASE" && env PATH="$STUBS:$PATH" HOME="$HOME_DIR" LIMA_STUB_STATE="$STATE" \
    LIMAVM_BASE=test-base /bin/bash "$LIMAVM" base 2>&1 </dev/null)" || RC=$?
assert_eq "base outside a repository fails" 1 "$RC"
assert_contains "base outside a repository says so" "inside a dotfiles checkout" "$OUT"
mkdir "$TMPBASE/other-repo"
git -C "$TMPBASE/other-repo" init -q
RC=0
OUT="$(cd "$TMPBASE/other-repo" && env PATH="$STUBS:$PATH" HOME="$HOME_DIR" LIMA_STUB_STATE="$STATE" \
    LIMAVM_BASE=test-base /bin/bash "$LIMAVM" base 2>&1 </dev/null)" || RC=$?
assert_eq "base in another repository fails" 1 "$RC"
assert_contains "base in another repository says so" "is not a dotfiles checkout" "$OUT"
assert_eq "neither touched limactl" "" "$(limactl_log)"

# --- 7. base failures ---
new_case
EXTRA_ENV=(STAGE_STUB_FAIL=1)
run_limavm base none
assert_eq "a staging failure fails base" 1 "$RC"
assert_eq "a staging failure starts no VM" "" "$(limactl_log)"
assert_eq "a staging failure removes the staging directory" 0 "$(staging_dirs_left)"

new_case
echo 3 > "$STATE/mounts"
run_limavm base none
assert_eq "a mounted base fails" 1 "$RC"
assert_contains "a mounted base says why" "mounts 3 host directories" "$OUT"
assert_eq "a mounted base runs no provisioning" 0 "$(count_in_log provision)"

new_case
EXTRA_ENV=(LIMA_STUB_FAIL=./provision.sh)
run_limavm base none
assert_eq "a provisioning failure fails base" 1 "$RC"
assert_eq "a provisioning failure stops before the identity reset and the protection" 0 \
    "$(grep -cE 'machine-id|reset-identity|protect|stop' "$STATE/limactl.log" || true)"
assert_contains "a provisioning failure keeps the VM for inspection" "left for inspection" "$OUT"
assert_eq "a provisioning failure keeps the VM" 1 "$(grep -cx test-base "$STATE/instances")"
assert_eq "a provisioning failure removes the staging directory" 0 "$(staging_dirs_left)"

# --- 8. new ---
new_case test-base
run_limavm new t1
assert_eq "new succeeds" 0 "$RC"
assert_eq "new call order" "list -q test-base
list -q t1
clone --tty=false --start test-base t1
shell t1 /home/linuxbrew/.linuxbrew/bin/brew upgrade --cask claude-code@latest codex
shell t1 sh -c exec \"\$HOME/bin/setup-secrets\" \"\$1\" _ GH_TOKEN
shell t1 sh -c exec \"\$HOME/bin/setup-secrets\" \"\$1\" _ CLAUDE_CODE_OAUTH_TOKEN
shell t1 sh -c exec \"\$HOME/bin/setup-secrets\" LGTMCP_CONFIG
shell t1" "$(limactl_log)"
assert_eq "the GH_TOKEN reaches setup-secrets on stdin" "$FAKE_GH" "$(stdin_of 5)"
assert_eq "the Claude token reaches setup-secrets on stdin" "$FAKE_CLAUDE" "$(stdin_of 6)"
assert_eq "the LGTMCP config reaches setup-secrets on stdin, byte for byte" same \
    "$(print -rn -- "$LGTMCP_YAML" | cmp -s - "$STATE/calls/7.stdin" && echo same || echo different)"
assert_eq "a token on stdin ends with its newline" same \
    "$(print -r -- "$FAKE_GH" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"
assert_eq "the upgrade call has no stdin" 0 "$(wc -c < "$STATE/calls/4.stdin" | tr -d ' ')"
assert_no_secret_leak "new"
assert_contains "new asks the Keychain for the current user's item" "-a
shields
-s
limavm-GH_TOKEN
-w" "$(cat "$STATE"/calls/security.*.argv)"
assert_contains "new reminds about the Codex login" "codex login --device-auth" "$OUT"
assert_contains "new mentions the ChatGPT setting" "ChatGPT" "$OUT"

# --- 9. new picks a name, and sizes ---
new_case test-base
EXTRA_ENV=(LIMAVM_CPUS=2 LIMAVM_MEMORY=4)
run_limavm new
assert_eq "new without a name succeeds" 0 "$RC"
assert_eq "new without a name makes a dated name" 1 \
    "$([[ "$(limactl_log | sed -n 3p)" == "clone --tty=false --cpus 2 --memory 4 --start test-base vm-"[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9] ]] && echo 1 || echo 0)"

# --- 10. new refuses before it makes anything ---
new_case test-base
run_limavm new test-base
assert_eq "new refuses the base name" 1 "$RC"
assert_eq "the base-name refusal touches no VM" "" "$(limactl_log)"

new_case test-base t1
run_limavm new t1
assert_eq "new refuses an existing name" 1 "$RC"
assert_contains "the existing-name refusal says so" "already exists" "$OUT"
assert_eq "the existing-name refusal clones nothing" 0 "$(count_in_log clone)"

new_case
run_limavm new t1
assert_eq "new without a base fails" 1 "$RC"
assert_contains "the missing-base failure says what to run" "limavm base" "$OUT"

for bad in -x ../x 'a b' .x 'x;y' ''; do
    new_case test-base
    run_limavm new "$bad"
    assert_eq "new rejects the name '$bad'" 1 "$RC"
    assert_eq "the bad name '$bad' clones nothing" 0 "$(count_in_log clone)"
done

# --- 11. A missing Keychain item ---
new_case test-base
printf '%s\n' limavm-CLAUDE_CODE_OAUTH_TOKEN > "$STATE/keychain"
run_limavm new t1
assert_eq "a missing GH_TOKEN item fails" 1 "$RC"
assert_contains "the message names the item" "limavm-GH_TOKEN" "$OUT"
assert_contains "the message gives the prompting form" \
    'security add-generic-password -a "$USER" -s limavm-GH_TOKEN -w' "$OUT"
assert_eq "a missing item clones nothing" 0 "$(count_in_log clone)"
assert_eq "a missing item reads no secret" 0 \
    "$(cat "$STATE"/calls/security.*.argv | grep -c -x -e '-w' || true)"

new_case test-base
printf '%s\n' limavm-GH_TOKEN > "$STATE/keychain"
run_limavm new t1
assert_eq "a missing CLAUDE_CODE_OAUTH_TOKEN item fails" 1 "$RC"
assert_contains "that message gives the prompting form" \
    'security add-generic-password -a "$USER" -s limavm-CLAUDE_CODE_OAUTH_TOKEN -w' "$OUT"
assert_eq "that failure clones nothing" 0 "$(count_in_log clone)"

new_case test-base
rm "$HOME_DIR/.config/lgtmcp/config.yaml"
run_limavm new t1
assert_eq "a missing LGTMCP config fails" 1 "$RC"
assert_contains "that message names the file" ".config/lgtmcp/config.yaml" "$OUT"
assert_eq "that failure clones nothing" 0 "$(count_in_log clone)"

# --- 12. The clone is deleted after any later failure ---
for failing in clone brew setup-secrets; do
    new_case test-base
    EXTRA_ENV=(LIMA_STUB_FAIL=$failing)
    run_limavm new t1
    assert_eq "a failing $failing fails new" 1 "$RC"
    assert_eq "a failing $failing deletes the clone last" "delete --tty=false --force t1" \
        "$(limactl_log | tail -1)"
    assert_eq "a failing $failing leaves no t1" 0 "$(grep -cx t1 "$STATE/instances")"
    assert_eq "a failing $failing keeps the base" 1 "$(grep -cx test-base "$STATE/instances")"
    assert_contains "a failing $failing says it removed the clone" "removing t1" "$OUT"
    assert_no_secret_leak "a failing $failing"
done

new_case test-base
EXTRA_ENV=(LIMA_STUB_FAIL=brew)
run_limavm new t1
assert_eq "no secret is sent when the cask upgrade fails" 0 "$(count_in_log setup-secrets)"

new_case test-base
EXTRA_ENV=(LIMA_STUB_FAIL=LGTMCP_CONFIG)
run_limavm new t1
assert_eq "a failing LGTMCP install deletes the clone" "delete --tty=false --force t1" "$(limactl_log | tail -1)"

# --- 13. rm ---
new_case test-base t1
run_limavm rm test-base
assert_eq "rm refuses the base" 1 "$RC"
assert_contains "the refusal says why" "refusing to delete test-base" "$OUT"
assert_eq "the refusal touches no VM" "" "$(limactl_log)"
run_limavm rm t1
assert_eq "rm succeeds" 0 "$RC"
assert_eq "rm deletes just that VM" "delete --tty=false --force t1" "$(limactl_log)"
run_limavm rm
assert_eq "rm without a name fails" 1 "$RC"
run_limavm rm ../x
assert_eq "rm with a bad name fails" 1 "$RC"
run_limavm rm t1 t2
assert_eq "rm with two names fails" 1 "$RC"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
