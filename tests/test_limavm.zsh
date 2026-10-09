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

# A git hook's environment would aim the fixture's git at the hook's repository.
for var in $(git rev-parse --local-env-vars); do
    unset "$var"
done

HERE="${0:A:h}"
LIMAVM="$HERE/../bin/limavm"
FAKE_GITHUB="$HERE/fake_github.py"
REAL_CURL="$(command -v curl)"
REAL_GIT="$(command -v git)"
REAL_GITLEAKS="$(command -v gitleaks)"
TMPBASE="${$(mktemp -d):A}"
FAKE_PID=
stop_fake() {
    if [[ -n $FAKE_PID ]]; then
        kill -KILL "$FAKE_PID" 2>/dev/null || true
        wait "$FAKE_PID" 2>/dev/null || true
        FAKE_PID=
    fi
}
trap 'stop_fake; rm -rf "$TMPBASE"' EXIT
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
        '{{.Status}}') [[ -e "$state/stopped-$3" ]] && echo Stopped || echo Running ;;
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
    'sh -c printf %s "$HOME"') [[ -e "$state/guest-home-unknown" ]] || printf '/home/guest' ;;
    'sh -c set -eu; rm -rf '*'tar -xf - '*)
        mkdir -p "$state/guest"
        tar -xf "$state/calls/$call.stdin" -C "$state/guest"
        ;;
    'env GIT_TERMINAL_PROMPT=0 git clone '*) ;;
    'git '*) git -C "$state/guest" "${@:2}" || exit $? ;;
    esac
    ;;
esac
exit 0
STUB

# gh answers the one read-only call limavm makes, with the id in
# $LIMA_STUB_STATE/gh-id, and fails if $LIMA_STUB_STATE/gh-fail exists.
cat > "$STUBS/gh" <<'STUB'
#!/bin/bash
state=$LIMA_STUB_STATE
call=$(mktemp "$state/calls/gh.XXXXXX")
printf '%s\n' "$@" > "$call.argv"
env | sort > "$call.env"
printf '%s\n' "$*" >> "$state/gh.log"
if [[ -e "$state/gh-fail" ]]; then
    echo "gh: HTTP 404: Not Found" >&2
    exit 1
fi
if [[ $* == "api repos/"*" --jq .id" ]]; then
    cat "$state/gh-id"
    exit 0
fi
echo "gh stub: unexpected call: $*" >&2
exit 2
STUB

# curl records its arguments and environment, so that the test can show that no
# secret reaches them, and then runs the real curl.
cat > "$STUBS/curl" <<'STUB'
#!/bin/bash
state=$LIMA_STUB_STATE
call=$(mktemp "$state/calls/curl.XXXXXX")
printf '%s\n' "$@" > "$call.argv"
env | sort > "$call.env"
exec "$REAL_CURL" "$@"
STUB

cat > "$STUBS/open" <<'STUB'
#!/bin/bash
printf '%s\n' "$*" >> "$LIMA_STUB_STATE/open.log"
[[ ! -e $LIMA_STUB_STATE/open-fail ]]
STUB

# pbcopy records what it was given, and fails if $LIMA_STUB_STATE/pbcopy-fail
# exists.
cat > "$STUBS/pbcopy" <<'STUB'
#!/bin/bash
state=$LIMA_STUB_STATE
call=$(mktemp "$state/calls/pbcopy.XXXXXX")
printf '%s\n' "$@" > "$call.argv"
env | sort > "$call.env"
if [[ -e $state/pbcopy-fail ]]; then
    echo "pbcopy: injected failure" >&2
    exit 1
fi
cat > "$state/pbcopy.stdin"
STUB

cat > "$STUBS/sleep" <<'STUB'
#!/bin/bash
printf '%s\n' "$*" >> "$LIMA_STUB_STATE/sleep.log"
STUB

# limavm has no use for the macOS security command, so any call is a failure.
cat > "$STUBS/security" <<'STUB'
#!/bin/bash
echo "called: $*" >> "$LIMA_STUB_STATE/security.log"
echo "security: limavm must not call this" >&2
exit 1
STUB
cat > "$STUBS/gitleaks" <<'STUB'
#!/bin/bash
if [[ $1 == git && -n ${HISTORY_SCAN_FAIL:-} ]]; then
    echo "gitleaks: injected history scan failure" >&2
    exit 1
fi
exec "$REAL_GITLEAKS" "$@"
STUB
cat > "$STUBS/git" <<'STUB'
#!/bin/bash
call=$(mktemp "$LIMA_STUB_STATE/calls/git.XXXXXX")
printf '%s\n' "$@" > "$call.argv"
env | sort > "$call.env"
exec "$REAL_GIT" "$@"
STUB
chmod +x "$STUBS"/*

cat > "$TMPBASE/stage_tree.sh" <<'STUB'
#!/bin/bash
printf '%s\n' "$@" > "$STAGE_ARGS_LOG"
if [[ -n ${STAGE_STUB_FAIL:-} ]]; then
    echo "stage_tree: injected failure" >&2
    exit 1
fi
exec "$(dirname "$0")/stage_tree_real.sh" "$@"
STUB

CHECKOUT="$TMPBASE/checkout"
mkdir -p "$CHECKOUT/lima" "$CHECKOUT/provision" "$CHECKOUT/tools"
git -C "$CHECKOUT" init -q -b main
: > "$CHECKOUT/lima/dev.yaml"
printf '#!/bin/bash\n' > "$CHECKOUT/provision.sh"
printf '#!/bin/bash\n' > "$CHECKOUT/provision/throwaway.sh"
printf '#!/bin/bash\n' > "$CHECKOUT/provision/reset-identity.sh"
cp "$TMPBASE/stage_tree.sh" "$CHECKOUT/tools/stage_tree.sh"
cp "$HERE/../tools/stage_tree.sh" "$CHECKOUT/tools/stage_tree_real.sh"
chmod +x "$CHECKOUT/provision.sh" "$CHECKOUT/tools/stage_tree.sh"
git -C "$CHECKOUT" add .
git -C "$CHECKOUT" -c user.name=Test -c user.email=test@example.com commit -qm initial
git -C "$CHECKOUT" tag initial
git -C "$CHECKOUT" branch other
printf 'tracked\n' > "$CHECKOUT/tracked.txt"
printf 'obsolete\n' > "$CHECKOUT/obsolete.txt"
git -C "$CHECKOUT" add .
git -C "$CHECKOUT" -c user.name=Test -c user.email=test@example.com commit -qm second
git -C "$CHECKOUT" remote add origin git@github.com:shields/dotfiles.git

GUEST_DIR=/home/guest/src/github.com/shields/dotfiles
OTHER_DIR=/home/guest/src/github.com/shields/other
CLIENT_ID=Iv23lim5x4MdkNNgv28z
FAKE_CLAUDE='sk-ant-oat01-FAKEclaudeToken-7f3c9a'
FAKE_LGTMCP='FAKE-LGTMCP-KEY-55'
LGTMCP_YAML="gemini_api_key: $FAKE_LGTMCP"$'\n'
FAKE_CODEX='FAKE-codex-access-token-4c8e'
CODEX_JSON=$'{\n  "OPENAI_API_KEY": null,\n  "tokens": {\n    "access_token": "'"$FAKE_CODEX"$'",\n    "refresh_token": "FAKE-codex-refresh-9d1a"\n  }\n}\n'
# What the fake GitHub hands out, and the device code it expects back.
GITHUB_SECRETS=(-e ghu_fake-access -e ghr_fake-refresh -e fake-device-code)
SETUP='shell t1 sh -c exec "$HOME/bin/setup-secrets" "$1" _'

CASE_N=0
EXTRA_ENV=()
FAKE_URL=

# new_case [INSTANCE...]: a fresh state with a home that holds a Codex login, a
# Claude token to type and the instances that exist.
new_case() {
    stop_fake
    (( ++CASE_N ))
    STATE="$TMPBASE/state-$CASE_N"
    HOME_DIR="$TMPBASE/home-$CASE_N"
    mkdir -p "$STATE/calls" "$HOME_DIR/.config/lgtmcp" "$HOME_DIR/.codex"
    print -rn -- "$LGTMCP_YAML" > "$HOME_DIR/.config/lgtmcp/config.yaml"
    print -rn -- "$CODEX_JSON" > "$HOME_DIR/.codex/auth.json"
    : > "$STATE/instances"
    (( $# == 0 )) || printf '%s\n' "$@" >> "$STATE/instances"
    : > "$STATE/limactl.log"
    echo 4242 > "$STATE/gh-id"
    print -r -- "$FAKE_CLAUDE" > "$STATE/prompt"
    STAGE_ARGS_LOG="$STATE/stage-args"
    FAKE_URL=
    RUN_PATH=
    EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/prompt")
}

# start_fake [JSON FIELDS]: a fake GitHub on a free port for the current case,
# with the repositories shields/dotfiles (4242) and shields/other (77), and
# limavm aimed at it. FAKE_CLIENT_ID is the client id it accepts.
start_fake() {
    local fields=${1:+, $1}
    print -r -- "{\"port_file\": \"$STATE/fake.port\", \"log\": \"$STATE/fake.log\", \"client_id\": \"${FAKE_CLIENT_ID:-$CLIENT_ID}\", \"repo_ids\": {\"shields/dotfiles\": 4242, \"shields/other\": 77}$fields}" > "$STATE/fake.json"
    python3 "$FAKE_GITHUB" "$STATE/fake.json" &
    FAKE_PID=$!
    local tries
    for tries in {1..200}; do
        [[ -s "$STATE/fake.port" ]] && break
        sleep 0.05
    done
    if [[ ! -s "$STATE/fake.port" ]]; then
        echo "the fake GitHub did not start" >&2
        exit 1
    fi
    FAKE_URL="http://127.0.0.1:$(<"$STATE/fake.port")"
    EXTRA_ENV+=(LIMAVM_GITHUB_WEB_URL="$FAKE_URL" LIMAVM_GITHUB_API_URL="$FAKE_URL/api/v3")
}

# no_pbcopy_path: the test PATH with every pbcopy removed, each directory that
# holds one replaced by a directory of links to its other commands.
no_pbcopy_path() {
    local dir farm cmd result= stubbed_path="$STUBS:$PATH"
    for dir in ${(s<:>)stubbed_path}; do
        if [[ -e $dir/pbcopy ]]; then
            farm="$TMPBASE/no-pbcopy$dir"
            if [[ ! -d $farm ]]; then
                mkdir -p "$farm"
                for cmd in "$dir"/*(N); do
                    [[ ${cmd:t} == pbcopy ]] || ln -s "$cmd" "$farm/${cmd:t}"
                done
            fi
            dir=$farm
        fi
        result+="${result:+:}$dir"
    done
    print -r -- "$result"
}

# Every run gets a GitHub that nothing listens on, so that the checkout's origin
# never leads a case to the real one; start_fake replaces it, later in the
# environment.
UNREACHABLE_GITHUB=http://127.0.0.1:9

# run_limavm ARGS...: sets OUT (stdout and stderr) and RC. RUN_PATH replaces
# the stubbed PATH for one case.
run_limavm() {
    RC=0
    OUT="$(cd "$CHECKOUT" && env PATH="${RUN_PATH:-$STUBS:$PATH}" HOME="$HOME_DIR" USER=shields \
        LIMA_STUB_STATE="$STATE" STAGE_ARGS_LOG="$STAGE_ARGS_LOG" LIMAVM_BASE=test-base \
        REAL_CURL="$REAL_CURL" REAL_GIT="$REAL_GIT" REAL_GITLEAKS="$REAL_GITLEAKS" \
        NO_PROXY=127.0.0.1 no_proxy=127.0.0.1 LIMAVM_GITHUB_WEB_URL="$UNREACHABLE_GITHUB" \
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

mode_of() {
    local -a mode
    zstat -A mode +mode "$1"
    printf '%o' $(( mode[1] & 8#7777 ))
}

fake_requests() {
    [[ -e "$STATE/fake.log" ]] || return 0
    jq -r '"\(.method) \(.path)"' "$STATE/fake.log"
}

fake_form() {
    [[ -e "$STATE/fake.log" ]] || return 0
    jq -r --arg path "$1" --arg field "$2" 'select(.path == $path) | .form[$field] // empty' "$STATE/fake.log"
}

stub_calls() {
    local name=$1
    ls "$STATE/calls" | grep -c "^$name\\..*\\.argv\$" || true
}

# None of the secrets may show in what a stub was given as arguments or
# environment, in the logs of the stubs, or in what limavm printed. They go over
# stdin and nowhere else.
assert_no_secret_leak() {
    local desc="$1" leaks
    leaks="$(cat "$STATE"/calls/*.argv(N) "$STATE"/calls/*.env(N) "$STATE/limactl.log" \
        "$STATE"/gh.log "$STATE"/open.log "$STATE"/sleep.log 2>/dev/null |
        grep -c -e "$FAKE_CLAUDE" -e "$FAKE_LGTMCP" -e "$FAKE_CODEX" "${GITHUB_SECRETS[@]}" || true)"
    assert_eq "$desc: no secret in any recorded argv, environment or log" 0 "$leaks"
    assert_eq "$desc: no secret in limavm's output" 0 \
        "$(printf '%s' "$OUT" | grep -c -e "$FAKE_CLAUDE" -e "$FAKE_LGTMCP" -e "$FAKE_CODEX" "${GITHUB_SECRETS[@]}" || true)"
}

# --- 1. Static checks ---
assert_eq "no bash 4 constructs" "" \
    "$(grep -vE '^[[:space:]]*#' "$LIMAVM" | grep -nE 'mapfile|readarray|declare +-A|local +-n|\$\{[A-Za-z_0-9]+(,,|\^\^|@[LUu])' || true)"
assert_eq "limavm is run by the system bash" "#!/bin/bash" "$(head -1 "$LIMAVM")"
assert_eq "no eval" 0 "$(grep -cE '(^|[^a-z_])eval ' "$LIMAVM" || true)"
assert_eq "limavm is executable" yes "$([[ -x $LIMAVM ]] && echo yes || echo no)"
assert_eq "limavm does not use the Keychain" 0 "$(grep -ciE 'keychain|need security|find-generic-password' "$LIMAVM" || true)"

# --- 2. help and bad invocations ---
new_case
run_limavm help
assert_eq "help succeeds" 0 "$RC"
assert_contains "help lists base" "limavm base" "$OUT"
assert_contains "help lists new" "limavm new" "$OUT"
assert_contains "help lists github" "limavm github NAME [OWNER/REPO]" "$OUT"
assert_contains "help names --repo" "--repo OWNER/REPO" "$OUT"
assert_contains "help names --no-repo" "--no-repo" "$OUT"
assert_contains "help names --no-claude-token" "--no-claude-token" "$OUT"
assert_contains "help names --no-codex-auth" "--no-codex-auth" "$OUT"
assert_contains "help says where Codex keeps its login" 'cli_auth_credentials_store = "file"' "$OUT"
assert_contains "help lists claude-token" "limavm claude-token" "$OUT"
assert_contains "help says rm does not revoke" "does not revoke" "$OUT"
run_limavm
assert_eq "no command fails" 2 "$RC"
RC=0
OUT="$(cd "$CHECKOUT" && env -u HOME PATH="$STUBS:$PATH" LIMA_STUB_STATE="$STATE" LIMAVM_BASE=test-base /bin/bash "$LIMAVM" help 2>&1)" || RC=$?
assert_eq "help without HOME fails" 1 "$RC"
assert_contains "help without HOME says so" "limavm: HOME is not set" "$OUT"
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
shell --workdir $GUEST_DIR test-base git reset --mixed --quiet HEAD
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
assert_eq "base preserves the commit history" "$(git -C "$CHECKOUT" log --format=%H)" \
    "$(git -C "$STATE/guest" log --format=%H 2>/dev/null || true)"
assert_eq "base preserves the branch" main "$(git -C "$STATE/guest" branch --show-current)"
assert_eq "base preserves tags" initial "$(git -C "$STATE/guest" tag)"
assert_eq "base copies other branches" "$(git -C "$CHECKOUT" rev-parse other)" \
    "$(git -C "$STATE/guest" rev-parse refs/remotes/origin/other)"
assert_eq "base leaves a clean checkout clean" "" "$(git -C "$STATE/guest" status --porcelain)"
assert_eq "base uses HTTPS for the guest's GitHub origin" https://github.com/shields/dotfiles.git \
    "$(git -C "$STATE/guest" remote get-url origin 2>/dev/null || true)"

new_case
printf 'edited\n' > "$CHECKOUT/tracked.txt"
printf 'untracked\n' > "$CHECKOUT/new.txt"
rm "$CHECKOUT/obsolete.txt"
run_limavm base none
assert_eq "base with local changes succeeds" 0 "$RC"
assert_eq "base keeps local edits" edited "$(<"$STATE/guest/tracked.txt")"
assert_eq "base keeps untracked files" untracked "$(<"$STATE/guest/new.txt")"
assert_eq "base reports only the local changes" " D obsolete.txt
 M tracked.txt
?? new.txt" "$(git -C "$STATE/guest" status --porcelain)"
git -C "$CHECKOUT" checkout -- tracked.txt obsolete.txt
rm "$CHECKOUT/new.txt"

new_case
git -C "$CHECKOUT" checkout -q --detach
run_limavm base none
assert_eq "base with a detached HEAD succeeds" 0 "$RC"
assert_eq "base preserves a detached HEAD" "" "$(git -C "$STATE/guest" branch --show-current)"
assert_eq "base preserves the detached commit" "$(git -C "$CHECKOUT" rev-parse HEAD)" \
    "$(git -C "$STATE/guest" rev-parse HEAD)"
git -C "$CHECKOUT" checkout -q main

new_case
git -C "$CHECKOUT" worktree add -q -b worktree "$TMPBASE/worktree"
MAIN_CHECKOUT=$CHECKOUT
CHECKOUT="$TMPBASE/worktree"
printf 'worktree\n' > "$CHECKOUT/tracked.txt"
run_limavm base none
assert_eq "base from a linked worktree succeeds" 0 "$RC"
assert_eq "base preserves the worktree branch" worktree "$(git -C "$STATE/guest" branch --show-current)"
assert_eq "base preserves the worktree's changes" worktree "$(<"$STATE/guest/tracked.txt")"
assert_eq "the guest has an independent Git directory" yes "$([[ -d "$STATE/guest/.git" ]] && echo yes || echo no)"
assert_eq "the guest has no host object dependency" no "$([[ -e "$STATE/guest/.git/objects/info/alternates" ]] && echo yes || echo no)"
CHECKOUT=$MAIN_CHECKOUT

for origin in https://github.com/shields/dotfiles.git ssh://git@github.com/shields/dotfiles.git; do
    new_case
    git -C "$CHECKOUT" remote set-url origin "$origin"
    run_limavm base none
    assert_eq "base accepts a credential-free origin" 0 "$RC"
    assert_eq "base configures the HTTPS origin" https://github.com/shields/dotfiles.git \
        "$(git -C "$STATE/guest" remote get-url origin)"
done

origin_marker=LIMAVM_ORIGIN_CREDENTIAL
for origin in \
    "https://user:$origin_marker@github.com/shields/dotfiles.git" \
    "https://$origin_marker@github.com/shields/dotfiles.git" \
    "HTTPS://$origin_marker@github.com/shields/dotfiles.git" \
    "ssh://git:$origin_marker@github.com/shields/dotfiles.git" \
    "https://github.com/shields/dotfiles.git?token=$origin_marker" \
    "https://github.com/shields/dotfiles.git#$origin_marker"; do
    new_case test-base
    git -C "$CHECKOUT" remote set-url origin "$origin"
    run_limavm base none
    assert_eq "base rejects an origin that can carry credentials" 1 "$RC"
    assert_contains "base explains the origin rejection" "origin must not contain" "$OUT"
    assert_eq "a rejected origin touches no VM" "" "$(limactl_log)"
    assert_eq "origin credentials reach no command arguments or environment" no \
        "$(grep -Fq "$origin_marker" "$STATE"/calls/*.argv "$STATE"/calls/*.env && echo yes || echo no)"
    assert_eq "origin credentials are not printed" no "$([[ $OUT == *$origin_marker* ]] && echo yes || echo no)"
    assert_eq "a rejected origin leaves no staging directory" 0 "$(staging_dirs_left)"
done
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_history_case() {
    new_case test-base
    CHECKOUT="$TMPBASE/history-$CASE_N"
    git clone -q --no-local "$MAIN_CHECKOUT" "$CHECKOUT"
    git -C "$CHECKOUT" config user.name Test
    git -C "$CHECKOUT" config user.email test@example.com
    git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git
    cat > "$STATE/gitleaks.toml" <<'EOF'
[[rules]]
id = "history-marker"
description = "Lima history regression marker"
regex = '''LIMAVM_HISTORY_SENTINEL'''
EOF
    EXTRA_ENV+=(GITLEAKS_CONFIG="$STATE/gitleaks.toml")
}

assert_history_rejected() {
    local desc=$1 filename=$2
    assert_eq "$desc fails base" 1 "$RC"
    assert_contains "$desc names the affected file" "$filename" "$OUT"
    assert_contains "$desc reports the history scan failure" "gitleaks found secrets in the repository history, or failed" "$OUT"
    assert_eq "$desc touches no VM" "" "$(limactl_log)"
    assert_eq "$desc leaves no staging directory" 0 "$(staging_dirs_left)"
    assert_eq "$desc is redacted" no "$([[ $OUT == *LIMAVM_HISTORY_SENTINEL* ]] && echo yes || echo no)"
}

new_history_case
git -C "$CHECKOUT" checkout -qb merge-side
printf 'side\n' > "$CHECKOUT/side.txt"
git -C "$CHECKOUT" add side.txt
git -C "$CHECKOUT" commit -qm side
git -C "$CHECKOUT" checkout -q main
printf 'main\n' > "$CHECKOUT/main.txt"
git -C "$CHECKOUT" add main.txt
git -C "$CHECKOUT" commit -qm main
git -C "$CHECKOUT" merge --no-ff --no-commit merge-side >/dev/null 2>&1
printf 'LIMAVM_HISTORY_SENTINEL\n' > "$CHECKOUT/merge-only.txt"
git -C "$CHECKOUT" add merge-only.txt
git -C "$CHECKOUT" commit -qm merge
git -C "$CHECKOUT" rm -q merge-only.txt
git -C "$CHECKOUT" commit -qm remove
run_limavm base none
assert_history_rejected "a secret introduced only by a merge and then removed" merge-only.txt
CHECKOUT=$MAIN_CHECKOUT

new_history_case
git -C "$CHECKOUT" checkout -qb unmerged
printf 'LIMAVM_HISTORY_SENTINEL\n' > "$CHECKOUT/unmerged-only.txt"
git -C "$CHECKOUT" add unmerged-only.txt
git -C "$CHECKOUT" commit -qm unmerged
git -C "$CHECKOUT" checkout -q main
git -C "$CHECKOUT" remote remove origin
run_limavm base none
assert_history_rejected "a secret on an unmerged branch with no origin" unmerged-only.txt
CHECKOUT=$MAIN_CHECKOUT

new_case
git -C "$CHECKOUT" remote remove origin
run_limavm base none
assert_eq "base accepts a safe repository without an origin" 0 "$RC"
assert_eq "a source without an origin leaves no host remote in the guest" "" "$(git -C "$STATE/guest" remote)"
assert_eq "base without an origin preserves history" "$(git -C "$CHECKOUT" log --format=%H)" \
    "$(git -C "$STATE/guest" log --format=%H)"
git -C "$CHECKOUT" remote add origin git@github.com:shields/dotfiles.git

# --- 4. base passes modules and sizes through ---
new_case
run_limavm base dev data
assert_contains "base passes the modules" "./provision.sh dev data" "$(limactl_log)"
assert_eq "base without a size passes none" 0 "$(count_in_log '--cpus')"
new_case
EXTRA_ENV+=(LIMAVM_CPUS=4 LIMAVM_MEMORY=8)
run_limavm base none
assert_contains "base passes the size to start" \
    "start --tty=false --name=test-base --cpus 4 --memory 8 CHECKOUT/lima/dev.yaml" "$(limactl_log)"
new_case
EXTRA_ENV+=(LIMAVM_CPUS=many)
run_limavm base none
assert_eq "a bad LIMAVM_CPUS fails" 1 "$RC"
assert_eq "a bad LIMAVM_CPUS touches no VM" "" "$(limactl_log)"
new_case
EXTRA_ENV+=(LIMAVM_MEMORY=8GiB)
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
    REAL_GIT="$REAL_GIT" LIMAVM_BASE=test-base /bin/bash "$LIMAVM" base 2>&1 </dev/null)" || RC=$?
assert_eq "base outside a repository fails" 1 "$RC"
assert_contains "base outside a repository says so" "inside a dotfiles checkout" "$OUT"
mkdir "$TMPBASE/other-repo"
git -C "$TMPBASE/other-repo" init -q
RC=0
OUT="$(cd "$TMPBASE/other-repo" && env PATH="$STUBS:$PATH" HOME="$HOME_DIR" LIMA_STUB_STATE="$STATE" \
    REAL_GIT="$REAL_GIT" LIMAVM_BASE=test-base /bin/bash "$LIMAVM" base 2>&1 </dev/null)" || RC=$?
assert_eq "base in another repository fails" 1 "$RC"
assert_contains "base in another repository says so" "is not a dotfiles checkout" "$OUT"
assert_eq "neither touched limactl" "" "$(limactl_log)"

# --- 7. base failures ---
new_case
EXTRA_ENV+=(STAGE_STUB_FAIL=1)
run_limavm base none
assert_eq "a staging failure fails base" 1 "$RC"
assert_eq "a staging failure starts no VM" "" "$(limactl_log)"
assert_eq "a staging failure removes the staging directory" 0 "$(staging_dirs_left)"

new_case test-base
EXTRA_ENV+=(HISTORY_SCAN_FAIL=1)
run_limavm base none
assert_eq "a history scan failure fails base" 1 "$RC"
assert_eq "a history scan failure touches no VM" "" "$(limactl_log)"
assert_eq "a history scan failure removes the staging directory" 0 "$(staging_dirs_left)"
assert_contains "a history scan failure says why" "gitleaks found secrets in the repository history, or failed" "$OUT"

new_case
echo 3 > "$STATE/mounts"
run_limavm base none
assert_eq "a mounted base fails" 1 "$RC"
assert_contains "a mounted base says why" "mounts 3 host directories" "$OUT"
assert_eq "a mounted base runs no provisioning" 0 "$(count_in_log provision)"

new_case
EXTRA_ENV+=(LIMA_STUB_FAIL=./provision.sh)
run_limavm base none
assert_eq "a provisioning failure fails base" 1 "$RC"
assert_eq "a provisioning failure stops before the identity reset and the protection" 0 \
    "$(grep -cE 'machine-id|reset-identity|protect|stop' "$STATE/limactl.log" || true)"
assert_contains "a provisioning failure keeps the VM for inspection" "left for inspection" "$OUT"
assert_eq "a provisioning failure keeps the VM" 1 "$(grep -cx test-base "$STATE/instances")"
assert_eq "a provisioning failure removes the staging directory" 0 "$(staging_dirs_left)"

new_case
: > "$STATE/guest-home-unknown"
run_limavm base none
assert_eq "an unknown guest home fails base" 1 "$RC"
assert_contains "an unknown guest home in base says so" "cannot tell the home directory in test-base" "$OUT"
assert_eq "an unknown guest home in base runs no provisioning" 0 "$(count_in_log provision)"
assert_contains "an unknown guest home in base keeps the VM for inspection" "left for inspection" "$OUT"
assert_eq "an unknown guest home in base removes the staging directory" 0 "$(staging_dirs_left)"

# --- 8. new with a repository ---
new_case test-base
start_fake
before=$(date +%s)
run_limavm new t1 --repo shields/dotfiles
after=$(date +%s)
assert_eq "new --repo succeeds" 0 "$RC"
assert_eq "new --repo call order" "list -q test-base
list -q t1
clone --tty=false --start test-base t1
shell t1 /home/linuxbrew/.linuxbrew/bin/brew upgrade --cask claude-code@latest codex
$SETUP GITHUB_APP_AUTH
shell t1 sh -c printf %s \"\$HOME\"
$SETUP CLAUDE_CODE_OAUTH_TOKEN
$SETUP CODEX_AUTH
$SETUP LGTMCP_CONFIG
shell --workdir $GUEST_DIR t1" "$(limactl_log)"
assert_eq "the repository the base holds is not cloned again" 0 "$(count_in_log 'git clone')"
assert_contains "new says the shell opens in the base's checkout" "the checkout from test-base is at $GUEST_DIR" "$OUT"
assert_eq "gh is asked for the repository's id, once" "api repos/shields/dotfiles --jq .id" "$(<"$STATE/gh.log")"
assert_eq "the fake GitHub saw the device flow and one poll" "POST /login/device/code
POST /login/oauth/access_token" "$(fake_requests)"
assert_eq "the device flow starts with the client id of the app" "$CLIENT_ID" \
    "$(fake_form /login/device/code client_id)"
assert_eq "the poll names the repository by id" 4242 "$(fake_form /login/oauth/access_token repository_id)"
assert_eq "the poll carries the device code" fake-device-code "$(fake_form /login/oauth/access_token device_code)"
assert_eq "the poll uses the device grant" urn:ietf:params:oauth:grant-type:device_code \
    "$(fake_form /login/oauth/access_token grant_type)"
assert_eq "the poll names the client id" "$CLIENT_ID" "$(fake_form /login/oauth/access_token client_id)"
assert_eq "limavm opens the verification page" "$FAKE_URL/login/device" "$(<"$STATE/open.log")"
assert_contains "new shows the user code" "WDJB-MJHT" "$OUT"
assert_contains "new shows the verification page" "$FAKE_URL/login/device" "$OUT"
assert_eq "the user code goes to the clipboard, with no newline" same \
    "$(printf '%s' WDJB-MJHT | cmp -s - "$STATE/pbcopy.stdin" && echo same || echo different)"
assert_eq "pbcopy is run once" 1 "$(stub_calls pbcopy)"
assert_eq "pbcopy gets no arguments" "" "$(cat "$STATE"/calls/pbcopy.*.argv)"
assert_contains "new says the code is on the clipboard" "the code is on the clipboard" "$OUT"
record="$(stdin_of 5)"
assert_eq "the record is one JSON object on one line" 1 "$(printf '%s\n' "$record" | wc -l | tr -d ' ')"
assert_eq "the record names the client id" "$CLIENT_ID" "$(jq -r .client_id <<<"$record")"
assert_eq "the record names the repository" shields/dotfiles "$(jq -r .repository <<<"$record")"
assert_eq "the record holds the access token" ghu_fake-access-1 "$(jq -r .access_token <<<"$record")"
assert_eq "the record holds the refresh token" ghr_fake-refresh-1 "$(jq -r .refresh_token <<<"$record")"
assert_eq "the record holds the web URL" "$FAKE_URL" "$(jq -r .web_url <<<"$record")"
assert_eq "the record holds the API URL" "$FAKE_URL/api/v3" "$(jq -r .api_url <<<"$record")"
assert_eq "the record's access token lasts 8 hours" 1 \
    "$(jq --argjson low $(( before + 28800 )) --argjson high $(( after + 28800 )) \
        '.access_expires_at >= $low and .access_expires_at <= $high' <<<"$record" | grep -c true)"
assert_eq "the record's refresh token lasts 6 months" 1 \
    "$(jq --argjson low $(( before + 15811200 )) --argjson high $(( after + 15811200 )) \
        '.refresh_expires_at >= $low and .refresh_expires_at <= $high' <<<"$record" | grep -c true)"
assert_eq "the token manager accepts the record, and reads its fields the same way" \
    "shields/dotfiles $CLIENT_ID $FAKE_URL $FAKE_URL/api/v3" \
    "$(printf '%s\n' "$record" | python3 -c '
import importlib.util
import sys

spec = importlib.util.spec_from_file_location("github_app_token", sys.argv[1])
assert spec is not None and spec.loader is not None
module = importlib.util.module_from_spec(spec)
sys.modules["github_app_token"] = module
spec.loader.exec_module(module)
parsed = module.parse_record(sys.stdin.read())
print(parsed.repository, parsed.client_id, parsed.web_url, parsed.api_url)
' "$HERE/../bin/github_app_token.py")"
assert_eq "the Claude token reaches setup-secrets on stdin, with a newline" same \
    "$(print -r -- "$FAKE_CLAUDE" | cmp -s - "$STATE/calls/7.stdin" && echo same || echo different)"
assert_eq "the Codex login reaches setup-secrets on stdin, byte for byte" same \
    "$(print -rn -- "$CODEX_JSON" | cmp -s - "$STATE/calls/8.stdin" && echo same || echo different)"
assert_eq "the LGTMCP config reaches setup-secrets on stdin, byte for byte" same \
    "$(print -rn -- "$LGTMCP_YAML" | cmp -s - "$STATE/calls/9.stdin" && echo same || echo different)"
assert_eq "the upgrade call has no stdin" 0 "$(wc -c < "$STATE/calls/4.stdin" | tr -d ' ')"
assert_no_secret_leak "new --repo"
assert_eq "curl was run for the flow" 2 "$(stub_calls curl)"
assert_eq "limavm never calls security" 0 "$(cat "$STATE/security.log" 2>/dev/null | wc -l | tr -d ' ')"
assert_contains "new says what the VM can reach" "shields/dotfiles only" "$OUT"
assert_eq "new with a Codex login does not ask for a Codex login" 0 "$(printf '%s' "$OUT" | grep -c 'codex login' || true)"

# --- 9. new without a repository, and without a Claude token ---
new_case test-base
run_limavm new t1
assert_eq "new without --repo succeeds" 0 "$RC"
assert_eq "new without --repo installs no GitHub authorization" "list -q test-base
list -q t1
clone --tty=false --start test-base t1
shell t1 /home/linuxbrew/.linuxbrew/bin/brew upgrade --cask claude-code@latest codex
$SETUP CLAUDE_CODE_OAUTH_TOKEN
$SETUP CODEX_AUTH
$SETUP LGTMCP_CONFIG
shell t1" "$(limactl_log)"
assert_eq "new without --repo never runs gh" 0 "$(stub_calls gh)"
assert_eq "new without --repo never runs curl" 0 "$(stub_calls curl)"
assert_eq "new without --repo never opens a browser" 0 "$([[ -e "$STATE/open.log" ]] && echo 1 || echo 0)"
assert_contains "new without --repo says the VM has no GitHub access" "no GitHub access" "$OUT"
assert_contains "new without --repo says how to add some" "limavm github t1 OWNER/REPO" "$OUT"
assert_contains "new without a token file says how to keep the token" "limavm claude-token" "$OUT"
assert_no_secret_leak "new without --repo"

new_case test-base
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1 --no-claude-token
assert_eq "new --no-claude-token succeeds without a prompt" 0 "$RC"
assert_eq "new --no-claude-token installs no Claude token" "list -q test-base
list -q t1
clone --tty=false --start test-base t1
shell t1 /home/linuxbrew/.linuxbrew/bin/brew upgrade --cask claude-code@latest codex
$SETUP CODEX_AUTH
$SETUP LGTMCP_CONFIG
shell t1" "$(limactl_log)"
assert_contains "new --no-claude-token says to use claude auth login" "claude auth login" "$OUT"

new_case test-base
start_fake
run_limavm new t1 --no-claude-token --repo=shields/dotfiles
assert_eq "new --repo=OWNER/REPO --no-claude-token succeeds" 0 "$RC"
assert_eq "the GitHub authorization is the only secret but the Codex login and the LGTMCP config" "$SETUP GITHUB_APP_AUTH
$SETUP CODEX_AUTH
$SETUP LGTMCP_CONFIG" \
    "$(limactl_log | sed -n '5p;7,8p')"

# --- 9b. The repository comes from the checkout's origin ---
# The fake GitHub is on 127.0.0.1, so an origin there is on GitHub's host, and
# the checkout's usual github.com origin is not.
ORIGIN_CASES=(
    'git@127.0.0.1:shields/other.git'
    'git@127.0.0.1:shields/other'
    'git@127.0.0.1:/shields/other.git'
    'ssh://git@127.0.0.1/shields/other.git'
    'ssh://git@127.0.0.1/shields/other/'
    'ssh://git@127.0.0.1:2222/shields/other.git'
    'ssh://git@127.0.0.1:2222/shields/other'
    'https://127.0.0.1/shields/other.git'
    'https://127.0.0.1/shields/other'
    'https://127.0.0.1/shields/other.git/'
)
for origin in "${ORIGIN_CASES[@]}"; do
    new_case test-base
    start_fake
    git -C "$CHECKOUT" remote set-url origin "$origin"
    run_limavm new t1
    assert_eq "new infers the repository from the origin $origin" 0 "$RC"
    assert_contains "new says where the repository came from: $origin" \
        "GitHub access for shields/other, from the origin of this checkout" "$OUT"
    assert_eq "the inferred repository is authorized and cloned: $origin" "$SETUP GITHUB_APP_AUTH
shell t1 sh -c printf %s \"\$HOME\"
shell t1 env GIT_TERMINAL_PROMPT=0 git clone --quiet $FAKE_URL/shields/other.git $OTHER_DIR" "$(limactl_log | sed -n '5,7p')"
    assert_eq "the clone call has no stdin: $origin" 0 "$(wc -c < "$STATE/calls/7.stdin" | tr -d ' ')"
    assert_eq "the shell opens in the clone: $origin" "shell --workdir $OTHER_DIR t1" "$(limactl_log | tail -1)"
    assert_contains "new says where the repository is cloned: $origin" "cloned at $OTHER_DIR" "$OUT"
    assert_eq "the inferred repository is looked up by name: $origin" "api repos/shields/other --jq .id" "$(<"$STATE/gh.log")"
    assert_eq "the record names the inferred repository: $origin" shields/other "$(stdin_of 5 | jq -r .repository)"
    assert_no_secret_leak "an inferred repository: $origin"
done
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_case test-base
start_fake
git -C "$CHECKOUT" remote set-url origin git@127.0.0.1:shields/dotfiles.git
run_limavm new t1
assert_eq "new infers the repository the base holds" 0 "$RC"
assert_eq "the repository the base holds is authorized, not cloned" "$SETUP GITHUB_APP_AUTH
shell t1 sh -c printf %s \"\$HOME\"
$SETUP CLAUDE_CODE_OAUTH_TOKEN" "$(limactl_log | sed -n '5,7p')"
assert_eq "the shell opens in the base's checkout" "shell --workdir $GUEST_DIR t1" "$(limactl_log | tail -1)"
assert_contains "new says the shell opens in the base's checkout" "the checkout from test-base is at $GUEST_DIR" "$OUT"
assert_eq "the record names the repository the base holds" shields/dotfiles "$(stdin_of 5 | jq -r .repository)"
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_case test-base
start_fake
run_limavm new t1 --repo Shields/Dotfiles
assert_eq "the repository the base holds is known in any case" 0 "$RC"
assert_eq "the repository the base holds, named in another case, is not cloned" 0 "$(count_in_log 'git clone')"
assert_eq "the shell opens in the base's checkout for the name in another case" "shell --workdir $GUEST_DIR t1" "$(limactl_log | tail -1)"
assert_contains "new names the base's checkout for the name in another case" "the checkout from test-base is at $GUEST_DIR" "$OUT"
assert_eq "the record keeps the name as given" Shields/Dotfiles "$(stdin_of 5 | jq -r .repository)"

new_case test-base
start_fake
git -C "$CHECKOUT" remote set-url origin git@127.0.0.1:shields/dotfiles.git
run_limavm new t1 --no-repo
assert_eq "new --no-repo succeeds" 0 "$RC"
assert_eq "new --no-repo installs no GitHub authorization" 0 "$(count_in_log GITHUB_APP_AUTH)"
assert_eq "new --no-repo clones nothing in the VM" 0 "$(count_in_log 'git clone')"
assert_eq "new --no-repo never runs gh" 0 "$(stub_calls gh)"
assert_eq "new --no-repo contacts no GitHub" "" "$(fake_requests)"
assert_eq "new --no-repo opens the shell at home" "shell t1" "$(limactl_log | tail -1)"
assert_contains "new --no-repo says the VM has no GitHub access" "no GitHub access" "$OUT"

new_case test-base
start_fake
git -C "$CHECKOUT" remote set-url origin git@127.0.0.1:shields/dotfiles.git
run_limavm new t1 --repo shields/other
assert_eq "--repo wins over the origin" 0 "$RC"
assert_eq "--repo's repository is the one looked up" "api repos/shields/other --jq .id" "$(<"$STATE/gh.log")"
assert_eq "--repo's repository is the one cloned" 1 "$(count_in_log "env GIT_TERMINAL_PROMPT=0 git clone --quiet $FAKE_URL/shields/other.git /home/guest/src/github.com/shields/other")"
assert_eq "--repo's repository is in the record" shields/other "$(stdin_of 5 | jq -r .repository)"
assert_eq "the origin's repository is not mentioned" 0 "$(printf '%s' "$OUT" | grep -c 'from the origin' || true)"

new_case test-base
start_fake
git -C "$CHECKOUT" remote set-url origin git@127.0.0.1:shields/dotfiles.git
run_limavm new t1 --repo shields/other --no-repo
assert_eq "--repo with --no-repo fails" 1 "$RC"
assert_contains "--repo with --no-repo says they exclude each other" "exclude each other" "$OUT"
assert_eq "--repo with --no-repo touches no VM" "" "$(limactl_log)"

for origin in \
    'git@github.com:shields/dotfiles.git' \
    'ssh://git@gitlab.com/shields/dotfiles.git' \
    'https://127.0.0.2/shields/dotfiles.git' \
    'https://127.0.0.1.example.com/shields/dotfiles.git' \
    'https://user@127.0.0.1/shields/dotfiles.git' \
    '/srv/git/shields/dotfiles.git'; do
    new_case test-base
    start_fake
    git -C "$CHECKOUT" remote set-url origin "$origin"
    run_limavm new t1
    assert_eq "an origin elsewhere gives no GitHub access: $origin" 0 "$RC"
    assert_eq "an origin elsewhere installs no GitHub authorization: $origin" 0 "$(count_in_log GITHUB_APP_AUTH)"
    assert_eq "an origin elsewhere never runs gh: $origin" 0 "$(stub_calls gh)"
    assert_eq "an origin elsewhere contacts no GitHub: $origin" "" "$(fake_requests)"
    assert_eq "an origin elsewhere is not mentioned: $origin" 0 "$(printf '%s' "$OUT" | grep -c 'from the origin' || true)"
done
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_case test-base
: > "$STATE/gh-fail"
EXTRA_ENV+=(LIMAVM_GITHUB_WEB_URL=http://localhost:9)
git -C "$CHECKOUT" remote set-url origin git@LOCALHOST:shields/other.git
run_limavm new t1
assert_contains "an origin host that differs from GitHub's only in case is inferred" \
    "GitHub access for shields/other, from the origin of this checkout" "$OUT"
assert_contains "an origin host that differs only in case goes on to gh" "gh cannot read shields/other" "$OUT"
assert_eq "an origin host that differs only in case touches no VM" "list -q test-base
list -q t1" "$(limactl_log)"
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_case test-base
: > "$STATE/gh-fail"
EXTRA_ENV+=(LIMAVM_GITHUB_WEB_URL=http://LOCALHOST:9)
git -C "$CHECKOUT" remote set-url origin git@localhost:shields/other.git
run_limavm new t1
assert_contains "a GitHub host that differs from the origin's only in case is inferred" \
    "GitHub access for shields/other, from the origin of this checkout" "$OUT"
assert_contains "a GitHub host that differs only in case goes on to gh" "gh cannot read shields/other" "$OUT"
assert_eq "a GitHub host that differs only in case touches no VM" "list -q test-base
list -q t1" "$(limactl_log)"
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

# An empty LIMAVM_GITHUB_WEB_URL leaves limavm its default, https://github.com;
# the failing gh stub ends the run before anything would be sent there.
for origin in git@github.com:shields/other.git https://github.com/shields/other.git; do
    new_case test-base
    : > "$STATE/gh-fail"
    EXTRA_ENV+=(LIMAVM_GITHUB_WEB_URL=)
    git -C "$CHECKOUT" remote set-url origin "$origin"
    run_limavm new t1
    assert_contains "an origin on github.com is inferred with the default address: $origin" \
        "GitHub access for shields/other, from the origin of this checkout" "$OUT"
    assert_contains "an origin on github.com goes on to gh with the default address: $origin" "gh cannot read shields/other" "$OUT"
    assert_eq "an origin on github.com with the default address runs no curl: $origin" 0 "$(stub_calls curl)"
    assert_eq "an origin on github.com with the default address touches no VM: $origin" "list -q test-base
list -q t1" "$(limactl_log)"
done
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

for origin in 'https://127.0.0.1/shields/dotfiles/more.git' 'ssh://git@127.0.0.1:2222/shields/dotfiles.git/extra'; do
    new_case test-base
    start_fake
    git -C "$CHECKOUT" remote set-url origin "$origin"
    run_limavm new t1
    assert_eq "a GitHub origin whose path is not OWNER/REPO fails new: $origin" 1 "$RC"
    assert_contains "a GitHub origin whose path is not OWNER/REPO says where the value came from: $origin" \
        "from the origin of this checkout (--no-repo gives none)" "$OUT"
    assert_contains "a GitHub origin whose path is not OWNER/REPO says what is expected: $origin" "OWNER/REPO" "$OUT"
    assert_eq "a GitHub origin whose path is not OWNER/REPO touches no VM: $origin" "" "$(limactl_log)"
    assert_eq "a GitHub origin whose path is not OWNER/REPO contacts no GitHub: $origin" "" "$(fake_requests)"
done
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_case test-base
start_fake
git -C "$CHECKOUT" remote remove origin
run_limavm new t1
assert_eq "a checkout without an origin gives no GitHub access" 0 "$RC"
assert_eq "a checkout without an origin installs no GitHub authorization" 0 "$(count_in_log GITHUB_APP_AUTH)"
assert_eq "a checkout without an origin contacts no GitHub" "" "$(fake_requests)"
git -C "$CHECKOUT" remote add origin git@github.com:shields/dotfiles.git

new_case test-base
start_fake
mkdir -p "$TMPBASE/not-a-repo"
MAIN_CHECKOUT=$CHECKOUT
CHECKOUT="$TMPBASE/not-a-repo"
run_limavm new t1
assert_eq "new outside a work tree gives no GitHub access" 0 "$RC"
assert_eq "new outside a work tree installs no GitHub authorization" 0 "$(count_in_log GITHUB_APP_AUTH)"
assert_eq "new outside a work tree contacts no GitHub" "" "$(fake_requests)"
CHECKOUT=$MAIN_CHECKOUT

new_case test-base
start_fake
EXTRA_ENV+=(LIMA_STUB_FAIL='git clone')
run_limavm new t1 --repo shields/other
assert_eq "a failing clone in the VM fails new" 1 "$RC"
assert_eq "a failing clone in the VM sends no later secret" "$SETUP GITHUB_APP_AUTH" "$(limactl_log | grep setup-secrets)"
assert_eq "a failing clone in the VM deletes the VM last" "delete --tty=false --force t1" "$(limactl_log | tail -1)"
assert_eq "a failing clone in the VM leaves no t1" 0 "$(grep -cx t1 "$STATE/instances")"
assert_no_secret_leak "a failing clone in the VM"

new_case test-base
start_fake
: > "$STATE/guest-home-unknown"
run_limavm new t1 --repo shields/other
assert_eq "an unknown guest home fails new" 1 "$RC"
assert_contains "an unknown guest home in new says so" "cannot tell the home directory in t1" "$OUT"
assert_eq "an unknown guest home in new clones nothing and sends no later secret" "$SETUP GITHUB_APP_AUTH" \
    "$(limactl_log | grep -e setup-secrets -e 'git clone')"
assert_eq "an unknown guest home in new deletes the VM last" "delete --tty=false --force t1" "$(limactl_log | tail -1)"
assert_no_secret_leak "an unknown guest home in new"

# --- 9a. The Codex login ---
new_case test-base
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1 --no-claude-token --no-codex-auth
assert_eq "new --no-codex-auth succeeds" 0 "$RC"
assert_eq "new --no-codex-auth installs no Codex login" "list -q test-base
list -q t1
clone --tty=false --start test-base t1
shell t1 /home/linuxbrew/.linuxbrew/bin/brew upgrade --cask claude-code@latest codex
$SETUP LGTMCP_CONFIG
shell t1" "$(limactl_log)"
assert_contains "new --no-codex-auth says how to sign Codex in" "codex login --device-auth" "$OUT"
assert_contains "new --no-codex-auth mentions the ChatGPT setting" "ChatGPT" "$OUT"
assert_no_secret_leak "new --no-codex-auth"

new_case test-base
rm "$HOME_DIR/.codex/auth.json"
run_limavm new t1 --no-codex-auth
assert_eq "new --no-codex-auth needs no Codex login on the Mac" 0 "$RC"

new_case test-base
rm "$HOME_DIR/.codex/auth.json"
run_limavm new t1
assert_eq "a missing Codex login fails new" 1 "$RC"
assert_contains "a missing Codex login names the file" "$HOME_DIR/.codex/auth.json does not exist" "$OUT"
assert_contains "a missing Codex login says what to run" "codex login" "$OUT"
assert_contains "a missing Codex login says where Codex keeps it otherwise" 'cli_auth_credentials_store in ~/.codex/config.toml is keyring or auto: set it to "file"' "$OUT"
assert_contains "a missing Codex login points to the flag" "--no-codex-auth" "$OUT"
assert_eq "a missing Codex login comes before the clone" "list -q test-base
list -q t1" "$(limactl_log)"

new_case test-base
rm "$HOME_DIR/.codex/auth.json"
mkdir "$HOME_DIR/.codex/auth.json"
run_limavm new t1
assert_eq "a directory at the Codex login fails new" 1 "$RC"
assert_contains "a directory at the Codex login is named" "$HOME_DIR/.codex/auth.json is not a regular file" "$OUT"
assert_contains "a directory at the Codex login points to the flag" "--no-codex-auth" "$OUT"
assert_eq "a directory at the Codex login comes before the clone" "list -q test-base
list -q t1" "$(limactl_log)"

new_case test-base
: > "$HOME_DIR/.codex/auth.json"
run_limavm new t1
assert_eq "an empty Codex login fails new" 1 "$RC"
assert_contains "an empty Codex login is named" "$HOME_DIR/.codex/auth.json is empty" "$OUT"
assert_contains "an empty Codex login points to the flag" "--no-codex-auth" "$OUT"
assert_eq "an empty Codex login comes before the clone" "list -q test-base
list -q t1" "$(limactl_log)"

new_case test-base
chmod 000 "$HOME_DIR/.codex/auth.json"
run_limavm new t1
assert_eq "an unreadable Codex login fails new" 1 "$RC"
assert_contains "an unreadable Codex login names the file" "cannot read $HOME_DIR/.codex/auth.json" "$OUT"
assert_eq "an unreadable Codex login clones nothing" 0 "$(count_in_log clone)"
chmod 600 "$HOME_DIR/.codex/auth.json"

for content in "\"$FAKE_CODEX\"" '[]' 'not json' '' "{\"tokens\": \"$FAKE_CODEX\"" '{} {}'; do
    new_case test-base
    start_fake
    printf '%s\n' "$content" > "$HOME_DIR/.codex/auth.json"
    run_limavm new t1 --repo shields/dotfiles
    assert_eq "a Codex login that is not a JSON object fails new: $content" 1 "$RC"
    assert_contains "a Codex login that is not a JSON object is named: $content" "$HOME_DIR/.codex/auth.json" "$OUT"
    assert_contains "a Codex login that is not a JSON object points to the flag: $content" "--no-codex-auth" "$OUT"
    assert_eq "a Codex login that is not a JSON object comes before the clone: $content" "list -q test-base
list -q t1" "$(limactl_log)"
    assert_eq "a Codex login that is not a JSON object contacts no GitHub: $content" "" "$(fake_requests)"
    assert_no_secret_leak "a Codex login that is not a JSON object: $content"
done

# --- 10. The Claude token prompt ---
new_case test-base
printf '  %s \r\n' "$FAKE_CLAUDE" > "$STATE/prompt"
run_limavm new t1
assert_eq "a token with blanks around it is accepted" 0 "$RC"
assert_eq "a token is trimmed before it is installed" same \
    "$(print -r -- "$FAKE_CLAUDE" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"

new_case test-base
EXTRA_ENV+=(CLAUDE_CODE_OAUTH_TOKEN=from-the-environment-0a1b)
run_limavm new t1
assert_eq "a token in the environment is not used" same \
    "$(print -r -- "$FAKE_CLAUDE" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"
assert_eq "a token in the environment is not passed on" 0 \
    "$(cat "$STATE"/calls/*.stdin "$STATE"/calls/*.argv | grep -c from-the-environment-0a1b || true)"

for content in '' $'\n' $'  \r\n'; do
    new_case test-base
    start_fake
    printf '%s' "$content" > "$STATE/prompt"
    run_limavm new t1 --repo shields/dotfiles
    assert_eq "an empty Claude token fails" 1 "$RC"
    assert_contains "an empty Claude token says so" "the Claude token is empty" "$OUT"
    assert_contains "an empty Claude token points to the flag" "--no-claude-token" "$OUT"
    assert_eq "an empty Claude token comes before the clone" "list -q test-base
list -q t1" "$(limactl_log)"
    assert_eq "an empty Claude token never runs gh" 0 "$(stub_calls gh)"
    assert_eq "an empty Claude token contacts no GitHub" "" "$(fake_requests)"
done

new_case test-base
print -r -- 'sk-ant one two' > "$STATE/prompt"
run_limavm new t1
assert_eq "a Claude token with spaces in it fails" 1 "$RC"
assert_eq "a Claude token with spaces in it clones nothing" 0 "$(count_in_log clone)"
assert_eq "a Claude token with spaces in it is not echoed" 0 "$(printf '%s' "$OUT" | grep -c 'sk-ant' || true)"

new_case test-base
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1
assert_eq "an unreadable prompt file fails" 1 "$RC"
assert_eq "an unreadable prompt file clones nothing" 0 "$(count_in_log clone)"

# --- 10a. The Claude token file on the Mac ---
TOKEN_FILE_SUFFIX=.config/secrets/CLAUDE_CODE_OAUTH_TOKEN
FILE_CLAUDE='sk-ant-oat01-FAKEfileToken-9b2d'
write_token_file() {
    mkdir -p "$HOME_DIR/.config/secrets"
    printf '%s' "$1" > "$HOME_DIR/$TOKEN_FILE_SUFFIX"
}

new_case test-base
write_token_file "$FILE_CLAUDE"$'\n'
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1
assert_eq "a token file means no prompt" 0 "$RC"
assert_eq "the token file's token reaches setup-secrets on stdin, with a newline" same \
    "$(print -r -- "$FILE_CLAUDE" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"
assert_eq "the token file's token is in no argument list, environment or log" 0 \
    "$(cat "$STATE"/calls/*.argv "$STATE"/calls/*.env "$STATE/limactl.log" | grep -c "$FILE_CLAUDE" || true)"
assert_eq "the token file's token is not printed" 0 "$(printf '%s' "$OUT" | grep -c "$FILE_CLAUDE" || true)"
assert_eq "a token file means no hint about keeping the token" 0 "$(printf '%s' "$OUT" | grep -c 'limavm claude-token' || true)"

new_case test-base
write_token_file $'  \r\n'"$FILE_CLAUDE"$'  \r\n\n'
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1
assert_eq "a token file with blanks around the token is accepted" 0 "$RC"
assert_eq "a token file's token is trimmed before it is installed" same \
    "$(print -r -- "$FILE_CLAUDE" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"

for content in '' $'\n' $'  \r\n'; do
    new_case test-base
    write_token_file "$content"
    run_limavm new t1
    assert_eq "an empty token file fails" 1 "$RC"
    assert_contains "an empty token file is named" "$HOME_DIR/$TOKEN_FILE_SUFFIX is empty" "$OUT"
    assert_contains "an empty token file points to claude-token" "limavm claude-token" "$OUT"
    assert_eq "an empty token file comes before the clone" "list -q test-base
list -q t1" "$(limactl_log)"
done

new_case test-base
write_token_file 'sk-ant one two'
run_limavm new t1
assert_eq "a token file with two words fails" 1 "$RC"
assert_contains "a token file with two words is named" "$HOME_DIR/$TOKEN_FILE_SUFFIX" "$OUT"
assert_eq "a token file with two words clones nothing" 0 "$(count_in_log clone)"
assert_eq "a token file with two words is not echoed" 0 "$(printf '%s' "$OUT" | grep -c 'sk-ant' || true)"

new_case test-base
write_token_file "$FILE_CLAUDE"
chmod 000 "$HOME_DIR/$TOKEN_FILE_SUFFIX"
run_limavm new t1
assert_eq "an unreadable token file fails" 1 "$RC"
assert_contains "an unreadable token file is named" "cannot read $HOME_DIR/$TOKEN_FILE_SUFFIX" "$OUT"
assert_eq "an unreadable token file clones nothing" 0 "$(count_in_log clone)"
chmod 600 "$HOME_DIR/$TOKEN_FILE_SUFFIX"

new_case test-base
mkdir -p "$HOME_DIR/$TOKEN_FILE_SUFFIX"
run_limavm new t1
assert_eq "a directory at the token file fails new" 1 "$RC"
assert_contains "a directory at the token file is named" "$HOME_DIR/$TOKEN_FILE_SUFFIX is not a regular file" "$OUT"
assert_eq "a directory at the token file is not called empty" 0 "$(printf '%s' "$OUT" | grep -c 'is empty' || true)"
assert_eq "a directory at the token file clones nothing" 0 "$(count_in_log clone)"

new_case test-base
write_token_file 'sk-ant one two'
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1 --no-claude-token
assert_eq "--no-claude-token ignores the token file" 0 "$RC"
assert_eq "--no-claude-token with a token file installs no Claude token" 0 "$(count_in_log CLAUDE_CODE_OAUTH_TOKEN)"

new_case test-base
run_limavm claude-token
assert_eq "claude-token succeeds" 0 "$RC"
assert_eq "claude-token writes the token with a newline" same \
    "$(print -r -- "$FAKE_CLAUDE" | cmp -s - "$HOME_DIR/$TOKEN_FILE_SUFFIX" && echo same || echo different)"
assert_eq "claude-token's file mode" 600 "$(mode_of "$HOME_DIR/$TOKEN_FILE_SUFFIX")"
assert_eq "claude-token's directory mode" 700 "$(mode_of "$HOME_DIR/.config/secrets")"
assert_eq "claude-token leaves no temporary file" CLAUDE_CODE_OAUTH_TOKEN "$(ls -A "$HOME_DIR/.config/secrets")"
assert_contains "claude-token says where it wrote" "$HOME_DIR/$TOKEN_FILE_SUFFIX" "$OUT"
assert_eq "claude-token touches no VM" "" "$(limactl_log)"
assert_no_secret_leak "claude-token"

new_case test-base
write_token_file 'old-token'
chmod 644 "$HOME_DIR/$TOKEN_FILE_SUFFIX"
chmod 755 "$HOME_DIR/.config/secrets"
run_limavm claude-token
assert_eq "claude-token replaces an existing file" 0 "$RC"
assert_eq "claude-token's replacement content" "$FAKE_CLAUDE" "$(<"$HOME_DIR/$TOKEN_FILE_SUFFIX")"
assert_eq "claude-token's replacement file mode" 600 "$(mode_of "$HOME_DIR/$TOKEN_FILE_SUFFIX")"
assert_eq "claude-token's replacement directory mode" 700 "$(mode_of "$HOME_DIR/.config/secrets")"

new_case test-base
mkdir -p "$HOME_DIR/$TOKEN_FILE_SUFFIX"
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm claude-token
assert_eq "claude-token with a directory at the file fails" 1 "$RC"
assert_contains "claude-token with a directory at the file names it" "$HOME_DIR/$TOKEN_FILE_SUFFIX is not a regular file" "$OUT"
assert_eq "claude-token with a directory at the file asks for nothing" 0 "$(printf '%s' "$OUT" | grep -c LIMAVM_PROMPT_INPUT || true)"
assert_eq "claude-token with a directory at the file writes nothing into it" "" "$(ls -A "$HOME_DIR/$TOKEN_FILE_SUFFIX")"
assert_eq "claude-token with a directory at the file leaves no temporary file" CLAUDE_CODE_OAUTH_TOKEN "$(ls -A "$HOME_DIR/.config/secrets")"

for content in '' $'\n' 'sk-ant one two'; do
    new_case test-base
    printf '%s' "$content" > "$STATE/prompt"
    run_limavm claude-token
    assert_eq "claude-token with the input '$content' fails" 1 "$RC"
    assert_eq "claude-token with the input '$content' writes nothing" no \
        "$([[ -e "$HOME_DIR/.config/secrets" ]] && echo yes || echo no)"
    assert_eq "claude-token with the input '$content' is not echoed" 0 "$(printf '%s' "$OUT" | grep -c 'sk-ant' || true)"
    assert_eq "claude-token with the input '$content' suggests no --no-claude-token" 0 \
        "$(printf '%s' "$OUT" | grep -c -- '--no-claude-token' || true)"
done

new_case test-base
run_limavm claude-token extra
assert_eq "claude-token with an argument fails" 1 "$RC"
assert_eq "claude-token with an argument writes nothing" no \
    "$([[ -e "$HOME_DIR/.config/secrets" ]] && echo yes || echo no)"

new_case test-base
run_limavm claude-token
EXTRA_ENV=(LIMAVM_PROMPT_INPUT="$STATE/does-not-exist")
run_limavm new t1
assert_eq "new after claude-token succeeds without a prompt" 0 "$RC"
assert_eq "new after claude-token gives no hint about keeping the token" 0 "$(printf '%s' "$OUT" | grep -c 'limavm claude-token' || true)"
assert_eq "new after claude-token installs the kept token" same \
    "$(print -r -- "$FAKE_CLAUDE" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"

cat > "$TMPBASE/tty_run.py" <<'PY'
import json
import os
import pty
import select
import signal
import subprocess
import sys
import termios
import time

mode, token, *argv = sys.argv[1:]
if mode == "notty":
    result = subprocess.run(
        argv,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        check=False,
        start_new_session=True,
        timeout=60,
    )
    print(json.dumps({"exit": result.returncode, "output": result.stdout + result.stderr}))
    sys.exit(0)

pid, master = pty.fork()
if pid == 0:
    os.execvp(argv[0], argv)
prompt = b"press Enter: "
transcript = b""
sent = False
deadline = time.monotonic() + 60
while time.monotonic() < deadline:
    ready, _, _ = select.select([master], [], [], 1)
    if not ready:
        continue
    try:
        data = os.read(master, 4096)
    except OSError:
        break
    if not data:
        break
    transcript += data
    if not sent and prompt in transcript:
        # The prompt is printed before read turns the echo off. A person types
        # later than that, so wait for it, but not for ever.
        wait = time.monotonic() + 5
        while termios.tcgetattr(master)[3] & termios.ECHO and time.monotonic() < wait:
            time.sleep(0.01)
        os.write(master, token.encode() + b"\n")
        sent = True
status = None
while time.monotonic() < deadline:
    done, status = os.waitpid(pid, os.WNOHANG)
    if done:
        break
    time.sleep(0.05)
else:
    os.kill(pid, signal.SIGKILL)
    _, status = os.waitpid(pid, 0)
print(
    json.dumps(
        {
            "exit": os.waitstatus_to_exitcode(status),
            "prompted": prompt in transcript,
            "echoed": token.encode() in transcript,
            "output": transcript.decode(errors="replace"),
        }
    )
)
PY

tty_run() {
    local mode=$1 token=$2
    shift 2
    (cd "$CHECKOUT" && python3 "$TMPBASE/tty_run.py" "$mode" "$token" env PATH="$STUBS:$PATH" HOME="$HOME_DIR" USER=shields \
        LIMA_STUB_STATE="$STATE" LIMAVM_BASE=test-base REAL_CURL="$REAL_CURL" REAL_GIT="$REAL_GIT" \
        LIMAVM_GITHUB_WEB_URL="$UNREACHABLE_GITHUB" /bin/bash "$LIMAVM" "$@" 2>&1)
}

new_case test-base
TTY_TOKEN='sk-ant-oat01-FAKEtypedToken-3d5e'
RESULT="$(tty_run pty "$TTY_TOKEN" new t1 --no-claude-token)"
assert_eq "--no-claude-token never prompts, even on a terminal" false "$(jq -r .prompted <<<"$RESULT")"

new_case test-base
RESULT="$(tty_run pty "$TTY_TOKEN" new t1)"
assert_eq "new prompts on the terminal" true "$(jq -r .prompted <<<"$RESULT")"
assert_eq "new succeeds with a token typed on the terminal" 0 "$(jq -r .exit <<<"$RESULT")"
assert_eq "the typed token is not echoed" false "$(jq -r .echoed <<<"$RESULT")"
assert_eq "the typed token reaches setup-secrets on stdin" same \
    "$(print -r -- "$TTY_TOKEN" | cmp -s - "$STATE/calls/5.stdin" && echo same || echo different)"
assert_eq "the typed token is in no argument list or environment" 0 \
    "$(cat "$STATE"/calls/*.argv "$STATE"/calls/*.env "$STATE/limactl.log" | grep -c -e "$TTY_TOKEN" || true)"

new_case test-base
RESULT="$(tty_run notty x new t1)"
assert_eq "new without a terminal fails" 1 "$(jq -r .exit <<<"$RESULT")"
assert_contains "new without a terminal says what to do" "--no-claude-token" "$(jq -r .output <<<"$RESULT")"
assert_contains "new without a terminal points to claude-token" "limavm claude-token" "$(jq -r .output <<<"$RESULT")"
assert_eq "new without a terminal clones nothing" 0 "$(count_in_log clone)"

new_case test-base
RESULT="$(tty_run pty "$TTY_TOKEN" claude-token)"
assert_eq "claude-token prompts on the terminal" true "$(jq -r .prompted <<<"$RESULT")"
assert_eq "claude-token succeeds with a token typed on the terminal" 0 "$(jq -r .exit <<<"$RESULT")"
assert_eq "claude-token does not echo the typed token" false "$(jq -r .echoed <<<"$RESULT")"
assert_eq "claude-token keeps the typed token" "$TTY_TOKEN" "$(<"$HOME_DIR/$TOKEN_FILE_SUFFIX")"

new_case test-base
RESULT="$(tty_run notty x claude-token)"
assert_eq "claude-token without a terminal fails" 1 "$(jq -r .exit <<<"$RESULT")"
assert_contains "claude-token without a terminal says what to do" "run in a terminal" "$(jq -r .output <<<"$RESULT")"
assert_eq "claude-token without a terminal suggests no --no-claude-token" 0 \
    "$(jq -r .output <<<"$RESULT" | grep -c -- '--no-claude-token' || true)"
assert_eq "claude-token without a terminal writes nothing" no \
    "$([[ -e "$HOME_DIR/.config/secrets" ]] && echo yes || echo no)"

# --- 11. new picks a name, and sizes ---
new_case test-base
EXTRA_ENV+=(LIMAVM_CPUS=2 LIMAVM_MEMORY=4)
run_limavm new
assert_eq "new without a name succeeds" 0 "$RC"
assert_eq "new without a name makes a dated name" 1 \
    "$([[ "$(limactl_log | sed -n 3p)" == "clone --tty=false --cpus 2 --memory 4 --start test-base vm-"[0-9][0-9][0-9][0-9][0-9][0-9]-[0-9][0-9][0-9][0-9][0-9][0-9] ]] && echo 1 || echo 0)"

# --- 12. new refuses before it makes anything ---
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

for bad in foo a/b/c ../x a/.. ./b 'a b/c' 'a/b;id' '/b' 'a/' ''; do
    new_case test-base
    start_fake
    run_limavm new t1 --repo "$bad"
    assert_eq "new rejects the repository '$bad'" 1 "$RC"
    assert_contains "the bad repository '$bad' says what is expected" "OWNER/REPO" "$OUT"
    assert_eq "the bad repository '$bad' touches no VM" "" "$(limactl_log)"
    assert_eq "the bad repository '$bad' never runs gh" 0 "$(stub_calls gh)"
    assert_eq "the bad repository '$bad' contacts no GitHub" "" "$(fake_requests)"
done

new_case test-base
run_limavm new t1 --repo
assert_eq "--repo without a value fails" 1 "$RC"
run_limavm new t1 --frobnicate
assert_eq "an unknown option fails" 1 "$RC"
assert_contains "an unknown option is named" "--frobnicate" "$OUT"
assert_eq "an unknown option touches no VM" "" "$(limactl_log)"

new_case test-base
rm "$HOME_DIR/.config/lgtmcp/config.yaml"
run_limavm new t1
assert_eq "a missing LGTMCP config fails" 1 "$RC"
assert_contains "that message names the file" ".config/lgtmcp/config.yaml" "$OUT"
assert_eq "that failure clones nothing" 0 "$(count_in_log clone)"

new_case test-base
rm "$HOME_DIR/.config/lgtmcp/config.yaml"
mkdir "$HOME_DIR/.config/lgtmcp/config.yaml"
run_limavm new t1
assert_eq "a directory at the LGTMCP config fails" 1 "$RC"
assert_contains "a directory at the LGTMCP config is named" "cannot read $HOME_DIR/.config/lgtmcp/config.yaml" "$OUT"
assert_eq "a directory at the LGTMCP config clones nothing" 0 "$(count_in_log clone)"

# --- 13. The device flow ---
new_case test-base
start_fake '"device_polls": ["pending", "pending", "success"]'
run_limavm new t1 --repo shields/dotfiles
assert_eq "a flow that waits for the browser succeeds" 0 "$RC"
assert_eq "limavm polls until the code is entered" 3 "$(fake_requests | grep -c 'POST /login/oauth/access_token' || true)"
assert_eq "a poll interval of zero sleeps not at all" 0 "$([[ -e "$STATE/sleep.log" ]] && echo 1 || echo 0)"

new_case test-base
start_fake '"device_interval": 2, "device_polls": ["pending", "slow_down", "success"]'
run_limavm new t1 --repo shields/dotfiles
assert_eq "a flow that is told to slow down succeeds" 0 "$RC"
assert_eq "limavm waits the interval before each poll, and the longer one GitHub asks for" "2
2
7" "$(<"$STATE/sleep.log")"

new_case test-base
start_fake '"device_polls": ["slow_down", "success"]'
run_limavm new t1 --repo shields/dotfiles
assert_eq "limavm slows down even when the interval was zero" "5" "$(<"$STATE/sleep.log")"

for failing in denied:denied expired:expired disabled:"not enabled"; do
    poll=${failing%%:*}
    message=${failing#*:}
    new_case test-base
    start_fake "\"device_polls\": [\"pending\", \"$poll\"]"
    run_limavm new t1 --repo shields/dotfiles
    assert_eq "a $poll code fails new" 1 "$RC"
    assert_contains "a $poll code says so" "$message" "$OUT"
    assert_eq "a $poll code creates no VM" "list -q test-base
list -q t1" "$(limactl_log)"
    assert_no_secret_leak "a $poll code"
done

new_case test-base
start_fake '"device_expires_in": 0'
run_limavm new t1 --repo shields/dotfiles
assert_eq "a code that is out of time fails new" 1 "$RC"
assert_contains "a code that is out of time says so" "expired" "$OUT"
assert_eq "a code that is out of time polls nothing" "POST /login/device/code" "$(fake_requests)"

new_case test-base
FAKE_CLIENT_ID=Iv-somebody-else start_fake
run_limavm new t1 --repo shields/dotfiles
assert_eq "an unknown client id fails new" 1 "$RC"
assert_contains "an unknown client id says what GitHub said" "incorrect_client_credentials" "$OUT"
assert_eq "an unknown client id creates no VM" 0 "$(count_in_log clone)"

new_case test-base
FAKE_CLIENT_ID=Iv-custom start_fake
EXTRA_ENV+=(LIMAVM_GITHUB_CLIENT_ID=Iv-custom)
run_limavm new t1 --repo shields/dotfiles
assert_eq "LIMAVM_GITHUB_CLIENT_ID replaces the client id" 0 "$RC"
assert_eq "the record has the replaced client id" Iv-custom "$(stdin_of 5 | jq -r .client_id)"

new_case test-base
echo 99999 > "$STATE/gh-id"
start_fake
run_limavm new t1 --repo shields/dotfiles
assert_eq "a refused authorization fails new" 1 "$RC"
assert_contains "a refused authorization says what GitHub said" "bad_repository" "$OUT"
assert_eq "a refused authorization creates no VM" 0 "$(count_in_log clone)"

new_case test-base
: > "$STATE/gh-fail"
start_fake
run_limavm new t1 --repo shields/dotfiles
assert_eq "a repository gh cannot read fails new" 1 "$RC"
assert_contains "that failure names the repository" "shields/dotfiles" "$OUT"
assert_eq "that failure contacts no GitHub" "" "$(fake_requests)"
assert_eq "that failure creates no VM" 0 "$(count_in_log clone)"

new_case test-base
echo notanumber > "$STATE/gh-id"
start_fake
run_limavm new t1 --repo shields/dotfiles
assert_eq "an id that is not a number fails new" 1 "$RC"
assert_eq "an id that is not a number contacts no GitHub" "" "$(fake_requests)"

new_case test-base
EXTRA_ENV+=(LIMAVM_GITHUB_WEB_URL=http://127.0.0.1:9)
run_limavm new t1 --repo shields/dotfiles
assert_eq "an unreachable GitHub fails new" 1 "$RC"
assert_contains "an unreachable GitHub says so" "cannot reach http://127.0.0.1:9" "$OUT"
assert_eq "an unreachable GitHub creates no VM" 0 "$(count_in_log clone)"

new_case test-base
start_fake
: > "$STATE/open-fail"
run_limavm new t1 --repo shields/dotfiles
assert_eq "a failing open does not stop the flow" 0 "$RC"
assert_contains "a failing open asks for the address to be opened by hand" "open that address yourself" "$OUT"

new_case test-base
start_fake
: > "$STATE/pbcopy-fail"
run_limavm new t1 --repo shields/dotfiles
assert_eq "a failing pbcopy does not stop the flow" 0 "$RC"
assert_eq "a failing pbcopy still installs the authorization" 1 "$(count_in_log GITHUB_APP_AUTH)"
assert_contains "a failing pbcopy still shows the user code" "WDJB-MJHT" "$OUT"
assert_contains "a failing pbcopy asks for the code to be typed" "type it" "$OUT"
assert_eq "a failing pbcopy does not claim the clipboard" 0 "$(printf '%s' "$OUT" | grep -c 'the code is on the clipboard' || true)"

new_case test-base
start_fake
RUN_PATH=$(no_pbcopy_path)
assert_eq "the PATH without pbcopy has none" no "$(env PATH="$RUN_PATH" sh -c 'command -v pbcopy' >/dev/null 2>&1 && echo yes || echo no)"
run_limavm new t1 --repo shields/dotfiles
assert_eq "no pbcopy on the PATH does not stop the flow" 0 "$RC"
assert_eq "no pbcopy on the PATH still installs the authorization" 1 "$(count_in_log GITHUB_APP_AUTH)"
assert_contains "no pbcopy on the PATH still shows the user code" "WDJB-MJHT" "$OUT"
assert_eq "no pbcopy on the PATH says nothing about the clipboard" 0 "$(printf '%s' "$OUT" | grep -c 'clipboard' || true)"
assert_eq "no pbcopy on the PATH runs none" 0 "$(stub_calls pbcopy)"

new_case test-base
run_limavm new t1
assert_eq "new without --repo never runs pbcopy" 0 "$(stub_calls pbcopy)"

# --- 14. The clone is deleted after any later failure ---
for failing in clone brew setup-secrets; do
    new_case test-base
    start_fake
    EXTRA_ENV+=(LIMA_STUB_FAIL=$failing)
    run_limavm new t1 --repo shields/dotfiles
    assert_eq "a failing $failing fails new" 1 "$RC"
    assert_eq "a failing $failing deletes the clone last" "delete --tty=false --force t1" \
        "$(limactl_log | tail -1)"
    assert_eq "a failing $failing leaves no t1" 0 "$(grep -cx t1 "$STATE/instances")"
    assert_eq "a failing $failing keeps the base" 1 "$(grep -cx test-base "$STATE/instances")"
    assert_contains "a failing $failing says it removed the clone" "removing t1" "$OUT"
    assert_no_secret_leak "a failing $failing"
done

for failing in GITHUB_APP_AUTH CLAUDE_CODE_OAUTH_TOKEN CODEX_AUTH LGTMCP_CONFIG; do
    new_case test-base
    start_fake
    EXTRA_ENV+=(LIMA_STUB_FAIL=$failing)
    run_limavm new t1 --repo shields/dotfiles
    assert_eq "a failing $failing install fails new" 1 "$RC"
    assert_eq "a failing $failing install deletes the clone" "delete --tty=false --force t1" \
        "$(limactl_log | tail -1)"
    assert_no_secret_leak "a failing $failing install"
done

new_case test-base
start_fake
EXTRA_ENV+=(LIMA_STUB_FAIL=brew)
run_limavm new t1 --repo shields/dotfiles
assert_eq "no secret is sent when the cask upgrade fails" 0 "$(count_in_log setup-secrets)"

# --- 15. github re-authorizes an existing VM ---
new_case test-base t1
start_fake
run_limavm github t1 shields/dotfiles
assert_eq "github succeeds" 0 "$RC"
assert_eq "github call order" "list -q t1
list --format {{.Status}} t1
$SETUP GITHUB_APP_AUTH" "$(limactl_log)"
assert_eq "github clones nothing" 0 "$(count_in_log 'git clone')"
assert_eq "github sends the record on stdin" shields/dotfiles "$(stdin_of 3 | jq -r .repository)"
assert_eq "github sends tokens for that repository only" "ghu_fake-access-1" "$(stdin_of 3 | jq -r .access_token)"
assert_eq "github narrows the token to the repository" 4242 "$(fake_form /login/oauth/access_token repository_id)"
assert_contains "github says what the VM can reach" "shields/dotfiles" "$OUT"
assert_eq "github keeps the VM" 1 "$(grep -cx t1 "$STATE/instances")"
assert_no_secret_leak "github"

new_case test-base t1
start_fake
: > "$STATE/stopped-t1"
run_limavm github t1 shields/other
assert_eq "github on a stopped VM succeeds" 0 "$RC"
assert_eq "github starts a stopped VM first" "list -q t1
list --format {{.Status}} t1
start --tty=false t1
$SETUP GITHUB_APP_AUTH" "$(limactl_log)"
assert_eq "github asks gh for the other repository" "api repos/shields/other --jq .id" "$(<"$STATE/gh.log")"

new_case test-base t1
start_fake '"device_polls": ["denied"]'
run_limavm github t1 shields/dotfiles
assert_eq "a denied github fails" 1 "$RC"
assert_eq "a denied github installs nothing" 0 "$(count_in_log setup-secrets)"
assert_eq "a denied github keeps the VM" 1 "$(grep -cx t1 "$STATE/instances")"
assert_eq "a denied github deletes nothing" 0 "$(count_in_log delete)"

new_case test-base
start_fake
run_limavm github t1 shields/dotfiles
assert_eq "github on a missing VM fails" 1 "$RC"
assert_contains "github on a missing VM says so" "does not exist" "$OUT"
assert_eq "github on a missing VM contacts no GitHub" "" "$(fake_requests)"

new_case test-base
start_fake
run_limavm github test-base shields/dotfiles
assert_eq "github refuses the base" 1 "$RC"
assert_eq "github on the base touches no VM" "" "$(limactl_log)"

for args in "" "t1" "t1 shields/dotfiles extra" "t1 notarepo" "../x shields/dotfiles"; do
    new_case test-base t1
    start_fake
    run_limavm github ${=args}
    assert_eq "github with '$args' fails" 1 "$RC"
    assert_eq "github with '$args' contacts no GitHub" "" "$(fake_requests)"
    assert_eq "github with '$args' installs nothing" 0 "$(count_in_log setup-secrets)"
done
new_case test-base t1
start_fake
run_limavm github t1
assert_contains "github without a repository, outside a GitHub checkout, asks for one" "limavm github t1 OWNER/REPO" "$OUT"

new_case test-base t1
: > "$STATE/gh-fail"
EXTRA_ENV+=(LIMAVM_GITHUB_WEB_URL=)
run_limavm github t1
assert_contains "github infers the checkout's github.com origin with the default address" \
    "GitHub access for shields/dotfiles, from the origin of this checkout" "$OUT"
assert_contains "github with the default address goes on to gh" "gh cannot read shields/dotfiles" "$OUT"
assert_eq "github with the default address runs no curl" 0 "$(stub_calls curl)"
assert_eq "github with the default address installs nothing" 0 "$(count_in_log setup-secrets)"

new_case test-base t1
start_fake
git -C "$CHECKOUT" remote set-url origin https://127.0.0.1/shields/other.git
run_limavm github t1
assert_eq "github infers the repository from the origin" 0 "$RC"
assert_contains "github says where the repository came from" "GitHub access for shields/other, from the origin of this checkout" "$OUT"
assert_eq "github asks gh for the inferred repository" "api repos/shields/other --jq .id" "$(<"$STATE/gh.log")"
assert_eq "github installs the inferred repository's record" shields/other "$(stdin_of 3 | jq -r .repository)"
assert_eq "github with an inferred repository clones nothing" 0 "$(count_in_log 'git clone')"
git -C "$CHECKOUT" remote set-url origin git@github.com:shields/dotfiles.git

new_case test-base t1
start_fake
MAIN_CHECKOUT=$CHECKOUT
CHECKOUT="$TMPBASE/not-a-repo"
run_limavm github t1
assert_eq "github outside a work tree fails" 1 "$RC"
assert_contains "github outside a work tree asks for the repository" "limavm github t1 OWNER/REPO" "$OUT"
assert_eq "github outside a work tree contacts no GitHub" "" "$(fake_requests)"
CHECKOUT=$MAIN_CHECKOUT

# --- 16. rm ---
new_case test-base t1
run_limavm rm test-base
assert_eq "rm refuses the base" 1 "$RC"
assert_contains "the refusal says why" "refusing to delete test-base" "$OUT"
assert_eq "the refusal touches no VM" "" "$(limactl_log)"
run_limavm rm t1
assert_eq "rm succeeds" 0 "$RC"
assert_eq "rm deletes just that VM" "delete --tty=false --force t1" "$(limactl_log)"
assert_contains "rm says the tokens are not revoked" "not revoked" "$OUT"
assert_contains "rm says how to end them" "https://github.com/settings/apps/authorizations" "$OUT"
assert_contains "rm says the end is for every VM" "every VM" "$OUT"
run_limavm rm
assert_eq "rm without a name fails" 1 "$RC"
run_limavm rm ../x
assert_eq "rm with a bad name fails" 1 "$RC"
run_limavm rm t1 t2
assert_eq "rm with two names fails" 1 "$RC"

# --- 17. No case ever called security ---
assert_eq "no case called security" 0 "$(cat "$TMPBASE"/state-*/security.log(N) | wc -l | tr -d ' ')"

echo ""
echo "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
