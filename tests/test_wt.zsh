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

SCRIPT="${0:A:h}/../.zsh.d/wt.zsh"
WORDLIST="${SCRIPT:h}/eff_short_wordlist_2_0.txt"
TMPBASE="$(mktemp -d)"
TMPBASE="${TMPBASE:A}"
trap 'rm -rf "$TMPBASE"' EXIT

# Keep repository creation independent of the invoking user's Git settings.
export GIT_CONFIG_GLOBAL=/dev/null
export GIT_CONFIG_NOSYSTEM=1

pass=0
fail=0

assert_eq() {
    local desc="$1" expected="$2" actual="$3"
    if [[ "$expected" == "$actual" ]]; then
        print -r -- "PASS: $desc"
        (( ++pass ))
    else
        print -r -- "FAIL: $desc (expected=${(qqq)expected} actual=${(qqq)actual})"
        (( ++fail ))
    fi
}

assert_contains() {
    local desc="$1" needle="$2" actual="$3"
    if [[ "$actual" == *"$needle"* ]]; then
        print -r -- "PASS: $desc"
        (( ++pass ))
    else
        print -r -- "FAIL: $desc (missing=${(qqq)needle} actual=${(qqq)actual})"
        (( ++fail ))
    fi
}

assert_command_status() {
    local desc="$1" expected="$2"
    shift 2

    local -i actual
    if "$@"; then
        actual=0
    else
        actual=$?
    fi
    assert_eq "$desc" "$expected" "$actual"
}

array_contains() {
    local needle="$1"
    shift

    local value
    for value in "$@"; do
        [[ "$value" == "$needle" ]] && return 0
    done
    return 1
}

make_repo() {
    local repo="$TMPBASE/$1"

    command mkdir -p -- "$repo"
    command git init -q -b main "$repo"
    command git -C "$repo" config user.email test@example.com
    command git -C "$repo" config user.name "Test User"
    command git -C "$repo" config commit.gpgsign false
    command git -C "$repo" commit -q --allow-empty -m initial
    print -r -- "$repo"
}

add_worktree() {
    local repo="$1" branch="$2" worktree_path="$3"

    command mkdir -p -- "${worktree_path:h}"
    command git -C "$repo" worktree add -q -b "$branch" "$worktree_path" HEAD
}

typeset -gi LAST_RC=0
typeset -g LAST_OUT=""

run_wt() {
    local output_file="$TMPBASE/wt-output"

    : >"$output_file"
    if wt "$@" >"$output_file" 2>&1; then
        LAST_RC=0
    else
        LAST_RC=$?
    fi
    LAST_OUT="$(<"$output_file")"
}

# `command sort` deliberately bypasses shell functions, so place a deterministic
# executable on PATH. Its second result would succeed after a first collision;
# the counter therefore detects any forbidden redraw/retry behavior. Emit a
# complete 1,000-row shuffle by default so tests exercise the production
# minimum without relying on the host's random-sort implementation.
MOCKBIN="$TMPBASE/mock-bin"
command mkdir -p -- "$MOCKBIN"
cat >"$MOCKBIN/sort" <<'MOCK_SORT'
#!/bin/sh

count=0
if [ -r "$WT_SORT_COUNT" ]; then
    IFS= read -r count <"$WT_SORT_COUNT"
fi
case "$count" in
    "" | *[!0-9]*) count=0 ;;
esac
count=$((count + 1))
printf '%s\n' "$count" >"$WT_SORT_COUNT"

row_count=${WT_SORT_ROWS:-1000}
if [ "$count" -gt 1 ] && [ -n "${WT_SORT_FOLLOWUP_FIRST:-}" ]; then
    first=$WT_SORT_FOLLOWUP_FIRST
    second=$WT_SORT_FOLLOWUP_SECOND
else
    first=$WT_SORT_FIRST
    second=$WT_SORT_SECOND
fi

input_file=
for argument do
    input_file=$argument
done

if [ "$row_count" -ge 1 ]; then
    printf '%s\n' "$first"
fi
if [ "$row_count" -ge 2 ]; then
    printf '%s\n' "$second"
fi
if [ "$row_count" -gt 2 ]; then
    awk -v limit="$((row_count - 2))" 'NR <= limit { print }' "$input_file"
fi
MOCK_SORT
command chmod +x "$MOCKBIN/sort"
export PATH="$MOCKBIN:$PATH"

configure_sort() {
    local counter="$1" first="$2" second="$3" rows="${4:-1000}"

    : >"$counter"
    export WT_SORT_COUNT="$counter"
    export WT_SORT_FIRST="$first"
    export WT_SORT_SECOND="$second"
    export WT_SORT_ROWS="$rows"
    unset WT_SORT_FOLLOWUP_FIRST WT_SORT_FOLLOWUP_SECOND
}

typeset -gA EFF_WORDS
local_wordlist_code=""
local_wordlist_word=""
while IFS=$'\t' read -r local_wordlist_code local_wordlist_word; do
    [[ -n "$local_wordlist_word" ]] && EFF_WORDS[$local_wordlist_word]=1
done <"$WORDLIST"
unset local_wordlist_code local_wordlist_word

valid_eff_pair() {
    local name="$1" first second

    # Try every exact word as the first component. This remains correct for
    # entries such as "yo-yo", unlike splitting the generated name on "-".
    for first in "${(@k)EFF_WORDS}"; do
        if [[ "$name" == "${first}-"* ]]; then
            second="${name#${first}-}"
            [[ -n "${EFF_WORDS[$second]-}" ]] && return 0
        fi
    done
    return 1
}

# Load the real completion system before the module so its compdef guard runs.
autoload -Uz compinit
compinit -D
source "$SCRIPT"

assert_eq "wt completion is registered" "_wt" "${_comps[wt]-}"

# --- Creation from the main worktree, including a nested directory ---
create_repo="$(make_repo "creation main")"
create_head="$(command git -C "$create_repo" rev-parse HEAD)"
exclude="$create_repo/.git/info/exclude"
print -n -r -- "keep-this-pattern" >"$exclude"
command mkdir -p -- "$create_repo/nested/directory"
cd "$create_repo/nested/directory"

create_counter="$TMPBASE/create-sort-count"
configure_sort "$create_counter" $'6643\tyo-yo' $'1111\taardvark'
run_wt

first_name="yo-yo-aardvark"
first_path="$create_repo/.worktrees/$first_name"
assert_eq "creation returns 0" 0 "$LAST_RC"
assert_eq "creation enters the linked worktree" "$first_path" "$PWD"
assert_eq "creation uses both selected words, including a hyphenated word" "$first_name" "${PWD:t}"
assert_command_status "generated name is two exact EFF words" 0 valid_eff_pair "${PWD:t}"
assert_eq "creation uses the worktree- branch prefix" \
    "worktree-$first_name" \
    "$(command git branch --show-current)"
assert_eq "new branch starts at the main worktree HEAD" \
    "$create_head" \
    "$(command git rev-parse HEAD)"
assert_eq "sort is invoked exactly once for creation" 1 "$(<"$create_counter")"
assert_eq "exclude preserves an unterminated existing entry" \
    $'keep-this-pattern\n.worktrees/' \
    "$(<"$exclude")"
assert_eq "exclude contains one exact .worktrees entry" \
    1 \
    "$(command awk '$0 == ".worktrees/" { ++count } END { print count + 0 }' "$exclude")"

# --- No argument in a linked worktree toggles back to the main worktree ---
worktrees_before="$(command git -C "$create_repo" worktree list --porcelain |
    command awk '$1 == "worktree" { ++count } END { print count + 0 }')"
run_wt
assert_eq "no argument in a linked worktree returns 0" 0 "$LAST_RC"
assert_eq "no argument in a linked worktree enters the main worktree" "$create_repo" "$PWD"
assert_eq "returning to main does not select more words" 1 "$(<"$create_counter")"
assert_eq "returning to main creates no worktree" \
    "$worktrees_before" \
    "$(command git -C "$create_repo" worktree list --porcelain |
        command awk '$1 == "worktree" { ++count } END { print count + 0 }')"

# A second creation proves that updating info/exclude is idempotent.
second_counter="$TMPBASE/second-sort-count"
configure_sort "$second_counter" $'6652\tzebra' $'6664\tzombie'
run_wt
second_path="$create_repo/.worktrees/zebra-zombie"
assert_eq "second creation returns 0" 0 "$LAST_RC"
assert_eq "second creation enters its worktree" "$second_path" "$PWD"
assert_eq "second creation invokes sort once" 1 "$(<"$second_counter")"
assert_eq "repeated creation keeps one exact exclude entry" \
    1 \
    "$(command awk '$0 == ".worktrees/" { ++count } END { print count + 0 }' "$exclude")"
assert_eq "repeated creation preserves prior exclude content" \
    $'keep-this-pattern\n.worktrees/' \
    "$(<"$exclude")"

# --- A collision fails after one selection; there is no redraw or retry ---
collision_repo="$(make_repo "collision main")"
command git -C "$collision_repo" branch worktree-apple-apricot
cd "$collision_repo"

collision_counter="$TMPBASE/collision-sort-count"
configure_sort "$collision_counter" $'1322\tapple' $'1323\tapricot'
export WT_SORT_FOLLOWUP_FIRST=$'6643\tyo-yo'
export WT_SORT_FOLLOWUP_SECOND=$'1111\taardvark'
run_wt

assert_eq "branch-name collision returns 1" 1 "$LAST_RC"
assert_contains "branch-name collision is reported" "branch already exists" "$LAST_OUT"
assert_eq "collision leaves the current directory unchanged" "$collision_repo" "$PWD"
assert_eq "collision invokes sort exactly once" 1 "$(<"$collision_counter")"
assert_eq "collision does not create the retry candidate" \
    0 \
    "$([[ -e "$collision_repo/.worktrees/yo-yo-aardvark" ]] && print 1 || print 0)"
assert_eq "collision does not create a follow-up branch" \
    1 \
    "$(command git -C "$collision_repo" show-ref --verify --quiet \
        refs/heads/worktree-yo-yo-aardvark && print 0 || print 1)"
unset WT_SORT_FOLLOWUP_FIRST WT_SORT_FOLLOWUP_SECOND

# --- A shuffled word list below the 1,000-entry minimum is rejected ---
short_list_repo="$(make_repo "short word list main")"
cd "$short_list_repo"

short_list_counter="$TMPBASE/short-list-sort-count"
configure_sort "$short_list_counter" $'1322\tapple' $'1323\tapricot' 999
run_wt

assert_eq "word list with 999 entries returns 1" 1 "$LAST_RC"
assert_contains "short word list reports the 1,000-entry minimum" \
    "fewer than 1000 entries" \
    "$LAST_OUT"
assert_eq "short word list leaves the current directory unchanged" \
    "$short_list_repo" \
    "$PWD"
assert_eq "short word list invokes sort exactly once" \
    1 \
    "$(<"$short_list_counter")"
assert_eq "short word list creates no branch" \
    1 \
    "$(command git -C "$short_list_repo" show-ref --verify --quiet \
        refs/heads/worktree-apple-apricot && print 0 || print 1)"

# --- Named worktrees may live anywhere and may contain spaces ---
navigation_repo="$(make_repo "navigation main")"
solo_path="$navigation_repo/.worktrees/solo"
locked_path="$navigation_repo/.worktrees/locked-name"
space_path="$TMPBASE/external parent/blue moon"
twin_one="$TMPBASE/twin parent one/twin"
twin_two="$TMPBASE/twin parent two/twin"
stale_path="$TMPBASE/stale parent/stale-name"

add_worktree "$navigation_repo" solo-branch "$solo_path"
add_worktree "$navigation_repo" locked-branch "$locked_path"
add_worktree "$navigation_repo" space-branch "$space_path"
add_worktree "$navigation_repo" twin-one-branch "$twin_one"
add_worktree "$navigation_repo" twin-two-branch "$twin_two"
add_worktree "$navigation_repo" stale-branch "$stale_path"
command git -C "$navigation_repo" worktree lock \
    --reason "removable checkout unavailable" \
    "$locked_path"
command rm -rf -- "$locked_path"
# A locked registration is not marked prunable. Recreate an ordinary directory
# inside the main checkout so both -d and a naive `git -C` check succeed.
command mkdir -p -- "$locked_path"
command rm -rf -- "$stale_path"
# Recreate a plain directory at the registered path. This makes -d succeed and
# ensures stale rejection relies on Git's porcelain "prunable" status.
command mkdir -p -- "$stale_path"

cd "$navigation_repo"
typeset -g CHPWD_PATH=""
chpwd() {
    CHPWD_PATH="$PATH"
}
navigation_path="$PATH"
run_wt "blue moon"
assert_eq "named external worktree returns 0" 0 "$LAST_RC"
assert_eq "named external worktree with spaces is entered" "$space_path" "$PWD"
assert_eq "named navigation preserves PATH while chpwd hooks run" \
    "$navigation_path" \
    "$CHPWD_PATH"
unfunction chpwd

run_wt solo
assert_eq "named lookup works from another linked worktree" 0 "$LAST_RC"
assert_eq "named lookup enters an internal worktree" "$solo_path" "$PWD"

run_wt
assert_eq "no argument returns from an internal linked worktree" 0 "$LAST_RC"
assert_eq "no argument returns to navigation main" "$navigation_repo" "$PWD"

run_wt missing-name
assert_eq "unknown worktree name returns 1" 1 "$LAST_RC"
assert_contains "unknown worktree name is reported" "no linked worktree named" "$LAST_OUT"
assert_eq "unknown worktree name leaves PWD unchanged" "$navigation_repo" "$PWD"

run_wt stale-name
assert_eq "stale registered worktree returns 1" 1 "$LAST_RC"
assert_contains "stale registered worktree is reported" "does not exist" "$LAST_OUT"
assert_eq "stale registered worktree leaves PWD unchanged" "$navigation_repo" "$PWD"

run_wt locked-name
assert_eq "locked missing worktree returns 1" 1 "$LAST_RC"
assert_contains "locked missing worktree is reported" "does not exist" "$LAST_OUT"
assert_eq "locked missing worktree leaves PWD unchanged" "$navigation_repo" "$PWD"

run_wt twin
assert_eq "ambiguous basename returns 1" 1 "$LAST_RC"
assert_contains "ambiguous basename is reported" "ambiguous" "$LAST_OUT"
assert_eq "ambiguous basename leaves PWD unchanged" "$navigation_repo" "$PWD"

# --- Invalid invocation contexts ---
run_wt one two
assert_eq "more than one argument returns 1" 1 "$LAST_RC"
assert_contains "more than one argument prints usage" "wt: usage: wt" "$LAST_OUT"
assert_eq "arity error leaves PWD unchanged" "$navigation_repo" "$PWD"

nonrepo="$TMPBASE/not a repository"
command mkdir -p -- "$nonrepo"
cd "$nonrepo"
run_wt
assert_eq "outside a repository returns 1" 1 "$LAST_RC"
assert_contains "outside a repository reports a wt error" "wt:" "$LAST_OUT"
assert_eq "outside a repository leaves PWD unchanged" "$nonrepo" "$PWD"

# --- Completion offers only usable, unambiguous linked-worktree basenames ---
typeset -ga CAPTURED_COMPLETIONS
CAPTURED_COMPLETIONS=()
compadd() {
    local candidate
    for candidate in "$@"; do
        [[ "$candidate" == -- ]] || CAPTURED_COMPLETIONS+=("$candidate")
    done
}

cd "$navigation_repo"
CURRENT=2
_wt
assert_eq "completion returns two usable unique names" 2 "${#CAPTURED_COMPLETIONS[@]}"
assert_command_status "completion includes internal worktree name" \
    0 array_contains solo "${CAPTURED_COMPLETIONS[@]}"
assert_command_status "completion preserves a name containing spaces" \
    0 array_contains "blue moon" "${CAPTURED_COMPLETIONS[@]}"
assert_command_status "completion excludes an ambiguous basename" \
    1 array_contains twin "${CAPTURED_COMPLETIONS[@]}"
assert_command_status "completion excludes a stale worktree" \
    1 array_contains stale-name "${CAPTURED_COMPLETIONS[@]}"
assert_command_status "completion excludes a locked missing worktree" \
    1 array_contains locked-name "${CAPTURED_COMPLETIONS[@]}"
assert_command_status "completion excludes the main worktree" \
    1 array_contains "${navigation_repo:t}" "${CAPTURED_COMPLETIONS[@]}"

CAPTURED_COMPLETIONS=()
CURRENT=3
_wt
assert_eq "completion offers nothing for a second argument" 0 "${#CAPTURED_COMPLETIONS[@]}"

print
print -r -- "Results: $pass passed, $fail failed"
[[ $fail -eq 0 ]]
