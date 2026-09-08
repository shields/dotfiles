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

# Time each way of using wt with hyperfine against a throwaway repository, so
# the numbers do not depend on the caller's checkout. Every measurement runs in
# a fresh `zsh -f`; hyperfine measures that shell's own startup and subtracts
# it, but sourcing the script (including its top-level `zmodload`) is inside
# every sample, so treat the numbers as comparisons between bench() blocks or
# between -s scripts rather than as wt's cost to the last millisecond. With
# --rc the shell is `zsh -i` instead, which layers ~/.zshrc on top: the chpwd
# hooks it registers run inside wt's cd and usually dominate what a user
# perceives.
#
# usage: zsh tools/bench_wt.zsh [-n WORKTREES] [-p PRUNABLE] [--rc]
#            [-s WT.ZSH]... [-- HYPERFINE-OPTION...]
#
# Repeat -s to compare implementations side by side; each needs
# eff_short_wordlist_2_0.txt beside it. Anything after -- goes to hyperfine.

set -euo pipefail

zmodload zsh/zutil

# Inside a function $0 is the function's own name, so capture the script's.
typeset script_name="${0:t}"

usage() {
    print -u2 -r -- "usage: $script_name [-n worktrees] [-p prunable] [--rc]" \
        "[-s wt.zsh]... [-- hyperfine options]"
    exit 2
}

typeset -a opt_worktrees opt_prunable opt_rc opt_scripts opt_help
zparseopts -D -F -- n:=opt_worktrees p:=opt_prunable -rc=opt_rc \
    s+:=opt_scripts h=opt_help -help=opt_help || usage
(( ${#opt_help[@]} == 0 )) || usage
typeset -a hyperfine_options=("$@")

typeset worktrees_option="${opt_worktrees[2]:-20}" prunable_option="${opt_prunable[2]:-0}"
[[ "$worktrees_option" == <-> && "$prunable_option" == <-> ]] || usage
typeset -i worktrees="$worktrees_option" prunable="$prunable_option"
(( worktrees >= 1 && prunable < worktrees )) || usage

typeset -a scripts
typeset -i argument
for (( argument = 2; argument <= ${#opt_scripts[@]}; argument += 2 )); do
    scripts+=("${opt_scripts[argument]:A}")
done
(( ${#scripts[@]} > 0 )) || scripts=("${${0:A:h}:h}/.zsh.d/wt.zsh")
typeset script
for script in "${scripts[@]}"; do
    if [[ ! -r "$script" || ! -r "${script:h}/eff_short_wordlist_2_0.txt" ]]; then
        print -u2 -r -- "$script_name: not a wt.zsh with its word list beside it: $script"
        exit 1
    fi
done

if ! (( $+commands[hyperfine] )); then
    print -u2 -r -- "$script_name: hyperfine is required"
    exit 1
fi

typeset shell_command="zsh -f"
(( ${#opt_rc[@]} == 0 )) || shell_command="zsh -i"

# Keep the fixture independent of the invoking user's Git settings.
export GIT_CONFIG_GLOBAL=/dev/null
export GIT_CONFIG_NOSYSTEM=1

typeset base
base="$(mktemp -d)"
base="${base:A}"
trap 'rm -rf "$base"' EXIT

typeset repo="$base/repo"
command git init -q -b main -- "$repo"
command git -C "$repo" config user.email bench@example.com
command git -C "$repo" config user.name Bench
command git -C "$repo" config commit.gpgsign false
# A modest tree so that creating a worktree checks out real files.
typeset -i file dir
for (( dir = 0; dir <= 4; ++dir )); do
    command mkdir -p -- "$repo/dir$dir"
done
for (( file = 1; file <= 50; ++file )); do
    print -r -- "file $file" >"$repo/dir$(( file % 5 ))/file$file"
done
command git -C "$repo" add -A
command git -C "$repo" commit -q -m initial
print -r -- ".worktrees/" >>"$repo/.git/info/exclude"

typeset -i index
for (( index = 1; index <= worktrees; ++index )); do
    command git -C "$repo" worktree add -q -b "branch$index" \
        "$repo/.worktrees/w$index" HEAD
done
# Deleting a checkout leaves its registration behind as prunable.
for (( index = 1; index <= prunable; ++index )); do
    command rm -rf -- "$repo/.worktrees/w$index"
done
typeset linked="$repo/.worktrees/w$worktrees"

# Creation leaves a new worktree behind on every run; remove them before the
# next one so the fixture stays the same size throughout. This assumes every
# -s script under comparison uses wt's own `worktree-<name>` branch-naming
# convention; a script that names branches differently will leak worktrees and
# branches across samples instead of being cleaned up here.
typeset reset="$base/reset.zsh"
cat >"$reset" <<'RESET'
#!/bin/zsh
set -euo pipefail
repo="$1"
for branch in ${(f)"$(
    command git -C "$repo" for-each-ref --format='%(refname:short)' 'refs/heads/worktree-*'
)"}; do
    command git -C "$repo" worktree remove --force -- "$repo/.worktrees/${branch#worktree-}"
    command git -C "$repo" branch -q -D -- "$branch"
done
RESET

print -r -- "fixture: $worktrees linked worktrees, $prunable prunable; shell: $shell_command"
for script in "${scripts[@]}"; do
    print -r -- "script: $script"
done

# Time $1, run after sourcing each script in turn, passing on any further
# arguments to hyperfine.
bench() {
    local body="$1"
    shift
    local -a names commands
    local script
    for script in "${scripts[@]}"; do
        names+=(--command-name "$script")
        commands+=("source ${(q)script}; $body")
    done
    command hyperfine --shell "$shell_command" --warmup 3 "${names[@]}" \
        "$@" "${hyperfine_options[@]}" -- "${commands[@]}"
}

print
print -r -- "== wt: from a linked worktree, back to the main worktree"
bench "cd ${(q)linked}; wt"

print
print -r -- "== wt NAME: from the main worktree into a linked worktree"
bench "cd ${(q)repo}; wt w$worktrees"

print
print -r -- "== completion: candidates for wt <TAB>"
bench "cd ${(q)repo}; compadd() { :; }; CURRENT=2; _wt"

print
print -r -- "== wt: from the main worktree, create a linked worktree"
bench "cd ${(q)repo}; wt" --prepare "zsh ${(q)reset} ${(q)repo}"
