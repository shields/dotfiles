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

# Time how long a new interactive login shell takes to reach its first prompt,
# and optionally profile where that time goes. Every hyperfine sample starts
# `zsh -l -i` on a pseudo-terminal (zsh/zpty) with `exit` already typed ahead
# and waits for it to finish, so a sample spans the startup files, the precmd
# hooks, drawing the first prompt, and the exit hooks: what opening a terminal
# tab feels like. The floor is the same run of `zsh -f`, which skips every
# $ZDOTDIR startup file (the one global /etc/zshenv still always runs, per
# zsh(1), but is empty on a stock install).
#
# Candidates are .zshrc files, the repo's by default, loaded through a
# throwaway ZDOTDIR so that an edited copy can be timed before it is installed.
# The repo's .zprofile is always used alongside, and whatever the candidate
# sources itself (~/.zsh.d, oh-my-zsh, ...) comes from $HOME as installed. The
# completion dumps and the history file are copied into the ZDOTDIR, so startup
# is timed with warm caches and a realistic history without the real files
# being written to. The environment is inherited: run this from the terminal
# whose startup you care about, because plugins key off TERM_PROGRAM.
#
# usage: zsh tools/bench_startup.zsh [-C DIR] [-p] [--ready] [-s ZSHRC]...
#            [-- HYPERFINE-OPTION...]
#
# -C DIR  Start the shell in DIR instead of an empty directory. A git checkout
#         makes the prompt and the git plugins do more work, and git-auto-fetch
#         will use the network.
# -p      Also profile each candidate with one traced run, reported as time by
#         phase, by file or function, and by step, plus zprof's function table.
#         Tracing overhead scales with how many lines run (a heavy,
#         completion-laden .zshrc can pay several times its own wall time), so
#         take totals from the benchmark and attribution from the profile.
# -s      Repeat to compare .zshrc candidates side by side.
# --ready Wait for zsh-defer's queue before exiting, to measure initialization
#         of all features as well as the first prompt. Without this flag,
#         deferred tasks may never run because exit is already typed ahead.
#         The traced profile misses code run inside widgets; zprof still
#         measures those functions.
#
# Anything after -- goes to hyperfine.

set -euo pipefail

zmodload zsh/zutil

# Inside a function $0 is the function's own name, so capture the script's.
typeset script_name="${0:t}"
typeset repo="${${0:A:h}:h}"

usage() {
    print -u2 -r -- "usage: $script_name [-C dir] [-p] [--ready] [-s zshrc]..." \
        "[-- hyperfine options]"
    exit 2
}

typeset -a opt_dir opt_profile opt_scripts opt_help opt_ready
zparseopts -D -F -- C:=opt_dir p=opt_profile -profile=opt_profile \
    s+:=opt_scripts h=opt_help -help=opt_help -ready=opt_ready || usage
(( ${#opt_help[@]} == 0 )) || usage
export BENCH_STARTUP_READY=${#opt_ready[@]}
typeset -a hyperfine_options=("$@")
# zparseopts only consumes the first --, so a second one here would collide
# with the -- this script itself puts before hyperfine_commands below.
(( ${hyperfine_options[(I)--]} == 0 )) || usage

typeset -a candidates
candidates=("${(@)opt_scripts:#-s}")
candidates=("${candidates[@]:A}")
(( ${#candidates[@]} > 0 )) || candidates=("$repo/.zshrc")
typeset zprofile="$repo/.zprofile"
typeset candidate
for candidate in "${candidates[@]}" "$zprofile"; do
    if [[ ! -r "$candidate" ]]; then
        print -u2 -r -- "$script_name: cannot read $candidate"
        exit 1
    fi
done

if ! (( $+commands[hyperfine] )); then
    print -u2 -r -- "$script_name: hyperfine is required"
    exit 1
fi

if ! zmodload zsh/zpty 2>/dev/null; then
    print -u2 -r -- "$script_name: the zsh/zpty module is required"
    exit 1
fi

typeset base
base="$(mktemp -d)"
base="${base:A}"
trap 'rm -rf "$base"' EXIT

typeset start_dir
if (( ${#opt_dir[@]} )); then
    start_dir="${opt_dir[2]:A}"
    if [[ ! -d "$start_dir" ]]; then
        print -u2 -r -- "$script_name: not a directory: $start_dir"
        exit 1
    fi
else
    start_dir="$base/cwd"
    command mkdir -- "$start_dir"
fi

# Start a shell on a pseudo-terminal with `exit` typed ahead and wait for it
# to finish. hyperfine runs this directly, so it is a script rather than a
# function; the profile run below uses it too.
typeset driver="$base/run.zsh"
cat >"$driver" <<'DRIVER'
#!/bin/zsh -f
# usage: run.zsh START_DIR ZDOTDIR SHELL-COMMAND...
set -euo pipefail
zmodload zsh/zpty
cd -- "$1"
export ZDOTDIR="$2"
shift 2
zpty shell "$@"
if (( ${BENCH_STARTUP_READY:-0} )); then
    zpty -w shell 'if (( $+functions[zsh-defer] )); then zsh-defer -a exit; else exit; fi'
else
    zpty -w shell exit
fi
while zpty -r shell >/dev/null; do :; done
zpty -d shell
DRIVER

# Populate $base/$1 as a ZDOTDIR that loads candidate $2 as its .zshrc. With
# $3 non-empty, the shell also traces every step to $base/$1/trace and writes
# zprof's table to $base/$1/zprof when it exits. The driver exports ZDOTDIR,
# so the generated files can refer to it.
make_zdotdir() {
    local zdotdir="$base/$1" candidate="$2" profile="${3:-}"
    local file
    command mkdir -- "$zdotdir"
    # -p keeps each dump older than its .zwc, or oh-my-zsh would recompile it.
    for file in "$HOME"/.zcompdump*(N) "$HOME/.zsh_history"(N); do
        command cp -p -- "$file" "$zdotdir/"
    done
    # /etc/zshrc on macOS already points HISTFILE into ZDOTDIR; this covers
    # systems that do not, so the benchmark never appends to the real history.
    print -r -- 'HISTFILE="$ZDOTDIR/.zsh_history"' >"$zdotdir/.zshenv"
    # Likewise isolate anything a candidate keys off $XDG_CACHE_HOME (e.g.
    # .zshrc's _startup_cached) under this candidate's own ZDOTDIR: shared
    # across this candidate's own hyperfine samples, so --warmup populates it
    # like real steady-state use, but never the real machine's cache and
    # never another candidate's.
    print -r -- 'export XDG_CACHE_HOME="$ZDOTDIR/cache"' >>"$zdotdir/.zshenv"
    print -r -- "source ${(q)zprofile}" >"$zdotdir/.zprofile"
    print -r -- "source ${(q)candidate}" >"$zdotdir/.zshrc"
    [[ -n "$profile" ]] || return 0
    # Timestamps come from $EPOCHREALTIME rather than %D{%s.%6.}: strftime in
    # every PS4 expansion costs several times more per traced line. The price
    # is that a function running under nopromptsubst (oh-my-zsh's title) gets
    # no timestamps, so its steps are charged to the step before it. Tracing
    # starts here, in .zshenv, so that /etc/zprofile onward is covered. A
    # candidate that itself toggles xtrace off (to silence a noisy plugin) or
    # redirects fd 2 hides that stretch from the trace; its time then lands on
    # whatever step runs next instead of being flagged as untraced.
    cat >>"$zdotdir/.zshenv" <<'ZSHENV'
zmodload zsh/zprof
zmodload zsh/datetime
setopt prompt_subst
PS4=$'+${EPOCHREALTIME}\t%N\t%i\t'
exec 2>>"$ZDOTDIR/trace"
setopt xtrace
ZSHENV
    # zle-line-init runs once the first prompt is on screen. Wrapping the
    # candidate's own widget by hand costs nothing measurable, whereas
    # autoloading add-zle-hook-widget would charge a few milliseconds to the
    # prompt phase. zle runs widgets with xtrace off, so the wrapper writes its
    # own trace line. The exit hook is appended after the candidate has
    # loaded, so it runs after any exit hooks the candidate registers and the
    # trace covers them too.
    cat >>"$zdotdir/.zshrc" <<'ZSHRC'
: bench-rc-done
if (( $+widgets[zle-line-init] )); then
    zle -A zle-line-init _bench_startup_line_init
fi
_bench_startup_first_prompt() {
    print -u2 -r -- "+$EPOCHREALTIME"$'\t_bench_startup_first_prompt\t1\t: bench-first-prompt'
    if (( $+widgets[_bench_startup_line_init] )); then
        # Deliberately bare: no -w/-Nw and no argument forwarding. A
        # candidate that chains hooks via zsh's own add-zle-hook-widget
        # dispatches by looking up $WIDGET, which -w would change to
        # _bench_startup_line_init here, silently breaking every hook it
        # chained onto zle-line-init. Keep this call exactly as-is.
        zle _bench_startup_line_init
    fi
}
zle -N zle-line-init _bench_startup_first_prompt
_bench_startup_exit() {
    unsetopt xtrace
    zprof >"$ZDOTDIR/zprof"
}
zshexit_functions+=(_bench_startup_exit)
ZSHRC
}

print -r -- "shell: $(zsh --version); TERM_PROGRAM=${TERM_PROGRAM:-}"
print -r -- "start directory: $start_dir"
for candidate in "${candidates[@]}"; do
    print -r -- "candidate: $candidate"
done

typeset -a hyperfine_names hyperfine_commands
typeset -i index
hyperfine_names=(--command-name "floor: zsh -f")
# ZDOTDIR is set to a directory that is never created: the inner -f means
# this shell never reads any $ZDOTDIR startup file, so nothing ever opens it.
hyperfine_commands=("zsh -f ${(q)driver} ${(q)start_dir} ${(q)base}/floor zsh -f -l -i")
for (( index = 1; index <= ${#candidates[@]}; ++index )); do
    make_zdotdir "bench-$index" "${candidates[index]}"
    hyperfine_names+=(--command-name "${candidates[index]}")
    hyperfine_commands+=("zsh -f ${(q)driver} ${(q)start_dir} ${(q)base}/bench-$index zsh -l -i")
done

print
print -r -- "== startup: login shell on a pseudo-terminal, until the first prompt is drawn and exit has run"
(( ! ${#opt_ready[@]} )) || print -r -- "   waiting for all deferred initialization before exit"
# hyperfine errors on a repeated flag rather than letting the last one win, so
# skip each of our own defaults the caller already passed through --.
typeset -a hyperfine_defaults
(( ${hyperfine_options[(I)--shell*]} + ${hyperfine_options[(I)-S]} + ${hyperfine_options[(I)-N]} )) ||
    hyperfine_defaults+=(-N)
(( ${hyperfine_options[(I)--warmup*]} + ${hyperfine_options[(I)-w]} )) ||
    hyperfine_defaults+=(--warmup 3)
command hyperfine "${hyperfine_defaults[@]}" "${hyperfine_names[@]}" "${hyperfine_options[@]}" -- "${hyperfine_commands[@]}"

(( ${#opt_profile[@]} )) || exit 0

# zsh's xtrace runs the PS4 timestamp for a nested command (a `[[ ... ]]` test
# or a command substitution) before finishing the line that contains it, so
# two "+timestamp\t..." records can land on one physical line with no
# separating newline. Split those apart first so every record below is one
# physical line, or the second record's fields get appended onto the first
# one's command instead of parsed as their own step. The $'...' quoting puts
# a literal tab in the pattern and a backslash-newline in the replacement,
# the only spellings every sed accepts.
normalize_trace() {
    LC_ALL=C command sed -E $'s/(.)(\\+[0-9]+\\.[0-9]+\t)/\\1\\\n\\2/g' "$1"
}

# Print each traced step as "milliseconds until the next step, where, line,
# command" from the trace at $1. A step's time is the gap before the next
# trace line, so a command's own cost lands on the line that ran it, and the
# call into a file or function is charged to the caller. The trace holds
# whatever bytes the startup files expand, so text tools run in the C locale.
trace_steps() {
    normalize_trace "$1" | LC_ALL=C command awk -F '\t' '
        $1 ~ /^\+[0-9]+\.[0-9]+$/ && NF >= 4 {
            t = substr($1, 2) + 0
            if (seen) printf "%.3f\t%s\t%s\t%s\n", (t - prev) * 1000, where, line, cmd
            seen = 1; prev = t; where = $2; line = $3; cmd = $4
        }'
}

# Print "steps untimed first rc_done first_prompt last" for the trace at $1,
# timestamps in seconds. Untimed lines are continuations of a traced command
# that contained a newline, or steps run under nopromptsubst.
trace_phases() {
    normalize_trace "$1" | LC_ALL=C command awk -F '\t' '
        $1 ~ /^\+[0-9]+\.[0-9]+$/ && NF >= 4 {
            t = substr($1, 2) + 0
            if (!steps) first = t
            steps++; last = t
            if ($4 == ": bench-rc-done") rc_done = t
            if ($4 == ": bench-first-prompt") first_prompt = t
            next
        }
        { untimed++ }
        END {
            printf "%d %d %.6f %.6f %.6f %.6f\n", steps, untimed, first, rc_done, first_prompt, last
        }'
}

typeset -F 1 ms
typeset -i steps untimed
typeset first rc_done first_prompt last where line cmd count entry
typeset -a lines
for (( index = 1; index <= ${#candidates[@]}; ++index )); do
    candidate="${candidates[index]}"
    make_zdotdir "prof-$index" "$candidate" trace
    # The first run warms whatever the copied caches did not cover.
    zsh -f "$driver" "$start_dir" "$base/prof-$index" zsh -l -i
    command rm -f -- "$base/prof-$index/trace" "$base/prof-$index/zprof"
    zsh -f "$driver" "$start_dir" "$base/prof-$index" zsh -l -i

    print
    print -r -- "== profile: $candidate"
    read -r steps untimed first rc_done first_prompt last \
        < <(trace_phases "$base/prof-$index/trace")
    if (( steps == 0 || rc_done == 0 || first_prompt == 0 )); then
        print -u2 -r -- "$script_name: incomplete trace in $base/prof-$index/trace" \
            "(does $candidate redirect fd 2 or reset PS4/xtrace?)"
        exit 1
    fi
    ms=$(( (last - first) * 1000 ))
    print -r -- "$steps steps traced over $ms ms" \
        "(tracing inflates this; $untimed untimed lines)"

    print
    print -r -- "-- by phase"
    ms=$(( (rc_done - first) * 1000 )); printf '%8.1f ms  startup files\n' "$ms"
    ms=$(( (first_prompt - rc_done) * 1000 )); printf '%8.1f ms  precmd hooks and first prompt\n' "$ms"
    ms=$(( (last - first_prompt) * 1000 )); printf '%8.1f ms  exit\n' "$ms"

    # Each table is sorted into an array and sliced rather than piped through
    # `head`: once the sorted output outgrows a pipe buffer, `head` closing
    # early would SIGPIPE sort, and pipefail+errexit would abort the script.
    print
    print -r -- "-- by file or function, self time (top 15)"
    lines=("${(@f)$(
        trace_steps "$base/prof-$index/trace" |
            LC_ALL=C command awk -F '\t' '{ ms[$2] += $1; n[$2]++ }
                END { for (w in ms) printf "%.3f\t%d\t%s\n", ms[w], n[w], w }' |
            LC_ALL=C command sort -t $'\t' -k1,1 -rn
    )}")
    for entry in "${(@)lines[1,15]}"; do
        IFS=$'\t' read -r ms count where <<<"$entry"
        printf '%8.1f ms  %5d steps  %s\n' "$ms" "$count" "${where/#$HOME/~}"
    done

    print
    print -r -- "-- slowest steps (top 25)"
    lines=("${(@f)$(
        trace_steps "$base/prof-$index/trace" |
            LC_ALL=C command sort -t $'\t' -k1,1 -rn
    )}")
    for entry in "${(@)lines[1,25]}"; do
        IFS=$'\t' read -r ms where line cmd <<<"$entry"
        printf '%8.1f ms  %s:%s  %s\n' "$ms" "${where/#$HOME/~}" "$line" "${cmd[1,100]}"
    done

    print
    print -r -- "-- zprof: functions by self time (top 15; time and self in ms)"
    # zprof's flat table ends at the second row of dashes.
    lines=("${(@f)$(
        command awk '/^-{20,}$/ { if (++dashes == 2) exit } { print }' \
            "$base/prof-$index/zprof"
    )}")
    print -rl -- "${(@)lines[1,17]}"
done
