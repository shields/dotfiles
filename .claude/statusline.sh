#!/bin/bash

# Commands are resolved via PATH, matching the other scripts in this repo;
# hardcoded /opt/homebrew paths would break Intel Macs. The style guide's
# "prefer absolute paths" refers to file paths.

set -euo pipefail

input=$(cat)
if [[ -z "$input" ]]; then
    input='{}'
fi

# Strip ANSI color codes for plain text output, to avoid confusing
# Claude Code's rendering engine. The $'...' quoting makes bash pass a
# literal ESC byte to sed, so no sed \x escape support is needed.
left=$(
    # Command substitution drops errexit/pipefail; restore them so a git
    # failure (e.g. not a repository) still fails the whole script.
    # stderr is deliberately not redirected: surface failures loudly
    # rather than render a silently degraded status line.
    set -eo pipefail
    (
        echo -n ' '; git rev-parse --short HEAD
        starship module git_status
        echo '' # Starship does not print a newline
    ) | tr -d '\n' | sed -e $'s/\x1b\\[[0-9;]*m//g' -e 's/ *$//'
)

# Model, effort, cost, and rate-limit data from the statusline JSON; each
# field may be absent (rate_limits only appears after the first API response;
# effort is absent when the current model has no reasoning-effort setting).
# Cost is read as integer cents (dollars * 100, rounded) so the display can
# format it without bash floating-point arithmetic.
# The str()/num() helpers coerce each field to a single scalar line so the
# positional reads stay aligned: an absent or wrong-typed value becomes an
# empty line rather than being dropped or pretty-printed across several.
# The token count is left to Claude Code's own counter, shown in verbose
# mode. Claude Code always sends valid JSON; if it ever doesn't, jq emits
# nothing and the failing read aborts the script — intentional fail-fast,
# not a bug to paper over with `|| true`. Empty field values are omitted
# from display below and never reach bash arithmetic.
{
    read -r model
    read -r effort
    read -r cost_cents
    read -r five_pct
    read -r five_reset
    read -r week_pct
    read -r week_reset
} < <(jq -r '
    def num(v; r): v | if type == "number" then r else "" end;
    def str(v): v | if type == "string" then . else "" end;
    str(.model.display_name),
    str(.effort.level),
    num(.cost.total_cost_usd; . * 100 | round),
    num(.rate_limits.five_hour.used_percentage; round),
    num(.rate_limits.five_hour.resets_at; floor),
    num(.rate_limits.seven_day.used_percentage; round),
    num(.rate_limits.seven_day.resets_at; floor)
' <<<"$input")

bold=$'\e[1m'
nobold=$'\e[22m'
now=$(date +%s)
# The JSON carries only resets_at, so the elapsed-time display depends on
# these assumed window lengths.
five_window=18000  # 5 hours
week_window=604800 # 7 days

# Set secs to the time elapsed in the window of length $2 that resets at
# $1, clamped into [0, $2] so the bold comparisons below cannot misfire
# on clock skew.
clamp_elapsed() {
    secs=$(( now - ($1 - $2) ))
    if (( secs < 0 )); then
        secs=0
    elif (( secs > $2 )); then
        secs=$2
    fi
}

right=''
if [[ -n "$five_reset" ]]; then
    clamp_elapsed "$five_reset" "$five_window"
    right=$(printf '%d:%02d' $(( secs / 3600 )) $(( secs % 3600 / 60 )))
fi
if [[ -n "$five_pct" ]]; then
    style=''
    # Bold when usage runs ahead of the elapsed share of the window.
    if [[ -n "$five_reset" ]] && (( five_pct * five_window > secs * 100 )); then
        style=$bold
    fi
    right+="${right:+ }$style$five_pct%${style:+$nobold}"
fi
if [[ -n "$week_pct" ]]; then
    style=''
    if [[ -n "$week_reset" ]]; then
        clamp_elapsed "$week_reset" "$week_window"
        if (( week_pct * week_window > secs * 100 )); then
            style=$bold
        fi
    fi
    right+="${right:+ · }week $style$week_pct%${style:+$nobold}"
fi

model_seg=''
if [[ -n "$model" ]]; then
    # The 1M-context model reports a verbose display name; abbreviate it.
    model=${model/'(1M context)'/(1M)}
    model_seg="$model${effort:+ $effort}"
fi

cost_seg=''
if [[ -n "$cost_cents" ]] && (( cost_cents != 0 )); then
    cost_seg=$(printf '$%d.%02d' $(( cost_cents / 100 )) $(( cost_cents % 100 )))
fi

out="$left"
out+="${model_seg:+ · $model_seg}"
out+="${right:+ · $right}"
out+="${cost_seg:+ · $cost_seg}"
printf '%s' "$out"
