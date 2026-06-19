#!/bin/bash

# Copyright © 2025-2026 Michael Shields
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
    # Command substitution drops errexit/pipefail; restore them so a
    # failure in starship/tr/sed still aborts the whole script. The git
    # hash lookup is the exception: it fails on an unborn HEAD (a repo
    # with no commits yet) or outside a repo, and either is a valid state
    # in which we simply omit the hash rather than blanking the line.
    set -eo pipefail
    (
        if hash=$(git rev-parse --short HEAD 2>/dev/null); then
            echo "$hash"
        fi
        starship module git_status
        echo '' # Starship does not print a newline
    ) | tr -d '\n' | sed -e $'s/\x1b\\[[0-9;]*m//g' -e 's/^ *//' -e 's/ *$//'
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
    secs=$((now - ($1 - $2)))
    if ((secs < 0)); then
        secs=0
    elif ((secs > $2)); then
        secs=$2
    fi
}

right=''
if [[ -n "$five_reset" ]]; then
    clamp_elapsed "$five_reset" "$five_window"
    right=$(printf '%d:%02d' $((secs / 3600)) $((secs % 3600 / 60)))
fi
if [[ -n "$five_pct" ]]; then
    style=''
    # Bold when usage runs ahead of the elapsed share of the window.
    if [[ -n "$five_reset" ]] && ((five_pct * five_window > secs * 100)); then
        style=$bold
    fi
    right+="${right:+ }$style$five_pct%${style:+$nobold}"
fi
if [[ -n "$week_pct" ]]; then
    style=''
    if [[ -n "$week_reset" ]]; then
        clamp_elapsed "$week_reset" "$week_window"
        if ((week_pct * week_window > secs * 100)); then
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
if [[ -n "$cost_cents" ]] && ((cost_cents != 0)); then
    cost_seg=$(printf '$%d.%02d' $((cost_cents / 100)) $((cost_cents % 100)))
fi

# Join the non-empty segments with ' · '. A leading space provides the
# left margin (the statusline config sets padding to 0); it is added only
# when there is something to show, and never doubles up because no segment
# carries its own leading space.
out=''
for seg in "$left" "$model_seg" "$right" "$cost_seg"; do
    [[ -z "$seg" ]] && continue
    out+="${out:+ · }$seg"
done
printf '%s' "${out:+ }$out"
