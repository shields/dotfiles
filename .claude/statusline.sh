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
# The JSON carries only resets_at, so the countdown and the run-rate
# comparisons below depend on these assumed window lengths.
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

# Fable's weekly usage is exposed only by the undocumented OAuth usage
# endpoint, never in the statusline JSON, so we fetch it ourselves. The
# outcome — success or failure — is cached for 60s and the request is
# time-bounded, so a slow, hung, or failing endpoint is retried at most once
# a minute and never stalls the 5-second render loop. A cached failure is an
# empty object that parses to nothing, so the caller shows "Fable ??" rather
# than a stale number or a silently missing segment. Label and percent come
# from the endpoint.
fable_usage() {
    local cache_dir="${XDG_CACHE_HOME:-$HOME/Library/Caches}/claude-code-statusline"
    local cache="$cache_dir/usage.json"
    local mtime
    mtime=$(stat -f %m "$cache" 2>/dev/null) || mtime=0
    if ((now - mtime >= 60)); then
        local tok=''
        if [[ -f "$HOME/.claude/.credentials.json" ]]; then
            tok=$(jq -r '.claudeAiOauth.accessToken // empty' \
                "$HOME/.claude/.credentials.json" 2>/dev/null) || tok=''
        fi
        if [[ -z "$tok" ]]; then
            tok=$(security find-generic-password -s 'Claude Code-credentials' -w 2>/dev/null |
                jq -r '.claudeAiOauth.accessToken // empty' 2>/dev/null) || tok=''
        fi
        mkdir -p "$cache_dir" 2>/dev/null
        local tmp
        if tmp=$(mktemp "$cache.XXXXXX" 2>/dev/null); then
            # -f so an HTTP error (e.g. an expired token's 401) is a failure,
            # not a valid-JSON error body we would cache and misparse. The
            # token is passed through a process-substituted header file
            # (printf is a shell builtin) so it never lands in curl's argv,
            # where any same-user process could read it from ps.
            if [[ -n "$tok" ]] &&
                curl -fsS --connect-timeout 1 --max-time 2 \
                    -H @<(printf 'Authorization: Bearer %s\n' "$tok") \
                    -H 'anthropic-beta: oauth-2025-04-20' \
                    -o "$tmp" https://api.anthropic.com/api/oauth/usage 2>/dev/null &&
                jq -e . "$tmp" >/dev/null 2>&1; then
                : # $tmp holds the fresh response
            else
                printf '{}' >"$tmp" # cached failure → parses to nothing → "Fable ??"
            fi
            mv -f "$tmp" "$cache" 2>/dev/null || rm -f "$tmp"
        fi
    fi
    local line
    line=$(jq -r '
        .limits[]?
        | select(.scope.model.display_name == "Fable")
        | "\(.scope.model.display_name)\t\(.percent | floor)"
    ' "$cache" 2>/dev/null | head -n1) || true
    [[ -n "$line" ]] || return 1
    printf '%s\n' "$line"
}

right=''
if [[ -n "$five_reset" ]]; then
    clamp_elapsed "$five_reset" "$five_window"
    # Time left, not elapsed, so it counts down 5:00 -> 0:00 alongside the
    # percentages.
    left_secs=$((five_window - secs))
    right=$(printf '%d:%02d' $((left_secs / 3600)) $((left_secs % 3600 / 60)))
fi
if [[ -n "$five_pct" ]]; then
    style=''
    # Bold when usage runs ahead of the elapsed share of the window.
    if [[ -n "$five_reset" ]] && ((five_pct * five_window > secs * 100)); then
        style=$bold
    fi
    # Displayed as remaining, not used, so it counts down 100% -> 0%.
    right+="${right:+ }$style$((100 - five_pct))%${style:+$nobold}"
fi
if [[ -n "$week_pct" ]]; then
    style=''
    if [[ -n "$week_reset" ]]; then
        clamp_elapsed "$week_reset" "$week_window"
        if ((week_pct * week_window > secs * 100)); then
            style=$bold
        fi
    fi
    right+="${right:+ · }week $style$((100 - week_pct))%${style:+$nobold}"
fi

# Fable's weekly-scoped limit isn't in the statusline JSON; fable_usage
# pulls its label and percent from the usage endpoint. Only bother in a
# subscriber session — one that reports rate limits — so API-key sessions
# with no OAuth token don't show a permanent "Fable ??" or make needless
# calls. The scoped window shares the account's weekly reset, so reuse
# week_reset/week_window to bold it on the same run-rate basis as above.
if [[ -n "$five_reset" || -n "$week_reset" ]]; then
    if fable_line=$(fable_usage); then
        IFS=$'\t' read -r fable_name fable_pct <<<"$fable_line"
        style=''
        if [[ -n "$week_reset" ]]; then
            clamp_elapsed "$week_reset" "$week_window"
            if ((fable_pct * week_window > secs * 100)); then
                style=$bold
            fi
        fi
        right+="${right:+ · }$fable_name $style$((100 - fable_pct))%${style:+$nobold}"
    else
        right+="${right:+ · }Fable ??"
    fi
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
