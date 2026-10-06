#!/bin/bash

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

# Delete the identifiers that Claude Code and Codex stored in this home while
# it was provisioned, so that VMs cloned from one image do not share them.
# Each agent creates new ones at its next start. Run as the user, not as root.

set -euo pipefail

die() {
    printf 'reset-identity.sh: %s\n' "$*" >&2
    exit 1
}

[[ -n ${HOME:-} ]] || die "HOME is not set"
command -v jq >/dev/null || die "jq is not installed"

rm -f "$HOME/.codex/installation_id"

claude_json=$HOME/.claude.json
if [[ -e $claude_json ]]; then
    tmp=$(mktemp "$claude_json.XXXXXX")
    trap 'rm -f "$tmp"' EXIT
    jq 'del(.machineID, .userID, .firstStartTime, .firstStartVersion)' "$claude_json" >"$tmp" ||
        die "cannot update $claude_json"
    [[ -s $tmp ]] || die "cannot update $claude_json"
    mv -f "$tmp" "$claude_json"
fi
