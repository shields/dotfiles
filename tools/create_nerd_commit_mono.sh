#!/bin/bash

set -euo pipefail

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

# Patch the custom Commit Mono build in commit-mono/ (generated at
# https://commitmono.com/ from commit-mono/custom-settings.json) with the Nerd
# Fonts glyphs, producing "CommitMonoShields Nerd Font" in Library/Fonts/ for
# provision.sh to install. tests/test_nerd_commit_mono.py checks the result.
#
# The flags match how the Nerd Fonts release builds its own CommitMono, so this
# is the stock "CommitMono Nerd Font" plus the custom weight, alternates, and
# metrics. In particular there is no --careful: Commit Mono ships its own
# Powerline glyphs sized for its default line height, and the patcher's
# replacements are stretched to fill the taller custom cell.
#
# The patcher runs in the Nerd Fonts project's own image rather than a local
# fontforge: fontforge isn't in the Brewfile, so `brew bundle cleanup` would
# remove it, and the image bundles the fontforge build the patcher is tested
# against. The image tag is a font-patcher script version (4.27.3 shipped
# with Nerd Fonts v3.5.1) and is pinned so the patcher can't drift away from
# PATCHED_RANGES in tests/test_nerd_commit_mono.py, which is transcribed from
# that version; bump the tag and that table together.

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source_dir="$repo_dir/commit-mono"
output_dir="$repo_dir/Library/Fonts"

temp_dir="$(mktemp -d)"
trap 'rm -rf "$temp_dir"' EXIT
trap 'exit 1' INT TERM

mkdir "$temp_dir/out"

docker run --rm \
    --volume "$source_dir:/in:ro" \
    --volume "$temp_dir/out:/out" \
    nerdfonts/patcher:4.27.3 --complete --no-progressbars

# The container's entrypoint patches each font in a separate job, so check the
# whole set came out rather than trusting its exit status. -maxdepth 1
# matches what the cp below actually picks up, so a patcher that ever nests
# its output is caught here instead of silently copying fewer fonts.
expected="$(find "$source_dir" -maxdepth 1 -name '*.otf' | wc -l)"
produced="$(find "$temp_dir/out" -maxdepth 1 -name '*.otf' | wc -l)"
if [[ $produced -ne $expected ]]; then
    echo "expected $expected patched fonts, got $produced" >&2
    exit 1
fi

mkdir -p "$output_dir"
rm -f "$output_dir"/CommitMonoShieldsNerdFont-*.otf
cp "$temp_dir"/out/*.otf "$output_dir"
