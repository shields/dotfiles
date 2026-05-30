#!/bin/bash

set -euo pipefail

# Copyright © 2025 Michael Shields
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

temp_dir="$(mktemp -d)"
trap 'rm -rf "$temp_dir"' EXIT
trap 'exit 1' INT TERM
cd "$temp_dir"

curl -OLs https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FontPatcher.zip
unzip FontPatcher.zip

cp '/System/Library/Fonts/Supplemental/Andale Mono.ttf' .

fontforge -script font-patcher --complete --careful 'Andale Mono.ttf'

cp AndaleMonoNerdFont-Regular.ttf "$HOME/Library/Fonts"
