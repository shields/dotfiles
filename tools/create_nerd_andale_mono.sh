#!/bin/bash

set -euo pipefail

temp_dir="$(mktemp -d)"
trap 'rm -r "$temp_dir"' 0 1 15
cd "$temp_dir"

curl -OLs https://github.com/ryanoasis/nerd-fonts/releases/latest/download/FontPatcher.zip
unzip FontPatcher.zip

cp '/System/Library/Fonts/Supplemental/Andale Mono.ttf' .

fontforge -script font-patcher --complete --careful 'Andale Mono.ttf'

cp AndaleMonoNerdFont-Regular.ttf "$HOME/Library/Fonts"
