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

# shellcheck shell=bash

# New npm releases wait this long before provisioning installs them, as in
# bunfig.toml and Renovate. brew bundle hard-codes a one-day wait
# (RELEASE_COOLDOWN_DAYS in Library/Homebrew/release_cooldown.rb) and installs
# without scripts, so npm_install_global then reinstalls each package at a
# version that has waited this long, before anything runs it.
npm_release_cooldown_days=3

# usage: npm_install_global PACKAGE
# cf publishes only prereleases under its latest tag, and npm's own
# --min-release-age falls back from a prerelease to the old stable version, so
# the version is chosen here; that flag holds back the dependencies. The
# registry keeps the publish time of an unpublished version, so only the
# versions it still lists are candidates. The semver that npm bundles orders
# prereleases correctly, and node is in the base module.
npm_install_global() {
    local package=$1 global_root view_json version installed=
    local pick='
const semver = require(process.argv[1] + "/npm/node_modules/semver");
const view = JSON.parse(require("fs").readFileSync(0, "utf8"));
const [, , name, days] = process.argv;
const latest = view["dist-tags"]?.latest;
const cutoff = Date.now() - Number(days) * 86400000;
const listed = new Set(view.versions ?? []);
const version = semver.valid(latest) && Object.entries(view.time ?? {})
    .filter(([v, t]) => listed.has(v) && semver.valid(v) && semver.lte(v, latest) && Date.parse(t) <= cutoff)
    .map(([v]) => v)
    .sort(semver.rcompare)[0];
if (!version) {
    console.error(name + ": no release up to its latest tag is " + days + " days old");
    process.exit(1);
}
console.log(version);
'
    global_root=$(npm root -g) || return 1
    view_json=$(npm view "$package" time dist-tags versions --json) || return 1
    version=$(printf '%s' "$view_json" | node -e "$pick" "$global_root" "$package" "$npm_release_cooldown_days") || return 1
    if [[ -f $global_root/$package/package.json ]]; then
        installed=$(node -p 'require(process.argv[1]).version' "$global_root/$package/package.json") || return 1
    fi
    if [[ $installed != "$version" ]]; then
        npm install -g --ignore-scripts "--min-release-age=$npm_release_cooldown_days" "$package@$version" || return 1
    fi
}

# The directory that Oh My Zsh's generated completions live in, as .zshrc
# computes it.
completion_dir() {
    printf '%s/completions\n' "${ZSH_CACHE_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}/oh-my-zsh}"
}

completion_remove_startup_cache() {
    rm -f "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/$1-completion"
}

# usage: completion_install NAME COMMAND [ARGS...]
completion_install() {
    local name=$1 dir temp
    shift
    dir=$(completion_dir) || return 1
    # compinit ignores a group-writable directory, which umask 002 creates.
    (umask 022 && mkdir -p "$dir") || return 1
    chmod go-w "$dir" || return 1
    temp=$(mktemp "$dir/.$name.XXXXXX") || return 1
    if ! "$@" >"$temp" || [[ ! -s $temp ]]; then
        rm -f "$temp"
        echo "provision: cannot generate the $name completion from: $*" >&2
        return 1
    fi
    if ! chmod 644 "$temp" || ! mv -f "$temp" "$dir/_$name"; then
        rm -f "$temp"
        return 1
    fi
    completion_remove_startup_cache "$name"
}

completion_remove() {
    local dir
    dir=$(completion_dir) || return 1
    rm -f "$dir/_$1"
    completion_remove_startup_cache "$1"
}
