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

# New npm, cargo and go releases wait this long before provisioning installs
# them, as in bunfig.toml and Renovate. brew bundle hard-codes a one-day wait
# for npm (RELEASE_COOLDOWN_DAYS in Library/Homebrew/release_cooldown.rb) and
# none for cargo or go. Installing an npm package without scripts or building a
# go package runs none of its code, so brew bundle may install those first and
# npm_install_global and go_install_global replace them before anything runs
# them. Building a crate runs its build scripts and macros, so
# cargo_install_global runs before brew bundle.
release_cooldown_days=3

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
    version=$(printf '%s' "$view_json" | node -e "$pick" "$global_root" "$package" "$release_cooldown_days") || return 1
    if [[ -f $global_root/$package/package.json ]]; then
        installed=$(node -p 'require(process.argv[1]).version' "$global_root/$package/package.json") || return 1
    fi
    if [[ $installed != "$version" ]]; then
        npm install -g --ignore-scripts "--min-release-age=$release_cooldown_days" "$package@$version" || return 1
    fi
}

# usage: cargo_install_global CRATE
# A version without a publish time in the index counts as new. brew bundle
# counts a crate installed from a path as installed and one from git as not, so
# this leaves the first alone and replaces the second.
cargo_install_global() {
    local crate=$1 list installed lower index_path index global_root version
    local pick='
const semver = require(process.argv[1] + "/npm/node_modules/semver");
const [, , name, days] = process.argv;
const cutoff = Date.now() - Number(days) * 86400000;
const version = require("fs").readFileSync(0, "utf8").split("\n").filter(Boolean)
    .map((line) => JSON.parse(line))
    .filter((v) => !v.yanked && semver.valid(v.vers) && !semver.prerelease(v.vers) && Date.parse(v.pubtime) <= cutoff)
    .map((v) => v.vers)
    .sort(semver.rcompare)[0];
if (!version) {
    console.error(name + ": no release is " + days + " days old");
    process.exit(1);
}
console.log(version);
'
    list=$(cargo install --list) || return 1
    installed=$(awk -v name="$crate" '/^[^ ]/ && $1 == name { print (NF == 2 ? substr($2, 2, length($2) - 2) : $3 ~ /^\(\// ? "(path)" : "(git)") }' <<<"$list") || return 1
    [[ $installed != "(path)" ]] || return 0
    lower=$(tr '[:upper:]' '[:lower:]' <<<"$crate") || return 1
    case ${#lower} in
    1 | 2) index_path=${#lower}/$lower ;;
    3) index_path=3/${lower:0:1}/$lower ;;
    *) index_path=${lower:0:2}/${lower:2:2}/$lower ;;
    esac
    index=$(curl -fsSL --proto =https "https://index.crates.io/$index_path") || return 1
    global_root=$(npm root -g) || return 1
    version=$(printf '%s\n' "$index" | node -e "$pick" "$global_root" "$crate" "$release_cooldown_days") || return 1
    if [[ $installed != "$version" ]]; then
        cargo install --locked "$crate" --version "=$version" || return 1
    fi
}

# usage: go_install_global PACKAGE
# The candidates follow what `go install PACKAGE@latest` chooses: the highest
# release, else the highest prerelease, else the newest commit. A binary built
# from a checkout records no module checksum, which is how it is told apart
# from one that go install fetched.
go_install_global() {
    local package=$1 name gobin gopath binary info fields ipath imodule iversion isum
    local installed='' module error versions candidates candidate found version='' now cutoff
    name=${package##*/}
    if [[ $name =~ ^v[1-9][0-9]*$ && $name != v1 ]]; then
        name=${package%/*}
        name=${name##*/}
    fi
    gobin=$(go env GOBIN) || return 1
    if [[ -z $gobin ]]; then
        gopath=$(go env GOPATH) || return 1
        gobin=${gopath%%:*}/bin
    fi
    binary=$gobin/$name
    if [[ -e $binary ]]; then
        info=$(go version -m "$binary") || return 1
        fields=$(awk -F '\t' '$2 == "path" { path = $3 } $2 == "mod" { mod = $3 " " $4 " " $5 } END { print path, mod }' <<<"$info") || return 1
        read -r ipath imodule iversion isum <<<"$fields"
        if [[ $ipath == "$package" ]]; then
            [[ -n $isum ]] || return 0
            installed=$iversion
        fi
    fi
    module=$package
    until error=$(go list -m -f '{{.Path}}' "$module@latest" 2>&1 >/dev/null); do
        if [[ $module != */* ]]; then
            printf '%s\nprovision: no module provides %s\n' "$error" "$package" >&2
            return 1
        fi
        module=${module%/*}
    done
    # The owner of these dotfiles publishes msrl.dev, mostly without tags, so a
    # wait there would only stop provisioning for three days after each push.
    if [[ $package == msrl.dev/* ]]; then
        version=$(go list -m -f '{{.Version}}' "$module@latest") || return 1
    else
        versions=$(go list -m -versions -f '{{range .Versions}}{{println .}}{{end}}' "$module@latest") || return 1
        candidates=$(awk '!/-/' <<<"$versions") || return 1
        [[ -n $candidates ]] || candidates=$versions
        [[ -n $candidates ]] || candidates=latest
        now=$(date +%s) || return 1
        cutoff=$((now - release_cooldown_days * 86400))
        while read -r candidate; do
            found=$(go list -m -f '{{.Version}} {{.Time.Unix}}' "$module@$candidate") || return 1
            if ((${found#* } <= cutoff)); then
                version=${found%% *}
                break
            fi
        done < <(awk '{ line[NR] = $0 } END { for (i = NR; i > 0; i--) print line[i] }' <<<"$candidates")
    fi
    if [[ -z $version ]]; then
        if [[ -n $installed ]]; then
            found=$(go list -m -f '{{.Time.Unix}}' "$imodule@$installed") || return 1
            ((found > cutoff)) || return 0
        fi
        echo "$package: no version is $release_cooldown_days days old" >&2
        return 1
    fi
    if [[ $installed != "$version" ]]; then
        go install "$package@$version" || return 1
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
