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

modules_reserved="base macos linux"

modules_has() {
    [[ -n $2 && $2 != *[[:space:]]* ]] || return 1
    case " $1 " in
    *" $2 "*) return 0 ;;
    esac
    return 1
}

modules_join() {
    sed '/^$/d' | LC_ALL=C sort -u | paste -sd' ' -
}

modules_available() {
    local dir=$1 file name
    if [[ ! -d $dir ]]; then
        echo "provision.sh: no such directory: $dir" >&2
        return 1
    fi
    for file in "$dir"/*.Brewfile; do
        [[ -e $file ]] || continue
        name=${file##*/}
        name=${name%.Brewfile}
        if ! modules_has "$modules_reserved" "$name"; then
            printf '%s\n' "$name"
        fi
    done | modules_join
}

modules_normalize() {
    local available=$1 name kept="" reserved="" unknown=""
    while IFS= read -r name || [[ -n $name ]]; do
        if [[ -z $name || $name == none ]]; then
            continue
        elif modules_has "$modules_reserved" "$name"; then
            reserved="$reserved $name"
        elif modules_has "$available" "$name"; then
            kept=$kept$'\n'$name
        else
            unknown="$unknown $name"
        fi
    done
    if [[ -n $reserved ]]; then
        echo "provision.sh: reserved modules$reserved cannot be selected; base and the OS module always install" >&2
        return 1
    fi
    if [[ -n $unknown ]]; then
        echo "provision.sh: unknown modules$unknown; available modules: ${available:-none}" >&2
        return 1
    fi
    printf '%s\n' "$kept" | modules_join
}

modules_default() {
    case $1 in
    macos) printf '%s\n' "$2" ;;
    linux) echo dev | modules_normalize "$2" ;;
    *)
        echo "provision.sh: no default modules for OS $1" >&2
        return 1
        ;;
    esac
}

modules_minus() {
    LC_ALL=C comm -23 \
        <(tr ' ' '\n' <<<"$1" | sed '/^$/d' | LC_ALL=C sort -u) \
        <(tr ' ' '\n' <<<"$2" | sed '/^$/d' | LC_ALL=C sort -u) | modules_join
}

modules_usage() {
    echo "usage: ./provision.sh [--remove-modules] [MODULE... | none]" >&2
}

modules_resolve() {
    local os=$1 brew_dir=$2 selection_file=$3 have_brew=$4
    local arg count available persisted="" persisted_set=0 installed
    shift 4
    modules_selection=""
    modules_dropped=""
    modules_remove=0
    count=$#
    while [[ $count -gt 0 ]]; do
        arg=$1
        shift
        count=$((count - 1))
        case $arg in
        --remove-modules) modules_remove=1 ;;
        "")
            echo "provision.sh: empty module name" >&2
            modules_usage
            return 2
            ;;
        -*)
            echo "provision.sh: unknown option $arg" >&2
            modules_usage
            return 2
            ;;
        *) set -- "$@" "$arg" ;;
        esac
    done
    if [[ $# -gt 1 ]]; then
        for arg in "$@"; do
            if [[ $arg == none ]]; then
                echo "provision.sh: none cannot be combined with other modules" >&2
                modules_usage
                return 2
            fi
        done
    fi

    available=$(modules_available "$brew_dir") || return 1
    if [[ -e $selection_file ]]; then
        persisted=$(tr -s '[:space:]' '\n' <"$selection_file" | modules_normalize "$available") || {
            echo "provision.sh: fix or delete $selection_file" >&2
            return 1
        }
        persisted_set=1
    fi

    if [[ $# -gt 0 ]]; then
        modules_selection=$(printf '%s\n' "$@" | modules_normalize "$available") || return 1
    elif [[ $persisted_set -eq 1 ]]; then
        modules_selection=$persisted
    else
        modules_selection=$(modules_default "$os" "$available") || return 1
    fi

    # Homebrew without a persisted selection has the default installed, because
    # the Brewfile loader falls back to that same default.
    if [[ $have_brew -eq 0 ]]; then
        installed=""
    elif [[ $persisted_set -eq 1 ]]; then
        installed=$persisted
    else
        installed=$(modules_default "$os" "$available") || return 1
    fi
    modules_dropped=$(modules_minus "$installed" "$modules_selection") || return 1
    if [[ -n $modules_dropped && $modules_remove -eq 0 ]]; then
        echo "provision.sh: this selection drops installed modules: $modules_dropped" >&2
        echo "Add them to the selection, or pass --remove-modules to uninstall their packages." >&2
        return 3
    fi
    return 0
}

modules_persist() {
    mkdir -p "$(dirname "$1")"
    if [[ -n $2 ]]; then
        printf '%s\n' "$2"
    fi >"$1"
}
