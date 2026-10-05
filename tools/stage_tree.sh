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

set -euo pipefail

usage="usage: tools/stage_tree.sh DEST"

die() {
    printf 'stage_tree: %s\n' "$*" >&2
    exit 1
}

# Paths are compared by identity (-ef): a case-insensitive volume has several
# spellings of one directory, and rm -rf acts on whichever one it is given.
within() {
    local path=$1
    while [[ $path == /* ]]; do
        [[ -e $path && $path -ef $2 ]] && return 0
        [[ $path == / ]] && return 1
        path=${path%/*}
        path=${path:-/}
    done
    return 1
}

case ${1:-} in
-h | --help)
    printf '%s\n' "$usage"
    exit 0
    ;;
esac
[[ $# -eq 1 ]] || die "$usage"
dest=$1
[[ -n $dest ]] || die "DEST is empty"
[[ $dest != -* ]] || die "DEST $dest starts with a dash; write ./$dest"

for tool in git tar gitleaks; do
    command -v "$tool" >/dev/null || die "$tool is not installed"
done
gitleaks dir --help >/dev/null 2>&1 || die "gitleaks has no dir command; 8.19 or newer is required"

unset CDPATH
# Git hooks run with GIT_DIR, GIT_INDEX_FILE and the like set; left in place,
# the git commands below would act on the source repository, not on DEST.
for var in $(git rev-parse --local-env-vars); do
    unset "$var"
done

script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)
root=$(cd "$script_dir/.." && pwd -P)
top=$(git -C "$root" rev-parse --show-toplevel)
[[ $top == "$root" ]] || die "$root is not the top level of a git repository"
git_dir=$(git -C "$root" rev-parse --absolute-git-dir)
git_common_dir=$(cd "$(git -C "$root" rev-parse --path-format=absolute --git-common-dir)" && pwd -P)

[[ -n ${HOME:-} ]] || die "HOME is not set"
home=$(cd "$HOME" 2>/dev/null && pwd -P) || home=$HOME

while [[ $dest == */ && $dest != / ]]; do
    dest=${dest%/}
done
[[ $dest != / ]] || die "refusing to replace /"
[[ ! -L $dest ]] || die "DEST $dest is a symlink"
base=${dest##*/}
case $base in
. | ..) die "DEST $dest must not end in . or .." ;;
esac
case $dest in
*/*)
    parent=${dest%/*}
    parent=${parent:-/}
    ;;
*) parent=. ;;
esac
parent=$(cd "$parent" 2>/dev/null && pwd -P) || die "the parent directory of $dest does not exist"
if [[ $parent == / ]]; then
    dest=/$base
else
    dest=$parent/$base
fi

for protected in "$home" "$root" "$git_dir" "$git_common_dir"; do
    if within "$protected" "$dest"; then
        die "refusing to replace $dest: it is or contains $protected"
    fi
done
for git_area in "$git_dir" "$git_common_dir"; do
    if within "$dest" "$git_area"; then
        die "refusing to replace $dest: it is inside $git_area"
    fi
done
if within "$dest" "$root"; then
    [[ $base == .context ]] || die "refusing to replace $dest: inside the repository only a .context directory may be replaced"
    rel=
    walk=$dest
    until [[ $walk -ef $root ]]; do
        [[ $walk == /?* ]] || die "cannot relate $dest to $root"
        rel=${walk##*/}${rel:+/}$rel
        walk=${walk%/*}
    done
    tracked=$(git --icase-pathspecs -C "$root" ls-files -- ":(literal)$rel") ||
        die "cannot list the tracked files under $dest"
    [[ -z $tracked ]] || die "refusing to replace $dest: it contains tracked files"
fi

case $(tar --version 2>&1) in
*bsdtar*)
    create_flags=(--no-xattrs --no-acls)
    extract_flags=(--no-xattrs --no-acls --no-same-owner)
    ;;
*"GNU tar"*)
    create_flags=(--no-xattrs --no-acls --no-selinux)
    extract_flags=(--no-xattrs --no-acls --no-selinux --no-same-owner)
    ;;
*) die "unsupported tar: need bsdtar or GNU tar" ;;
esac
export COPYFILE_DISABLE=1

if [[ -e $dest ]]; then
    [[ -d $dest ]] || die "refusing to replace $dest: it is not a directory"
    entries=$(ls -A "$dest") || die "cannot list $dest"
    if [[ -n $entries && ! -f $dest/.git/stage_tree ]]; then
        die "refusing to replace $dest: it is not empty and was not created by stage_tree.sh"
    fi
fi

work=$(mktemp -d "${TMPDIR:-/tmp}/stage_tree.XXXXXX")
all=$work/all
list=$work/list
created=0
finished=0
cleanup() {
    rm -rf "$work"
    if ((created && ! finished)); then
        rm -rf "$dest"
    fi
}
trap cleanup EXIT

created=1
rm -rf "$dest"
mkdir -p "$dest/.git"
# The marker lets a rerun replace what an earlier run left, even a partial copy,
# and nothing else. A .git directory that is not yet a repository keeps the file
# list below from mentioning DEST when DEST is inside the source repository.
: >"$dest/.git/stage_tree"

git -C "$root" ls-files -z --cached --others --exclude-standard --deduplicate >"$all"

count=0
while IFS= read -r -d '' path; do
    if [[ -d $root/$path && ! -L $root/$path ]]; then
        printf 'stage_tree: skipping %s (a submodule or nested repository)\n' "$path" >&2
    elif [[ -e $root/$path || -L $root/$path ]]; then
        printf './%s\0' "$path"
        count=$((count + 1))
    fi
done <"$all" >"$list"
((count > 0)) || die "no files to stage"

(cd "$root" && tar -c -f - "${create_flags[@]}" --no-recursion --null -T "$list") |
    tar -x -p -f - "${extract_flags[@]}" -C "$dest"

# bsdtar on macOS writes non-ASCII names in decomposed form, which a Linux
# consumer would then see as different from the names git records.
(cd "$dest" && find . -path ./.git -prune -o ! -type d -print0) |
    LC_ALL=C sort -z >"$work/copied"
LC_ALL=C sort -z "$list" >"$work/expected"
if ! cmp -s "$work/expected" "$work/copied"; then
    diff <(tr '\0' '\n' <"$work/expected") <(tr '\0' '\n' <"$work/copied") >&2 || :
    die "the copied file names differ from the source file names (listed above)"
fi

# The empty template keeps the caller's template hooks out of the new repository.
git -C "$dest" init -q --template=
# Not even a global or per-repo ignore may keep a copied file out of the index.
git -C "$dest" -c core.excludesFile=/dev/null add -Af .
indexed=$(git -C "$dest" ls-files -z | tr -cd '\0' | wc -c)
((indexed == count)) || die "the index holds $((indexed)) files but $count were copied"
# The tree is used on Linux, where the case-insensitivity that init probed on a
# macOS volume does not apply.
git -C "$dest" config core.ignorecase false

# Redacted so that a failure names the file and rule without echoing the secret.
gitleaks dir --no-banner --no-color --redact --verbose --log-level warn \
    --gitleaks-ignore-path "$dest" "$dest" >&2 ||
    die "gitleaks found secrets in the staged tree, or failed"

finished=1
printf 'stage_tree: staged %d files into %s\n' "$count" "$dest"
