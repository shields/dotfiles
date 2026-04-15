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

# Clone a git repo into a <host>/<owner>/<repo> directory structure under $GCL_ROOT (default ~/src).
# e.g. gcl https://github.com/octocat/Hello-World → ~/src/github.com/octocat/Hello-World
# Supports nested paths (e.g. GitLab subgroups). Always clones via HTTPS;
# private repo auth is handled by git credential helpers.
unalias gcl 2>/dev/null || true
gcl() {
    if [[ $# -ne 1 ]] || [[ -z "$1" ]]; then
        echo "Usage: gcl <repo-url>" >&2
        return 1
    fi

    local url="$1"
    local root="${GCL_ROOT:-$HOME/src}"

    # Normalize: strip scheme, trailing slash, and .git suffix
    url="${url#https://}"
    url="${url#http://}"
    url="${url%/}"
    url="${url%.git}"

    local parts=(${(s:/:)url})
    if [[ ${#parts[@]} -lt 2 ]]; then
        echo "gcl: expected <host>/<path>" >&2
        return 1
    fi

    local clean_url="${(j:/:)parts}"
    local target="$root/$clean_url"

    if [[ -d "$target/.git" ]]; then
        echo "gcl: already cloned at $target"
        cd "$target" || return 1
        return 0
    fi

    local parent="$(dirname "$target")"
    mkdir -p "$parent"
    if ! git clone "https://$clean_url.git" "$target"; then
        # Clean up empty parent directories on failure
        rmdir -p "$parent" 2>/dev/null || true
        return 1
    fi
    cd "$target"
    git-add-upstream # errors intentionally fatal
}
