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

zmodload -F zsh/stat b:zstat 2>/dev/null ||
    print -u2 -r -- "wt.zsh: zsh/stat module unavailable; tab-completion will be disabled"

# Populate the caller's local $current_worktree and $common_dir, as absolute
# paths with symlinks resolved, for the worktree containing the current
# directory (or $1), from a single git invocation. Fail outside a worktree:
# in a bare repository, inside a .git directory, or when GIT_DIR and friends
# in the environment name a repository that does not contain the directory,
# which --show-toplevel alone would happily report.
_wt_locate() {
    emulate -L zsh

    local output inside
    local -a in_dir lines

    (( $# == 0 )) || in_dir=(-C "$1")
    current_worktree=""
    common_dir=""
    output="$(
        command git "${in_dir[@]}" rev-parse --path-format=absolute \
            --is-inside-work-tree --show-toplevel --git-common-dir 2>/dev/null
    )" || return 1
    lines=("${(@f)output}")
    if (( ${#lines[@]} == 3 )); then
        inside="${lines[1]}"
        current_worktree="${lines[2]}"
        common_dir="${lines[3]}"
    else
        # A newline inside either path makes the combined output ambiguous,
        # so fall back to one value per invocation.
        inside="$(
            command git "${in_dir[@]}" rev-parse --is-inside-work-tree 2>/dev/null
        )" || return 1
        current_worktree="$(
            command git "${in_dir[@]}" rev-parse --show-toplevel 2>/dev/null
        )" || return 1
        common_dir="$(
            command git "${in_dir[@]}" rev-parse --path-format=absolute \
                --git-common-dir 2>/dev/null
        )" || return 1
    fi
    [[ "$inside" == true && -n "$current_worktree" && -n "$common_dir" ]] || return 1
    current_worktree="${current_worktree:A}"
    common_dir="${common_dir:A}"
}

# Populate the caller's local $reply with registered worktree paths and
# $reply_prunable with their corresponding porcelain status. Zsh preserves NUL
# bytes in command substitutions, so paths containing whitespace (including
# newlines) remain unambiguous.
_wt_worktree_paths() {
    emulate -L zsh

    local porcelain field
    local -a fields
    local -i current_index=0

    reply=()
    reply_prunable=()
    porcelain="$(command git worktree list --porcelain -z 2>/dev/null)" || return 1
    fields=("${(@0)porcelain}")
    for field in "${fields[@]}"; do
        if [[ "$field" == worktree\ * ]]; then
            reply+=("${field#worktree }")
            reply_prunable+=(0)
            current_index=${#reply[@]}
        elif (( current_index > 0 )) &&
            [[ "$field" == prunable || "$field" == prunable\ * ]]; then
            reply_prunable[current_index]=1
        fi
    done

    (( ${#reply[@]} > 0 ))
}

# Return success only when a registered path is still a worktree belonging to
# the same repository as the current one, according to Git itself, which also
# rejects a checkout whose administrative files are damaged. In particular, an
# existing ordinary directory must not pass merely because Git discovers a
# repository above it.
_wt_is_usable_worktree() {
    emulate -L zsh

    local candidate_worktree="$1"
    local main_common_dir="$2"
    local current_worktree common_dir

    [[ -d "$candidate_worktree" ]] || return 1
    _wt_locate "$candidate_worktree" || return 1
    [[ "$current_worktree" == "${candidate_worktree:A}" &&
        "$common_dir" == "$main_common_dir" ]]
}

# Return success when a registered path holds a linked checkout of this
# repository: .git must be a gitfile whose gitdir entry resolves to a directory
# directly under the common directory's worktrees/ that has a HEAD file, the
# shape Git's validate_worktree checks. A registration whose HEAD was removed
# (e.g. manual tampering) is listed by Git as present and not prunable, yet Git
# itself refuses to operate in it, so completion must reject it too rather than
# offering a name that then fails to enter. An ordinary directory recreated at
# a locked registration has no gitfile, and an unrelated repository has a .git
# directory or a gitfile pointing elsewhere. Only builtins run, so completion
# stays cheap however many worktrees are registered; entering a worktree still
# asks Git.
_wt_is_linked_checkout() {
    emulate -L zsh

    local candidate_worktree="$1"
    local common_dir="$2"
    local gitfile="$candidate_worktree/.git"
    local gitdir
    local -a size

    [[ -f "$gitfile" && -r "$gitfile" ]] || return 1
    # A gitfile holds one path, so anything larger is not one, and reading it
    # whole into the shell could exhaust memory.
    zstat -A size +size -- "$gitfile" 2>/dev/null || return 1
    (( size[1] <= 8192 )) || return 1
    gitdir="$(<"$gitfile")"
    # Git tolerates a CRLF line ending here.
    gitdir="${gitdir%$'\r'}"
    [[ "$gitdir" == "gitdir: "* ]] || return 1
    gitdir="${gitdir#gitdir: }"
    # `git worktree add --relative-paths` records the gitdir relative to the
    # worktree rather than as an absolute path.
    [[ "$gitdir" == /* ]] || gitdir="$candidate_worktree/$gitdir"
    gitdir="${gitdir:A}"
    [[ -d "$gitdir" && -f "$gitdir/HEAD" && "${gitdir:h}" == "$common_dir/worktrees" ]]
}

unalias wt 2>/dev/null || true
wt() {
    emulate -L zsh

    if (( $# > 1 )); then
        print -u2 -r -- "wt: usage: wt [worktree-name]"
        return 1
    fi

    local current_worktree common_dir
    if ! _wt_locate; then
        print -u2 -r -- "wt: current directory is not inside a Git worktree"
        return 1
    fi

    # Git derives the main worktree the same way (get_main_worktree in
    # worktree.c): the common directory's real path minus a trailing /.git. A
    # bare or --separate-git-dir repository keeps the directory itself, which
    # matches the first entry `git worktree list` reports.
    local main_worktree="$common_dir"
    if [[ "${main_worktree:t}" == .git ]]; then
        main_worktree="${main_worktree:h}"
    fi

    # With no argument, a linked worktree is a toggle back to the main one.
    if (( $# == 0 )) && [[ "$current_worktree" != "$main_worktree" ]]; then
        if ! builtin cd -- "$main_worktree"; then
            print -u2 -r -- "wt: unable to enter the main worktree: $main_worktree"
            return 1
        fi
        return 0
    fi

    local -a reply reply_prunable
    if ! _wt_worktree_paths; then
        print -u2 -r -- "wt: unable to list Git worktrees"
        return 1
    fi
    local -i index

    if (( $# == 1 )); then
        local requested_name="$1"
        local matched_path=""
        local worktree_path
        local -i matched_index=0 match_count=0

        # The first porcelain entry is always the main worktree. A name only
        # addresses linked worktrees, wherever their directories live.
        for (( index = 2; index <= ${#reply[@]}; ++index )); do
            worktree_path="${reply[index]}"
            if [[ "${worktree_path:t}" == "$requested_name" ]]; then
                matched_path="$worktree_path"
                matched_index=$index
                (( ++match_count ))
            fi
        done

        if (( match_count == 0 )); then
            print -u2 -r -- "wt: no linked worktree named: $requested_name"
            return 1
        fi
        if (( match_count > 1 )); then
            print -u2 -r -- "wt: linked worktree name is ambiguous: $requested_name"
            return 1
        fi
        if (( reply_prunable[matched_index] )) ||
            ! _wt_is_usable_worktree "$matched_path" "$common_dir"; then
            print -u2 -r -- "wt: linked worktree does not exist: $matched_path"
            return 1
        fi
        if ! builtin cd -- "$matched_path"; then
            print -u2 -r -- "wt: unable to enter linked worktree: $matched_path"
            return 1
        fi
        return 0
    fi

    local exclude="$common_dir/info/exclude"
    if [[ ! -d "${exclude:h}" ]] && ! command mkdir -p -- "${exclude:h}"; then
        print -u2 -r -- "wt: unable to create the Git info directory"
        return 1
    fi

    # Append our exact entry once. `read` fails on a final line that lacks a
    # newline but still stores it, so afterwards $line holds any unterminated
    # tail, which must be completed before appending.
    local line
    local -i excluded=0
    if [[ -f "$exclude" ]]; then
        while IFS= read -r line; do
            [[ "$line" == .worktrees/ ]] && excluded=1
        done <"$exclude"
        [[ "$line" == .worktrees/ ]] && excluded=1
    fi
    if (( ! excluded )); then
        if [[ -n "$line" ]] && ! print >>"$exclude"; then
            print -u2 -r -- "wt: unable to update $exclude"
            return 1
        fi
        if ! print -r -- ".worktrees/" >>"$exclude"; then
            print -u2 -r -- "wt: unable to update $exclude"
            return 1
        fi
    fi

    local module_dir="${${(%):-%x}:A:h}"
    local wordlist="$module_dir/eff_short_wordlist_2_0.txt"
    if [[ ! -r "$wordlist" ]]; then
        print -u2 -r -- "wt: word list is not readable: $wordlist"
        return 1
    fi

    # Capture the complete shuffle before selecting rows so sort cannot receive
    # SIGPIPE from a downstream command that exits after two lines. The macOS
    # and GNU sort implementations targeted here both support --random-sort.
    local shuffled
    if ! shuffled="$(LC_ALL=C command sort --random-sort -- "$wordlist")"; then
        print -u2 -r -- "wt: sort --random-sort failed"
        return 1
    fi

    local -a rows
    rows=("${(@f)shuffled}")
    if (( ${#rows[@]} < 1000 )); then
        print -u2 -r -- "wt: word list contains fewer than 1000 entries"
        return 1
    fi

    local first_word="${rows[1]##*[[:space:]]}"
    local second_word="${rows[2]##*[[:space:]]}"
    if [[ -z "$first_word" || -z "$second_word" ]]; then
        print -u2 -r -- "wt: invalid word list entry"
        return 1
    fi

    local worktree_name="$first_word-$second_word"
    local branch_name="worktree-$worktree_name"
    local target="$main_worktree/.worktrees/$worktree_name"

    if [[ -e "$target" || -L "$target" ]]; then
        print -u2 -r -- "wt: worktree path already exists: $target"
        return 1
    fi

    command git -C "$main_worktree" show-ref --verify --quiet "refs/heads/$branch_name"
    local -i branch_status=$?
    if (( branch_status == 0 )); then
        print -u2 -r -- "wt: branch already exists: $branch_name"
        return 1
    fi
    if (( branch_status != 1 )); then
        print -u2 -r -- "wt: unable to check for branch: $branch_name"
        return 1
    fi

    local registered_path
    for (( index = 2; index <= ${#reply[@]}; ++index )); do
        registered_path="${reply[index]}"
        if [[ "${registered_path:t}" == "$worktree_name" ]]; then
            print -u2 -r -- "wt: linked worktree name already exists: $worktree_name"
            return 1
        fi
    done

    if [[ ! -d "${target:h}" ]] && ! command mkdir -p -- "${target:h}"; then
        print -u2 -r -- "wt: unable to create the worktree directory"
        return 1
    fi
    if ! command git -C "$main_worktree" worktree add -b "$branch_name" "$target" HEAD; then
        print -u2 -r -- "wt: unable to create linked worktree: $target"
        return 1
    fi
    if ! builtin cd -- "$target"; then
        print -u2 -r -- "wt: linked worktree was created but could not be entered: $target"
        return 1
    fi
    return 0
}

_wt() {
    emulate -L zsh

    # The command accepts at most one positional argument.
    (( CURRENT == 2 )) || return 0

    local current_worktree common_dir
    _wt_locate || return 0

    local -a reply reply_prunable
    _wt_worktree_paths || return 0

    local -a candidates
    local worktree_path name other_path
    local -i index other_index name_count

    for (( index = 2; index <= ${#reply[@]}; ++index )); do
        worktree_path="${reply[index]}"
        (( reply_prunable[index] == 0 )) &&
            _wt_is_linked_checkout "$worktree_path" "$common_dir" || continue

        name="${worktree_path:t}"
        name_count=0
        for (( other_index = 2; other_index <= ${#reply[@]}; ++other_index )); do
            other_path="${reply[other_index]}"
            if [[ "${other_path:t}" == "$name" ]]; then
                (( ++name_count ))
            fi
        done

        (( name_count == 1 )) && candidates+=("$name")
    done

    (( ${#candidates[@]} > 0 )) && compadd -- "${candidates[@]}"
    return 0
}

if (( $+functions[compdef] )); then
    compdef _wt wt
fi
