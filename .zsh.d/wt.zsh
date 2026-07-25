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
# the same repository as the main worktree. In particular, an existing ordinary
# directory must not pass merely because Git discovers a repository above it.
_wt_is_usable_worktree() {
    emulate -L zsh

    local candidate_worktree="$1"
    local main_worktree="$2"
    local candidate_toplevel candidate_common_dir main_common_dir

    [[ -d "$candidate_worktree" ]] || return 1

    candidate_toplevel="$(
        command git -C "$candidate_worktree" rev-parse --show-toplevel 2>/dev/null
    )" || return 1
    [[ "${candidate_toplevel:A}" == "${candidate_worktree:A}" ]] || return 1

    candidate_common_dir="$(
        command git -C "$candidate_worktree" rev-parse --git-common-dir 2>/dev/null
    )" || return 1
    if [[ "$candidate_common_dir" != /* ]]; then
        candidate_common_dir="$candidate_worktree/$candidate_common_dir"
    fi

    main_common_dir="$(
        command git -C "$main_worktree" rev-parse --git-common-dir 2>/dev/null
    )" || return 1
    if [[ "$main_common_dir" != /* ]]; then
        main_common_dir="$main_worktree/$main_common_dir"
    fi

    [[ "${candidate_common_dir:A}" == "${main_common_dir:A}" ]]
}

unalias wt 2>/dev/null || true
wt() {
    emulate -L zsh

    if (( $# > 1 )); then
        print -u2 -r -- "wt: usage: wt [worktree-name]"
        return 1
    fi

    local inside_worktree
    if ! inside_worktree="$(command git rev-parse --is-inside-work-tree 2>/dev/null)" ||
        [[ "$inside_worktree" != true ]]; then
        print -u2 -r -- "wt: current directory is not inside a Git worktree"
        return 1
    fi

    local -a reply reply_prunable
    if ! _wt_worktree_paths; then
        print -u2 -r -- "wt: unable to list Git worktrees"
        return 1
    fi
    local -i index

    local main_worktree="${reply[1]}"
    local current_worktree
    if ! current_worktree="$(command git rev-parse --show-toplevel 2>/dev/null)"; then
        print -u2 -r -- "wt: unable to locate the current Git worktree"
        return 1
    fi

    # With no argument, a linked worktree is a toggle back to the main one.
    if (( $# == 0 )) && [[ "${current_worktree:A}" != "${main_worktree:A}" ]]; then
        if ! builtin cd -- "$main_worktree"; then
            print -u2 -r -- "wt: unable to enter the main worktree: $main_worktree"
            return 1
        fi
        return 0
    fi

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
            ! _wt_is_usable_worktree "$matched_path" "$main_worktree"; then
            print -u2 -r -- "wt: linked worktree does not exist: $matched_path"
            return 1
        fi
        if ! builtin cd -- "$matched_path"; then
            print -u2 -r -- "wt: unable to enter linked worktree: $matched_path"
            return 1
        fi
        return 0
    fi

    local common_dir
    if ! common_dir="$(
        command git -C "$main_worktree" rev-parse --git-common-dir 2>/dev/null
    )"; then
        print -u2 -r -- "wt: unable to locate the Git metadata directory"
        return 1
    fi
    if [[ "$common_dir" != /* ]]; then
        common_dir="$main_worktree/$common_dir"
    fi
    common_dir="${common_dir:A}"

    local exclude="$common_dir/info/exclude"
    if ! command mkdir -p -- "${exclude:h}"; then
        print -u2 -r -- "wt: unable to create the Git info directory"
        return 1
    fi
    if [[ ! -f "$exclude" ]] || ! command grep -Fqx -- ".worktrees/" "$exclude"; then
        # Preserve a final unterminated line before appending our exact entry.
        if [[ -s "$exclude" ]] && [[ -n "$(command tail -c 1 -- "$exclude")" ]]; then
            if ! print >>"$exclude"; then
                print -u2 -r -- "wt: unable to update $exclude"
                return 1
            fi
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

    if ! command mkdir -p -- "${target:h}"; then
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

    local inside_worktree
    inside_worktree="$(command git rev-parse --is-inside-work-tree 2>/dev/null)" || return 0
    [[ "$inside_worktree" == true ]] || return 0

    local -a reply reply_prunable
    _wt_worktree_paths || return 0

    local -a candidates
    local worktree_path name other_path
    local -i index other_index name_count

    for (( index = 2; index <= ${#reply[@]}; ++index )); do
        worktree_path="${reply[index]}"
        (( reply_prunable[index] == 0 )) &&
            _wt_is_usable_worktree "$worktree_path" "${reply[1]}" || continue

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
