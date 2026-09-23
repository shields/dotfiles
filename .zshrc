#!/bin/zsh

# Copyright © 2020-2026 Michael Shields
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

# The shebang line is for the benefit of shfmt.

# Re-sourcing must not duplicate pending tasks. Ctrl-C can leave the queue
# without a handler, since zsh-defer removes it before running each batch.
# In that case resume the queue, retrying the interrupted task first.
if (( ${_startup_pending:-0} && $+functions[_zsh-defer-schedule] &&
    ${#_zsh_defer_tasks} )); then
    if [[ -z ${zsh_defer_options+x} &&
        "$(zle -F -L)" != *' _zsh-defer-resume'* ]]; then
        _zsh-defer-schedule 0
    fi
    return
fi

# https://donottrack.sh
export DO_NOT_TRACK=1

# Keep PATH free of duplicates so re-sourcing this file (common while editing
# config) doesn't accumulate repeated entries. -U keeps the first occurrence,
# which is the one PATH lookup already uses, so resolution is unchanged.
typeset -U path PATH

# Cache generated initialization scripts until the tool or its configuration
# changes. Print COMMAND's output from NAME, rebuilding it when missing or
# older than any WATCH file. If rebuilding fails, say so and run COMMAND.
#
# A watch is compared without following symlinks: Homebrew remakes the link
# in /opt/homebrew/bin on every install, whereas the binary behind it keeps
# the bottle's build time, which can predate the cache. A watch that does not
# exist is ignored, so a config file created later still counts once it does.
#
# usage: _startup_cached NAME WATCH... -- COMMAND...
zmodload -F zsh/stat b:zstat
_startup_cached() {
    local cache="${XDG_CACHE_HOME:-$HOME/.cache}/zsh/$1" watch
    local -a watches
    local -A cache_stat watch_stat
    shift
    while [[ "$1" != -- ]]; do
        watches+=("$1")
        shift
    done
    shift
    local fresh=0
    if zstat -H cache_stat -- "$cache" 2>/dev/null && (( cache_stat[size] > 0 )); then
        fresh=1
        for watch in "${watches[@]}"; do
            if zstat -L -H watch_stat -- "$watch" 2>/dev/null &&
                (( watch_stat[mtime] >= cache_stat[mtime] )); then
                fresh=0
                break
            fi
        done
    fi
    # Build into a temporary file and rename it into place, so a shell killed
    # mid-rebuild, or two shells rebuilding at once, cannot leave a truncated
    # cache for the next startup to eval.
    if (( ! fresh )) && ! { command mkdir -p -- "${cache:h}" &&
        "$@" >"$cache.$$" && command mv -f -- "$cache.$$" "$cache"; }; then
        print -u2 -r -- "zshrc: cannot rebuild $cache from: $*"
        command rm -f -- "$cache.$$" "$cache"
        "$@"
        return
    fi
    print -r -- "$(<"$cache")"
}

# brew shellenv's output comes from the script that formats it as much as
# from brew itself.
if [[ -d /opt/homebrew ]]; then
    eval "$(_startup_cached brew-shellenv /opt/homebrew/bin/brew \
        /opt/homebrew/Library/Homebrew/cmd/shellenv.sh -- \
        /opt/homebrew/bin/brew shellenv)"
fi
if [[ -x /usr/local/bin/brew ]]; then
    eval "$(_startup_cached brew-shellenv-intel /usr/local/bin/brew \
        /usr/local/Homebrew/Library/Homebrew/cmd/shellenv.sh -- \
        /usr/local/bin/brew shellenv)"
fi

export HOMEBREW_NO_AUTO_UPDATE=1
export HOMEBREW_NO_ENV_HINTS=1

export PATH="$HOME/bin:$PATH"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

zstyle ':omz:update' mode disabled

# Using Starship prompt instead of oh-my-zsh theme
# See config in ~/.config/starship.toml
ZSH_THEME=""

# Uncomment the following line to use case-sensitive completion.
CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# On macOS, always create a new Emacs frame when running emacsclient. Otherwise,
# we might reuse a frame from another workspace, causing an undesired workspace
# switch.
#
# The oh-my-zsh emacsclient wrapper tries to look for "suitable" frames, but
# Emacs does not seem to have any awareness of workspaces -- or at least, that
# isn't exposed as a frame properties. In particular, unminimized frames on
# other workspaces have (visibility . t).
#
# This does correctly count the Emacs frames in the current workspace:
#
#     osascript -e 'tell application "System Events" to count (every window of process "Emacs")')
#
# but it requires granting accessibility permissions to osascript, which seems
# risky.
if [[ "$OSTYPE" == darwin* ]]; then
    export EDITOR="$ZSH/plugins/emacs/emacsclient.sh --create-frame"
fi

# Asking about the merge commit message is unnecessary, since in the
# rare cases where I want to edit it, there is always --amend.
export GIT_MERGE_AUTOEDIT=no

# Emacs shell dir tracking; see comments in term.el.
if [ -n "$INSIDE_EMACS" ]; then
    chpwd() {
        echo -n '\032/'
        pwd
    }
fi

# Even in 2020, macOS 10.15.6 doesn't fully support UTF-8 by default.
# In particular, pbcopy doesn't work correctly.  The fix for this is
# very obscure.  Because users deserve choice, it can be fixed using a
# file or using an environment variable, neither of which is a
# standard macOS preference mechanism.  Let's just do both.
#
# https://developer.apple.com/documentation/corefoundation/cfstringbuiltinencodings/utf8
# https://superuser.com/questions/82123/mac-whats-cfusertextencoding-for
export __CF_USER_TEXT_ENCODING="$UID:134217984:134217984"

# Needed by Terraform:
export KUBE_CONFIG_PATH="$HOME/.kube/config"

if [ -d "$HOME/.cargo" ]; then
    PATH="$PATH:$HOME/.cargo/bin"
fi

# Homebrew's rustup is keg-only and puts its cargo/rustc proxies here, not in
# ~/.cargo/bin (it no longer ships rustup-init).
if [ -d "/opt/homebrew/opt/rustup/bin" ]; then
    PATH="$PATH:/opt/homebrew/opt/rustup/bin"
fi

# This is what `go env GOPATH` would print, without starting the toolchain;
# it only differs if GOPATH is ever pinned with `go env -w`.
if whence go >/dev/null; then
    PATH="$PATH:${GOPATH:-$HOME/go}/bin"
fi

export FZF_DEFAULT_OPTS="--color hl:red:bold,selected-hl:red:bold,current-hl:red:bold"

export FZF_ALT_C_COMMAND=""
export FZF_CTRL_T_COMMAND="fd --type f --hidden --exclude .git"

export FZF_CTRL_R_OPTS="--height 16 --layout reverse --border none --color bg:#eeeeee"
export FZF_CTRL_T_OPTS="$FZF_CTRL_R_OPTS"

# fzf-tab configuration
zstyle ':fzf-tab:*' fzf-command fzf
zstyle ':fzf-tab:*' fzf-flags --height 16 --layout=reverse --border=none --color bg:#eeeeee --bind "one:accept"
zstyle ':fzf-tab:*' switch-group ',' '.'
zstyle ':fzf-tab:*' continuous-trigger '/'
# Make Enter only accept completion without executing command
zstyle ':fzf-tab:*' accept-line ''
zstyle ':fzf-tab:*' prefix ''
zstyle ':completion:*:descriptions' format '[%d]'

# zoxide, but call it j instead of z, because of autojump muscle memory and
# because the z key is poorly placed for this.
export ZOXIDE_CMD_OVERRIDE=j

# Starship owns the prompt; AWS otherwise adds a right prompt when loaded late.
SHOW_AWS_PROMPT=false

plugins=(
    aws
    colorize
    direnv
    docker
    emacs
    fzf
    fzf-tab
    gcloud
    git
    git-auto-fetch
    git-prompt-watcher
    kubectl
    starship
    zoxide
)

if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    zstyle :omz:plugins:iterm2 shell-integration yes
    plugins+=(iterm2)
    # Unless preset, the shell integration forks `hostname -f` at load and
    # before every prompt; zsh already knows the answer.
    export iterm2_hostname="$HOST"
fi

# The Git plugin only needs the installed version during initialization.
git() {
    if (( $# == 1 )) && [[ "$1" == version ]]; then
        _startup_cached git-version "$commands[git]" -- command git version
    else
        command git "$@"
    fi
}

# starship's init script also forks `starship prompt --continuation` for
# PROMPT2 every time it is evaluated; bake that into the cached copy. Unlike
# the rest of the script, that value depends on the configuration.
_startup_starship_init() {
    setopt localoptions pipefail
    command starship init zsh | command sed '/^PROMPT2=/d' &&
        print -r -- "PROMPT2=${(qq)$(command starship prompt --continuation)}"
}
starship() {
    if (( $# == 2 )) && [[ "$1" == init && "$2" == zsh ]]; then
        _startup_cached starship-init "$commands[starship]" \
            "${STARSHIP_CONFIG:-$HOME/.config/starship.toml}" -- \
            _startup_starship_init
    else
        command starship "$@"
    fi
}

fzf() {
    case "$*" in
    --version | --zsh)
        _startup_cached "fzf-${1#--}" "$commands[fzf]" -- command fzf "$1"
        ;;
    *)
        command fzf "$@"
        ;;
    esac
}

# The command name is part of the generated script, so it is in the key.
zoxide() {
    if [[ "$*" == "init --cmd $ZOXIDE_CMD_OVERRIDE zsh" ]]; then
        _startup_cached "zoxide-init-$ZOXIDE_CMD_OVERRIDE" "$commands[zoxide]" -- \
            command zoxide "$@"
    else
        command zoxide "$@"
    fi
}

# Load OMZ libraries explicitly: options, history and basic editing must be
# established in the main shell before zsh-defer's local execution scope.
# Completion initialization and individual plugins are queued below.
: ${ZSH_CUSTOM:="$ZSH/custom"}
: ${ZSH_CACHE_DIR:="${XDG_CACHE_HOME:-$HOME/.cache}/oh-my-zsh"}
: ${ZSH_COMPDUMP:="${ZDOTDIR:-$HOME}/.zcompdump-${HOST%%.*}-$ZSH_VERSION"}
[[ -d "$ZSH_CACHE_DIR/completions" ]] || command mkdir -p "$ZSH_CACHE_DIR/completions"
typeset -U fpath
fpath=("$ZSH/functions" "$ZSH/completions" "$ZSH_CUSTOM/functions"
    "$ZSH_CUSTOM/completions" "$ZSH_CACHE_DIR/completions" $fpath)
for _startup_name in $plugins; do
    if [[ -d "$ZSH_CUSTOM/plugins/$_startup_name" ]]; then
        fpath=("$ZSH_CUSTOM/plugins/$_startup_name" $fpath)
    elif [[ -d "$ZSH/plugins/$_startup_name" ]]; then
        fpath=("$ZSH/plugins/$_startup_name" $fpath)
    fi
done

# Preserve completion registrations made by libraries and our own commands
# before compinit exists. Quoting each argument preserves spaces and options.
typeset -ga _startup_compdefs=()
compdef() { _startup_compdefs+=("${(j: :)${(q)@}}") }

_startup_source() {
    local startup_file="$1"
    if [[ -f "$ZSH_CUSTOM/$startup_file" ]]; then
        source "$ZSH_CUSTOM/$startup_file"
    elif [[ -f "$ZSH/$startup_file" ]]; then
        source "$ZSH/$startup_file"
    else
        print -u2 -r -- "zshrc: missing $startup_file"
        return 1
    fi
}
for _startup_file in "$ZSH"/lib/*.zsh; do
    _startup_source "lib/${_startup_file:t}"
done
unset _startup_file _startup_name
[[ -z "$LS_COLORS" ]] || zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# Environment changes must precede the first command. The cached Starship
# initialization is small; keeping it and iTerm together avoids swapping out
# a prompt while a command is being entered.
for _startup_name in direnv gcloud starship; do
    _startup_source "plugins/$_startup_name/$_startup_name.plugin.zsh"
done
unset _startup_name

# Starship's right prompt is empty with this configuration.
_startup_starship_config="${STARSHIP_CONFIG:-$HOME/.config/starship.toml}"
if [[ ! -r "$_startup_starship_config" || "$(<"$_startup_starship_config")" != *right_format* ]]; then
    unset RPROMPT
fi
unset _startup_starship_config
if [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    _startup_source plugins/iterm2/iterm2.plugin.zsh
    source "$HOME/.iTerm2/shell_integration.zsh"
fi

for _startup_file in "$HOME/.zsh.d/"*.zsh(N); do source "$_startup_file"; done
unset _startup_file

# Apply our overrides before the first prompt and again before each deferred
# plugin yields, so loading Git never changes gc or shadows our gcl function.
_startup_overrides() {
    unalias gcl md 2>/dev/null || true
    # Start Claude with a per-session tmpdir and sandbox write permit for it.
    c() {
        local tmpdir
        tmpdir=$(mktemp -d "${${TMPDIR:-/tmp}%/}/claude.XXXXXX") || return 1
        # Permit sandbox writes to this session's tmpdir; scoped to this run only.
        # --effort stays on the command line: settings.json's modelSettings only
        # sets effortLevel for claude-opus-5, which --model=fable does not match.
        TMPDIR="$tmpdir" claude \
            --model=opus --effort xhigh \
            --permission-mode=auto \
            --settings "{\"ultracode\":true,\"sandbox\":{\"filesystem\":{\"allowWrite\":[\"$tmpdir\"]}}}" "$@"
    }
    alias cw='c --worktree'

    alias count='sort | uniq -c | sort -n'

    alias drit='docker run -it --rm'

    alias gc='gcloud'

    alias gdi='git diff refs/remotes/origin/HEAD'
    alias glf='git ls-files'

    alias kc='kubectl'
    alias kcy='kubectl -o yaml'

    if whence freshl >/dev/null; then
        alias l='freshl'
    else
        alias l='ls -lA'
    fi
    alias lr='l -R'

    function md {
        if [ $# -ne 1 ] || [ -z "$1" ]; then
            echo "Usage: md <directory>" >&2
            return 1
        fi
        mkdir -p -- "$1" && cd -- "$1"
    }

    # https://docs.brew.sh/Homebrew-and-Python
    p() {
        if [ -n "$VIRTUAL_ENV" ]; then
            "$VIRTUAL_ENV/bin/python" "$@"
        else
            "$(brew --prefix python)/libexec/bin/python" "$@"
        fi
    }

    r() {
        rg --pretty --line-buffered "$@" | less -R -E --redraw-on-quit
    }

    if [ -x /usr/bin/pbcopy ]; then
        alias pc=pbcopy
        alias pv=pbpaste
    fi

    alias s='$PAGER'

    whence tf >/dev/null || alias tf='tofu'
    alias tfa='tf apply -parallelism=100'
    alias tfi='tf init'
    alias tfia='tfi && tfa'
    alias tfp='tf plan -parallelism=100 -refresh=false'
    alias tfpr='tf plan -parallelism=100 -refresh=true'
}
_startup_overrides

_startup_completion() {
    # Either an OMZ update or a changed fpath can replace completion names or
    # registrations without changing their count. Check both, while keeping
    # compinit's ownership checks and checks for added/removed files. The Git
    # lookup happens after the first prompt along with the rest of this task.
    local revision="$(command git -C "$ZSH" rev-parse HEAD 2>/dev/null)"
    local metadata="# dotfiles revision: $revision fpath: $fpath"
    local -a dump_lines
    [[ ! -r "$ZSH_COMPDUMP" ]] || dump_lines=("${(@f)$(<"$ZSH_COMPDUMP")}")
    if [[ "${dump_lines[-1]-}" != "$metadata" ]]; then
        command rm -f -- "$ZSH_COMPDUMP" "$ZSH_COMPDUMP.zwc"
    fi
    autoload -Uz compinit
    # Despite its name, compinit sets _comp_secure=yes when compaudit fails
    # and -i excludes insecure entries. Reset it before each initialization.
    unset _comp_secure
    compinit -i -d "$ZSH_COMPDUMP" || return
    if [[ "$_comp_secure" == yes ]]; then
        handle_completion_insecurities
    fi
    dump_lines=("${(@f)$(<"$ZSH_COMPDUMP")}")
    [[ "${dump_lines[-1]-}" == "$metadata" ]] || print -r -- "$metadata" >>"$ZSH_COMPDUMP"
    local definition
    for definition in "${_startup_compdefs[@]}"; do
        eval "compdef $definition"
    done
    unset _startup_compdefs
}

_startup_plugin() {
    # zsh-defer uses LOCAL_TRAPS; the watcher's signal handlers must survive
    # this call. Options themselves are established synchronously above.
    unsetopt localtraps
    if [[ "$1" == git-prompt-watcher ]] && (( $+functions[_stop_git_watcher] )); then
        _stop_git_watcher
        chpwd_functions=("${(@)chpwd_functions:#_check_git_repo_change}")
        zshexit_functions=("${(@)zshexit_functions:#_git_prompt_watcher_exit}")
    fi
    _startup_source "plugins/$1/$1.plugin.zsh"
    local result=$?
    _startup_overrides
    # The initial zle-line-init already happened before this task was run.
    if [[ "$1" == git-auto-fetch && -n ${zsh_defer_options+x} ]] &&
        (( $+functions[git-fetch-all] )); then
        git-fetch-all
    fi
    return $result
}

_startup_finish() {
    local startup_file
    for startup_file in "$ZSH_CUSTOM"/*.zsh(N); do source "$startup_file"; done
    _startup_overrides
    unfunction git starship _startup_starship_init fzf zoxide _startup_cached
    unfunction _startup_source _startup_completion _startup_plugin _startup_overrides _startup_enqueue _startup_finish
    _startup_pending=0
}

# The installer owns downloads; opening a terminal never requires the network.
# A missing dependency or a `zsh -ic command` still gets a fully initialized
# shell, using the same tasks synchronously.
if [[ -o interactive && -o zle && -t 0 && -z ${ZSH_EXECUTION_STRING:-} &&
    -r "${ZSH_DEFER_DIR:-$HOME/.local/share/zsh-defer}/zsh-defer.plugin.zsh" ]]; then
    source "${ZSH_DEFER_DIR:-$HOME/.local/share/zsh-defer}/zsh-defer.plugin.zsh"
    # Upstream registers a plain function, where PENDING/KEYS_QUEUED_COUNT
    # are unavailable. Register its existing widget so typing takes priority
    # between tasks. This substitution becomes a no-op if upstream fixes it.
    functions[_zsh-defer-schedule]=${functions[_zsh-defer-schedule]/'zle -F $fd _zsh-defer-resume'/'zle -F -w $fd _zsh-defer-resume'}
    # Redisplay without rerunning precmd (direnv and Starship) after every
    # plugin. Keep output and errors visible.
    _startup_enqueue() { zsh-defer -a +r "$@" }
else
    _startup_enqueue() { "$@" }
fi
_startup_pending=1
# Completion comes first; keep plugin order (especially fzf before fzf-tab).
# Tasks run cooperatively in this shell: typing can interrupt the queue
# between tasks, but cannot preempt a task that has already started.
_startup_enqueue _startup_completion
for _startup_name in $plugins; do
    case "$_startup_name" in
        direnv|gcloud|starship|iterm2) continue ;;
    esac
    _startup_enqueue _startup_plugin "$_startup_name"
done
unset _startup_name
_startup_enqueue _startup_finish
