#!/bin/zsh

# Copyright © 2020 Michael Shields
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

[[ -d /opt/homebrew ]] && eval "$(/opt/homebrew/bin/brew shellenv)"
[[ -x /usr/local/bin/brew ]] && eval "$(/usr/local/bin/brew shellenv)"

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
if [[ "$(uname)" == "Darwin" ]]; then
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
export __CF_USER_TEXT_ENCODING="$(id -u):134217984:134217984"

# Needed by Terraform:
export KUBE_CONFIG_PATH="$HOME/.kube/config"

if [ -d "$HOME/.cargo" ]; then
    PATH="$PATH:$HOME/.cargo/bin"
fi

if whence go >/dev/null; then
    PATH="$PATH:$(go env GOPATH)/bin"
fi

export FZF_DEFAULT_OPTS="--color hl:red,hl:bold,selected-hl:red,selected-hl:bold,current-hl:red,current-hl:bold"

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
fi

source "$ZSH/oh-my-zsh.sh"

for f in "$HOME/.zsh.d/"*.zsh(N); do source "$f"; done

# Keep aliases below after OMZ initialization, since some of them override
# what's defined by OMZ plugins.

# Start Claude with a per-session tmpdir and sandbox write permit for it.
c() {
    local tmpdir settings
    tmpdir=$(mktemp -d "${${TMPDIR:-/tmp}%/}/claude.XXXXXX") || return 1
    # Run from project root; .claude/settings.local.json is project-scoped.
    settings=./.claude/settings.local.json
    mkdir -p ./.claude
    [[ -s "$settings" ]] || echo '{}' > "$settings"
    # Appends rather than replaces; stale entries are harmless and the OS prunes /tmp.
    # jq += creates intermediate objects, so this works on a bare {}.
    jq --arg dir "$tmpdir" '.sandbox.filesystem.allowWrite += [$dir]' "$settings" > "${settings}.tmp" \
        && mv "${settings}.tmp" "$settings" || { rm -f "${settings}.tmp"; return 1; }
    # Claude Code takes effort as a CLI flag but not in the config.
    # https://github.com/anthropics/claude-code/issues/31923
    TMPDIR="$tmpdir" claude --effort max "$@"
}

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

unalias md 2>/dev/null || true # from oh-my-zsh lib/directories.zsh
md() {
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

[[ "$TERM_PROGRAM" == "iTerm.app" ]] && source "$HOME/.iTerm2/shell_integration.zsh"
