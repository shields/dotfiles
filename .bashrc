#!/bin/bash

# Copyright © 2001 Michael Shields
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

# This entire file is useless if this is a non-interactive shell, so
# just quit now.
test -z "$PS1" && return

if [ "$TERM" = xterm-debian -a ! -e /etc/terminfo/x/xterm-debian \
    -a ! -e /usr/share/terminfo/x/xterm-debian ]; then
    TERM=xterm
fi

trimmed_hostname="$(hostname | sed -e 's/\.msrl\.com$//' \
    -e 's/\.above\.net$//' \
    -e 's/\.mfnx\.net$//')"
if [ "$TERM" = xterm -o "$TERM" = xterm-debian -o "$TERM" = xterm-256color ]; then
    PS1="\\[\\033]0;\\u@$trimmed_hostname\\007\\]\\w\\$ "
    # On iTerm2, display git branch name on the touch bar.  For
    # safety, limit characters allowed.
    if [ -x ~/.iterm2/it2check ] && ~/.iterm2/it2check; then
        PS1='\[$(~/.iterm2/it2setkeylabel set status $((git branch-name 2>/dev/null || echo ∅) | tr -Cd ∅A-Za-z0-9_-/))\]'"$PS1"
    fi
else
    PS1="\\u@$trimmed_hostname:\\w\\$ "
fi
export PS1

alias drit='docker run -it --rm'

alias g='git'
alias gdi='git diff refs/remotes/origin/HEAD'

gc() {
    gcloud config get-value project
    [[ $# == 0 ]] && return
    gcloud "$@"
}
kc() {
    kubectl config current-context
    [[ $# == 0 ]] && return
    kubectl "$@"
}

hpr() {
    # Verify that the working directory is clean.
    if ! git_output="$(git status --porcelain=v1 2>/dev/null)" || [ -n "$git_output" ]; then
        git status --short 1>&2
        return 1
    fi
    hub pull-request --no-edit -p "$@"
}

alias ll='ls -l'
alias lla='ls -la'
alias llr='ls -lR'
alias llar='ls -laR'

alias which='type -path'

zdate() {
    TZ=UTC0 date "$@"
}

# For Linux:
test -r /etc/bash_completion && . /etc/bash_completion
# For macOS Homebrew:
test -r /opt/homebrew/etc/profile.d/bash_completion.sh && . /opt/homebrew/etc/profile.d/bash_completion.sh

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"

# Google Cloud SDK
if [ -d "$HOME/google-cloud-sdk" ]; then
    . "$HOME/google-cloud-sdk/path.bash.inc"
    . "$HOME/google-cloud-sdk/completion.bash.inc"
fi

# pyenv
if command -v pyenv 1> /dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

# Travis CLI
if [ -f "$HOME/.travis/travis.sh" ]; then
    . "$HOME/.travis/travis.sh"
fi

true
