#!/bin/bash
#
# ~/.bashrc
# Michael Shields <shields@msrl.com>
#

# This entire file is useless if this is a non-interactive shell, so
# just quit now.
test -z "$PS1" && return

if [ "$TERM" = xterm-debian -a ! -e /etc/terminfo/x/xterm-debian \
     -a ! -e /usr/share/terminfo/x/xterm-debian ]; then
    TERM=xterm
fi

trimmed_hostname="`hostname | sed -e 's/\.msrl\.com$//' \
				  -e 's/\.above\.net$//' \
				  -e 's/\.mfnx\.net$//'`"
if [ "$TERM" = xterm -o "$TERM" = xterm-debian -o "$TERM" = xterm-256color ]; then
    PS1="\\[\\033]0;\\u@$trimmed_hostname\\007\\]\w\\$ "
    # On iTerm2, display git branch name on the touch bar.  For
    # safety, limit characters allowed.
    if [ -x ~/.iterm2/it2check ] && ~/.iterm2/it2check; then
	PS1='\[$(~/.iterm2/it2setkeylabel set status $((git branch-name 2>/dev/null || echo ∅) | tr -Cd ∅a-z0-9_-/))\]'"$PS1"
    fi
else
    PS1="\\u@$trimmed_hostname:\\w\\$ "
fi
export PS1

alias g='git'

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
test -r /usr/local/etc/profile.d/bash_completion.sh && . /usr/local/etc/profile.d/bash_completion.sh

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"

# Google Cloud SDK
if [ -d "$HOME/google-cloud-sdk" ]; then
    . "$HOME/google-cloud-sdk/path.bash.inc"
    . "$HOME/google-cloud-sdk/completion.bash.inc"
fi

# pyenv
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

# Travis CLI
if [ -f "$HOME/.travis/travis.sh" ]; then
    . "$HOME/.travis/travis.sh"
fi

true
