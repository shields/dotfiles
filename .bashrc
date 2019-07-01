#!/bin/bash
#
# ~/.bashrc
# Michael Shields <shields@msrl.com>
#

# This entire file is useless if this is a non-interactive shell, so
# just quit now.
test -z "$PS1" && return

alias which='type -path'

if [ "$TERM" = xterm-debian -a ! -e /etc/terminfo/x/xterm-debian \
     -a ! -e /usr/share/terminfo/x/xterm-debian ]; then
    TERM=xterm
fi

trimmed_hostname="`hostname | sed -e 's/\.msrl\.com$//' \
				  -e 's/\.above\.net$//' \
				  -e 's/\.mfnx\.net$//'`"
if [ "$TERM" = xterm -o "$TERM" = xterm-debian ]; then
    PS1="\\[\\033]0;\\u@$trimmed_hostname\\007\\]\w\\$ "
else
    PS1="\\u@$trimmed_hostname:\\w\\$ "
fi
export PS1

alias ll='ls -l'
alias lla='ls -la'
alias llr='ls -lR'
alias llar='ls -laR'

test -f /etc/bash_completion && . /etc/bash_completion

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"

true
