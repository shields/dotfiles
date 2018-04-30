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

# "slow ssh"
alias slssh='ssh -C -x'

if [ "$trimmed_hostname" = mulligatwani ]; then
    alias mull=:
    alias slmull=:
else
    alias mull='ssh mulligatwani.msrl.com'
    alias slmull='slssh mulligatwani.msrl.com'
fi
if [ "$trimmed_hostname" = challah ]; then
    alias chal=:
    alias slchal=:
else
    alias chal='ssh challah.msrl.com'
    alias slchal='slssh challah.msrl.com'
fi

alias ll='ls -l'

test -f /etc/bash_completion && . /etc/bash_completion

true
