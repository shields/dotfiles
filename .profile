#
# ~/.profile
# Michael Shields <shields@msrl.com>
#

#{{{ bash-specific setup

# This is basically in the .bashrc.
test ! -z "$BASH_VERSION" && . .bashrc

# Some things are in variables, and we can just set them here and have
# it exported.  Stock sh's will just be oblivious to these.
notify=glorf; export notify
HISTCONTROL=ignoredups; export HISTCONTROL
command_oriented_history=glorf; export command_oriented_history
HOSTFILE=/etc/hosts; export HOSTFILE

#}}}
#{{{ *PATH

NEWPATH="$HOME/bin:$HOME/go/bin:/opt/homebrew/bin:/usr/local/bin:/bin:/usr:/usr/bin"
MANPATH="$HOME/man:/opt/homebrew/man:/usr/local/man:/usr/man"
INFOPATH="$HOME/info:/usr/local/info"

test -d /usr/share/man && MANPATH="$MANPATH:/usr/share/man"
test -d /usr/local/share/man && MANPATH="$MANPATH:/usr/local/share/man"

test -d "$HOME/google-cloud-sdk/bin" && NEWPATH="$NEWPATH:$HOME/google-cloud-sdk/bin"

PATH="$NEWPATH"
export PATH MANPATH INFOPATH

#}}}
#{{{ General configuration

umask 022

# Terminal and locale setup.
stty erase '^?'
stty cs8
stty -ixon

if [ -z "$LANG" ]; then
    LANG=en_US.UTF-8
    export LANG
fi

if [ "$TERM" = linux ]; then
    tty | fgrep -q /dev/vc && unicode_start
fi

if [ ! -w / ]; then	# It's not good to play with time zones as root.
    if [ -f /usr/lib/zoneinfo/UTC -o /usr/share/zoneinfo/UTC ]; then
	TZ=UTC
    else
	TZ=GMT0
    fi
    export TZ
fi

# Set default editor and pager.
EDITOR=vi
VISUAL="$EDITOR"
test -x /usr/bin/less && PAGER=less
export EDITOR VISUAL PAGER

# For Mutt, maybe others.
EMAIL=shields@msrl.com
export EMAIL

#}}}
#{{{ Setup for specific apps

# debchange.
DEBEMAIL="$EMAIL"
DEBFULLNAME='Michael Shields'
export DEBEMAIL DEBFULLNAME

# Go.
export GOPATH="$HOME/go"

# IRC.
test "$LOGNAME" = shields && IRCNICK=Shields
export IRCNICK

# less.
LESS='-eMX'
export LESS

# PGP.
if [ -d "$HOME/.pgp" ]; then
    PGPPATH="$HOME/.pgp"
    export PGPPATH
fi

# pyenv.
if command -v pyenv 1>/dev/null 2>&1; then
    eval "$(pyenv init -)"
fi

# ripgrep.
RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"
export RIPGREP_CONFIG_PATH

# rsync.
RSYNC_RSH=ssh
export RSYNC_RSH

# vi.
EXINIT=':set ai'; export EXINIT

#}}}

#{{{ Emacs local variables

# local variables:
# folded-file: t
# fold-marks: ("#{{{ " "#}}}")
# end:

#}}}
