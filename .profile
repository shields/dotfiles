#
# ~/.profile
# Michael Shields <shields@msrl.com>
#

test -z "$BASH_VERSION" -a -f "$HOME/bin/bash" && exec "$HOME/bin/bash"

# Parse out the base of the hostname, for use later in the script.
hostname="`hostname`"
hostname="`expr $hostname : '\([^.]*\).*'`"

uname="`uname`"

# I do this because I'm paranoid.
cd "$HOME"

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

NEWPATH="" MANPATH="" INFOPATH="."

# $HOME/*.
test -d "$HOME/bin" && NEWPATH="$HOME/bin:"
test -d "$HOME/man" && MANPATH="$HOME/man:"
test -d "$HOME/info" && INFOPATH="$INFOPATH:$HOME/info"

NEWPATH="$NEWPATH/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
MANPATH="$MANPATH/usr/man"
test -d /usr/local/info && INFOPATH="$INFOPATH:/usr/local/info"
test -d /usr/info && INFOPATH="$INFOPATH:/usr/info"

test -d /usr/ccs/bin && NEWPATH="$NEWPATH:/usr/ccs/bin"

test -d /usr/share/man && MANPATH="$MANPATH:/usr/share/man"
test -d /usr/local/share/man && MANPATH="$MANPATH:/usr/local/share/man"
test -d /usr/local/man && MANPATH="$MANPATH:/usr/local/man"

test -d /usr/X/bin && NEWPATH="$NEWPATH:/usr/X/bin"
test -d /usr/X/man && MANPATH="$MANPATH:/usr/X/man"

test -d /usr/ucb && NEWPATH="$NEWPATH:/usr/ucb"

test -d /usr/games && NEWPATH="$NEWPATH:/usr/games"

PATH="$NEWPATH"
export PATH MANPATH INFOPATH

if [ "$uname" = SunOS ]; then
    LD_RUN_PATH=/usr/local/lib
    export LD_RUN_PATH
fi

#}}}
#{{{ General configuration

#ulimit -c 0

umask 022

# Terminal and locale setup.
if [ ! "`expr $uname : MINGW32`" ]; then
    #eval `resize -u`
    stty erase '^?'
    test "$uname" = SunOS || stty pass8
    stty cs8
    stty -ixon
fi

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

# IRC.
test "$LOGNAME" = shields && IRCNICK=Shields
export IRCNICK

# less.
LESS='-eMc'
export LESS

# PGP.
if [ -d "$HOME/.pgp" ]; then
    PGPPATH="$HOME/.pgp"
    export PGPPATH
fi

# rsync.
RSYNC_RSH=ssh
export RSYNC_RSH

# vi.
EXINIT=':set ai'; export EXINIT

#}}}

test "$tty" != "not a tty" && uptime

#{{{ Emacs local variables

# local variables:
# folded-file: t
# fold-marks: ("#{{{ " "#}}}")
# end:

#}}}
