#
# ~/.profile 2001-11-08
# Michael Shields <shields@msrl.com>
#

test -z "$BASH_VERSION" -a -f $HOME/bin/bash && exec $HOME/bin/bash

# Parse out the base of the hostname, for use later in the script.
hostname=`hostname`
hostname=`expr $hostname : '\([^.]*\).*'`
# Also get the tty.
tty=`tty`

# I do this because I'm paranoid.
cd $HOME

# Set up .confile to be run on logout.  We could use .bash_logout, but
# nothing in it is Bash-specific, and this gives more control over when
# .confile should be run.
trap ". $HOME/.confile" 0 1

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
test -d $HOME/bin && NEWPATH="$HOME/bin:"
test -d $HOME/man && MANPATH="$HOME/man:"
test -d $HOME/info && INFOPATH="$INFOPATH:$HOME/info"

NEWPATH="$NEWPATH/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin"
MANPATH="$MANPATH/usr/man"
test -d /usr/local/info && INFOPATH="$INFOPATH:/usr/local/info"
test -d /usr/info && INFOPATH="$INFOPATH:/usr/info"

test -d /usr/ccs/bin && NEWPATH="$NEWPATH:/usr/ccs/bin"

test -d /usr/share/man && MANPATH="$MANPATH:/usr/share/man"
test -d /usr/local/share/man && MANPATH="$MANPATH:/usr/local/share/man"
test -d /usr/local/man && MANPATH="$MANPATH:/usr/local/man"

test -d /usr/X11R6/bin && NEWPATH="$NEWPATH:/usr/X11R6/bin"
test -d /usr/X11R6/man && MANPATH="$MANPATH:/usr/X11R6/man"

test -d /usr/X/bin && NEWPATH="$NEWPATH:/usr/X/bin"
test -d /usr/X/man && MANPATH="$MANPATH:/usr/X/man"

# MacOS X with Fink
test -d /sw && NEWPATH="$NEWPATH:/sw/bin:/sw/sbin"
test -d /sw && MANPATH="$MANPATH:/sw/share/man"

test -d /usr/ucb && NEWPATH="$NEWPATH:/usr/ucb"

test -d /usr/games && NEWPATH="$NEWPATH:/usr/games"

PATH=$NEWPATH
export PATH MANPATH INFOPATH

if [ "`uname`" = SunOS ]; then
    LD_RUN_PATH=/usr/local/lib
    export LD_RUN_PATH
fi

#}}}
#{{{ General configuration

ulimit -c 0

umask 022

# Terminal and locale setup.
#eval `resize -u`
stty erase '^?'
test "`uname`" = SunOS || stty pass8
stty cs8
stty -ixon
LANG=en_US.UTF-8
export LANG

# I'm curmudgeonly and don't believe in local time.  NB: Don't set TZ
# yourself unless you know what you're doing.  And have read RFC 1305
# without being bored and going off to play Doom.
if [ ! -w / ]; then
    # It's not good to play with time zones as root.
    if [ -f /usr/lib/zoneinfo/right/UTC ]; then
	# Violate POSIX.1 and account for leap seconds.
	#TZ=right/UTC
	# No, don't.  INN calls it the -0001 time zone.  That's too weird.
	TZ=UTC
    else
	TZ=GMT0 #UTC0
    fi
    export TZ
fi

# Set default editor and pager.
EDITOR=vi
VISUAL="$EDITOR"
PAGER=less
export EDITOR VISUAL PAGER

# Set $MAIL so notification of new mail works.
if [ -d /var/spool/mail ]; then
    MAIL=/var/spool/mail/`whoami`; export MAIL
elif [ -d /var/mail ]; then
    MAIL=/var/mail/`whoami`; export MAIL
elif [ -d /usr/spool/mail ]; then
    MAIL=/usr/spool/mail/`whoami`; export MAIL
elif [ -d /usr/mail ]; then
    MAIL=/usr/mail/`whoami`; export MAIL
#else
#    echo ".profile: Couldn't find mail directory" 1>&2
fi

if [ "`uname`" = SunOS ]; then
    CC=gcc
    export CC
fi

#}}}
#{{{ Setup for specific apps

# CVS.
# "The Master wants you but he can't have you..."
if [ -d /usr/local/cvsroot ]; then
    CVSROOT=/usr/local/cvsroot
elif [ -d /var/cvs ]; then
    CVSROOT=/var/cvs
elif [ -d /home/cvs ]; then
    CVSROOT=/home/cvs
elif [ -d /var/mfnx/cvs ]; then
    CVSROOT=/var/mfnx/cvs
elif [ "$hostname" = mulligatwani ]; then
    CVSROOT=:ext:challah.msrl.com:/usr/local/cvsroot
else
    CVSROOT=:ext:box.mfnx.net:/var/mfnx/cvs
fi
CVS_RSH=ssh
export CVSROOT CVS_RSH

# debchange.
DEBEMAIL='shields@msrl.com'
DEBFULLNAME='Michael Shields'
export DEBEMAIL DEBFULLNAME

# IRC.
test `whoami` = shields && IRCNICK=Shields
export IRCNICK

# less.
LESS='-eMc'
export LESS

# Netscape -- thanks JD!
MOZILLA_NO_ASYNC_DNS=True
export MOZILLA_NO_ASYNC_DNS

# PGP.
if [ -d $HOME/.pgp ]; then
    PGPPATH="$HOME/.pgp"
    export PGPPATH
fi

# pilot-link.
PILOTRATE=115200
export PILOTRATE

# vi.
EXINIT=':set ai'; export EXINIT

#}}}

#{{{ Proxy -- see also http://www.msrl.com/proxy.pac for Netscape

if [ "`uname`" = FreeBSD -o "`uname`" = SunOS ]; then
   MY_IP="`ifconfig -a | awk '$1 == \"inet\" { print $2 }' | grep -v '^127\.'`"
elif [ "`uname`" = Linux ]; then
   MY_IP="`ifconfig -a | sed -e '/inet addr:/!d' -e '/addr:127\\./d' -e 's/.*inet addr:\([0-9.]\+\).*/\\1/'`"
else
   echo ".profile: warning: don't know how to calculate IP on `uname`"
   unset MY_IP
fi

#if [ `expr "$MY_IP" : '172.17.204\.'` -gt 0 ]; then
#   http_proxy='http://172.17.204.5:3128/'
#   ftp_proxy='http://172.17.204.5:3128/'
#fi
if [ "$hostname" = mulligatwani ]; then
   http_proxy='http://127.0.0.1:3128/'
   ftp_proxy='http://127.0.0.1:3128/'
fi

export http_proxy ftp_proxy

#}}}

#{{{ Oracle
unset oratab
test -f /etc/oratab && oratab=/etc/oratab
test -f /var/opt/oracle/oratab && oratab=/var/opt/oracle/oratab
if [ -n "$oratab" ]; then
    ORACLE_SID=`awk -F: '$3 == "Y" { print $1; exit }' $oratab`
    ORACLE_HOME=`awk -F: '$3 == "Y" { print $2; exit }' $oratab`
    export ORACLE_SID ORACLE_HOME
    PATH=$PATH:$ORACLE_HOME/bin
fi
#}}}

# used by .xsession to get this run even under xdm:
PROFILE_WAS_RUN=yes
export PROFILE_WAS_RUN

# for interactive shells, display uptime:
test -z "$PS1" || uptime

#{{{ Emacs local variables

# local variables:
# folded-file: t
# fold-marks: ("#{{{ " "#}}}")
# end:

#}}}
