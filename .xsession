#!/bin/sh
#
# ~/.xsession 2002-03-31
# Michael Shields <shields@msrl.com>
#

cd $HOME
# ash looks in $PATH for .'s arguments; bash looks in cwd.
test "$PROFILE_WAS_RUN" = yes || . $HOME/.profile

# Not all OSes do this automatically.
xrdb -merge .Xresources

test "`hostname`" = vichyssoise || xmodmap .xmodmap-dvorak
xmodmap .xmodmap-pckb
test -f .Xmodmap && xmodmap .Xmodmap

xset m 4 4

# Make Insert an Alt key on the Libretto:
test "`hostname`" = vichyssoise && \
    xmodmap -e 'keycode 113 =' -e 'keycode 106 = Alt_R' \
	    -e 'clear mod1' -e 'add mod1 = Alt_L Alt_R'

# xloadimage -onroot /usr/local/lib/mac-7.5.3-bgs/23295.ppm
# xearth &
#xv -root -rm 5 -quit $HOME/.bgs/linside.gif
xsetroot -solid black

xscreensaver &
xautolock -time 30 -locker 'xscreensaver-command -lock' \
    -corners +000 -cornerredelay 3600 &

eval `ssh-agent -s`

if test -f /proc/net/wireless; then
    wmwave &
fi

wmaker &

exec gnome-session
