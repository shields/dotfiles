#!/bin/sh
#
# ~/.xsession
# Michael Shields <shields@msrl.com>
# $Id$
#

cd $HOME
# ash looks in $PATH for .'s arguments; bash looks in cwd.
test "$PROFILE_WAS_RUN" = yes || . $HOME/.profile

# Not all OSes do this automatically.
xrdb -merge .Xresources

xmodmap .xmodmap-pckb
test -f .Xmodmap && xmodmap .Xmodmap

# fast mouse
xset m 4 4

# xloadimage -onroot /usr/local/lib/mac-7.5.3-bgs/23295.ppm
# xearth &
#xv -root -rm 5 -quit $HOME/.bgs/linside.gif
xsetroot -solid black

xscreensaver &

test -x /usr/bin/ssh-askpass && SSH_ASKPASS=/usr/bin/ssh-askpass
export SSH_ASKPASS
eval `ssh-agent -s`

GDK_USE_XFT=1
export GDK_USE_XFT

if test -f /proc/net/wireless; then
    wmwave &
fi

if test -d $HOME/.xplanet; then
    $HOME/bin/xplanet-moon-update
    xplanetbg -moonside -wait 60 -blend -radius 46 -center 680,520 \
        -cloud_image $HOME/.xplanet/clouds_2000.jpg -cloud_threshold 0 \
	-background $HOME/.xplanet/background.png &
fi

#xwrits clock after=15 top &

exec gnome-session
