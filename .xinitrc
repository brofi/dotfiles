#!/bin/sh

# don't run in background
xrdb ~/.Xresources

# load scripts from xinitrc.d
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

~/.fehbg &
~/.mouse &
compton -b
udiskie &
mpd ~/.mpd/mpd.conf &
dropbox &
skypeforlinux &
pidgin &
steam &

unclutter \
    -grab \
    -idle 3 \
    -noevents \
    -root &

trayer \
    --SetDockType true \
    --edge top \
    --align left \
    --widthtype request \
    --height 19 \
    --margin 970 \
    --distance 1 &

xsetroot -cursor_name left_ptr &
setxkbmap -layout us -variant altgr-intl &

exec xmonad
# exec openbox
# exec startxfce4
