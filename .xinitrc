#!/bin/sh

. ~/.xinitrc_base

compton -b
udiskie &
mpd ~/.mpd/mpd.conf &
dropbox &
skypeforlinux &
pidgin &
steam-native &

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


exec xmonad
# exec openbox
# exec startxfce4
