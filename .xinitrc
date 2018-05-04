#!/bin/sh

# shellcheck source=.xinitrc_base
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


# exec openbox-session
exec xmonad
