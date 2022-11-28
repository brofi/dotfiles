#!/bin/sh

# shellcheck source=.xinitrc_base
. ~/.xinitrc_base

xscreensaver -no-splash &
udiskie &
steam &

unclutter \
    -grab \
    -idle 3 \
    -noevents \
    -root &

# exec openbox-custom
exec xmonad
