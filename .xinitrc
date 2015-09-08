~/.fehbg &
xsetroot -cursor_name left_ptr
xrdb ~/.Xresources &
setxkbmap -layout us -variant altgr-intl
trayer --SetDockType true --edge top --align left --widthtype request --height 19 --margin 970 --distance 1 &
# xcompmgr &
compton -b &
dunst &
udiskie &
mpd ~/.mpd/mpd.conf &
dropbox &
skype &
pidgin &
# urxvt -e weechat &
# tox &
steam &
unclutter -grab -idle 3 -noevents -root &
exec xmonad
# exec openbox
# exec startxfce4
