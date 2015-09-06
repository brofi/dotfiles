#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

#
# start X on login
#
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
