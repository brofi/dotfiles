#!/bin/bash
# shellcheck disable=1090

# clear /etc/issue here to not hide potential errors
clear

[ -f ~/.bash_exports ] && . ~/.bash_exports

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    export TERM="xterm-256color"
    exec startx
fi

[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set filetype=sh:
