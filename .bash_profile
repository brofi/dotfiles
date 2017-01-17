#!/bin/bash
# shellcheck disable=1090

[ -f ~/.bash_exports ] && . ~/.bash_exports

if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" -eq 1 ]; then
    export TERM="xterm-256color"
    exec startx
elif [ "$XDG_VTNR" -eq 2 ]; then
    export TERM="fbterm"
    exec fbterm
fi

[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set filetype=sh:
