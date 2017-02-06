#!/bin/bash
# shellcheck disable=1090

[ -f ~/.bash_exports ] && . ~/.bash_exports

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ]; then
    if [ "$XDG_VTNR" -eq 1 ]; then
        export TERM="xterm-256color"
        exec startx
    elif [ "$XDG_VTNR" -lt 5 ]; then
        export TERM="fbterm"
        exec fbterm
    fi
fi

[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set filetype=sh:
