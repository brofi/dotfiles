#!/bin/bash
# shellcheck disable=1090

[ -f ~/.bash_exports ] && . ~/.bash_exports

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    export TERM="xterm-256color"
    exec startx
fi

[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set filetype=sh:
