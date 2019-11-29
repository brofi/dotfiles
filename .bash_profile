#!/bin/bash
# shellcheck disable=1090

# clear /etc/issue here to not hide potential errors
clear

[ -f ~/.bash_exports ] && . ~/.bash_exports

# if we ssh into this machine but it doesn't know the client's $TERM
if ! infocmp "$TERM" > /dev/null 2>&1; then
    for t in xterm-256color xterm linux; do
        infocmp $t > /dev/null 2>&1 && TERM=$t && break
    done
fi

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
    export TERM="xterm-256color"
    exec startx
fi

[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set filetype=sh:
