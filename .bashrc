#!/bin/bash
# shellcheck disable=1090

# If not running interactively, don't do anything.
[[ $- != *i* ]] && return

# If the executed command is the name of a directory, change to this directory.
shopt -s autocd

# Check the window size after each command and, if necessary, update the values
# of LINES and COLUMNS.
shopt -s checkwinsize

# Append to the history file, don't overwrite it.
shopt -s histappend
# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth
# Don't save commands matching these patterns in the history.
HISTIGNORE="ls:fg:bg:h:history"
# Number of commands to remember in the command history.
HISTSIZE=1000
# Maximum number of lines contained in the history file.
HISTFILESIZE=2000

# Source aliases and functions.
for f in ~/.bash_{aliases,functions}; do
    [ -f "$f" ] && . "$f"
done
unset f

# Source custom completion functions from ~/.bash_completion.d which are not
# sourced by /usr/share/bash-completion/bash_completion.
if [ -d ~/.bash_completion.d ] ; then
    for f in ~/.bash_completion.d/*; do
        [ -f "$f" ] && . "$f"
    done
    unset f
fi

# Set up Node Version Manager
[ -f /usr/share/nvm/init-nvm.sh ] && . /usr/share/nvm/init-nvm.sh

# Get number of colors this terminal supports.
# Make sure $TERM is set correctly at this point.
colors=0
[ -x "$(command -v tput)" ] \
    && tput setaf 1 > /dev/null 2>&1 \
    && tput colors > /dev/null 2>&1 \
    && colors=$(tput colors)

# Set prompt dependent on the number of colors available.
[ -f ~/.bash_prompt ] && . ~/.bash_prompt "$colors"

if [ "$colors" -ge 8 ]; then
    # Setup colors for ls.
    [ -x "$(command -v dircolors)" ] && eval "$(dircolors -b)"

    # Try to initialize 16-color palette with colors from Xresources.
    [ -x ~/bin/terminitc ] && ~/bin/terminitc

    # Try to initialize 256-color palette with precise gruvbox colors.
    [ "$colors" -ge 256 ] \
        && [ -f ~/.vim/plugged/gruvbox/gruvbox_256palette.sh ] \
        && . ~/.vim/plugged/gruvbox/gruvbox_256palette.sh
fi

unset colors

# vim: set filetype=sh:
