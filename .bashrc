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

# Set up Node Version Manager
[ -f /usr/share/nvm/init-nvm.sh ] && . /usr/share/nvm/init-nvm.sh

# pacman alias completion
if command -v pacman > /dev/null; then
    # Set up dynamic completion loading for pacman, so we don't have to source
    # /usr/share/bash-completion/completions/pacman
    _completion_loader pacman

    for a in S Sy Syy Su Syu Syyu Qnn R Rs Rnn; do
        # Store completion words for aliased command in cw
        _alias_comp_words $a _cw
        # Create a wrapper function and use it to complete the alias
        # shellcheck disable=SC2154
        _alias_comp_wrapper _pacman "_pacman_$a" $a "${_cw[@]}"
        complete -o default -F "_pacman_$a" $a
    done
    unset a _cw
fi

# Custom bash completion.
complete -F _display display
complete -F _fu fu

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
