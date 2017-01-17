#!/bin/sh

### Colorize
alias ls='ls --color=auto'
alias diff='diff --color=auto'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

### List
alias ll='ls -lah'
alias llf='ll -F'
alias la='ls -A'
alias laf='la -F'
alias lr='ls -R'
alias lra='lr -A'

alias ltop='du -sh * | sort -rh | head -10'
alias lsw="watch -n 0.5 -c 'tree -a'"

### Pacman
### TODO command line completion
## Sync
alias S='sudo pacman -S'
alias Sy='S -y'
alias Syy='Sy -y'
alias Su='S -u'
alias Syu='Sy -u'
alias Syyu='Syy -u'
## Query
alias Qnn='pacman -Qdtq'
## Remove
alias R='sudo pacman -R'
alias Rs='R -s'
alias Rnn='Rs $(Qnn)'

### Web
alias awiki="xdg-open 'https://wiki.archlinux.org'"
alias aur="xdg-open 'https://aur.archlinux.org'"

### Games
alias csgo="xrun steamlaunch 730"

### Mount
alias smount="go-mtpfs ~/mnt &"
alias sumount="fusermount -u ~/mnt"

### Misc
alias h='history'
# alias adbtop='adb shell top -m 5 -d 1 -n 1'
alias adbtop='adb shell top -d 1'
# Shows volume in % (like alsamixer would show it).
alias vol="amixer sget -M Master | grep -om1 '[0-9]\{1,3\}%'"
# Generates package metadata in current directory.
alias srcinfo='makepkg --printsrcinfo > .SRCINFO'

### Safety last
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# vim: set filetype=sh:
