#!/bin/sh

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias n4-mount="go-mtpfs ~/mnt &"
alias n4-umount="fusermount -u ~/mnt"
alias arch-wiki="xdg-open 'https://wiki.archlinux.org'"
alias aur="xdg-open 'https://aur.archlinux.org'"
alias l10="du -sh * | sort -rh | head -10"
alias lsw="watch -n 0.5 -c 'tree -a'"
alias csgo="xrun steamlaunch 730"
# Shows volume in % (like alsamixer would show it).
alias vol="amixer sget -M Master | grep -om1 '[0-9]\{1,3\}%'"

# vim: set filetype=sh:
