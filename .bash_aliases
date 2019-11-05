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
# 'Expand when defined, not when used' is intended.
# shellcheck disable=SC2139
if command -v pacman > /dev/null; then
    # We need the full command for completion (no alias within alias)
    p="pacman"
    sp="sudo $p"
    ## Sync
    alias S="$sp -S"
    alias Sy="$sp -Sy"
    alias Syy="$sp -Syy"
    alias Su="$sp -Su"
    alias Syu="$sp -Syu"
    alias Syyu="$sp -Syyu"
    ## Query
    alias Qnn="$p -Qdtq"
    ## Remove
    alias R="$sp -R"
    alias Rs="$sp -Rs"
    alias Rnn="$sp -Rs \$($p -Qdtq)"
    unset p sp
fi

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
alias vol="amixer sget -M Master | grep -om1 '[0-9]\\{1,3\\}%'"
# Generates package metadata in current directory.
alias srcinfo='makepkg --printsrcinfo > .SRCINFO'
# Filter xev for keycodes only.
alias xkeycodes="xev | grep -A2 --line-buffered '^KeyRelease' \
    | sed -n '/keycode /s/^.*keycode \\([0-9]*\\).* (.*, \\(.*\\)).*$/\\1 \\2/p'"
# Play random video from /media/videos wit VLC.
alias vlcrand="fu vlc --media-library /media/videos --random --no-audio \
    --no-sub-autodetect-file --no-osd --no-spu --no-video-title-show \
    --no-overlay --no-video-deco"
# Connect to KIT VPN.
alias kitvpn='sudo openvpn /etc/openvpn/kit.ovpn'

### Safety last
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

# vim: set filetype=sh:
