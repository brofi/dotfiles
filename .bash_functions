#!/bin/bash

# colored man pages
# Termcap/Terminfo: {{{
# termcap terminfo
# ks      smkx      make the keypad send commands
# ke      rmkx      make the keypad send digits
# vb      flash     emit visual bell
# mb      blink     start blink
# md      bold      start bold
# me      sgr0      turn off bold, blink and underline
# so      smso      start standout (invert)
# se      rmso      stop standout
# us      smul      start underline
# ue      rmul      stop underline
# }}}
man() {
    local red=1
    local green=2
    local yellow=3
    local blue=4

    # Get these colors e.g. from vim color scheme (cterm color) or use a
    # software to find the closest 8bit color for a given rgb color.
    if [ "$(tput colors)" -gt 255 ]; then
        red=167
        green=142
        yellow=214
        blue=109
    fi

    # We're setting an additional sgr0 (turn off all attributes) after rmso and
    # rmul, otherwise the color isn't reset.
    env LESS_TERMCAP_mb="$(tput blink; tput bold; tput setaf $red)" \
    LESS_TERMCAP_md="$(tput bold; tput setaf $green)" \
    LESS_TERMCAP_me="$(tput sgr0)" \
    LESS_TERMCAP_so="$(tput smso; tput setaf $yellow)" \
    LESS_TERMCAP_se="$(tput rmso; tput sgr0)" \
    LESS_TERMCAP_us="$(tput smul; tput setaf $blue)" \
    LESS_TERMCAP_ue="$(tput rmul; tput sgr0)" \
    man "$@"
}

# Convert given hex color value to a (nearest) 0-255 color index.
cfromhex() {
    [ "$#" -eq 1 ] || return 1
    local c=${1#\#} r g b
    r=$(printf '0x%0.2s' "$c")
    g=$(printf '0x%0.2s' "${c:2}")
    b=$(printf '0x%0.2s' "${c:4}")
    printf '%03d' "$(( (r<75?0:(r-35)/40)*6*6 +
                       (g<75?0:(g-35)/40)*6   +
                       (b<75?0:(b-35)/40)     + 16 ))"
}

# Disown given command. Don't log it's output.
fu() {
    command -v "$1" > /dev/null || return 1
    nohup "$1" > /dev/null 2>&1 &
}

# vim: set ft=sh fdm=marker:
