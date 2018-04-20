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
    nohup "$@" > /dev/null 2>&1 &
}

# Exchange the names of two given files.
switch() {
    local tmp
    tmp=$(mktemp "/tmp/$(basename "$1").XXXXXXXXXX") || return 1
    mv "$1" "$tmp"
    mv "$2" "$1"
    mv "$tmp" "$2"
}

cssip() {
    read -rp "Server password: " pw
    local ipp='[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}'
    echo "connect" \
         "$(ping -c 1 andyroid.de | grep -Eo -m 1 "$ipp"):27015;" \
         "password $pw"
}

mintty_cfromhex() {
    if [ "$#" -eq 0 ] || [ "${#1}" -lt 6 ]; then
        return 1
    fi
    local c=${1#\#}
    printf '%d,%d,%d' "0x${c: :2}" "0x${c:2:2}" "0x${c: -2}"
}

# Creates a new completion wrapper function, where COMP_WORDS, COMP_CWORD,
# COMP_LINE and COMP_POINT are set to what they would be for the aliased
# command, so the called completion function works with the alias as well.
#
# COMP_WORDS has to be the actual command with arguments and the current words
# on the command line without the alias itself.
#
# The COMP_CWORD index into COMP_WORDS has to be increased by the number of
# words of the actual command. We substract 1, because while completing the
# alias we're already at index 1. On index 0 we would complete the alias itself
# and this function wouldn't be called.
#
# The COMP_LINE contains all COMP_WORDS separated by spaces.
#
# The current cursor position COMP_POINT, relative to the beginning of the
# current command, has to be increased by the length of the actual command minus
# the length of the alias. We again substract 1, because while completing the
# alias we're already at COMP_POINT 2.
#
# @param $1 name of the actual completion function
# @param $2 name of the completion wrapper function
# @param $3 name of the alias
# @param $4... actual command name and arguments
function _alias_comp_wrapper {
    local debug=false

    local comp_func_name="$1"
    local wrap_func_name="$2"
    local alias="$3"
    shift 3
    local line=$*
    local words=("$@")
    # We don't want to expand or re-split the words in our created wrapper
    # function. An alias='cmd1 arg $(cmd2 arg)' should have the inital words
    # 'cmd1', 'arg' and '$(cmd2 arg)'.
    words=("${words[@]/#/\'}")
    words=("${words[@]/%/\'}")

    if $debug; then
        local print_debug="
            printf '\\nwords: '
            printf \"'%s' \" \"\${COMP_WORDS[@]}\"
            printf '\\n'
            printf \"line: '%s'\\n\" \"\$COMP_LINE\"
            printf 'cword: %s\\n' \"\$COMP_CWORD\"
            printf 'point: %s\\n' \"\$COMP_POINT\"
        "
    fi

    local func="
        function $wrap_func_name {
            COMP_WORDS=(${words[*]} \"\${COMP_WORDS[@]:1}\")
            (( COMP_CWORD+=$#-1 ))
            printf -v COMP_LINE '%s ' \"\${COMP_WORDS[@]}\"
            COMP_LINE=\${COMP_LINE%?}
            (( COMP_POINT+=${#line}-${#alias}-1 ))
            $print_debug
            $comp_func_name
        }"

    eval "$func"
}

# Stores completion words for an aliased command in the given array, where each
# command substitution is treated as one word.
#
# @param $1 alias
# @param $2 array by name
function _alias_comp_words {
    local cmd=()
    local cmd_sub=""
    local cmd_sub_cnt=0
    for w in $(alias "$1" | sed "s/.*='\\(\\bsudo \\b\\)\\?\\(.*\\)'/\\2/"); do
        # No negative lookbehind required since the opening bracket needs to be
        # escaped if '$(' should be literal when executing the alias
        if grep "\$(" <<< "$w" > /dev/null; then
            (( cmd_sub_cnt+=$(grep -o "\$(" <<< "$w" | wc -l) ))
        fi
        if [ $cmd_sub_cnt -gt 0 ]; then
            cmd_sub="${cmd_sub}${w} "
            # Match closing brackets with negative lookbehind for a backslash
            if grep -P '(?<!\\)\)' <<< "$w" > /dev/null; then
                (( cmd_sub_cnt-=$(grep -oP '(?<!\\)\)' <<< "$w" | wc -l) ))
                if [ $cmd_sub_cnt -eq 0 ]; then
                    cmd_sub=${cmd_sub%?}
                    cmd=("${cmd[@]}" "${cmd_sub}")
                    cmd_sub=""
                fi
            fi
        else
            cmd=("${cmd[@]}" "$w")
        fi
    done
    eval "$2=(\"\${cmd[@]}\")"
}

# vim: set ft=sh fdm=marker:
