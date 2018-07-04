#!/bin/bash
#
# Collection of useful number functions

# Just true
function number_ { :;}

# True if $1 is natural number
function number_is_uint {
    case $1 in
        ''|*[!0-9]*) return 1 ;;
        *) return 0 ;;
    esac
}

# True if $1 is signed (+/-) natural number
function number_is_int {
    number_is_uint "${1#[-+]}"
}

# True if $1 is
# .n or n. or n.n
# with n element of natural numbers
function number_is_ufloat {
    number_is_uint && return 1

    local left="${1%%.*}"
    local right="${1#*.}"
    { [ -z "$left" ] && number_is_uint "$right"; } ||
        { [ -z "$right" ] && number_is_uint "$left"; } ||
            { number_is_uint "$left" && number_is_uint "$right"; }
}

# True if $1 is +f or -f
# with number_is_ufloat f = true
function number_is_float {
    number_is_ufloat "${1#[-+]}"
}
