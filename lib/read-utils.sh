#!/bin/bash
#
# Collection of functions for reading user input

__dir=$(dirname "${BASH_SOURCE[0]}")

# Check dependencies
if ! err_ > /dev/null 2>&1; then
    # shellcheck source=/dev/null
    . "$__dir/error-utils.sh"
fi
if ! array_ > /dev/null 2>&1; then
    # shellcheck source=/dev/null
    . "$__dir/array-utils.sh"
fi
if ! number_ > /dev/null 2>&1; then
    # shellcheck source=/dev/null
    . "$__dir/number-utils.sh"
fi


# Reads user input and returns 0 if the user confirms, 1 otherwise.
# $1 question text
# $2 default choice: 0 (yes) or 1 (no)
function read_confirm {
    local suffix

    if [ "$2" -eq 0 ]; then
        suffix="[Y/n]"
    elif [ "$2" -eq 1 ]; then
        suffix="[y/N]"
    else
        # We keep in mind that we can only retun values <= 255 anyway
        err_exit "Default choice must be 0 (yes) or 1 (no)" 1
    fi

    while : ; do
        read -rp "$1 $suffix " yn
        case $yn in
            [Yy]) return 0;;
            [Nn]) return 1;;
            *) return "$2";;
        esac
    done
}

# $1 question text
# $2 default option
# $3 action on confirm
# $4 action on discard
function read_confirm_do {
    if read_confirm "$1" "$2"; then $3; else $4; fi
}

# Prints choices with index, reads user input and returns chosen index
# $1 array with choices
function read_choice {
    local choices=("${@}")
    array_print_indexed "${choices[@]}"
    local max_idx=$((${#choices[@]} - 1))
    while true; do
        read -rp "> " idx
        case $idx in
            [0-"$max_idx"]) return "$idx";;
            "") ;;
            *) err_print "Selection '$idx' does not exist.";;
        esac
    done
}

# Reads unsigned integer into the given variable name
# $1 prompt
# $2 variable name
function read_uint {
    while :; do
        read -rp "$1" uint
        if number_is_uint "$uint"; then
            eval "$2"'=$uint'
            return 0
        else
            err_print "NaN: '$uint'"
        fi
    done
}
