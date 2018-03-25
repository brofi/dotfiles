#!/bin/bash
#
# Collection of general Linux functions

# True, if current user is root.
function linux_is_root {
    [ $EUID -eq 0 ]
}

# True, if current user is in group sudo or wheel.
function linux_is_sudoer {
    id -nG $EUID | grep -Eqw 'sudo|wheel'
}

# $1 group to check if existent.
function linux_is_group {
    getent group "$1" > /dev/null
}
