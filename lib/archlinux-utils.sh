#!/bin/bash
#
# Collection of Arch Linux specific functions

url_aur=https://aur.archlinux.org

# Checks if given package is installed and prints error message if not
# $1 package to check if installed
function archlinux_is_installed {
    if ! pacman -Q "$1" > /dev/null 2>&1; then
        error "package '$1' is not installed."
        return 1
    fi
}

# Is true if http code begins with 2, or curl isn't installed
# $1 package name
function archlinux_exists_in_aur {
    archlinux_is_installed "curl" || return 0

    # s: operate in silent mode
    # I: fetch header only
    # L: follow if page moved
    # f: fail silently
    # o: output (header) to /dev/null
    # w: print the http code
    curl -sILfo /dev/null -w '%{http_code}' "$url_aur/packages/$1" \
        | grep -q '^2'
}

# Prints all installed aur packages not existing anymore
function archlinux_print_missing_aur_packages {
    for pkg in $(pacman -Qqm); do
        if ! archlinux_exists_in_aur "$pkg"; then
            echo "$pkg is missing!"
        fi
    done
}

# True if window manager present, false otherwise
# see: https://specifications.freedesktop.org/wm-spec/1.3/ar01s03.html
function archlinux_wm_check {
    local child_id prop=_NET_SUPPORTING_WM_CHECK idP="0x[[:xdigit:]]\+"
    # Get id of child window created by window manager
    # If property not set, no WM is present
    child_id=$(xprop -root $prop | grep -o "$idP") || return 1
    # Child's prop must match child_id
    [ "$child_id" = "$(xprop -id "$child_id" $prop | grep -o "$idP")" ]
    # At this point we could query the WM name: xprop -id $child_id _NET_WM_NAME
}
