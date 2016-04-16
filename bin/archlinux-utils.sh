# Collection of Arch Linux specific functions

url_aur=https://aur.archlinux.org

# Checks if given package is installed and prints error message if not
# $1 package to check if installed
function archlinux_is_installed {
    if ! pacman -Q $1 > /dev/null 2>&1; then
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
    curl -sILfo /dev/null -w '%{http_code}' "$url_aur/packages/$1" | grep -q '^2'
}

# Prints all installed aur packages not existing anymore
function archlinux_print_missing_aur_packages {
    for pkg in $(pacman -Qqm); do
        if ! archlinux_exists_in_aur $pkg; then
            echo "$pkg is missing!"
        fi
    done
}
