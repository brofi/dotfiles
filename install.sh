#!/bin/bash

# This file's name
__self="$(basename "${BASH_SOURCE[0]}")"
# Full path of this file's directory
__dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# By default links are installed in user's home directory
install_dir=$HOME

# $1 exit code
# $2 error msg
function errexit {
    echo "Error: $2" 1>&2
    exit $1
}

# Parse key value pairs
while [ $# -gt 1 ]; do
    key="$1"
    case $key in
        -C|--directory)
            install_dir="${2%/}"
            [ -d "$install_dir" ] \
                || errexit $? "'$install_dir' is not a directory!"
            shift
            ;;
        --owner)
            owner="$2"
            getent passwd $owner > /dev/null \
                || errexit $? "user '$owner' does not exist!"
            shift
            ;;
        --group)
            group="$2"
            getent group $group > /dev/null \
                || errexit $? "group '$group' does not exist!"
            shift
            ;;
        *)
            # unknown
    esac
    shift
done

IFS=$'\n'
# Get dirs without children (only links to parent and itself)
# Prune content of .git directory and exclude .git itself
# Otherwise it will be found if it has no subdirectories
leaf_dirs=($(find $__dir -not \( -path "$__dir/.git" -prune \) -type d -links 2))

# Get file names. Ignore this file, README.md and files in .git directory
dotfiles=($(find $__dir -type f ! -name $__self ! -name README.md -not -path "$__dir/.git/*"))
unset IFS

echo 'Ensuring directory structure...'
for d in "${leaf_dirs[@]}"; do
    mkdir -p $install_dir/${d#$__dir/}
done

echo 'Setting up symlinks...'
for f in "${dotfiles[@]}"; do
    name=$install_dir/${f#$__dir/}
    # Save existing files
    if [ -f "$name" ]; then
        bak=${name}.dotsave
        while [ -f "$bak" ]; do
            bak=${bak}.dotsave
        done
        mv "$name" "$bak"
        echo "warning: '$name' saved as '$bak'"
    fi
    ln -sv "$f" "$name"
    # Fix permissions
    [ -z "$owner" ] || chown $owner "$name"
    [ -z "$group" ] || chgrp $group "$name"
done
