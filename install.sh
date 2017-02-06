#!/bin/bash

# This file's name
__self="$(basename "${BASH_SOURCE[0]}")"
# Full path of this file's directory
__dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# File name extension for backed up files
ext_bak=dotsave

# By default links are installed in user's home directory
install_dir=$HOME

# $1 exit code
# $2 error msg
function errexit {
    echo "Error: $2" 1>&2
    exit $1
}

function cleanup {
    IFS=$'\n'
    dotsave_files=($(find $install_dir -xdev -name "*.$ext_bak"))
    unset IFS
    for f in "${dotsave_files[@]}"; do
        rm "$f"
        echo "Deleted $f"
    done
}

function is_leaf_dir {
    [ -z "$(find "$1" -maxdepth 1 ! -path "$1" -type d)" ]
}
export -f is_leaf_dir

function get_leaf_dirs_ntfs {
    find "$__dir" \
        -not \( \
            -path "$__dir/.git" \
            -prune \
        \) \
        -type d \
        \( \
            -empty \
            -o \
            -exec sh -c 'is_leaf_dir "$1"' _ '{}' \; \
        \) \
        -print
}

# Get dirs without children (only links to parent and itself). Prune content of
# .git directory and exclude .git itself. Otherwise it will be found if it has
# no subdirectories.
function get_leaf_dirs_unix {
    find "$__dir" \
        -not \( \
            -path "$__dir/.git" \
            -prune \
        \) \
        -type d \
        -links 2
}

# Parse key value pairs
while [ $# -gt 0 ]; do
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
        -c|--clean)
            do_clean=true
            ;;
        *)
            # unknown
    esac
    shift
done

# If clear switch set, remove backed up files
[ "$do_clean" = true ] && cleanup && exit 0

IFS=$'\n'

if df -t ntfs "$__dir" > /dev/null 2>&1; then
    leaf_dirs=($(get_leaf_dirs_ntfs))
else
    leaf_dirs=($(get_leaf_dirs_unix))
fi

# Get file names. Ignore this file, README.md, TODO and files in .git directory
dotfiles=($(find $__dir -type f ! -name $__self ! -name README.md ! -name TODO -not -path "$__dir/.git/*"))

unset IFS

echo 'Ensuring directory structure...'
for d in "${leaf_dirs[@]}"; do
    name=$install_dir/${d#$__dir/}
    mkdir -p "$name"
    # set permission on directories
    if [ -n "$owner" ] || [ -n "$group" ]; then
        # 'owner:' sets owner and login group, so:
        owned=$owner; [ -z "$group" ] || owned+=:$group
        while true; do
            chown $owned "$name"
            name=$(dirname "$name")
            [ "$name" != "$install_dir" ] || break
        done
    fi
done

echo 'Setting up symlinks...'
for f in "${dotfiles[@]}"; do
    name=$install_dir/${f#$__dir/}
    # Save existing files
    if [ -f "$name" ]; then
        bak=${name}.${ext_bak}
        while [ -f "$bak" ]; do
            bak=${bak}.${ext_bak}
        done
        mv "$name" "$bak"
        echo "warning: '$name' saved as '$bak'"
    fi
    ln -sv "$f" "$name"
    # Fix permission on link (don't dereference)
    [ -z "$owner" ] || chown -h $owner "$name"
    [ -z "$group" ] || chgrp -h $group "$name"
done
