#!/bin/bash

# This file's name
__self="$(basename "${BASH_SOURCE[0]}")"
# Full path of this file's directory
__dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# By default links are installed in user's home directory
install_dir=$HOME

# Parse key value pairs
while [[ $# > 1 ]]; do
    key="$1"
    case $key in
        -C)
            install_dir="$2"
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
done
