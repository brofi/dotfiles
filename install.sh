#!/bin/bash

# Empty array if no match
shopt -s nullglob

# full directory name of this script, no matter from where it's being called
this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

echo 'Ensuring directory structure...'
# Dirs without children (only links to parent and itself)
# Ingore .git directory
directories=$(find -type d -links 2 -not -path './.git/*')
for d in $directories; do
    mkdir -p $HOME/${d#*/}
done

echo 'Setting up symlinks...'
# Get file names, ignore this file and README.md
# Ingore files in .git directory
dotfiles=$(find -type f \( ! -name ${0#*/} ! -name README.md \) -not -path './.git/*')
for f in $dotfiles; do
    target=$dir/${f#*/}
    name=$HOME/${f#*/}
    # Save existing files
    if [ -f $name ]; then
        bak=${name}.dotsave
        while [ -f $bak ]; do
            bak=${bak}.dotsave
        done
        mv $name $bak
        echo "warning: $name saved as $bak"
    fi
    ln -sv $target $name
done
