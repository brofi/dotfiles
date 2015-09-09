#!/bin/bash

# Empty array if no match
shopt -s nullglob

# full directory name of this script, no matter from where it's being called
this_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

echo 'Ensuring directory structure...'
# Dirs without children (only links to parent and itself)
# Ingore .git directory
directories=$(find $this_dir -type d -links 2 -not -path "$this_dir/.git/*")
for d in $directories; do
    mkdir -p $HOME/${d#$this_dir/}
done

echo 'Setting up symlinks...'
# Get file names, ignore this file and README.md
# Ingore files in .git directory
dotfiles=$(find $this_dir -type f \( ! -name ${0#$this_dir/} ! -name README.md \) -not -path "$this_dir/.git/*")
for f in $dotfiles; do
    name=$HOME/${f#$this_dir/}
    # Save existing files
    if [ -f $name ]; then
        bak=${name}.dotsave
        while [ -f $bak ]; do
            bak=${bak}.dotsave
        done
        mv $name $bak
        echo "warning: $name saved as $bak"
    fi
    ln -sv $f $name
done
