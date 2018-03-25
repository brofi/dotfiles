#!/bin/bash
# shellcheck disable=SC2207
# cc.sh: Utility to change color values of all xpm files in the current folder.

# shellcheck source=/dev/null
. ~/lib/error-utils.sh

# True, if $1 is a RGB hex value.
function is_color {
    printf "%s" "$1" | grep -q '^#[[:xdigit:]]\{6\}$'
}

# Reads current color values from xpm files.
function read_colors {
    _IFS=$IFS; IFS=$'\n'
    colors=($(grep -Eoh '#[[:xdigit:]]{6}' ./*.xpm | sort | uniq))
    IFS=$_IFS
}

# Prints main menu.
function print_colors {
    printf '\nChoose color to change:\n\n'
    for i in "${!colors[@]}"; do
        printf '%s) %s (%s)\n' "$i" "${colors[$i]}" "https://www.google.com/search?q=%23${colors[$i]:1}"
    done
    printf 'q) Exit\n\n'
}

read_colors
print_colors
max_idx=$((${#colors[@]} - 1))
while :; do
    read -rp "> " idx
    case $idx in
        [0-"$max_idx"])
            printf '\nEnter new color value:\n'
            while :; do
                read -rp "> " new_color
                if is_color "$new_color"; then
                    break
                else
                    err_print "'$new_color' is not a color value."
                fi
            done

            # change to new color
            for f in ./*.xpm; do
                sed -i "s/${colors[$idx]}/$new_color/" "$f"
            done

            read_colors
            print_colors
            ;;
        "") ;;
        q) exit 0;;
        *) err_print "Selection '$idx' does not exist.";;
    esac
done
