#!/bin/bash
# shellcheck disable=SC1090

__dir=$(dirname "${BASH_SOURCE[0]}")

. "$__dir/../lib/array-utils.sh"
. "$__dir/../lib/number-utils.sh"
. "$__dir/../lib/error-utils.sh"

_IFS=$IFS; IFS=$'\n'
# Get IDs of all slave pointers
slave_pointer_ids=($(xinput --list | grep 'slave \+pointer' | cut -f2 | cut -d= -f2))
IFS=$_IFS

# Get xinput device name for given id.
# $1 device id
function get_device_name {
    xinput --list --name-only "$1"
}

# Get pointer from user
while true; do
    echo
    for id in "${slave_pointer_ids[@]}"; do
        printf '%s: %s\n' "$id" "$(get_device_name "$id")"
    done
    printf '\n'
    read -rp 'Select pointer id: ' pointer_id
    if array_elem "$pointer_id" "${slave_pointer_ids[@]}"; then
        break
    elif [ -n "$pointer_id" ]; then
        err_print "Pointer '$pointer_id' does not exist."
    fi
done

# Get all acceleration and scrolling properties for selected pointer
_IFS=$IFS; IFS=$'\n'
mouse_props=($(xinput --list-props "$pointer_id" | grep 'Accel\|Scroll' | sed 's/^[[:space:]]*//'))
IFS=$_IFS

# Split properties in name, id and value
for i in "${!mouse_props[@]}"; do
    # Format: Profile Name (ProfileId): value
    prop=${mouse_props[$i]}
    mouse_prop_names[$i]=${prop%% (*}
    mouse_prop_ids[$i]=$(echo "$prop" | cut -d "(" -f2 | cut -d ")" -f1)
    mouse_prop_values[$i]=${prop##*:	} # ctrl+V TAB
done

# Get property from user
is_shortcut=false
while true; do
    echo
    for i in "${!mouse_prop_names[@]}"; do
        printf '%s: %s\n' "${mouse_prop_ids[$i]}" "${mouse_prop_names[$i]}"
    done

    # shortcuts
    printf '\n'
    printf 'a: Disable mouse acceleration\n'
    printf 'b: Enable middle button scrolling\n'
    printf '\n'

    read -rp "Select property id or shortcut for '$(get_device_name "$pointer_id")': " ps_choice
    idx=$(array_first_index_of "$ps_choice" "${mouse_prop_ids[@]}")
    if [ -n "$idx" ]; then
        property_names=("${mouse_prop_names[$idx]}")
        property_value=${mouse_prop_values[$idx]}
        break
    elif [ "$ps_choice" = 'a' ]; then
        # Disable mouse acceleration
        property_names=('libinput Accel Speed' 'libinput Accel Profile Enabled')
        new_values=('0' '0, 1')
        is_shortcut=true
        break
    elif [ "$ps_choice" = 'b' ]; then
        # Enable middle button scrolling
        # none: 0, 0, 0
        # two-finger: 1, 0, 0
        # edge: 0, 1, 0
        # button: 0, 0, 1
        property_names=('libinput Scroll Method Enabled')
        new_values=('0, 0, 1')
        is_shortcut=true
        break
    elif [ -n "$ps_choice" ]; then
        err_print "Pointer '$ps_choice' does not exist."
    fi
done


if [ "$is_shortcut" = false ]; then
    # Get new property value from user
    while true; do
        echo
        read -rp "Set ${property_names[0]} (current: $property_value): " new_value
        if [ -z "$new_value" ]; then
            continue
        else
            if number_is_int "$property_value" || number_is_float "$property_value"; then
                if ! number_is_int "$new_value" || ! number_is_float "$new_value"; then
                    err_print "'$new_value' is not a (floating point) number."
                else
                    break
                fi
            else
                # We assume the value has to be a list of numbers at this point
                # (no xinput string values supported).
                read -ra curr_value_array <<< "$property_value"
                read -ra new_value_array <<< "$new_value"
                if [ ${#curr_value_array[@]} -ne ${#new_value_array[@]} ]; then
                    err_print "Wrong number of arguments."
                else
                    is_number_list=true
                    for v in "${new_value_array[@]}"; do
                        if ! number_is_int "${v%,}" || ! number_is_float "${v%,}"; then
                            err_print "'${v%,}' is not a (floating point) number."
                            is_number_list=false
                        fi
                    done
                    [ "$is_number_list" = true ] && break
                fi
            fi
        fi
    done
    new_values=("$new_value")
fi

# Create mouse config for use with .xinitrc if not existent
[ ! -f ~/.mouse ] && echo "#!/bin/sh" > ~/.mouse && chmod u+x ~/.mouse

for i in "${!property_names[@]}"; do
    if xinput --set-prop "$pointer_id" "${property_names[$i]}" ${new_values[$i]} > /dev/null 2>&1; then
        # Replace value if property is already configured, else merge the new
        # property with the already sorted mouse config
        grep -q "xinput --set-prop $pointer_id '${property_names[$i]}'" ~/.mouse \
            && sed -i "s/\(xinput --set-prop $pointer_id '${property_names[$i]}'\).*/\1 ${new_values[$i]}/" ~/.mouse \
            || printf "xinput --set-prop %s '%s' %s\n" "$pointer_id" "${property_names[$i]}" "${new_values[$i]}" \
                | sort -o ~/.mouse -m - ~/.mouse
        echo "'${property_names[$i]}' set to ${new_values[$i]}"
    else
        err_print "Failed to set value ${new_values[$i]} to property '${property_names[$i]}'"
    fi
done
