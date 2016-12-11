#!/bin/bash

__dir=$(dirname "${BASH_SOURCE[0]}")

. "$__dir/../lib/array-utils.sh"
. "$__dir/../lib/number-utils.sh"

_IFS=$IFS; IFS=$'\n'
# Get IDs of all slave pointers
slave_pointer_ids=($(xinput --list | grep 'slave \+pointer' | cut -f2 | cut -d= -f2))
IFS=$_IFS

# Get xinput device name for given id.
# $1 device id
function get_device_name {
    xinput --list --name-only $1
}

# Get pointer from user
while true; do
    echo
    for id in "${slave_pointer_ids[@]}"; do
        printf '%s: %s\n' "$id" "$(get_device_name $id)"
    done
    read -p "Select pointer id: " pointer_id
    if array_elem "$pointer_id" "${slave_pointer_ids[@]}"; then
        break
    elif [ -n "$pointer_id" ]; then
        echo "Pointer '$pointer_id' does not exist." 1>&2
    fi
done

_IFS=$IFS; IFS=$'\n'
# Get all acceleration properties for selected pointer
accel_props=($(xinput --list-props $pointer_id | grep Accel | sed 's/^[[:space:]]*//'))
IFS=$_IFS

# Split properties in name, id and value
for i in "${!accel_props[@]}"; do
    # Format: Profile Name (ProfileId): value
    prop=${accel_props[$i]}
    accel_prop_names[$i]=${prop%% (*}
    accel_prop_ids[$i]=$(echo $prop | cut -d "(" -f2 | cut -d ")" -f1)
    accel_prop_values[$i]=${prop##*:	} # ctrl+V TAB
done

# Get property from user
while true; do
    echo
    for i in "${!accel_prop_names[@]}"; do
        printf '%s: %s\n' "${accel_prop_ids[$i]}" "${accel_prop_names[$i]}"
    done
    read -p "Select property id for '$(get_device_name $pointer_id)': " property_id
    idx=$(array_first_index_of $property_id "${accel_prop_ids[@]}")
    if [ -n "$idx" ]; then
        property_name=${accel_prop_names[$idx]}
        property_value=${accel_prop_values[$idx]}
        break
    elif [ -n "$property_id" ]; then
        echo "Pointer '$property_id' does not exist." 1>&2
    fi
done

cmd_noval="xinput --set-prop $pointer_id $property_id"

# Get new property value from user
while true; do
    echo
    read -p "Set $property_name (current: $property_value): " value
    if number_is_int $value || number_is_float $value; then
        if $cmd_noval $value > /dev/null; then
            echo "$property_name set to $value" && break
        fi
    elif [ -n "$value" ]; then
        echo "'$value' is not a (floating point) number." 1>&2
    fi
done

# Create mouse config for use with .xinitrc if not existent
[ ! -f ~/.mouse ] && echo "#!/bin/sh" > ~/.mouse && chmod u+x ~/.mouse

# Replace value if property is already configured,
# else merge the new property with the already sorted mouse config
grep -q "$cmd_noval" ~/.mouse \
    && sed -i "s/\($cmd_noval\).*/\1 $value/" ~/.mouse \
    || echo $cmd_noval $value | sort -o ~/.mouse -m - ~/.mouse
