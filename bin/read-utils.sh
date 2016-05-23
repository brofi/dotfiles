# Collection of functions for reading user input

# Check dependencies
if ! err_ > /dev/null 2>&1; then
    . error-utils.sh
fi

# Reads user input and returns 0 if the user confirms, 1 otherwise.
# $1 question text
# $2 default choice: 0 (yes) or 1 (no)
function read_confirm {
    local suffix

    if [ "$2" -eq 0 ]; then
        suffix="[Y/n]"
    elif [ "$2" -eq 1 ]; then
        suffix="[y/N]"
    else
        # We keep in mind that we can only retun values <= 255 anyway
        err_exit "Default choice must be 0 (yes) or 1 (no)" 1
    fi  

    while : ; do
        read -p "$1 $suffix " yn
        case $yn in
            [Yy]) return 0;; 
            [Nn]) return 1;; 
            *) return $2;;
        esac
    done
}

# $1 question text
# $2 default option
# $3 action on confirm
# $4 action on discard
function read_confirm_do {
    read_confirm "$1" $2 && $3 || $4
}
