# Collection of useful error handling functions

# Just true
function err_ { :;}

# $1 error message to print to stderr
function err_print {
    echo "Error: $1" 1>&2
}

# $1 error message to print to stderr
# $2 exit status
function err_exit {
    if [ "$2" -lt 0 ] || [ "$2" -gt 255 ]; then
        err_exit "Exit value should be between 0 and 255" 1
    fi

    err_print "$1" && exit $2
}
