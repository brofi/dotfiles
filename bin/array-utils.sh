# Collection of useful array functions

# Appends given suffix to each element of given array
# $1 suffix
# $2 array by name
function array_suffix {
    local suffix=$1

    local tmp=$2[@]
    local array=("${!tmp}")

    local suffixed_array=("${array[@]/%/$1}")

    eval "$2"'=("${suffixed_array[@]}")'
}

# Returns the given array with those elements that satisfy the predicate
# $1 predicate
# $2 array by name
function array_filter {
    local predicate=$1

    # We could've used
    # local -n array="$2"
    # without the next two statements and without eval at the end.
    # However local -n is bash >= 4.3
    local tmp=$2[@]
    local array=("${!tmp}")

    for i in "${!array[@]}"; do
        if ! $predicate "${array[$i]}"; then 
            unset array[$i]
        fi   
    done 
    eval "$2"'=("${array[@]}")'
}
