# Collection of useful array functions

# Just true
function array_ { :;}

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

# Applied to a predicate P and an array A:
# Returns the longest prefix (possibly empty) of A of elements that satisfy P.
# $1 predicate
# $2 array by name
function array_take_while {
    local predicate=$1
    local tmp=$2[@]
    local array=("${!tmp}")

    local idx=${#array[@]}
    for i in "${!array[@]}"; do
        if ! $predicate "${array[$i]}"; then
            idx=$i
            break
        fi
    done

    eval "$2"'=("${array[@]:0:'"$idx"'}")'
}

# True if $1 occurs in $2
# $1 string or number
# $2 array
function array_elem {
    for elem in "${@:2}"; do
        [ "$elem" == "$1" ] && return 0
    done
    return 1
}

# Returns the index of the first occurrence of $1 in $2,
# or nothing if not found.
# $1 string or number
# $2 array
function array_first_index_of {
    local array=("${@:2}")
    for i in "${!array[@]}"; do
        [ "${array[$i]}" == "$1" ] && echo $i
    done
}
