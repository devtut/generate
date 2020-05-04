---
metaTitle: "When to use eval"
description: "Using Eval, Using Eval with Getopt"
---

# When to use eval


First and foremost: know what you're doing!  Secondly, while you should avoid using `eval`, if its use makes for cleaner code, go ahead.



## Using Eval


For example, consider the following that sets the contents of `$@` to the contents of a given variable:

```bash
a=(1 2 3)
eval set -- "${a[@]}"

```

This code is often accompanied by `getopt` or `getopts` to set `$@` to the output of the aforementioned option parsers, however, you can also use it to create a simple `pop` function that can operate on variables silently and directly without having to store the result to the original variable:

```bash
isnum()
{
    # is argument an integer?
    local re='^[0-9]+$'
    if [[ -n $1 ]]; then
        [[ $1 =~ $re ]] && return 0
        return 1
    else
        return 2
    fi
}

isvar()
{
    if isnum "$1"; then
        return 1
    fi
    local arr="$(eval eval -- echo -n "\$$1")"
    if [[ -n ${arr[@]} ]]; then
        return 0
    fi
    return 1
}

pop()
{
    if [[ -z $@ ]]; then
        return 1
    fi

    local var=
    local isvar=0
    local arr=()
    
    if isvar "$1"; then # let's check to see if this is a variable or just a bare array
        var="$1"
        isvar=1
        arr=($(eval eval -- echo -n "\${$1[@]}")) # if it is a var, get its contents
    else
        arr=($@)
    fi
    
    # we need to reverse the contents of $@ so that we can shift
    # the last element into nothingness
    arr=($(awk <<<"${arr[@]}" '{ for (i=NF; i>1; --i) printf("%s ",$i); print $1; }'

    # set $@ to ${arr[@]} so that we can run shift against it.
    eval set -- "${arr[@]}"
    
    shift # remove the last element
    
    # put the array back to its original order
    arr=($(awk <<<"$@" '{ for (i=NF; i>1; --i) printf("%s ",$i); print $1; }'
    
    # echo the contents for the benefit of users and for bare arrays
    echo "${arr[@]}"

    if ((isvar)); then
        # set the contents of the original var to the new modified array
        eval -- "$var=(${arr[@]})" 
    fi
}

```



## Using Eval with Getopt


While eval may not be needed for a `pop` like function, it is however required whenever you use `getopt`:

Consider the following function that accepts `-h` as an option:

```bash
f()
{
    local __me__="${FUNCNAME[0]}"
    local argv="$(getopt -o 'h' -n $__me__ -- "$@")"

    eval set -- "$argv"
    
    while :; do
        case "$1" in
            -h)
              echo "LOLOLOLOL"
              return 0
              ;;
            --)
              shift
              break
              ;;
    done
    
    echo "$@"
}

```

Without `eval` `set -- "$argv"` generates `-h --` instead of the desired `(-h --)` and subsequently enters an infinite loop because `-h --` doesn't match `--` or `-h`.

