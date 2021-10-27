---
metaTitle: "Bash - Scripting with Parameters"
description: "Multiple Parameter Parsing, Accessing Parameters, Argument parsing using a for loop, Wrapper script, Split string into an array in Bash"
---

# Scripting with Parameters

## Multiple Parameter Parsing

To parse lots of parameters, the prefered way of doing this is using a **while** loop, a **case** statement, and **shift**.

`shift` is used to pop the first parameter in the series, making what used to be **\$2**, now be **\$1**. This is useful for processing arguments one at a time.

```bash
#!/bin/bash

# Load the user defined parameters
while [[ $# > 0 ]]
do
    case "$1" in

        -a|--valueA)
            valA="$2"
            shift
            ;;

        -b|--valueB)
            valB="$2"
            shift
            ;;

        --help|*)
            echo "Usage:"
            echo "    --valueA \"value\""
            echo "    --valueB \"value\""
            echo "    --help"
            exit 1
            ;;
    esac
    shift
done

echo "A: $valA"
echo "B: $valB"

```

Inputs and Outputs

```bash
$ ./multipleParams.sh --help
Usage:
    --valueA "value"
    --valueB "value"
    --help

$ ./multipleParams.sh
A:
B:

$ ./multipleParams.sh --valueB 2
A:
B: 2

$ ./multipleParams.sh --valueB 2 --valueA "hello world"
A: hello world
B: 2

```

## Accessing Parameters

When executing a Bash script, parameters passed into the script are named in accordance to their position: `$1` is the name of the first parameter, `$2` is the name of the second parameter, and so on.

A missing parameter simply evaluates to an empty string. Checking for the existence of a parameter can be done as follows:

```bash
if [ -z "$1" ]; then
    echo "No argument supplied"
fi

```

### Getting all the parameters

`$@` and `$*` are ways of interacting with all the script parameters. Referencing [the Bash man page](http://linux.die.net/man/1/bash), we see that:

- `$*`: Expands to the positional parameters, starting from one. When the expansion occurs within double quotes, it expands to a single word with the value of each parameter separated by the first character of the IFS special variable.
- `$@`: Expands to the positional parameters, starting from one. When the expansion occurs within double quotes, each parameter expands to a separate word.

### Getting the number of parameters

`$#` gets the number of parameters passed into a script. A typical use case would be to check if the appropriate number of arguments are passed:

```bash
if [ $# -eq 0 ]; then
    echo "No arguments supplied"
fi

```

### Example 1

Loop through all arguments and check if they are files:

```bash
for item in "$@"
do
    if [[ -f $item ]]; then
        echo "$item is a file"
    fi
done

```

### Example 2

Loop through all arguments and check if they are files:

```bash
for (( i = 1; i <= $#; ++ i ))
do
    item=${@:$i:1}

    if [[ -f $item ]]; then
        echo "$item is a file"
    fi
done

```

## Argument parsing using a for loop

A simple example which provides the options:

| Opt        | Alt. Opt          | Details                                              |
| ---------- | ----------------- | ---------------------------------------------------- |
| `-h`       | `--help`          | Show help                                            |
| `-v`       | `--version`       | Show version info                                    |
| `-dr path` | `--doc-root path` | An option which takes a secondary parameter (a path) |
| `-i`       | `--install`       | A boolean option (true/false)                        |
| `-*`       | --                | Invalid option                                       |

```bash
#!/bin/bash
dr=''
install=false

skip=false
for op in "$@";do
    if $skip;then skip=false;continue;fi
    case "$op" in
        -v|--version)
            echo "$ver_info"
            shift
            exit 0
            ;;
        -h|--help)
            echo "$help"
            shift
            exit 0
            ;;
        -dr|--doc-root)
            shift
            if [[ "$1" != "" ]]; then
                dr="${1/%\//}"
                shift
                skip=true
            else
                echo "E: Arg missing for -dr option"
                exit 1
            fi
            ;;
        -i|--install)
            install=true
            shift
            ;;
        -*)
            echo "E: Invalid option: $1"
            shift
            exit 1
            ;;
    esac
done

```

## Wrapper script

Wrapper script is a script that wraps another script or command to provide extra functionalities or just to make something less tedious.

For example, the actual `egrep` in new GNU/Linux system is being replaced by a wrapper script named `egrep`. This is how it looks:

```bash
#!/bin/sh
exec grep -E "$@"

```

So, when you run `egrep` in such systems, you are actually running `grep -E` with all the arguments forwarded.

In general case, if you want to run an example script/command `exmp` with another script `mexmp` then the wrapper `mexmp` script will look like:

```bash
#!/bin/sh
exmp "$@" # Add other options before "$@"
# or
#full/path/to/exmp "$@"

```

## Split string into an array in Bash

Let's say we have a String parameter and we want to split it by comma

```bash
my_param="foo,bar,bash"

```

To split this string by comma we can use;

```bash
IFS=',' read -r -a array <<< "$my_param"

```

Here, IFS is a special variable called [Internal field separator](https://en.wikipedia.org/wiki/Internal_field_separator) which defines the character or characters used to separate a pattern into tokens for some operations.

To access an individual element:

```bash
echo "${array[0]}"

```

To iterate over the elements:

```bash
for element in "${array[@]}"
do
    echo "$element"
done

```

To get both the index and the value:

```bash
for index in "${!array[@]}"
do
    echo "$index ${array[index]}"
done

```

#### Remarks

- `shift` shifts the positional parameters to the left so that `$2` becomes `$1`, `$3` becomes `$2` and so forth.
- `"$@"` is an array of all the positional parameters passed to the script/function.
- `"$*"` is an string composed of all the positional parameters passed to the script/function.
