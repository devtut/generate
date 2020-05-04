---
metaTitle: "Conditional Expressions"
description: "File type tests, String comparison and matching, Test on exit status of a command, File comparison, File access tests, Numerical comparisons, One liner test"
---

# Conditional Expressions



## File type tests


The `-e` conditional operator tests whether a file exists (including all file types: directories, etc.).

```bash
if [[ -e $filename ]]; then
  echo "$filename exists"
fi

```

There are tests for specific file types as well.

```bash
if [[ -f $filename ]]; then
  echo "$filename is a regular file"
elif [[ -d $filename ]]; then
  echo "$filename is a directory"
elif [[ -p $filename ]]; then
  echo "$filename is a named pipe"
elif [[ -S $filename ]]; then
  echo "$filename is a named socket"
elif [[ -b $filename ]]; then
  echo "$filename is a block device"
elif [[ -c $filename ]]; then
  echo "$filename is a character device"
fi
if [[ -L $filename ]]; then
  echo "$filename is a symbolic link (to any file type)"
fi

```

For a symbolic link, apart from `-L`, these tests apply to the target, and return false for a broken link.

```bash
if [[ -L $filename || -e $filename ]]; then
  echo "$filename exists (but may be a broken symbolic link)"
fi

if [[ -L $filename && ! -e $filename ]]; then
  echo "$filename is a broken symbolic link"
fi

```



## String comparison and matching


String comparison uses the `==` operator between **quoted** strings. The `!=` operator negates the comparison.

```bash
if [[ "$string1" == "$string2" ]]; then
  echo "\$string1 and \$string2 are identical"
fi
if [[ "$string1" != "$string2" ]]; then
  echo "\$string1 and \$string2 are not identical"
fi

```

If the right-hand side is not quoted then it is a wildcard pattern that `$string1` is matched against.

```bash
string='abc'
pattern1='a*'
pattern2='x*'
if [[ "$string" == $pattern1 ]]; then
  # the test is true
  echo "The string $string matches the pattern $pattern"
fi
if [[ "$string" != $pattern2 ]]; then
  # the test is false
  echo "The string $string does not match the pattern $pattern"
fi

```

The `<` and `>` operators compare the strings in lexicographic order (there are no less-or-equal or greater-or-equal operators for strings).

There are unary tests for the empty string.

```bash
if [[ -n "$string" ]]; then
  echo "$string is non-empty"
fi
if [[ -z "${string// }" ]]; then
  echo "$string is empty or contains only spaces"
fi
if [[ -z "$string" ]]; then
  echo "$string is empty"
fi

```

Above, the `-z` check may mean `$string` is unset, or it is set to an empty string. To distinguish between empty and unset, use:

```bash
if [[ -n "${string+x}" ]]; then
    echo "$string is set, possibly to the empty string"
fi
if [[ -n "${string-x}" ]]; then
    echo "$string is either unset or set to a non-empty string"
fi
if [[ -z "${string+x}" ]]; then
    echo "$string is unset"
fi
if [[ -z "${string-x}" ]]; then
    echo "$string is set to an empty string"
fi

```

where `x` is arbitrary. Or in [table form](http://serverfault.com/questions/7503/how-to-determine-if-a-bash-variable-is-empty):

```

                       +-------+-------+-----------+
            $string is: | unset | empty | non-empty |
+-----------------------+-------+-------+-----------+
| [[ -z ${string} ]]    | true  | true  | false     |
| [[ -z ${string+x} ]]  | true  | false | false     |
| [[ -z ${string-x} ]]  | false | true  | false     |
| [[ -n ${string} ]]    | false | false | true      |
| [[ -n ${string+x} ]]  | false | true  | true      |
| [[ -n ${string-x} ]]  | true  | false | true      |
+-----------------------+-------+-------+-----------+

```

[Alternatively](http://unix.stackexchange.com/a/147362), the state can be checked in a case statement:

```bash
case ${var+x$var} in
  (x) echo empty;;
  ("") echo unset;;
  (x*[![:blank:]]*) echo non-blank;;
  (*) echo blank
esac

```

Where `[:blank:]` is locale specific horizontal spacing characters (tab, space, etc).



## Test on exit status of a command


Exit status 0: success<br>
Exit status other than 0: failure

To test on the exit status of a command:

```bash
if command;then
    echo 'success'
else
    echo 'failure'
fi

```



## File comparison


```bash
if [[ $file1 -ef $file2 ]]; then
  echo "$file1 and $file2 are the same file"
fi

```

“Same file” means that modifying one of the files in place affects the other. Two files can be the same even if they have different names, for example if they are hard links, or if they are symbolic links with the same target, or if one is a symbolic link pointing to the other.

If two files have the same content, but they are distinct files (so that modifying one does not affect the other), then `-ef` reports them as different. If you want to compare two files byte by byte, use the `cmp` utility.

```bash
if cmp -s -- "$file1" "$file2"; then
  echo "$file1 and $file2 have identical contents"
else
  echo "$file1 and $file2 differ"
fi

```

To produce a human-readable list of differences between text files, use the `diff` utility.

```bash
if diff -u "$file1" "$file2"; then
  echo "$file1 and $file2 have identical contents"
else
  : # the differences between the files have been listed
fi

```



## File access tests


```bash
if [[ -r $filename ]]; then
  echo "$filename is a readable file"
fi
if [[ -w $filename ]]; then
  echo "$filename is a writable file"
fi
if [[ -x $filename ]]; then
  echo "$filename is an executable file"
fi

```

These tests take permissions and ownership into account to determine whether the script (or programs launched from the script) can access the file.

Beware of [race conditions (TOCTOU)](https://en.wikipedia.org/wiki/Time_of_check_to_time_of_use): just because the test succeeds now doesn't mean that it's still valid on the next line. It's usually better to try to access a file, and handle the error, rather than test first and then have to handle the error anyway in case the file has changed in the meantime.



## Numerical comparisons


Numerical comparisons use the `-eq` operators and friends

```bash
if [[ $num1 -eq $num2 ]]; then
  echo "$num1 == $num2"
fi
if [[ $num1 -le $num2 ]]; then
  echo "$num1 <= $num2"
fi

```

There are six numeric operators:

- `-eq` equal
- `-ne` not equal
- `-le` less or equal
- `-lt` less than
- `-ge` greater or equal
- `-gt` greater than

Note that the `<` and `>` operators inside `[[ … ]]` compare strings, not numbers.

```bash
if [[ 9 -lt 10 ]]; then
  echo "9 is before 10 in numeric order"
fi
if [[ 9 > 10 ]]; then
  echo "9 is after 10 in lexicographic order"
fi

```

The two sides must be numbers written in decimal (or in octal with a leading zero). Alternatively, use the `((…))` arithmetic expression syntax, which performs **integer** calculations in a C/Java/…-like syntax.

```bash
x=2
if ((2*x == 4)); then
  echo "2 times 2 is 4"
fi
((x += 1))
echo "2 plus 1 is $x"

```



## One liner test


You can do things like this:

```bash
[[ $s = 'something' ]] && echo 'matched' || echo "didn't match"
[[ $s == 'something' ]] && echo 'matched' || echo "didn't match"
[[ $s != 'something' ]] && echo "didn't match" || echo "matched"
[[ $s -eq 10 ]] && echo 'equal' || echo "not equal"
(( $s == 10 )) && echo 'equal' || echo 'not equal'

```

One liner test for exit status:

```bash
command && echo 'exited with 0' || echo 'non 0 exit'
cmd && cmd1 && echo 'previous cmds were successful' || echo 'one of them failed'
cmd || cmd1 #If cmd fails try cmd1

```



#### Syntax


- [[ -OP $filename ]]
- [[ $file1 -OP $file2 ]]
- [[ -z $string ]]
- [[ -n $string ]]
- [[ "$string1" == "$string2" ]]
- [[ "$string1" == $pattern ]]



#### Remarks


The `[[ … ]]` syntax surrounds bash built-in conditional expressions. Note that spaces are required on either side of the brackets.

Conditional expressions can use unary and binary operators to test properties of strings, integers and files. They can also use the logical operators `&&`, `||` and `!`.

