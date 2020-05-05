---
metaTitle: "Bash - Bash Parameter Expansion"
description: "Modifying the case of alphabetic characters, Length of parameter, Replace pattern in string, Substrings and subarrays, Delete a pattern from the beginning of a string, Parameter indirection, Default value substitution, Delete a pattern from the end of a string, Parameter expansion and filenames, Error if variable is empty or unset, Munging during expansion"
---

# Bash Parameter Expansion


The `$` character introduces parameter expansion, command substitution, or arithmetic expansion. The parameter name or symbol to be expanded may be enclosed in braces, which are optional but serve to protect the variable to be expanded from characters immediately following it which could be interpreted as part of the name.

Read more in the [Bash User Manual](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html).



## Modifying the case of alphabetic characters


To uppercase

```bash
$ v="hello"
# Just the first character
$ printf '%s\n' "${v^}"
Hello
# All characters
$ printf '%s\n' "${v^^}"
HELLO
# Alternative
$ v="hello world"
$ declare -u string="$v"
$ echo "$string"
HELLO WORLD

```

To lowercase

```bash
$ v="BYE"
# Just the first character
$ printf '%s\n' "${v,}"
bYE
# All characters
$ printf '%s\n' "${v,,}"
bye
# Alternative
$ v="HELLO WORLD"
$ declare -l string="$v"
$ echo "$string"
hello world

```

Toggle Case

```bash
$ v="Hello World"
# All chars
$ echo "${v~~}"
hELLO wORLD
$ echo "${v~}"
# Just the first char
hello World

```



## Length of parameter


```bash
# Length of a string
$ var='12345'
$ echo "${#var}"
5

```

Note that it's the length in number of **characters** which is not necessarily the same as the number of **bytes** (like in UTF-8 where most characters are encoded in more than one byte), nor the number of **glyphs/graphemes** (some of which are combinations of characters), nor is it necessarily the same as the display width.

```bash
# Number of array elements
$ myarr=(1 2 3)
$ echo "${#myarr[@]}"
3

# Works for positional parameters as well
$ set -- 1 2 3 4
$ echo "${#@}"
4

# But more commonly (and portably to other shells), one would use
$ echo "$#"
4

```



## Replace pattern in string


First match:

```bash
$ a='I am a string'
$ echo "${a/a/A}"
I Am a string

```

All matches:

```bash
$ echo "${a//a/A}"
I Am A string

```

Match at the beginning:

```bash
$ echo "${a/#I/y}"
y am a string

```

Match at the end:

```bash
$ echo "${a/%g/N}"
I am a strinN

```

Replace a pattern with nothing:

```bash
$ echo "${a/g/}"
I am a strin

```

Add prefix to array items:

```bash
$ A=(hello world)
$ echo "${A[@]/#/R}"
Rhello Rworld

```



## Substrings and subarrays


```bash
var='0123456789abcdef'

# Define a zero-based offset
$ printf '%s\n' "${var:3}"
3456789abcdef

# Offset and length of substring
$ printf '%s\n' "${var:3:4}"
3456

```

```bash
# Negative length counts from the end of the string
$ printf '%s\n' "${var:3:-5}"
3456789a

# Negative offset counts from the end
# Needs a space to avoid confusion with ${var:-6}
$ printf '%s\n' "${var: -6}"
abcdef

# Alternative: parentheses
$ printf '%s\n' "${var:(-6)}"
abcdef

# Negative offset and negative length
$ printf '%s\n' "${var: -6:-5}"
a

```

The same expansions apply if the parameter is a **positional parameter** or the **element of a subscripted array**:

```bash
# Set positional parameter $1
set -- 0123456789abcdef

# Define offset
$ printf '%s\n' "${1:5}"
56789abcdef

# Assign to array element
myarr[0]='0123456789abcdef'

# Define offset and length
$ printf '%s\n' "${myarr[0]:7:3}"
789

```

Analogous expansions apply to **positional parameters**, where offsets are one-based:

```bash
# Set positional parameters $1, $2, ...
$ set -- 1 2 3 4 5 6 7 8 9 0 a b c d e f

# Define an offset (beware $0 (not a positional parameter)
# is being considered here as well)
$ printf '%s\n' "${@:10}"
0
a
b 
c
d
e
f

# Define an offset and a length
$ printf '%s\n' "${@:10:3}"
0
a
b

# No negative lengths allowed for positional parameters
$ printf '%s\n' "${@:10:-2}"
bash: -2: substring expression < 0

# Negative offset counts from the end
# Needs a space to avoid confusion with ${@:-10:2}
$ printf '%s\n' "${@: -10:2}"
7
8

# ${@:0} is $0 which is not otherwise a positional parameters or part
# of $@
$ printf '%s\n' "${@:0:2}"
/usr/bin/bash
1

```

Substring expansion can be used with **indexed arrays**:

```bash
# Create array (zero-based indices)
$ myarr=(0 1 2 3 4 5 6 7 8 9 a b c d e f)

# Elements with index 5 and higher
$ printf '%s\n' "${myarr[@]:12}"
c
d
e
f

# 3 elements, starting with index 5
$ printf '%s\n' "${myarr[@]:5:3}"
5
6
7

# The last element of the array
$ printf '%s\n' "${myarr[@]: -1}"
f

```



## Delete a pattern from the beginning of a string


Shortest match:

```bash
$ a='I am a string'
$ echo "${a#*a}"
m a string

```

Longest match:

```bash
$ echo "${a##*a}"
 string

```



## Parameter indirection


`Bash` indirection permits to get the value of a variable whose name is contained in another variable. Variables example:

```bash
$ red="the color red"
$ green="the color green"

$ color=red
$ echo "${!color}"
the color red
$ color=green
$ echo "${!color}"
the color green

```

Some more examples that demonstrate the indirect expansion usage:

```

$ foo=10
 $ x=foo
 $ echo ${x}      #Classic variable print  
 foo  
 
 $ foo=10
 $ x=foo
 $ echo ${!x}     #Indirect expansion
 10

```

One more example:

```bash
$ argtester () { for (( i=1; i<="$#"; i++ )); do echo "${i}";done; }; argtester -ab -cd -ef 
1   #i expanded to 1 
2   #i expanded to 2
3   #i expanded to 3

$ argtester () { for (( i=1; i<="$#"; i++ )); do echo "${!i}";done; }; argtester -ab -cd -ef 
-ab     # i=1 --> expanded to $1 ---> expanded to first argument sent to function
-cd     # i=2 --> expanded to $2 ---> expanded to second argument sent to function
-ef     # i=3 --> expanded to $3 ---> expanded to third argument sent to function

```



## Default value substitution


> 

```bash
${parameter:-word}

```


<p>If parameter is unset or null, the expansion of word is substituted.
Otherwise, the value of parameter is substituted.</p>


```bash
$ unset var
$ echo "${var:-XX}"     # Parameter is unset -> expansion XX occurs
XX
$ var=""                # Parameter is null -> expansion XX occurs
$ echo "${var:-XX}"
XX
$ var=23                # Parameter is not null -> original expansion occurs
$ echo "${var:-XX}"
23

```

> 

```bash
${parameter:=word}

```


<p>If parameter is unset or null, the expansion of word is assigned to
parameter. The value of parameter is then substituted. Positional
parameters and special parameters may not be assigned to in this way.</p>


```bash
$ unset var
$ echo "${var:=XX}"     # Parameter is unset -> word is assigned to XX
XX
$ echo "$var"
XX
$ var=""                # Parameter is null -> word is assigned to XX
$ echo "${var:=XX}"
XX
$ echo "$var"
XX
$ var=23                # Parameter is not null -> no assignment occurs
$ echo "${var:=XX}"
23
$ echo "$var"
23

```



## Delete a pattern from the end of a string


Shortest match:

```bash
$ a='I am a string'
$ echo "${a%a*}"
I am 

```

Longest match:

```bash
$ echo "${a%%a*}"
I 

```



## Parameter expansion and filenames


You can use Bash Parameter Expansion to emulate common filename-processing operations like `basename` and `dirname`.

We will use this as our example path:

```bash
FILENAME="/tmp/example/myfile.txt"

```

To emulate `dirname` and return the directory name of a file path:

```bash
echo "${FILENAME%/*}"
#Out: /tmp/example

```

To emulate `basename $FILENAME` and return the filename of a file path:

```bash
echo "${FILENAME##*/}"
#Out: myfile.txt

```

To emulate `basename $FILENAME .txt` and return the filename without the `.txt.` extension:

```bash
BASENAME="${FILENAME##*/}"
echo "${BASENAME%%.txt}"
#Out: myfile

```



## Error if variable is empty or unset


The semantics for this are similar to that of default value substitution, but instead of substituting a default value, it errors out with the provided error message. The forms are `${VARNAME?ERRMSG}` and `${VARNAME:?ERRMSG}`. The form with `:` will error our if the variable is **unset** **or** **empty**, whereas the form without will only error out if the variable is **unset**. If an error is thrown, the `ERRMSG` is output and the exit code is set to `1`.

```bash
#!/bin/bash
FOO=
# ./script.sh: line 4: FOO: EMPTY
echo "FOO is ${FOO:?EMPTY}"
# FOO is 
echo "FOO is ${FOO?UNSET}"
# ./script.sh: line 8: BAR: EMPTY
echo "BAR is ${BAR:?EMPTY}"
# ./script.sh: line 10: BAR: UNSET
echo "BAR is ${BAR?UNSET}"

```

The run the full example above each of the erroring echo statements needs to be commented out to proceed.



## Munging during expansion


Variables don't necessarily have to expand to their values - substrings can be extracted during expansion, which can be useful for extracting file extensions or parts of paths. Globbing characters keep their usual meanings, so `.*` refers to a literal dot, followed by any sequence of characters; it's not a regular expression.

```bash
$ v=foo-bar-baz
$ echo ${v%%-*}
foo
$ echo ${v%-*}
foo-bar
$ echo ${v##*-}
baz
$ echo ${v#*-}
bar-baz

```

It's also possible to expand a variable using a default value - say I want to invoke the user's editor, but if they've not set one I'd like to give them `vim`.

```bash
$ EDITOR=nano
$ ${EDITOR:-vim} /tmp/some_file
# opens nano
$ unset EDITOR
$ $ ${EDITOR:-vim} /tmp/some_file
# opens vim

```

There are two different ways of performing this expansion, which differ in whether the relevant variable is empty or unset. Using `:-` will use the default if the variable is either unset or empty, whilst `-` only uses the default if the variable is unset, but will use the variable if it is set to the empty string:

```bash
$ a="set"
$ b=""
$ unset c
$ echo ${a:-default_a} ${b:-default_b} ${c:-default_c}
set default_b default_c
$ echo ${a-default_a} ${b-default_b} ${c-default_c}
set default_c

```

Similar to defaults, alternatives can be given; where a default is used if a particular variable isn't available, an alternative is used if the variable is available.

```bash
$ a="set"
$ b=""
$ echo ${a:+alternative_a} ${b:+alternative_b}
alternative_a

```

Noting that these expansions can be nested, using alternatives becomes particularly useful when supplying arguments to command line flags;

```bash
$ output_file=/tmp/foo
$ wget ${output_file:+"-o ${output_file}"} www.stackexchange.com
# expands to wget -o /tmp/foo www.stackexchange.com
$ unset output_file
$ wget ${output_file:+"-o ${output_file}"} www.stackexchange.com
# expands to wget  www.stackexchange.com 

```



#### Syntax


- ${parameter:offset} # Substring starting at offset
- ${parameter:offset:length} # Substring of length "length" starting at offset
- ${#parameter} # Length of parameter
- ${parameter/pattern/string} # Replace the first occurrence of pattern with string
- ${parameter//pattern/string} # Replace all occurrences of pattern with string
- ${parameter/#pattern/string} # Replace pattern with string if pattern is at the beginning
- ${parameter/%pattern/string} # Replace pattern with string if pattern is at the ending
- ${parameter#pattern} # Remove shortest match of pattern from beginning of parameter
- ${parameter##pattern} # Remove longest match of pattern from beginning of  parameter
- ${parameter%pattern} # Remove shortest match of pattern from end of parameter
- ${parameter%%pattern} # Remove longest match of pattern from end of parameter
- ${parameter:-word} # Expand to word if parameter unset/undefined
- ${parameter:=word} # Expand to word if parameter unset/undefined and set parameter
- ${parameter:+word} # Expand to word if parameter set/defined

