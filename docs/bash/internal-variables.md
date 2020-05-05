---
metaTitle: "Internal variables"
description: "Bash internal variables at a glance, $@, $#, $FUNCNAME, $HOME, $IFS, $OLDPWD, $PWD, $1 $2 $3 etc..., $*, $!, $?, $$, $HISTSIZE, $BASHPID, $BASH_ENV, $BASH_VERSINFO, $BASH_VERSION, $EDITOR, $HOSTNAME, $HOSTTYPE, $MACHTYPE, $OSTYPE, $PATH, $PPID, $SECONDS, $SHELLOPTS, $_, $RANDOM, $GROUPS, $LINENO, $SHLVL, $UID"
---

# Internal variables

## Bash internal variables at a glance

| Variable         | Details                                                                                                                                                                                                                                                                                                                                                                                             |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `$*` / `$@`      | Function/script positional parameters (arguments). Expand as follows:<br><br>`$*` and `$@` are the same as `$1 $2 ...` (note that it generally makes no sense to leave those unquoted)<br>`"$*"` is the same as `"$1 $2 ..."` <sup>1</sup> <br>`"$@"` is the same as `"$1" "$2" ...` <br> <sub>1. Arguments are separated by the first character of \$IFS, which does not have to be a space.</sub> |
| `$#`             | Number of positional parameters passed to the script or function                                                                                                                                                                                                                                                                                                                                    |
| `$!`             | Process ID of the last (righ-most for pipelines) command in the most recently job put into the background (note that it's not necessarily the same as the job's process group ID when job control is enabled)                                                                                                                                                                                       |
| `$$`             | ID of the process that executed `bash`                                                                                                                                                                                                                                                                                                                                                              |
| `$?`             | Exit status of the last command                                                                                                                                                                                                                                                                                                                                                                     |
| `$n`             | Positional parameters, where n=1, 2, 3, ..., 9                                                                                                                                                                                                                                                                                                                                                      |
| `${n}`           | Positional parameters (same as above), but n can be > 9                                                                                                                                                                                                                                                                                                                                             |
| `$0`             | In scripts, path with which the script was invoked; with `bash -c 'printf "%s\n" "$0"' name args'`: `name` (the first argument after the inline script), otherwise, the `argv[0]` that `bash` received.                                                                                                                                                                                             |
| `$_`             | Last field of the last command                                                                                                                                                                                                                                                                                                                                                                      |
| `$IFS`           | Internal field separator                                                                                                                                                                                                                                                                                                                                                                            |
| `$PATH`          | PATH environment variable used to look-up executables                                                                                                                                                                                                                                                                                                                                               |
| `$OLDPWD`        | Previous working directory                                                                                                                                                                                                                                                                                                                                                                          |
| `$PWD`           | Present working directory                                                                                                                                                                                                                                                                                                                                                                           |
| `$FUNCNAME`      | Array of function names in the execution call stack                                                                                                                                                                                                                                                                                                                                                 |
| `$BASH_SOURCE`   | Array containing source paths for elements in `FUNCNAME` array. Can be used to get the script path.                                                                                                                                                                                                                                                                                                 |
| `$BASH_ALIASES`  | Associative array containing all currently defined aliases                                                                                                                                                                                                                                                                                                                                          |
| `$BASH_REMATCH`  | Array of matches from the last regex match                                                                                                                                                                                                                                                                                                                                                          |
| `$BASH_VERSION`  | Bash version string                                                                                                                                                                                                                                                                                                                                                                                 |
| `$BASH_VERSINFO` | An array of 6 elements with Bash version information                                                                                                                                                                                                                                                                                                                                                |
| `$BASH`          | Absolute path to the currently executing Bash shell itself (heuristically determined by `bash` based on `argv[0]` and the value of `$PATH`; may be wrong in corner cases)                                                                                                                                                                                                                           |
| `$BASH_SUBSHELL` | Bash subshell level                                                                                                                                                                                                                                                                                                                                                                                 |
| `$UID`           | Real (not effective if different) User ID of the process running `bash`                                                                                                                                                                                                                                                                                                                             |
| `$PS1`           | Primary command line prompt; see [Using the PS\* Variables](http://stackoverflow.com/documentation/bash/7541/handling-the-system-prompt#t=201610212316553697922)                                                                                                                                                                                                                                    |
| `$PS2`           | Secondary command line prompt (used for additional input)                                                                                                                                                                                                                                                                                                                                           |
| `$PS3`           | Tertiary command line prompt (used in select loop)                                                                                                                                                                                                                                                                                                                                                  |
| `$PS4`           | Quaternary command line prompt (used to append info with verbose output)                                                                                                                                                                                                                                                                                                                            |
| `$RANDOM`        | A pseudo random integer between 0 and 32767                                                                                                                                                                                                                                                                                                                                                         |
| `$REPLY`         | Variable used by `read` by default when no variable is specified. Also used by `select` to return the user-supplied value                                                                                                                                                                                                                                                                           |
| `$PIPESTATUS`    | Array variable that holds the exit status values of each command in the most recently executed foreground pipeline.                                                                                                                                                                                                                                                                                 |

> Variable Assignment must have no space before and after. `a=123` not `a = 123`. The latter (an equal sign surrounded by spaces) in isolation means run the command `a` with the arguments `=` and `123`, though it is also seen in the string comparison operator (which syntactically is an argument to `[` or `[[` or whichever test you are using).

## \$@

`"$@"` expands to all of the command line arguments as separate words. It is different from `"$*"`, which expands to all of the arguments as a single word.

`"$@"` is especially useful for [looping](http://stackoverflow.com/documentation/bash/420/control-structures/9290/looping-over-an-array#t=201608301017209858205) through arguments and handling arguments with spaces.

Consider we are in a script that we invoked with two arguments, like so:

```bash
$ ./script.sh "␣1␣2␣" "␣3␣␣4␣"

```

The variables `$*` or `$@` will expand into `$1␣$2`, which in turn expand into `1␣2␣3␣4` so the loop below:

```bash
for var in $*; do # same for var in $@; do
    echo \<"$var"\>
done

```

will print for both

```bash
<1>
<2>
<3>
<4>

```

While `"$*"` will be expanded into `"$1␣$2"` which will in turn expand into `"␣1␣2␣␣␣3␣␣4␣"` and so the loop:

```bash
for var in "$*"; do
    echo \<"$var"\>
done

```

will only invoke `echo` once and will print

```bash
<␣1␣2␣␣␣3␣␣4␣>

```

And finally `"$@"` will expand into `"$1" "$2"`, which will expand into `"␣1␣2␣" "␣3␣␣4␣"` and so the loop

```bash
for var in "$@"; do
    echo \<"$var"\>
done

```

will print

```bash
<␣1␣2␣>
<␣3␣␣4␣>

```

thereby preserving both the internal spacing in the arguments and the arguments separation. Note that the construction `for var in "$@"; do ...` is so common and idiomatic that it is the default for a for loop and can be shortened to `for var; do ...`.

## \$

To get the number of command line arguments or positional parameters - type:

```bash
#!/bin/bash
echo "$#"

```

When run with three arguments the example above will result with the output:

```bash
~> $ ./testscript.sh firstarg secondarg thirdarg
3

```

## \$FUNCNAME

To get the name of the current function - type:

```bash
my_function()
{
    echo "This function is $FUNCNAME"    # This will output "This function is my_function"
}

```

This instruction will return nothing if you type it outside the function:

```bash
my_function

echo "This function is $FUNCNAME"    # This will output "This function is"

```

## \$HOME

The home directory of the user

```bash
~> $ echo $HOME
/home/user

```

## \$IFS

Contains the Internal Field Separator string that bash uses to split strings when looping etc. The default is the white space characters: `\n` (newline), `\t` (tab) and space. Changing this to something else allows you to split strings using different characters:

```bash
IFS=","
INPUTSTR="a,b,c,d"
for field in ${INPUTSTR}; do
    echo $field
done

```

The output of the above is:

```bash
a
b
c
d

```

**Notes:**

- This is responsible for the phenomenon known as [word splitting](http://stackoverflow.com/documentation/bash/5472/word-splitting/19453/what-when-and-why#t=201608151204564817213).

## \$OLDPWD

**OLDPWD** (**OLDP**rint**W**orking**D**irectory) contains directory before the last `cd` command:

```bash
~> $ cd directory
directory> $ echo $OLDPWD
/home/user

```

## \$PWD

**PWD** (**P**rint**W**orking**D**irectory) The current working directory you are in at the moment:

```bash
~> $ echo $PWD
/home/user
~> $ cd directory
directory> $ echo $PWD
/home/user/directory

```

## $1 $2 \$3 etc...

Positional parameters passed to the script from either the command line or a function:

```bash
#!/bin/bash
# $n is the n'th positional parameter
echo "$1"
echo "$2"
echo "$3"

```

The output of the above is:

```bash
~> $ ./testscript.sh firstarg secondarg thirdarg
firstarg
secondarg
thirdarg

```

If number of positional argument is greater than nine, curly braces must be used.

```bash
#  "set -- " sets positional parameters
set -- 1 2 3 4 5 6 7 8 nine ten eleven twelve
# the following line will output 10 not 1 as the value of $1 the digit 1
# will be concatenated with the following 0
echo $10   # outputs 1
echo ${10} # outputs ten
# to show this clearly:
set -- arg{1..12}
echo $10
echo ${10}

```

## \$\*

Will return all of the positional parameters in a single string.

**testscript.sh:**

```bash
#!/bin/bash
echo "$*"

```

Run the script with several arguments:

```bash
./testscript.sh firstarg secondarg thirdarg

```

Output:

```bash
firstarg secondarg thirdarg

```

## \$!

The Process ID (pid) of the last job run in the background:

```bash
~> $ ls &
testfile1 testfile2
[1]+  Done                    ls
~> $ echo $!
21715

```

## \$?

The exit status of the last executed function or command. Usually 0 will mean OK anything else will indicate a failure:

```bash
~> $ ls *.blah;echo $?
ls: cannot access *.blah: No such file or directory
2
~> $ ls;echo $?
testfile1 testfile2
0

```

## \$\$

The Process ID (pid) of the current process:

```bash
~> $ echo $$
13246

```

## \$HISTSIZE

The maximum number of remembered commands:

```bash
~> $ echo $HISTSIZE
1000

```

## \$BASHPID

Process ID (pid) of the current instance of Bash. This is not the same as the `$$` variable, but it often gives the same result. This is new in Bash 4 and doesn't work in Bash 3.

```bash
~> $ echo "\$\$ pid = $$  BASHPID = $BASHPID"
$$ pid = 9265  BASHPID = 9265

```

## \$BASH_ENV

An environment variable pointing to the Bash startup file which is read when a script is invoked.

## \$BASH_VERSINFO

An array containing the full version information split into elements, much more convenient than `$BASH_VERSION` if you're just looking for the major version:

```bash
~> $ for ((i=0; i<=5; i++)); do echo "BASH_VERSINFO[$i] = ${BASH_VERSINFO[$i]}"; done
BASH_VERSINFO[0] = 3
BASH_VERSINFO[1] = 2
BASH_VERSINFO[2] = 25
BASH_VERSINFO[3] = 1
BASH_VERSINFO[4] = release
BASH_VERSINFO[5] = x86_64-redhat-linux-gnu

```

## \$BASH_VERSION

Shows the version of bash that is running, this allows you to decide whether you can use any advanced features:

```bash
~> $ echo $BASH_VERSION
4.1.2(1)-release

```

## \$EDITOR

The default editor that will be involked by any scripts or programs, usually vi or emacs.

```bash
~> $ echo $EDITOR
vi

```

## \$HOSTNAME

The hostname assigned to the system during startup.

```bash
~> $ echo $HOSTNAME
mybox.mydomain.com

```

## \$HOSTTYPE

This variable identifies the hardware, it can be useful in determining which binaries to execute:

```bash
~> $ echo $HOSTTYPE
x86_64

```

## \$MACHTYPE

Similar to `$HOSTTYPE` above, this also includes information about the OS as well as hardware

```bash
~> $ echo $MACHTYPE
x86_64-redhat-linux-gnu

```

## \$OSTYPE

Returns information about the type of OS running on the machine, eg.

```bash
~> $ echo $OSTYPE
linux-gnu

```

## \$PATH

The search path for finding binaries for commands. Common examples include `/usr/bin` and `/usr/local/bin`.

When a user or script attempts to run a command, the paths in `$PATH` are searched in order to find a matching file with execute permission.

The directories in `$PATH` are separated by a `:` character.

```bash
~> $ echo "$PATH"
/usr/kerberos/bin:/usr/local/bin:/bin:/usr/bin

```

So, for example, given the above `$PATH`, if you type `lss` at the prompt, the shell will look for `/usr/kerberos/bin/lss`, then `/usr/local/bin/lss`, then `/bin/lss`, then `/usr/bin/lss`, in this order, before concluding that there is no such command.

## \$PPID

The Process ID (pid) of the script or shell's parent, meaning the process than invoked the current script or shell.

```bash
~> $ echo $$
13016
~> $ echo $PPID
13015

```

## \$SECONDS

The number of seconds a script has been running. This can get quite large if shown in the shell:

```bash
~> $ echo $SECONDS
98834

```

## \$SHELLOPTS

A readonly list of the options bash is supplied on startup to control its behaviour:

```bash
~> $ echo $SHELLOPTS
braceexpand:emacs:hashall:histexpand:history:interactive-comments:monitor

```

## \$\_

Outputs the last field from the last command executed, useful to get something to pass onwards to another command:

```

~> $ ls *.sh;echo $_
testscript1.sh  testscript2.sh
testscript2.sh

```

**test.sh:**

```bash
#!/bin/bash
echo "$_"

```

Output:

```bash
~> $ ./test.sh # running test.sh
./test.sh

```

**Note:** This is not a foolproof way to get the script path

## \$RANDOM

Each time this parameter is referenced, a random integer between 0 and 32767 is generated. Assigning a value to this variable seeds the random number generator ([source](https://www.gnu.org/software/bash/manual/bashref.html#Bash-Variables)).

```bash
~> $ echo $RANDOM
27119
~> $ echo $RANDOM
1349

```

## \$GROUPS

An array containing the numbers of groups the user is in:

```bash
#!/usr/bin/env bash
echo You are assigned to the following groups:
for group in ${GROUPS[@]}; do
   IFS=: read -r name dummy number members < <(getent group $group )
   printf "name: %-10s number: %-15s members: %s\n" "$name" "$number" "$members"
done

```

## \$LINENO

Outputs the line number in the current script. Mostly useful when debugging scripts.

```bash
#!/bin/bash
# this is line 2
echo something  # this is line 3
echo $LINENO # Will output 4

```

## \$SHLVL

When the bash command is executed a new shell is opened. The \$SHLVL environment variable holds the number of shell levels the **current** shell is running on top of.

In a **new** terminal window, executing the following command will produce different results based on the Linux distribution in use.

```bash
echo $SHLVL

```

Using **Fedora 25**, the output is "3". This indicates, that when opening a new shell, an initial bash command executes and performs a task. The initial bash command executes a child process (another bash command) which, in turn, executes a final bash command to open the new shell. When the new shell opens, it is running as a child process of 2 other shell processes, hence the output of "3".

In the following example (given the user is running Fedora 25), the output of $SHLVL in a new shell will be set to "3". As each bash command is executed, $SHLVL increments by one.

```bash
~> $ echo $SHLVL
3
~> $ bash
~> $ echo $SHLVL
4
~> $ bash
~> $ echo $SHLVL
5

```

One can see that executing the 'bash' command (or executing a bash script) opens a new shell. In comparison, sourcing a script runs the code in the current shell.

**test1.sh**

```bash
#!/usr/bin/env bash
echo "Hello from test1.sh. My shell level is $SHLVL"
source "test2.sh"

```

**test2.sh**

```bash
#!/usr/bin/env bash
echo "Hello from test2.sh. My shell level is $SHLVL"

```

**run.sh**

```bash
#!/usr/bin/env bash
echo "Hello from run.sh. My shell level is $SHLVL"
./test1.sh

```

**Execute:**

```bash
chmod +x test1.sh && chmod +x run.sh
./run.sh

```

**Output:**

```bash
Hello from run.sh. My shell level is 4
Hello from test1.sh. My shell level is 5
Hello from test2.sh. My shell level is 5

```

## \$UID

A read only variable that stores the users' ID number:

```bash
~> $ echo $UID
12345

```
