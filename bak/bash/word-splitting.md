---
metaTitle: "Bash - Word splitting"
description: "Bad effects of word splitting, Splitting with IFS, What, when and Why?, IFS & word splitting, Usefulness of word splitting, Splitting by separator changes"
---

# Word splitting

## Bad effects of word splitting

```bash
$ a='I am a string with spaces'
$ [ $a = $a ] || echo "didn't match"
bash: [: too many arguments
didn't match

```

> `[ $a = $a ]` was interpreted as `[ I am a string with spaces = I am a string with spaces ]`. `[` is the `test` command for which `I am a string with spaces` is not a single argument, rather it's **6** arguments!!

```bash
$ [ $a = something ] || echo "didn't match"
bash: [: too many arguments
didn't match

```

> `[ $a = something ]` was interpreted as `[ I am a string with spaces = something ]`

```bash
$ [ $(grep . file) = 'something' ]
bash: [: too many arguments

```

> The `grep` command returns a multiline string with spaces, so you can just imagine how many arguments are there...:D

**See [what, when and why](http://stackoverflow.com/documentation/bash/5472/word-splitting/19453/what-when-and-why#t=201608151204564817213) for the basics.**

## Splitting with IFS

To be more clear, let's create a script named `showarg`:

```bash
#!/usr/bin/env bash
printf "%d args:" $#
printf " <%s>" "$@"
echo

```

Now let's see the differences:

```bash
$ var="This is an example"
$ showarg $var
4 args: <This> <is> <an> <example>

```

> `$var` is split into 4 args. `IFS` is white space characters and thus word splitting occurred in spaces

```bash
$ var="This/is/an/example"
$ showarg $var
1 args: <This/is/an/example>

```

> In above word splitting didn't occur because the `IFS` characters weren't found.

Now let's set `IFS=/`

```bash
$ IFS=/
$ var="This/is/an/example"
$ showarg $var
4 args: <This> <is> <an> <example>

```

> The `$var` is splitting into 4 arguments not a single argument.

## What, when and Why?

When the shell performs **parameter expansion**, **command substitution**, **variable or arithmetic expansion**, it scans for word boundaries in the result. If any word boundary is found, then the result is split into multiple words at that position. The word boundary is defined by a shell variable `IFS` (Internal Field Separator). The default value for IFS are space, tab and newline, i.e. word splitting will occur on these three white space characters if not prevented explicitly.

```bash
set -x
var='I am
a
multiline string'
fun() {
    echo "-$1-"
    echo "*$2*"
    echo ".$3."
}
fun $var

```

In the above example this is how the `fun` function is being executed:

```bash
fun I am a multiline string

```

> `$var` is split into 5 args, only `I`, `am` and `a` will be printed.

## IFS & word splitting

**See [what, when and why](http://stackoverflow.com/documentation/bash/5472/word-splitting/19453/what-when-and-why#t=201608151204564817213) if you don't know about the affiliation of IFS to word splitting**

**let's set the IFS to space character only:**

```bash
set -x
var='I am
a
multiline string'
IFS=' '
fun() {
    echo "-$1-"
    echo "*$2*"
    echo ".$3."
}
fun $var

```

This time word splitting will only work on spaces. The `fun` function will be executed like this:

```bash
fun I 'am
a
multiline' string

```

> `$var` is split into 3 args. `I`, `am\na\nmultiline` and `string` will be printed

**Let's set the IFS to newline only:**

```bash
IFS=$'\n'
...

```

Now the `fun` will be executed like:

```bash
fun 'I am' a 'multiline string'

```

> `$var` is split into 3 args. `I am`, `a`, `multiline string` will be printed

**Let's see what happens if we set IFS to nullstring:**

```bash
IFS=
...

```

This time the `fun` will be executed like this:

```bash
fun 'I am
a
multiline string'

```

> `$var` is not split i.e it remained a single arg.

**You can prevent word splitting by setting the IFS to nullstring**

**A general way of preventing word splitting is to use double quote:**

```bash
fun "$var"

```

will prevent word splitting in all the cases discussed above i.e the `fun` function will be executed with only one argument.

## Usefulness of word splitting

There are some cases where word splitting can be useful:

**Filling up array:**

```bash
arr=($(grep -o '[0-9]\+' file))

```

> This will fill up `arr` with all numeric values found in **file**

**Looping through space separated words:**

```bash
words='foo bar baz'
for w in $words;do
    echo "W: $w"
done

```

Output:

```bash
W: foo
W: bar
W: baz

```

**Passing space separated parameters which don't contain white spaces:**

```bash
packs='apache2 php php-mbstring php-mysql'
sudo apt-get install $packs

```

or

```bash
packs='
apache2
php
php-mbstring
php-mysql
'
sudo apt-get install $packs

```

> This will install the packages. If you double quote the `$packs` then it will throw an error.

> Unquoetd `$packs` is sending all the space separated package names as arguments to `apt-get`, while quoting it will send the `$packs` string as a single argument and then `apt-get` will try to install a package named `apache2 php php-mbstring php-mysql` (for the first one) which obviously doesn't exist

**See [what, when and why](http://stackoverflow.com/documentation/bash/5472/word-splitting/19453/what-when-and-why#t=201608151204564817213) for the basics.**

## Splitting by separator changes

We can just do simple replacement of separators from space to new line, as following example.

```bash
echo $sentence | tr " " "\n"

```

It'll split the value of the variable `sentence` and show it line by line respectively.

#### Syntax

- Set IFS to newline: IFS=\$'\n'
- Set IFS to nullstring: IFS=
- Set IFS to / character: IFS=/

#### Parameters

| Parameter | Details                                                                |
| --------- | ---------------------------------------------------------------------- |
| IFS       | Internal field separator                                               |
| -x        | Print commands and their arguments as they are executed (Shell option) |

#### Remarks

- Word splitting is not performed during assignments e.g `newvar=$var`
- Word splitting is not performed in the `[[ ... ]]` construct
- Use double quotes on variables to prevent word splitting
