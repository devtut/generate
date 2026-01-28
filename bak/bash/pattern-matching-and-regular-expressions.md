---
metaTitle: "Bash - Pattern matching and regular expressions"
description: "Check if a string matches a regular expression, Behaviour when a glob does not match anything, Get captured groups from a regex match against a string, The * glob, The ** glob, The ? glob, The [ ] glob, Matching hidden files, Case insensitive matching, Extended globbing, Regex matching"
---

# Pattern matching and regular expressions

## Check if a string matches a regular expression

Check if a string consists in exactly 8 digits:

```bash
$ date=20150624
$ [[ $date =~ ^[0-9]{8}$ ]] && echo "yes" || echo "no"
yes
$ date=hello
$ [[ $date =~ ^[0-9]{8}$ ]] && echo "yes" || echo "no"
no

```

## Behaviour when a glob does not match anything

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

In case the glob does not match anything the result is determined by the
options `nullglob` and `failglob`. If neither of them are set, Bash will return the glob itself if nothing is matched

```bash
$ echo no*match
no*match

```

If `nullglob` is activated then nothing (`null`) is returned:

```bash
$ shopt -s nullglob
$ echo no*match

$

```

If `failglob` is activated then an error message is returned:

```bash
$ shopt -s failglob
$ echo no*match
bash: no match: no*match
$

```

Notice, that the `failglob` option supersedes the `nullglob` option, i.e.,
if `nullglob` and `failglob` are both set, then - in case of no match - an
error is returned.

## Get captured groups from a regex match against a string

```bash
a='I am a simple string with digits 1234'
pat='(.*) ([0-9]+)'
[[ "$a" =~ $pat ]]
echo "${BASH_REMATCH[0]}"
echo "${BASH_REMATCH[1]}"
echo "${BASH_REMATCH[2]}"

```

Output:

```bash
I am a simple string with digits 1234
I am a simple string with digits
1234

```

## The \* glob

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

The asterisk \* is probably the most commonly used glob. It simply matches any
String

```bash
$ echo *acy
macy stacy tracy

```

A single \* will not match files and folders that reside in subfolders

```bash
$ echo *
emptyfolder folder macy stacy tracy
$ echo folder/*
folder/anotherfolder folder/subfolder

```

## The \*\* glob

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -s globstar

```

Bash is able to interpret two adjacent asterisks as a single glob. With the `globstar`
option activated this can be used to match folders that reside deeper in the directory structure

```bash
echo **
emptyfolder folder folder/anotherfolder folder/anotherfolder/content folder/anotherfolder/content/deepfolder folder/anotherfolder/content/deepfolder/file folder/subfolder folder/subfolder/content folder/subfolder/content/deepfolder folder/subfolder/content/deepfolder/file macy stacy tracy

```

The `**` can be thought of a path expansion, no matter how deep the path is.
This example matches any file or folder that starts with `deep`, regardless of how
deep it is nested:

```bash
$ echo **/deep*
folder/anotherfolder/content/deepfolder folder/subfolder/content/deepfolder

```

## The ? glob

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

The `?` simply matches exactly one character

```bash
$ echo ?acy
macy
$ echo ??acy
stacy tracy

```

## The [ ] glob

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

If there is a need to match specific characters then '[]' can be used. Any
character inside '[]' will be matched exactly once.

```bash
$ echo [m]acy
macy
$ echo [st][tr]acy
stacy tracy

```

The `[]` glob, however, is more versatile than just that. It also allows
for a negative match and even matching ranges of characters and
characterclasses. A negative match is achieved by using `!` or `^` as the first
character following `[`. We can match `stacy` by

```bash
$ echo [!t][^r]acy
stacy

```

Here we are telling bash the we want to match only files which do not not
start with a `t` and the second letter is not an `r` and the file ends in
`acy`.

Ranges can be matched by seperating a pair of characters with a hyphen (`-`). Any
character that falls between those two enclosing characters - inclusive - will
be matched. E.g., `[r-t]` is equivalent to `[rst]`

```bash
$ echo [r-t][r-t]acy
stacy tracy

```

Character classes can be matched by `[:class:]`, e.g., in order to match files
that contain a whitespace

```bash
$ echo *[[:blank:]]*
file with space

```

## Matching hidden files

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

The Bash built-in option **dotglob** allows to match hidden files
and folders, i.e., files and folders that start with a `.`

```bash
$ shopt -s dotglob
$ echo *
file with space folder .hiddenfile macy stacy tracy

```

## Case insensitive matching

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

Setting the option `nocaseglob` will match the glob in a case insensitive
manner

```bash
$ echo M*
M*
$ shopt -s nocaseglob
$ echo M*
macy

```

## Extended globbing

**Preparation**

```bash
$ mkdir globbing
$ cd globbing
$ mkdir -p folder/{sub,another}folder/content/deepfolder/
touch macy stacy tracy "file with space" folder/{sub,another}folder/content/deepfolder/file .hiddenfile
$ shopt -u nullglob
$ shopt -u failglob
$ shopt -u dotglob
$ shopt -u nocaseglob
$ shopt -u extglob
$ shopt -u globstar

```

Bash's built-in `extglob` option can extend a glob's matching capabilities

```bash
shopt -s extglob

```

The following sub-patterns comprise valid extended globs:

- `?(pattern-list)` – Matches zero or one occurrence of the given patterns
- `*(pattern-list)` – Matches zero or more occurrences of the given patterns
- `+(pattern-list)` – Matches one or more occurrences of the given patterns
- `@(pattern-list)` – Matches one of the given patterns
- `!(pattern-list)` – Matches anything except one of the given patterns

The `pattern-list` is a list of globs separated by `|`.

```bash
$ echo *([r-t])acy
stacy tracy

$ echo *([r-t]|m)acy
macy stacy tracy

$ echo ?([a-z])acy
macy

```

The `pattern-list` itself can be another, nested extended glob. In the above
example we have seen that we can match `tracy` and `stacy` with `*(r-t)`.
This extended glob itself can be used inside the negated extended glob
`!(pattern-list)` in order to match `macy`

```bash
$ echo !(*([r-t]))acy
macy

```

It matches anything that does **not** start with zero or more occurrences of the
letters `r`, `s` and `t`, which leaves only `macy` as possible match.

## Regex matching

```bash
pat='[^0-9]+([0-9]+)'
s='I am a string with some digits 1024'
[[ $s =~ $pat ]] # $pat must be unquoted
echo "${BASH_REMATCH[0]}"
echo "${BASH_REMATCH[1]}"

```

Output:

```bash
I am a string with some digits 1024
1024

```

Instead of assigning the regex to a variable (`$pat`) we could also do:

```bash
[[ $s =~ [^0-9]+([0-9]+) ]]

```

**Explanation**

- The `[[ $s =~ $pat ]]` construct performs the regex matching
- The captured groups i.e the match results are available in an array named **BASH_REMATCH**
- The 0th index in the **BASH_REMATCH** array is the total match
- The **i'th** index in the **BASH_REMATCH** array is the **i'th** captured group, where **i** = 1, 2, 3 ...

#### Syntax

- \$ shopt -u option # Deactivate Bash's built-in 'option'
- \$ shopt -s option # Activate Bash's built-in 'option'

#### Remarks

**Character Classes**

Valid character classes for the `[]` glob are defined by the POSIX standard:

>

<p>alnum alpha ascii blank cntrl digit graph lower print punct space
upper word xdigit</p>

Inside `[]` more than one character class or range can be used, e.g.,

```bash
$ echo a[a-z[:blank:]0-9]*

```

will match any file that starts with an `a` and is followed by either a lowercase letter or a blank or a digit.

It should be kept in mind, though, that a `[]` glob can only be wholly negated and not only parts of it. The negating character **must** be the first character following the opening `[`, e.g., this expression matches all files that do **not** start with an `a`

```bash
$ echo [^a]*

```

The following does match all files that start with either a digit or a `^`

```bash
$ echo [[:alpha:]^a]*

```

It does **not** match any file or folder that starts with with letter except an `a` because the `^` is interpreted as a literal `^`.

**Escaping glob characters**

It is possible that a file or folder contains a glob character as part of its name. In this case a glob can be escaped with a preceding `\` in order for a literal match. Another approach is to use double `""` or single `''` quotes to address the file.
Bash does not process globs that are enclosed within `""` or `''`.

**Difference to Regular Expressions**

The most significant difference between globs and Regular Expressions is that
a valid Regular Expressions requires a qualifier as well as a quantifier.
A qualifier identifies **what** to match and a quantifier tells how often
to match the qualifier. The equivalent RegEx to the `*` glob is `.*` where
`.` stands for any character and `*` stands for zero or more matches of the
previous character. The equivalent RegEx for the `?` glob is `.{1}`. As
before, the qualifier `.` matches any character and the `{1}` indicates to
match the preceding qualifier exactly once. This should not be confused with
the `?` quantifier, which matches zero or once in a RegEx.
The `[]` glob is can be used just the same in a RegEx, as long as it is
followed by a mandatory quantifier.

**Equivalent Regular Expressions**

| Glob | RegEx |
| ---- | ----- |
| `*`  | `.*`  |
| `?`  | `.`   |
| `[]` | `[]`  |
