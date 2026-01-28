---
metaTitle: "Bash - Quoting"
description: "Double quotes for variable and command substitution, Difference between double quote and single quote, Newlines and control characters, Quoting literal text"
---

# Quoting

## Double quotes for variable and command substitution

Variable substitutions should only be used inside double quotes.

```bash
calculation='2 * 3'
echo "$calculation"         # prints 2 * 3
echo $calculation           # prints 2, the list of files in the current directory, and 3
echo "$(($calculation))"    # prints 6

```

Outside of double quotes, `$var` takes the value of `var`, splits it into whitespace-delimited parts, and interprets each part as a glob (wildcard) pattern. Unless you want this behavior, always put `$var` inside double quotes: `"$var"`.

The same applies to command substitutions: `"$(mycommand)"` is the output of `mycommand`, `$(mycommand)` is the result of split+glob on the output.

```bash
echo "$var"             # good
echo "$(mycommand)"     # good
another=$var            # also works, assignment is implicitly double-quoted
make -D THING=$var      # BAD! This is not a bash assignment.
make -D THING="$var"    # good
make -D "THING=$var"    # also good

```

Command substitutions get their own quoting contexts. Writing arbitrarily nested substitutions is easy because the parser will keep track of nesting depth instead of greedily searching for the first `"` character. The StackOverflow syntax highlighter parses this wrong, however. For example:

```bash
echo "formatted text: $(printf "a + b = %04d" "${c}")" # “formatted text: a + b = 0000”

```

Variable arguments to a command substitution should be double-quoted inside the expansions as well:

```bash
echo "$(mycommand "$arg1" "$arg2")"

```

## Difference between double quote and single quote

| Double quote                                                                                             | Single quote                                    |
| -------------------------------------------------------------------------------------------------------- | ----------------------------------------------- |
| Allows variable expansion                                                                                | Prevents variable expansion                     |
| Allows history expansion if enabled                                                                      | Prevents history expansion                      |
| Allows command substitution                                                                              | Prevents command substitution                   |
| `*` and `@` can have special meaning                                                                     | `*` and `@` are always literals                 |
| Can contain both single quote or double quote                                                            | Single quote is not allowed inside single quote |
| `$`, ```bash, `"`, `\` can be escaped with `\` to prevent their special meaning|All of them are literals |

**Properties that are common to both:**

- Prevents globbing
- Prevents word splitting

Examples:

```bash
$ echo "!cat"
echo "cat file"
cat file
$ echo '!cat'
!cat
echo "\"'\""
"'"
$ a='var'
$ echo '$a'
$a
$ echo "$a"
var

```

## Newlines and control characters

A newline can be included in a single-string or double-quoted string. Note that backslash-newline does not result in a newline, the line break is ignored.

```bash
newline1='
'
newline2="
"
newline3=$'\n'
empty=\

echo "Line${newline1}break"
echo "Line${newline2}break"
echo "Line${newline3}break"
echo "No line break${empty} here"

```

Inside dollar-quote strings, backslash-letter or backslash-octal can be used to insert control characters, like in many other programming languages.

```bash
echo $'Tab: [\t]'
echo $'Tab again: [\009]'
echo $'Form feed: [\f]'
echo $'Line\nbreak'

```

## Quoting literal text

All the examples in this paragraph print the line

```bash
!"#$&'()*;<=>?  @[\]^`{|}~

```

A backslash quotes the next character, i.e. the next character is interpreted literally. The one exception is a newline: backslash-newline expands to the empty string.

```bash
echo \!\"\#\$\&\'\(\)\*\;\<\=\>\?\ \ \@\[\\\]\^\`\{\|\}\~

```

All text between single quotes (forward quotes `'`, also known as apostrophe) is printed literally. Even backslash stands for itself, and it's impossible to include a single quote; instead, you can stop the literal string, include a literal single quote with a backslash, and start the literal string again. Thus the 4-character sequence `'\''` effectively allow to include a single quote in a literal string.

```bash
echo '!"#$&'\''()*;<=>?  @[\]^`{|}~'
#          ^^^^

```

Dollar-single-quote starts a string literal `$'…'` like many other programming languages, where backslash quotes the next character.

```bash
echo $'!"#$&\'()*;<=>?  @[\\]^`{|}~'
#           ^^            ^^

```

Double quotes `"` delimit semi-literal strings where only the characters `"` `\` `$` and ```bash retain their special meaning. These characters need a backslash before them (note that if backslash is followed by some other character, the backslash remains). Double quotes are mostly useful when including a variable or a command substitution.

```bash
echo "!\"#\$&'()*;<=>?  @[\\]^\`{|}~"
#      ^^                 ^^  ^^
echo "!\"#\$&'()*;<=>?  @[\]^\`{|}~"
#      ^^                 ^  ^^      \[ prints \[

```

Interactively, beware that `!` triggers history expansion inside double quotes: `"!oops"` looks for an older command containing `oops`; `"\!oops"` doesn't do history expansion but keeps the backslash. This does not happen in scripts.

#### Syntax

- \C (any one character except newline)
- 'all literal except single quotes'; 'this: '\'' is a single quote'
- \$'only \\ and \' are special; \n = newline etc.'
- "$variable and other text; \"\\\$\` are special"
