---
metaTitle: "Blaming"
description: "Only show certain lines, Show the commit that last modified a line, Ignore whitespace-only changes, To find out who changed a file"
---

# Blaming

## Only show certain lines

Output can be restricted by specifying line ranges as

`git blame -L <start>,<end>`

Where `<start>` and `<end>` can be:

<li>
line number
`git blame -L 10,30`
</li>
<li>
/regex/
`git blame -L /void main/`, `git blame -L 46,/void foo/`
</li>
<li>
+offset, -offset (only for `<end>`)
`git blame -L 108,+30`, `git blame -L 215,-15`
</li>

Multiple line ranges can be specified, and overlapping ranges are allowed.

`git blame -L 10,30 -L 12,80 -L 120,+10 -L ^/void main/,+40`

## Show the commit that last modified a line

`git blame <file>`

will show the file with each line annotated with the commit that last modified it.

## Ignore whitespace-only changes

Sometimes repos will have commits that only adjust whitespace, for example fixing indentation or switching between tabs and spaces. This makes it difficult to find the commit where the code was actually written.

`git blame -w`

will ignore whitespace-only changes to find where the line really came from.

## To find out who changed a file

```git
// Shows the author and commit per line of specified file
git blame test.c

// Shows the author email and commit per line of specified
git blame -e test.c file

// Limits the selection of lines by specified range
git blame -L 1,10 test.c

```

#### Syntax

- git blame [filename]
- git blame [-f][-e][-w][filename]
- git blame [-L range][filename]

#### Parameters

| Parameter       | Details                                                                                                |
| --------------- | ------------------------------------------------------------------------------------------------------ |
| filename        | Name of the file for which details need to be checked                                                  |
| -f              | Show the file name in the origin commit                                                                |
| -e              | Show the author email instead of author name                                                           |
| -w              | Ignore white spaces while making a comparison between child and parent's version                       |
| -L start,end    | Show only the given line range Example: `git blame -L 1,2 [filename]`                                  |
| --show-stats    | Shows additional statistics at end of blame output                                                     |
| -l              | Show long rev (Default: off)                                                                           |
| -t              | Show raw timestamp (Default: off)                                                                      |
| -reverse        | Walk history forward instead of backward                                                               |
| -p, --porcelain | Output for machine consumption                                                                         |
| -M              | Detect moved or copied lines within a file                                                             |
| -C              | In addition to -M, detect lines moved or copied from other files that were modified in the same commit |
| -h              | Show the help message                                                                                  |
| -c              | Use the same output mode as git-annotate (Default: off)                                                |
| -n              | Show the line number in the original commit (Default: off)                                             |

#### Remarks

The git blame command is very useful when it comes to know who has made changes to a file on a per line base.
