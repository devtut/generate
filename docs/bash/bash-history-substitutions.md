---
metaTitle: "Bash - Bash history substitutions"
description: "Quick Reference, Repeat previous command with sudo, Search in the command history by pattern, Switch to newly created directory with !#:N, Using !$, Repeat the previous command with a substitution"
---

# Bash history substitutions




## Quick Reference


### Interaction with the history

```bash
# List all previous commands
history

# Clear the history, useful if you entered a password by accident
history -c

```

### Event designators

```bash
# Expands to line n of bash history
!n

# Expands to last command
!!

# Expands to last command starting with "text"
!text

# Expands to last command containing "text"
!?text

# Expands to command n lines ago
!-n

# Expands to last command with first occurrence of "foo" replaced by "bar"
^foo^bar^

# Expands to the current command
!#

```

### Word designators

These are separated by `:` from the event designator they refer to. The colon can be omitted if the word designator doesn't start with a number: `!^` is the same as `!:^`.

```bash
# Expands to the first argument of the most recent command
!^

# Expands to the last argument of the most recent command (short for !!:$)
!$

# Expands to the third argument of the most recent command
!:3

# Expands to arguments x through y (inclusive) of the last command
# x and y can be numbers or the anchor characters ^ $
!:x-y

# Expands to all words of the last command except the 0th
# Equivalent to :^-$
!*

```

### Modifiers

These modify the preceding event or word designator.

```bash
# Replacement in the expansion using sed syntax
# Allows flags before the s and alternate separators
:s/foo/bar/ #substitutes bar for first occurrence of foo
:gs|foo|bar| #substitutes bar for all foo

# Remove leading path from last argument ("tail")
:t

# Remove trailing path from last argument ("head")
:h

# Remove file extension from last argument
:r

```

If the Bash variable `HISTCONTROL` contains either `ignorespace` or `ignoreboth` (or, alternatively, `HISTIGNORE` contains the pattern `[ ]*`), you can prevent your commands from being stored in Bash history by prepending them with a space:

```bash
# This command won't be saved in the history
 foo

# This command will be saved
bar

```



## Repeat previous command with sudo


```bash
$ apt-get install r-base
E: Could not open lock file /var/lib/dpkg/lock - open (13: Permission denied)
E: Unable to lock the administration directory (/var/lib/dpkg/), are you root?
$ sudo !!
sudo apt-get install r-base
[sudo] password for <user>: 

```



## Search in the command history by pattern


Press <kbd>control</kbd><kbd>r</kbd> and type a pattern.

For example, if you recently executed `man 5 crontab`, you can find it quickly by **starting to type** "crontab". The prompt will change like this:

```bash
(reverse-i-search)`cr': man 5 crontab

```

The ``cr'` there is the string I typed so far.
This is an incremental search, so as you continue typing, the search result gets updated to match the most recent command that contained the pattern.

Press the left or right arrow keys to edit the matched command before running it,
or the <kbd>enter</kbd> key to run the command.

By default the search finds the most recently executed command matching the pattern. To go further back in the history press <kbd>control</kbd><kbd>r</kbd> again. You may press it repeatedly until you find the desired command.



## Switch to newly created directory with !#:N


```bash
$ mkdir backup_download_directory && cd !#:1
mkdir backup_download_directory && cd backup_download_directory

```

This will substitute the Nth argument of the current command. In the example `!#:1` is replaced with the first argument, i.e. backup_download_directory.



## Using !$


You can use the `!$` to reduce repetition when using the command line:

```bash
$ echo ping
ping
$ echo !$
ping

```

You can also build upon the repetition

```bash
$ echo !$ pong
ping pong
$ echo !$, a great game
pong, a great game

```

Notice that in the last example we did not get `ping pong, a great game` because the last argument passed  to the previous command was `pong`, we can avoid issue like this by adding quotes. Continuing with the example, our last argument was `game`:

```bash
$ echo "it is !$ time"
it is game time
$ echo "hooray, !$!"
hooray, it is game time!

```



## Repeat the previous command with a substitution


```bash
$ mplayer Lecture_video_part1.mkv
$ ^1^2^
mplayer Lecture_video_part2.mkv

```

This command will replace `1` with `2` in the previously executed command. It will only replace the first occurrence of the string and is equivalent to `!!:s/1/2/`.

If you want to replace **all** occurrences, you have to use `!!:gs/1/2/` or `!!:as/1/2/`.

