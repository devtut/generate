---
metaTitle: "Handling the system prompt"
description: "Using the PROMPT_COMMAND envrionment variable, Using PS2, Using PS3, Using PS4, Using PS1"
---

# Handling the system prompt

## Using the PROMPT_COMMAND envrionment variable

When the last command in an interactive bash instance is done, the evaluated PS1 variable is displayes. Before actually displaying PS1 bash looks whether the PROMPT_COMMAND is set. This value of this var must be a callable program or script. If this var is set this program/script is called BEFORE the PS1 prompt is displayed.

```bash
# just a stupid function, we will use to demonstrate
# we check the date if Hour is 12 and Minute is lower than 59
lunchbreak(){
   if (( $(date +%H) == 12 && $(date +%M) < 59 )); then
      # and print colored \033[ starts the escape sequence
      # 5; is blinking attribute
      # 2; means bold
      # 31 says red
      printf "\033[5;1;31mmind the lunch break\033[0m\n";
   else
      printf "\033[33mstill working...\033[0m\n";
   fi;
}

# activating it
export PROMPT_COMMAND=lunchbreak

```

## Using PS2

PS2 is displayed when a command extends to more than one line and bash awaits more keystrokes. It is displayed too when a compound command like **while...do..done** and alike is entered.

```bash
export PS2="would you please complete this command?\n"
# now enter a command extending to at least two lines to see PS2

```

## Using PS3

When the select statement is executed, it displays the given items prefixed with a number and then displays the PS3 prompt:

```bash
export PS3="  To choose your language type the preceding number : "
select lang in EN CA FR DE; do
   # check input here until valid.
   break
done

```

## Using PS4

PS4 is displayes when bash is in debugging mode.

```bash
#!/usr/bin/env bash

# switch on debugging
set -x

# define a stupid_func
stupid_func(){
   echo I am line 1 of stupid_func
   echo I am line 2 of stupid_func
}

# setting the PS4 "DEBUG" prompt
export PS4='\nDEBUG level:$SHLVL subshell-level: $BASH_SUBSHELL \nsource-file:${BASH_SOURCE} line#:${LINENO} function:${FUNCNAME[0]:+${FUNCNAME[0]}(): }\nstatement: '

# a normal statement
echo something

# function call
stupid_func

# a pipeline of commands running in a subshell
( ls -l | grep 'x' )

```

## Using PS1

PS1 is the normal system prompt indicating that bash waits for commands being typed in. It understands some escape sequences and can execute functions or progams. As bash has to position the cursor after the displayes prompt, it needs to know how to calculate the effective length of the prompt string. To indicate non printing sequences of chars within the PS1 variable escaped braces are used: **\[** **a non printing sequence of chars** **\]**. All being said holds true for all PS\* vars.

(The black caret indicates cursor)

```bash
#everything not being an escape sequence will be literally printed
export PS1="literal sequence "  # Prompt is now:
literal sequence ▉

# \u == user \h == host \w == actual working directory
# mind the single quotes avoiding interpretation by shell
export PS1='\u@\h:\w > ' # \u == user, \h == host, \w actual working dir
looser@host:/some/path > ▉

# executing some commands within PS1
# following line will set foreground color to red, if user==root,
# else it resets attributes to default
# $( (($EUID == 0)) &&  tput setaf 1)
# later we do reset attributes to default with
# $(  tput sgr0 )
# assuming being root:
PS1="\[$( (($EUID == 0)) &&  tput setaf 1 \]\u\[$(tput sgr0)\]@\w:\w \$ "
looser@host:/some/path > ▉  # if not root else <red>root<default>@host....

```

#### Syntax

- export PS1="something" # displayes when bash awaits a command to be typed in
- export PS2="anotherthing" # dsplayed when statement extends to more lines
- export PS3="question prompt for select statement" # seldomly used prompt for select. First set PS3 to your needs, then call **select**. See **help select**
- export PS4="mostly useful for debugging; line number and so on" # used for debugging bash scripts.

#### Parameters

| Escape         | Details                                                                                                                                                                             |
| -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| \a             | A bell character.                                                                                                                                                                   |
| \d             | The date, in "Weekday Month Date" format (e.g., "Tue May 26").                                                                                                                      |
| \D{FORMAT}     | The FORMAT is passed to `strftime'(3) and the result is inserted into the prompt string; an empty FORMAT results in a locale-specific time representation. The braces are required. |
| \e             | An escape character. \033 works of course too.                                                                                                                                      |
| \h             | The hostname, up to the first `.'. (i.e. no domain part)                                                                                                                            |
| \H             | The hostname eventually with domain part                                                                                                                                            |
| \j             | The number of jobs currently managed by the shell.                                                                                                                                  |
| \l             | The basename of the shell's terminal device name.                                                                                                                                   |
| \n             | A newline.                                                                                                                                                                          |
| \r             | A carriage return.                                                                                                                                                                  |
| \s             | The name of the shell, the basename of `\$0' (the portion following the final slash).                                                                                               |
| \t             | The time, in 24-hour HH:MM:SS format.                                                                                                                                               |
| \T             | The time, in 12-hour HH:MM:SS format.                                                                                                                                               |
| @              | The time, in 12-hour am/pm format.                                                                                                                                                  |
| \A             | The time, in 24-hour HH:MM format.                                                                                                                                                  |
| \u             | The username of the current user.                                                                                                                                                   |
| \v             | The version of Bash (e.g., 2.00)                                                                                                                                                    |
| \V             | The release of Bash, version + patchlevel (e.g., 2.00.0)                                                                                                                            |
| \w             | The current working directory, with $HOME abbreviated with a tilde (uses the $PROMPT_DIRTRIM variable).                                                                             |
| \W             | The basename of $PWD, with $HOME abbreviated with a tilde.                                                                                                                          |
| !              | The history number of this command.                                                                                                                                                 |
| #              | The command number of this command.                                                                                                                                                 |
| \$             | If the effective uid is 0, **#**, otherwise **\$**.                                                                                                                                 |
| \NNN           | The character whose ASCII code is the octal value NNN.                                                                                                                              |
| \|A backslash. |
| \[             | Begin a sequence of non-printing characters. This could be used to embed a terminal control sequence into the prompt.                                                               |
| \]             | End a sequence of non-printing characters.                                                                                                                                          |
