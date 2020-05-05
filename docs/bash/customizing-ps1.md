---
metaTitle: "Bash - Customizing PS1"
description: "Colorize and customize terminal prompt, Show git branch name in terminal prompt, Change PS1 prompt, Show a git branch using PROMPT_COMMAND, Show time in terminal prompt, Show previous command return status and time"
---

# Customizing PS1

## Colorize and customize terminal prompt

This is how the author sets their personal `PS1` variable:

```bash
gitPS1(){
    gitps1=$(git branch 2>/dev/null | grep '*')
    gitps1="${gitps1:+ (${gitps1/#\* /})}"
    echo "$gitps1"
}
#Please use the below function if you are a mac user
gitPS1ForMac(){
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
timeNow(){
    echo "$(date +%r)"
}
if [ "$color_prompt" = yes ]; then
  if [ x$EUID = x0 ]; then
    PS1='\[\033[1;38m\][$(timeNow)]\[\033[00m\] \[\033[1;31m\]\u\[\033[00m\]\[\033[1;37m\]@\[\033[00m\]\[\033[1;33m\]\h\[\033[00m\] \[\033[1;34m\]\w\[\033[00m\]\[\033[1;36m\]$(gitPS1)\[\033[00m\] \[\033[1;31m\]:/#\[\033[00m\] '
  else
    PS1='\[\033[1;38m\][$(timeNow)]\[\033[00m\] \[\033[1;32m\]\u\[\033[00m\]\[\033[1;37m\]@\[\033[00m\]\[\033[1;33m\]\h\[\033[00m\] \[\033[1;34m\]\w\[\033[00m\]\[\033[1;36m\]$(gitPS1)\[\033[00m\] \[\033[1;32m\]:/$\[\033[00m\] '
  fi
else
    PS1='[$(timeNow)] \u@\h \w$(gitPS1) :/$ '
fi

```

And this is how my prompt looks like:
[<img src="http://i.stack.imgur.com/FmO2S.png" alt="enter image description here" />](http://i.stack.imgur.com/FmO2S.png)

Color reference:

```bash
# Colors
txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

```

**Notes:**

<li>
Make the changes in `~/.bashrc` or `/etc/bashrc` or `~/.bash_profile` or `~./profile` file (depending on the OS) and save it.
</li>
<li>
For `root` you might also need to edit the `/etc/bash.bashrc` or `/root/.bashrc` file
</li>
<li>
Run `source ~/.bashrc` (distro specific) after saving the file.
</li>
<li>
Note: if you have saved the changes in `~/.bashrc`, then remember to add `source ~/.bashrc` in your `~/.bash_profile` so that this change in `PS1` will be recorded every time the Terminal application starts.
</li>

## Show git branch name in terminal prompt

You can have functions in the PS1 variable, just make sure to single quote it or use escape for special chars:

```bash
gitPS1(){
    gitps1=$(git branch 2>/dev/null | grep '*')
    gitps1="${gitps1:+ (${gitps1/#\* /})}"
    echo "$gitps1"
}

PS1='\u@\h:\w$(gitPS1)$ '

```

It will give you a prompt like this:

```bash
user@Host:/path (master)$

```

**Notes:**

- Make the changes in `~/.bashrc` or `/etc/bashrc` or `~/.bash_profile` or `~./profile` file (depending on the OS) and save it.
- Run `source ~/.bashrc` (distro specific) after saving the file.

## Change PS1 prompt

To change PS1, you just have to change the value of PS1 shell variable. The value can be set in `~/.bashrc` or `/etc/bashrc` file, depending on the distro. PS1 can be changed to any plain text like:

```bash
PS1="hello "

```

Besides the plain text, a number of backslash-escaped special characters are supported:

| Format          | Action                                                                                                                                                                           |
| --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `\a`            | an ASCII bell character (07)                                                                                                                                                     |
| `\d`            | the date in “Weekday Month Date” format (e.g., “Tue May 26”)                                                                                                                     |
| `\D{format}`    | the format is passed to strftime(3) and the result is inserted into the prompt string; an empty format results in a locale-specific time representation. The braces are required |
| `\e`            | an ASCII escape character (033)                                                                                                                                                  |
| `\h`            | the hostname up to the first ‘.’                                                                                                                                                 |
| `\H`            | the hostname                                                                                                                                                                     |
| `\j`            | the number of jobs currently managed by the shell                                                                                                                                |
| `\l`            | the basename of the shell’s terminal device name                                                                                                                                 |
| `\n`            | newline                                                                                                                                                                          |
| `\r`            | carriage return                                                                                                                                                                  |
| `\s`            | the name of the shell, the basename of \$0 (the portion following the final slash)                                                                                               |
| `\t`            | the current time in 24-hour HH:MM:SS format                                                                                                                                      |
| `\T`            | the current time in 12-hour HH:MM:SS format                                                                                                                                      |
| `\@`            | the current time in 12-hour am/pm format                                                                                                                                         |
| `\A`            | the current time in 24-hour HH:MM format                                                                                                                                         |
| `\u`            | the username of the current user                                                                                                                                                 |
| `\v`            | the version of bash (e.g., 2.00)                                                                                                                                                 |
| `\V`            | the release of bash, version + patch level (e.g., 2.00.0)                                                                                                                        |
| `\w`            | the current working directory, with \$HOME abbreviated with a tilde                                                                                                              |
| `\W`            | the basename of the current working directory, with \$HOME abbreviated with a tilde                                                                                              |
| `\!`            | the history number of this command                                                                                                                                               |
| `\#`            | the command number of this command                                                                                                                                               |
| `\$`            | if the effective UID is 0, a #, otherwise a \$                                                                                                                                   |
| `\nnn*`         | the character corresponding to the octal number nnn                                                                                                                              |
| `\`|a backslash |
| `\[`            | begin a sequence of non-printing characters, which could be used to embed a terminal control sequence into the prompt                                                            |
| `\]`            | end a sequence of non-printing characters                                                                                                                                        |

So for example, we can set PS1 to:

```bash
PS1="\u@\h:\w\$ "

```

And it will output:

> user@machine:~\$

## Show a git branch using PROMPT_COMMAND

If you are inside a folder of a git repository it might be nice to show the current branch you are on. In `~/.bashrc` or `/etc/bashrc` add the following (git is required for this to work):

```bash
function prompt_command {
    # Check if we are inside a git repository
    if git status > /dev/null 2>&1; then
        # Only get the name of the branch
        export GIT_STATUS=$(git status | grep 'On branch' | cut -b 10-)
    else
        export GIT_STATUS=""
    fi
}
# This function gets called every time PS1 is shown
PROMPT_COMMAND=prompt_command

PS1="\$GIT_STATUS \u@\h:\w\$ "

```

If we are in a folder inside a git repository this will output:

> branch user@machine:~\$

And if we are inside a normal folder:

> user@machine:~\$

## Show time in terminal prompt

```bash
timeNow(){
    echo "$(date +%r)"
}
PS1='[$(timeNow)] \u@\h:\w$ '

```

It will give you a prompt like this:

```bash
[05:34:37 PM] user@Host:/path$

```

**Notes:**

- Make the changes in `~/.bashrc` or `/etc/bashrc` or `~/.bash_profile` or `~./profile` file (depending on the OS) and save it.
- Run `source ~/.bashrc` (distro specific) after saving the file.

## Show previous command return status and time

Sometimes we need a visual hint to indicate the return status of previous command. The following snippet make put it at the head of the PS1.

Note that the \_\_stat() function should be called every time a new PS1 is generated, or else it would stick to the return status of last command of your .bashrc or .bash_profile.

```bash
# -ANSI-COLOR-CODES- #
Color_Off="\033[0m"
###-Regular-###
Red="\033[0;31m"
Green="\033[0;32m"
Yellow="\033[0;33m"
####-Bold-####

function __stat() {
    if [ $? -eq 0 ]; then
        echo -en "$Green ✔ $Color_Off "
    else
        echo -en "$Red ✘ $Color_Off "
    fi
}

PS1='$(__stat)'
PS1+="[\t] "
PS1+="\e[0;33m\u@\h\e[0m:\e[1;34m\w\e[0m \n$ "

export PS1

```

[<img src="http://i.stack.imgur.com/ZjEQ5.png" alt="enter image description here" />](http://i.stack.imgur.com/ZjEQ5.png)
