---
metaTitle: "Linux - Shell"
description: "Changing default shell, Basic Shell Utilities, Create Your Own Command Alias, Locate a file on your system"
---

# Shell


The shell executes a program in response to its prompt. When you give a command, the shell searches for the program, and then executes it. For example, when you give the command ls, the shell searches for the utility/program named ls, and then runs it in the  shell. The arguments and the options that you provide with the utilities can impact the result that you get. The shell is also known as a CLI, or command line interface.



## Changing default shell


Most modern distributions will come with BASH (**B**ourne **A**gain **SH**ell) pre-installed and configured as a default shell.

The command (actually an executable binary, an ELF) that is responsible for changing shells in Linux is `chsh` (**ch**ange **sh**ell).

We can first check which shells are already installed and configured on our machine by using the `chsh -l` command, which will output a result similar to this:

```bash
[user@localhost ~]$ chsh -l
/bin/sh
/bin/bash
/sbin/nologin
/usr/bin/sh
/usr/bin/bash
/usr/sbin/nologin
/usr/bin/fish

```

In some Linux distributions, `chsh -l` is invalid. In this case, the list of all available shells can be found at /etc/shells file. You can show the file contents with `cat`:

```bash
[user@localhost ~]$ cat /etc/shells
# /etc/shells: valid login shells
/bin/sh
/bin/bash
/sbin/nologin
/usr/bin/sh
/usr/bin/bash
/usr/sbin/nologin
/usr/bin/fish

```

Now we can choose our new default shell, e.g. `fish`, and configure it by using `chsh -s`,

```bash
[user@localhost ~]$ chsh -s /usr/bin/fish
Changing shell for user.
Password: 
Shell changed.

```

Now all that is left to do is preform a logoff-logon cycle, and enjoy our new default shell.

If you wish to change the default shell for a different user, and you have administrative privileges on the machine, you'll be able to accomplish this by using `chsh` as `root`. So assuming we want to change `user_2`'s default shell to  fish, we will use the same command as before, but with the addition of the other user's username, `chsh -s /usr/bin/fish user_2`.

In order to check what the current default shell is, we can view the `$SHELL` environment variable, which points to the path to our default shell, so after our change, we would expect to get a result similar to this,

```

~  echo $SHELL                                 
/usr/bin/fish

```

### `chsh` options:

`-s shell`

Sets shell as the login shell.

`-l`, `--list-shells`

Print the list of shells listed in /etc/shells and exit.

`-h`, `--help`

Print a usage message and exit.

`-v`, `--version`

Print version information and exit.



## Basic Shell Utilities


### Customizing the Shell prompt

Default command prompt can be changed to look different and short. In case the current directory is long default command prompt becomes too large. Using `PS1` becomes useful in these cases. A short and customized command pretty and elegant. In the table below `PS1` has been used with a number of arguments to show different forms of shell prompts.
Default command prompt looks something like this: `user@host ~ $` in my case it looks like this: `bruce@gotham ~ $`. It can changed as per the table below:

|Command|Utility
|---|---|---|---|---|---|---|---|---|---
|PS1='\w $ '|`~ $ ` shell prompt as directory name. In this case root directory is Root.
|PS1='\h $ '|`gotham $ ` shell prompt as hostname
|PS1='\u $ '|`bruce $ ` shell prompt as username
|PS1='\t $ '|`22:37:31 $ ` shell prompt in 24 hour format
|PS1='@ $ '|`10:37 PM ` shell prompt in 12 hour time format
|PS1='! $ '|`732 ` will show the history number of command in place of shell prompt
|PS1='dude $ '|`dude $ ` will show the shell prompt the way you like

### Some basic shell commands

|Command|Utility
|---|---|---|---|---|---|---|---|---|---
|`Ctrl-k`|cut/kill
|`Ctrl-y`|yank/paste
|`Ctrl-a`|will take cursor to the start of the line
|`Ctrl-e`|will take cursor to the end of the line
|`Ctrl-d`|will delete the character after/at the cursor
|`Ctrl-l`|will clear the screen/terminal
|`Ctrl-u`|will clear everything between prompt and the cursor
|`Ctrl-_`|will undo the last thing typed on the command line
|`Ctrl-c`|will interrupt/stop the job/process running in the foreground
|`Ctrl-r`|reverse search in history
|`~/.bash_history`|stores last 500 commands/events used on the shell
|`history`|will show the command history
|`history | grep <key-word> `|will show all the commands in history having keyword <key-word> (useful in cases when you remember part of the command used in the past)



## Create Your Own Command Alias


If you are tired of using long commands in bash you can create your own command alias.

The best way to do this is to modify (or create if it does not exist) a file called .bash_aliases in your home folder. The general syntax is:

```bash
alias command_alias='actual_command'

```

where `actual_command` is the command you are renaming and `command_alias` is the new name you have given it. For example

```bash
alias install='sudo apt-get -y install'

```

maps the new command alias `install` to the actual command `sudo apt-get -y install`. This means that when you use **install** in a terminal this is interpreted by bash as **sudo apt-get -y install**.



## Locate a file on your system


Using bash you can easily locate a file with the `locate` command. For example say you are looking for the file mykey.pem:

```bash
locate mykey.pem

```

Sometimes files have strange names for example you might have a file like `random7897_mykey_0fidw.pem`. Let's say you're looking for this file but you only remember the mykey and pem parts. You could combine the `locate` command with `grep` using a pipe like this:

```bash
locate pem | grep mykey

```

Which would bring up all results which contain both of these pieces.

Note that not all systems have the `locate` utility installed, and many that do have not enabled it. `locate` is fast and efficient because it periodically scans your system and caches the names and locations for every file on it, but if that data collection is not enabled then it cannot tell you anything. You can use `updatedb` to manually initiate the filesystem scan in order to update the cached info about files on your filesystem.

Should you not have a working `locate`, you can fall back on the `find` utility:

```bash
find / -name mykey.pem -print

```

is roughly equivalent to `locate mykey.pem` but has to scan your filesystem(s) each time you run it for the file in question, rather than using cached data. This is obviously slower and less efficient, but more real-time.  The `find` utility can do much more than find files, but a full description of its capabilities is beyond the scope of this example.

