---
metaTitle: "Type of Shells"
description: "Introduction to dot files, Start an interactive shell, Detect type of shell"
---

# Type of Shells



## Introduction to dot files


In Unix, files and directories beginning with a period usually contain settings for a specific program/a series of programs. Dot files are usually hidden from the user, so you would need to run `ls -a` to see them.

An example of a dot file is [`.bash_history`](http://unix.stackexchange.com/a/145254), which contains the latest executed commands, assuming the user is using Bash.

There are various files that are [sourced](http://superuser.com/a/46146) when you are dropped into the Bash shell. The image below, taken from [this site](http://www.solipsys.co.uk/new/BashInitialisationFiles.html), shows the decision process behind choosing which files to source at startup.

[<img src="https://i.stack.imgur.com/ihulP.png" alt="Decision making tree for sourcing files" />](https://i.stack.imgur.com/ihulP.png)



## Start an interactive shell


```bash
bash

```



## Detect type of shell


```bash
shopt -q login_shell && echo 'login' || echo 'not-login'

```



#### Remarks


**Login Shell**

A login shell is one whose first character of argument zero is a -, or one started with the –login option.
The Initialization is more comprehensive than in an normal interactive (sub) shell.

**Interactive Shell**

An interactive shell is one started without non-option arguments and without the -c option whose standard input and error are both connected to terminals (as determined by isatty(3)), or one started with the -i option. PS1 is set and $- includes i if bash is interactive, allowing a shell script or a startup file to test this state.

**non-interactive Shell**

A non-interactive Shell is a shell in which the user can not interact with the shell. As en example, a shell running a script is always a non-interactive shell. All the same, the script can still access its tty.

**Configuring a login shell**

On logging in:

```bash
If '/etc/profile' exists, then source it. 
If '~/.bash_profile' exists, then source it, 
else if '~/.bash_login' exists, then source it, 
else if '~/.profile' exists, then source it. 

```

**For non-login interactive shells**

On starting up:

```bash
If `~/.bashrc' exists, then source it.

```

**For non-interactive shells**

On starting up:
If the environment variable ENV is non-null, expand the variable and source the file named by the value. If Bash is not started in Posix mode, it looks for BASH_ENV before ENV.

