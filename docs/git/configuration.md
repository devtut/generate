---
metaTitle: "Configuration"
description: "Setting which editor to use, Auto correct typos, Username and email address, Multiple git configurations, List and edit the current configuration, Multiple usernames and email address, Configuring line endings, configuration for one command only, Setup a proxy"
---

# Configuration

## Setting which editor to use

There are several ways to set which editor to use for committing, rebasing, etc.

<li>
Change the `core.editor` configuration setting.

```git
$ git config --global core.editor nano

```

</li>
<li>
Set the `GIT_EDITOR` environment variable.
For one command:

```git
$ GIT_EDITOR=nano git commit

```

Or for all commands run in a terminal. **Note:** This only applies until you close the terminal.

```git
$ export GIT_EDITOR=nano

```

</li>
<li>
To change the editor for **all** terminal programs, not just Git, set the `VISUAL` or `EDITOR` environment variable. (See [`VISUAL` vs `EDITOR`](https://unix.stackexchange.com/questions/4859/visual-vs-editor-whats-the-difference).)

```git
$ export EDITOR=nano

```

**Note:** As above, this only applies to the current terminal; your shell will usually have a configuration file to allow you to set it permanently. (On `bash`, for example, add the above line to your `~/.bashrc` or `~/.bash_profile`.)

</li>

Some text editors (mostly GUI ones) will only run one instance at a time, and generally quit if you already have an instance of them open. If this is the case for your text editor, Git will print the message `Aborting commit due to empty commit message.` without allowing you to edit the commit message first. If this happens to you, consult your text editor's documentation to see if it has a `--wait` flag (or similar) that will make it pause until the document is closed.

## Auto correct typos

```git
git config --global help.autocorrect 17

```

This enables autocorrect in git and will forgive you for your minor mistakes (e.g. `git stats` instead of `git status`). The parameter you supply to `help.autocorrect` determines how long the system should wait, in tenths of a second, before automatically applying the autocorrected command. In the command above, 17 means that git should wait 1.7 seconds before applying the autocorrected command.

However, bigger mistakes will be considered as missing commands, so typing something like `git testingit` would result in `testingit is not a git command.`

## Username and email address

Right after you install Git, the first thing you should do is set your username and email address. From a shell, type:

```git
git config --global user.name "Mr. Bean"
git config --global user.email mrbean@example.com

```

- `git config` is the command to get or set options
- `--global` means that the configuration file specific to your user account will be edited
- `user.name` and `user.email` are the keys for the configuration variables; `user` is the section of the configuration file. `name` and `email` are the names of the variables.
- `"Mr. Bean"` and `mrbean@example.com` are the values that you're storing in the two variables. Note the quotes around `"Mr. Bean"`, which are required because the value you are storing contains a space.

## Multiple git configurations

You have up to 5 sources for git configuration:

<li>6 files:
<ul>
- **`%ALLUSERSPROFILE%\Git\Config`** (Windows only)
<li>(system) **`<git>/etc/gitconfig`**, with `<git>` being the git installation path.<br />
(on Windows, it is **`<git>\mingw64\etc\gitconfig`**)</li>
- (system) **`$XDG_CONFIG_HOME/git/config`** (Linux/Mac only)
- (global) `~/.gitconfig` (Windows: `%USERPROFILE%\.gitconfig`)
- (local) `.git/config` (within a git repo `$GIT_DIR`)
- a **dedicated file** (with `git config -f`), used for instance to modify the config of submodules: `git config -f .gitmodules ...`

The order is important: any config set in one source can be overridden by a source listed below it.

`git config --system/global/local` is the command to list 3 of those sources, but only git config -l would list **all** **resolved** configs.<br />
"resolved" means it lists only the final overridden config value.

Since git 2.8, if you want to see which config comes from which file, you type:

```git
git config --list --show-origin

```

## List and edit the current configuration

Git config allows you to customize how git works. It is commonly used to set your name and email or favorite editor or how merges should be done.

To see the current configuration.

```git
$ git config --list
...
core.editor=vim
credential.helper=osxkeychain
...

```

To edit the config:

```git
$ git config <key> <value>
$ git config core.ignorecase true

```

If you intend the change to be true for all your repositories, use `--global`

```git
$ git config --global user.name "Your Name"
$ git config --global user.email "Your Email"
$ git config --global core.editor vi

```

You can list again to see your changes.

## Multiple usernames and email address

Since Git 2.13, multiple usernames and email addresses could be configured by using a folder filter.

### Example for Windows:

### .gitconfig

Edit: `git config --global -e`

Add:

```git
[includeIf "gitdir:D:/work"]
  path = .gitconfig-work.config

[includeIf "gitdir:D:/opensource/"]
  path = .gitconfig-opensource.config

```

- The order is depended, the last one who matches "wins".
- the `/` at the end is needed - e.g. `"gitdir:D:/work"` won't work.
- the `gitdir:` prefix is required.

### .gitconfig-work.config

File in the same directory as **.gitconfig**

```git
[user]
  name = Money
  email = work@somewhere.com

```

### .gitconfig-opensource.config

File in the same directory as **.gitconfig**

```git
[user]
  name = Nice
  email = cool@opensource.stuff

```

### Example for Linux

```git
[includeIf "gitdir:~/work/"]
  path = .gitconfig-work
[includeIf "gitdir:~/opensource/"]
  path = .gitconfig-opensource

```

The file content and notes under section Windows.

## Configuring line endings

### Description

When working with a team who uses different operating systems (OS) across the project, sometimes you may run into trouble when dealing with line endings.

### Microsoft Windows

When working on Microsoft Windows operating system (OS), the line endings are normally of form - carriage return + line feed (CR+LF). Opening a file which has been edited using Unix machine such as Linux or OSX may cause trouble, making it seem that text has no line endings at all. This is due to the fact that Unix systems apply different line-endings of form line feeds (LF) only.

In order to fix this you can run following instruction

```git
git config --global core.autocrlf=true

```

On **checkout**, This instruction will ensure line-endings are configured in accordance with Microsoft Windows OS (LF -> CR+LF)

### Unix Based (Linux/OSX)

Similarly, there might be issues when the user on Unix based OS tries to read files which have been edited on Microsoft Windows OS. In order to prevent any unexpected issues run

```git
git config --global core.autocrlf=input

```

On **commit**, this will change line-endings from CR+LF -> +LF

## configuration for one command only

you can use `-c <name>=<value>` to add a configuration only for one command.

To commit as an other user without having to change your settings in .gitconfig :

```git
git -c user.email = mail@example commit -m "some message"

```

Note: for that example you don't need to precise both `user.name` and `user.email`, git will complete the missing information from the previous commits.

## Setup a proxy

If you are behind a proxy, you have to tell git about it:

```git
git config --global http.proxy http://my.proxy.com:portnumber

```

If you are no more behind a proxy:

```git
git config --global --unset http.proxy

```

#### Syntax

- git config [<file-option>] name [value] # one of the more common use cases of git config

#### Parameters

| Parameter  | Details                                                                                                                               |
| ---------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| `--system` | Edits the system-wide configuration file, which is used for every user (on Linux, this file is located at `$(prefix)/etc/gitconfig`)  |
| `--global` | Edits the global configuration file, which is used for every repository you work on (on Linux, this file is located at `~/.gitconfig` |
| `--local`  | Edits the respository-specific configuration file, which is located at `.git/config` in your repository; this is the default setting  |
