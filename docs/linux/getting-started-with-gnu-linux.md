---
metaTitle: "Linux - Getting started with GNU/Linux"
description: "Useful shortcuts, File Management Commands, Hello World, Basic Linux Utilities, Searching for files by patterns in name/contents, File Manipulation, File/Directory details"
---

# Getting started with GNU/Linux



## Useful shortcuts


### Using The Terminal

> 
The examples in this document assume that you are using a POSIX-compliant (such as **bash**, **sh**, **zsh**, **ksh**) shell.


Large portions of GNU/Linux functionality are achieved using the terminal. Most distributions of Linux include terminal emulators that allow users to interact with a shell from their desktop environment. A shell is a command-line interpreter that executes user inputted commands. **Bash** (Bourne Again SHell) is a common default shell among many Linux distributions and is the default shell for macOS.

These shortcuts will work if you are using **Bash** with the **emacs** keybindings (set by default):

### Open terminal

- <kbd> Ctrl + Alt + T</kbd> or  <kbd> Super + T</kbd>

### Cursor movement

- <kbd>Ctrl + A</kbd>   Go to the beginning of the line you are currently typing on.
- <kbd>Ctrl + E</kbd>   Go to the end of the line you are currently typing on.
- <kbd>Ctrl + XX</kbd>  Move between the beginning of the line and the current position of the cursor.
- <kbd>Alt + F</kbd>    Move cursor forward one word on the current line.
- <kbd>Alt + B</kbd>    Move cursor backward one word on the current line.
- <kbd>Ctrl + F</kbd>   Move cursor forward one character on the current line.
- <kbd>Ctrl + B</kbd>   Move cursor backward one character on the current line.

### Text manipulation

- <kbd>Ctrl + U</kbd>   Cut the line from the current position to the beginning of the line, adding it to the clipboard. If you are at the end of the line, cut the entire line.
- <kbd>Ctrl + K</kbd>   Cut the line from the current position to the end of the line, adding it to the clipboard. If you are at the beginning of the line, cut the entire line.
- <kbd>Ctrl + W</kbd>   Delete the word before the cursor, adding it to the clipboard.
- <kbd>Ctrl + Y</kbd>   Paste the last thing from the clipboard that you cut recently (undo the last delete at the **current** cursor position).
- <kbd>Alt + T</kbd>    Swap the last two words before the cursor.
- <kbd>Alt + L</kbd>    Make lowercase from cursor to end of word.
- <kbd>Alt + U</kbd>    Make uppercase from cursor to end of word.
- <kbd>Alt + C</kbd>    Capitalize to end of word starting at cursor (whole word if cursor is at the beginning of word).
- <kbd>Alt + D</kbd>    Delete to end of word starting at cursor (whole word if cursor is at the beginning of word).
- <kbd>Alt + .</kbd>    Prints the last word written in previous command.
- <kbd>Ctrl + T</kbd>   Swap the last two characters before the cursor.

### History access

- <kbd>Ctrl + R</kbd>     Lets you search through previously used commands.
- <kbd>Ctrl + G</kbd>     Leave history searching mode without running a command.
- <kbd>Ctrl + J</kbd>     Lets you copy current matched command to command line without running it, allowing you to make modifications before running the command.
- <kbd>Alt + R</kbd>      Revert any changes to a command you’ve pulled from your history, if you’ve edited it.
- <kbd>Ctrl + P</kbd>     Shows last executed command, i.e. walk back through the command history (Similar to up arrow).
- <kbd>Ctrl + N</kbd>     Shows next executed command, i.e. walk forward through the command history (Similar to down arrow).

### Terminal control

- <kbd>Ctrl + L</kbd>     Clears the screen, similar to the clear command.
- <kbd>Ctrl + S</kbd>     Stop all output to the screen. This is useful when running commands with lots of long output. But this doesn't stop the running command.
- <kbd>Ctrl + Q</kbd>     Resume output to the screen after stopping it with Ctrl+S.
- <kbd>Ctrl + C</kbd>     End currently running process and return the prompt.
- <kbd>Ctrl + D</kbd>     Log out of the current shell session, similar to the exit or logout command. In some commands, acts as End of File signal to indicate that a file end has been reached.
- <kbd>Ctrl + Z</kbd>     Suspends (pause) currently running foreground process, which returns shell prompt. You can then use `bg` command allowing that process to run in the background. To again bring that process to foreground, use `fg` command. To view all background processes, use `jobs` command.
- <kbd>Tab</kbd>     Auto-complete files and directory names.
- <kbd>Tab</kbd><kbd>Tab</kbd> Shows all possibilities, when typed characters doesn't uniquely match to a file or directory name.

### Special characters

- <kbd>Ctrl + H</kbd>   Same as Backspace.
- <kbd>Ctrl + J</kbd>   Same as Return (historically Line Feed).
- <kbd>Ctrl + M</kbd>   Same as Return (historically Carriage Return).
- <kbd>Ctrl + I</kbd>   Same as Tab.
- <kbd>Ctrl + G</kbd>   Bell Character.
- <kbd>Ctrl + @</kbd>   Null Character.
- <kbd>Esc</kbd> [Deadkey](https://en.wikipedia.org/wiki/Dead_key) equivalent to the <kbd>Alt</kbd> modifier.

### Close Terminal

- <kbd> Ctrl + Shift + W </kbd> To close terminal tab.
- <kbd> Ctrl + Shift + Q </kbd> To close entire terminal.

Alternatively, you can switch to the **vi** keybindings in **bash** using `set -o vi`. Use `set -o emacs` to switch back to the **emacs** keybindings.



## File Management Commands


Linux uses some conventions for present and parent directories. This can be a little confusing for beginners.

Whenever you are in a terminal in Linux, you will be in what is called the **current working directory**. Often your command prompt will display either the full working directory, or just the last part of that directory. Your prompt could look like one of the following:

```bash
user@host ~/somedir $
user@host somedir $
user@host /home/user/somedir $

```

which says that your current working directory is `/home/user/somedir`.

In Linux ` .. ` represents the parent directory and
` . ` represents the current directory.

Therefore, if the current directory is `/home/user/somedir`, then `cd ../somedir` will not change the working directory.

The table below lists some of the most used file management commands

### Directory navigation

|Command|Utility
|---|---|---|---|---|---|---|---|---|---
|`pwd`|Get the full path of the current working directory.
|`cd -`|Navigate to the last directory you were working in.
|`cd ~` or just `cd`|Navigate to the current user's home directory.
|`cd ..`|Go to the parent directory of current directory (mind the space between `cd` and `..`)

### Listing files inside a directory

|Command|Utility
|---|---|---|---|---|---|---|---|---|---
|`ls -l`|List the files and directories in the current directory in long (table) format (It is recommended to use -l with ls for better readability).
|`ls -ld dir-name`|List information about the directory `dir-name` instead of its contents.
|`ls -a`|List all the files including the hidden ones (File names starting with a `.` are hidden files in Linux).
|`ls -F`|Appends a symbol at the end of a file name to indicate its type (`*` means executable, `/` means directory, `@` means symbolic link, `=` means socket, | means named pipe, `>` means door).
|`ls -lt`|List the files sorted by last modified time with most recently modified files showing at the top (remember -l option provides the long format which has better readability).
|`ls -lh`|List the file sizes in human readable format.
|`ls -lR`|Shows all subdirectories recursively.
|`tree`|Will generate a tree representation of the file system starting from the current directory.

### File/directory create, copy and remove

|Command|Utility
|---|---|---|---|---|---|---|---|---|---
|`cp -p source destination`|Will copy the file from `source` to **destination**. -p stands for preservation. It preserves the original attributes of file while copying like file owner, timestamp, group, permissions etc.
|`cp -R source_dir destination_dir`|Will copy source directory to specified destination recursively.
|`mv file1 file2`|In Linux there is no **rename** command as such. Hence `mv` moves/renames the file1 to file2.
|`rm -i filename`|Asks you before every file removal for confirmation. **IF YOU ARE A NEW USER TO LINUX COMMAND LINE, YOU SHOULD ALWAYS USE `rm -i`.** You can specify multiple files.
|`rm -R dir-name`|Will remove the directory `dir-name` recursively.
|`rm -rf dir-name`|Will remove the directory `dir` recursively, ignoring non-existent files and will never prompt for anything. **BE CAREFUL USING THIS COMMAND!** You can specify multiple directories.
|`rmdir dir-name`|Will remove the directory `dir-name`, if it's empty. This command can only remove empty directories.
|`mkdir dir-name`|Create a directory `dir-name`.
|`mkdir -p dir-name/dir-name`|Create a directory hierarchy.  Create parent directories as needed, if they don't exist. You can specify multiple directories.
|`touch filename`|Create a file `filename`, if it doesn't exist, otherwise change the timestamp of the file to current time.

### File/directory permissions and groups

|Command|Utility
|---|---|---|---|---|---|---|---|---|---
|`chmod <specification> filename`|Change the file permissions. Specifications = `u` user, `g` group, `o` other, `+` add permission, `-` remove, `r` read, `w` write,`x` execute.
|`chmod -R <specification> dir-name`|Change the permissions of a directory recursively. To change permission of a directory and everything within that directory, use this command.
|`chmod go=+r myfile`|Add read permission for the owner and the group.
|`chmod a +rwx myfile`|Allow all users to read, write or execute `myfile`.
|`chmod go -r myfile`|Remove read permission from the group and others.
|`chown owner1 filename`|Change ownership of a file to user `owner1`.
|`chgrp grp_owner filename`|Change primary group ownership of file `filename` to group `grp_owner`.
|`chgrp -R grp_owner dir-name`|Change primary group ownership of directory `dir-name` to group `grp_owner` recursively. To change group ownership of a directory and everything within that directory, use this command.



## Hello World


Type the following code into your terminal, then press <kbd>Enter</kbd>:

```bash
echo "Hello World"

```

This will produce the following output:

```bash
Hello World

```



## Basic Linux Utilities


Linux has a command for almost any tasks and most of them are intuitive and easily interpreted.

**Getting Help in Linux**

|Command|Usability
|---|---|---|---|---|---|---|---|---|---
|`man <name>`|Read the manual page of <name>.
|`man <section> <name>`|Read the manual page of <name>, related to the given section.
|`man -k <editor>`|Output all the software whose man pages contain <editor> keyword.
|`man -K <keyword>`|Outputs all man pages containing <keyword> within them.
|`apropos <editor>`|Output all the applications whose one line description matches the word **editor**. When **not able to recall** the name of the application, use this command.
|`help`|In Bash shell, this will display the list of all available bash commands.
|`help <name>`|In Bash shell, this will display the info about the <name> bash command.
|`info <name>`|View all the information about <name>.
|`dpkg -l`|Output a list of all installed packages on a Debian-based system.
|`dpkg -L packageName`|Will list out the files installed and path details for a given package on Debian.
|`dpkg -l | grep -i <edit>`|Return all .deb installed packages with <edit> irrespective of cases.
|`less /var/lib/dpkg/available`|Return descriptions of all available packages.
|`whatis vim`|List a one-line description of vim.
|`<command-name> --help`|Display usage information about the <tool-name>. Sometimes `command -h` also works, but not for all commands.

**User identification and who is who in Linux world**

|Command|Usability
|---|---|---|---|---|---|---|---|---|---
|`hostname`|Display hostname of the system.
|`hostname -f`|Displays Fully Qualified Domain Name (FQDN) of the system.
|`passwd`|Change password of current user.
|`whoami`|Username of the users logged in at the terminal.
|`who`|List of all the users currently logged in as a user.
|`w`|Display current system status, time, duration, list of users currently logged in on system and other user information.
|`last`|Who recently used the system.
|`last root`|When was the last time **root** logged in as user.
|`lastb`|Shows all bad login attempts into the system.
|`chmod`|Changing permissions - read,write,execute of a file or directory.

**Process related information**

|Command|Usability
|---|---|---|---|---|---|---|---|---|---
|`top`|List all processes sorted by their current system resource usage. Displays a continually updated display of processes (By default 3 seconds). Use `q` key to exit top.
|`ps`|List processes currently running on current shell session
|`ps -u root`|List all of the processes and commands root is running
|`ps aux`|List all the processes by all users on the current system



## Searching for files by patterns in name/contents


A common and task of someone using the Linux Command Line (shell) is to search for files/directories with a certain name or containing certain text.  There are 2 commands you should familiarise yourself with in order to accomplish this:

### Find files by name

```bash
find /var/www -name '*.css'

```

This will print out the full path/filename to all files under `/var/www` that end in `.css`.  Example output:

```bash
/var/www/html/text-cursor.css
/var/www/html/style.css

```

For more info:

```bash
man find

```

### Find files containing text

```bash
grep font /var/www/html/style.css 

```

This will print all lines containing the pattern `font` in the specified file.  Example output:

```bash
font-weight: bold;
font-family: monospace;

```

Another example:

```bash
grep font /var/www/html/

```

This doesn't work as you'd hoped.  You get:

```bash
grep: /var/www/html/: Is a directory

```

You need to `grep` recursively to make it work, using the `-R` option:

```bash
grep -R font /var/www/html/

```

Hey nice!  Check out the output of this one:

```bash
/var/www/html/admin/index.php:  echo '<font color=red><b>Error: no dice</b></font><br/>';
/var/www/html/admin/index.php:  echo '<font color=red><b>Error: try again</b></font><br/>';
/var/www/html/style.css:  font-weight: bold;
/var/www/html/style.css:  font-family: monospace;

```

Notice that when `grep` is matching multiple files, it prefixes the matched lines with the filenames.  You can use the `-h` option to get rid of that, if you want.

For more info:

```bash
man grep

```



## File Manipulation


Files and directories (another name for folders) are at the heart of Linux, so being able to create, view, move, and delete them from the command line is very important and quite powerful. These file manipulation commands allow you to perform the same tasks that a graphical file explorer would perform.

Create an empty text file called `myFile`:

```bash
touch myFile

```

Rename `myFile` to `myFirstFile`:

```bash
mv myFile myFirstFile 

```

View the contents of a file:

```bash
cat myFirstFile

```

View the content of a file with pager (one screenful at a time):

```bash
less myFirstFile

```

View the first several lines of a file:

```bash
head myFirstFile

```

View the last several lines of a file:

```bash
tail myFirstFile

```

Edit a file:

```bash
vi myFirstFile

```

See what files are in your current working directory:

```bash
ls

```

Create an empty directory called `myFirstDirectory`:

```bash
mkdir myFirstDirectory

```

Create multi path directory: (creates two directories, src and myFirstDirectory)

```bash
mkdir -p src/myFirstDirectory

```

Move the file into the directory:

```bash
mv myFirstFile myFirstDirectory/

```

You can also rename the file:

```bash
user@linux-computer:~$ mv myFirstFile secondFileName

```

Change the current working directory to `myFirstDirectory`:

```bash
cd myFirstDirectory

```

Delete a file:

```bash
rm myFirstFile

```

Move into the parent directory (which is represented as `..`):

```bash
cd ..

```

Delete an empty directory:

```bash
rmdir myFirstDirectory

```

Delete a non-empty directory (i.e. contains files and/or other directories):

```bash
rm -rf myFirstDirectory

```

**Make note that when deleting directories, that you delete `./` not `/` that will wipe your whole filesystem.**



## File/Directory details


The `ls` command has several options that can be used together to show more information.

**Details/Rights**

The `l` option shows the file permissions, size, and last modified date. So if the root directory contained a dir called `test` and a file `someFile` the command:

```bash
user@linux-computer:~$ ls -l

```

Would output something like

```bash
-rw-r--r-- 1 user users   70 Jul 22 13:36 someFile.txt
drwxrwxrwx 2 user users 4096 Jul 21 07:18 test

```

The permissions are in format of `drwxrwxrwx`. The first character represents the file type `d` if it's a directory `-` otherwise. The next three `rwx` are the permissions the user has over the file, the next three are the permissions the group has over the file, and the last three are the permissions everyone else has over the file.

The `r` of `rwx` stands for if a file can be read, the `w` represents if the file can be modified, and the `x` stands for if the file can be executed. If any permission isn't granted a `-` will be in place of `r`, `w`, or `x`.

So from above `user` can read and modify `someFile.txt` but the group has only read-only rights.

To change rights you can use the `chmod ### fileName` command if you have sudo rights. `r` is represented by a value of 4, `w` is represented by 2, and `x` is represented by a 1.
So if only you want to be able to modify the contents to the `test` directory

```bash
Owner rwx = 4+2+1 = 7
Group r-x = 4+0+1 = 5
Other r-x = 4+0+1 = 5

```

So the whole command is

```bash
chmod 755 test

```

Now doing a `ls -l` would show something like

```bash
drwxr-xr-x 2 user users 4096 Jul 21 07:20 test

```

**Readable Size**

Used in conjunction with the `l` option the `h` option shows file sizes that are human readable. Running

```bash
user@linux-computer:~$ ls -lh

```

Would output:

```bash
total 4166
-rw-r--r-- 1 user users   70 Jul 22 13:36 someFile.txt
drwxrwxrwx 2 user users 4.0K Jul 21 07:18 test

```

**Hidden**

To view hidden files use the `a` option. For example

```bash
user@linux-computer:~$ ls -a

```

Might list

```bash
.profile
someFile.txt
test

```

**Total Directory Size**

To view the size of the current directory use the `s` option (the `h` option can also be used to make the size more readable).

```bash
user@linux-computer:~$ ls -s

```

Outputs

```bash
total 4166
someFile.txt      test

```

**Recursive View**

Lets say `test` directory had a file `anotherFile` and you wanted to see it from the root folder, you could use the `R` option which would list the recursive tree.

```bash
user@linux-computer:~$ ls -R

```

Outputs

```bash
.:
someFile.txt    test

./test:
anotherFile

```

