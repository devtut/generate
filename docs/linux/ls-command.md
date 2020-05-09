---
metaTitle: "Linux - ls command"
description: "Options for ls command, ls command with most used options."
---

# ls command



## Options for ls command


**Full list of options:**

`ls -a`     list all files including hidden file starting with '.'

`ls --color`     colored list [=always/never/auto]

`ls -d`     list directories - with ' */'

`ls -F`     add one char of */=>@| to enteries

`ls -i`     list file's inode index number

`ls -l`     list with long format - show permissions

`ls -la`     list long format including hidden files

`ls -lh`     list long format with readable file size

`ls -ls`     list with long format with file size

`ls -r`     list in reverse order

`ls -R`     list recursively directory tree

`ls -s`     list file size

`ls -S`     sort by file size

`ls -t`     sort by time & date

`ls -X`     sort by extension name



## ls command with most used options.


ls shows files and directories in present working directory. (if no arguments are passed.) (It doesn't show hidden files which starts with . by default.)

```bash
user@ubuntu14:/usr$ ls
bin  games  include  lib  lib32  local  sbin  share  src

```

To see all files (hidden files/folders also).
Use `ls -a` OR `ls -all`

```bash
user@ubuntu14:/usr$ ls -a
.  ..  bin  games  include  lib  lib32  local  sbin  share  src

```

To differentiate between files and folders and symbolic links and other, use `ls -F` OR `ls --classify`

```bash
user@ubuntu14:~$ ls -F
bash_profile_course  chat_apps/      Desktop/    Downloads/    foxitsoftware/   
Public/     test/    bin/    ClionProjects/  Documents/    IDE/    Music/
Pictures/  Templates/  Videos/

```

Here, ending characters are used to distinguish files and folders.

“/” suggest directory.

“*”suggest executables.

“@” suggest symbolic links.

To get more details about the files and directories, use `ls -l`

```bash
user@ubuntu14:~/example$ ls -l
total 6464

-rw-r--r-- 1 dave dave      41 Dec 24 12:19 Z.txt
drwxr-xr-x 2 user group    4096 Dec 24 12:00 a_directory
-rw-r--r-- 1 user group       6 Dec 24 12:01 a_file
lrwxrwxrwx 1 user group       6 Dec 24 12:04 a_link -> a_file
-rw-r--r-- 1 user group       6 Dec 24 12:03 a_newer_file
-rw-r----- 1 user group 6586816 Dec 24 12:07 big.zip

```

In this example, the total size of the contents is 6460KB.

Then there is an entry for each file/directory in alphabetical order with upper case before lower case.

The first character is the type (e.g. d - directory, l - link).

The next 9 characters show the permissions for the user, group and other.

This is followed by the number of hard links, then the owner's name and group.

The next field is the size in bytes.  This can be displayed in a human friendly form by adding the `-h` option e.g. 6586816 is displayed as 6.3M

There then follows a timestamp (usually the modification time).

The final field is the name. Note: links also show the target of the link.

