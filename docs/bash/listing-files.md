---
metaTitle: "Listing Files"
description: "List Files in a Long Listing Format, List the Ten Most Recently Modified Files, List All Files Including Dotfiles, List Files, List Files Without Using `ls`, List Files in a Tree-Like Format, List Files Sorted by Size"
---

# Listing Files

## List Files in a Long Listing Format

The `ls` command's `-l` option prints a specified directory's contents in a long listing format. If no directory is specified then, by default, the contents of the current directory are listed.

```bash
ls -l /etc

```

Example Output:

```bash
total 1204
drwxr-xr-x  3 root root    4096 Apr 21 03:44 acpi
-rw-r--r--  1 root root    3028 Apr 21 03:38 adduser.conf
drwxr-xr-x  2 root root    4096 Jun 11 20:42 alternatives
...

```

The output first displays `total`, which indicates the total size in **blocks** of all the files in the listed directory. It then displays eight columns of information for each file in the listed directory. Below are the details for each column in the output:

| Column No. | Example        | Description                 |
| ---------- | -------------- | --------------------------- |
| 1.1        | `d`            | File type (see table below) |
| 1.2        | `rwxr-xr-x`    | Permission string           |
| 2          | `3`            | Number of hard links        |
| 3          | `root`         | Owner name                  |
| 4          | `root`         | Owner group                 |
| 5          | `4096`         | File size in bytes          |
| 6          | `Apr 21 03:44` | Modification time           |
| 7          | `acpi`         | File name                   |

### File Type

The file type can be one of any of the following characters.

| Character | File Type                                      |
| --------- | ---------------------------------------------- |
| `-`       | Regular file                                   |
| `b`       | Block special file                             |
| `c`       | Character special file                         |
| `C`       | High performance ("contiguous data") file      |
| `d`       | Directory                                      |
| `D`       | Door (special IPC file in Solaris 2.5+ only)   |
| `l`       | Symbolic link                                  |
| `M`       | Off-line ("migrated") file (Cray DMF)          |
| `n`       | Network special file (HP-UX)                   |
| `p`       | FIFO (named pipe)                              |
| `P`       | Port (special system file in Solaris 10+ only) |
| `s`       | Socket                                         |
| `?`       | Some other file type                           |

## List the Ten Most Recently Modified Files

The following will list up to ten of the most recently modified files in the current directory, using a long listing format (`-l`) and sorted by time (`-t`).

```bash
ls -lt | head

```

## List All Files Including Dotfiles

A **dotfile** is a file whose names begin with a `.`. These are normally hidden by `ls` and not listed unless requested.

For example the following output of `ls`:

```bash
$ ls
bin  pki

```

The `-a` or `--all` option will list all files, including dotfiles.

```bash
$ ls -a
.   .ansible       .bash_logout   .bashrc  .lesshst  .puppetlabs  .viminfo
..  .bash_history  .bash_profile  bin      pki       .ssh

```

The `-A` or `--almost-all` option will list all files, including dotfiles, but does not list implied `.` and `..`. Note that `.` is the current directory and `..` is the parent directory.

```bash
$ ls -A
.ansible       .bash_logout   .bashrc  .lesshst  .puppetlabs  .viminfo
.bash_history  .bash_profile  bin      pki       .ssh

```

## List Files

The `ls` command lists the contents of a specified directory, **excluding** dotfiles. If no directory is specified then, by default, the contents of the current directory are listed.

Listed files are sorted alphabetically, by default, and aligned in columns if they don’t fit on one line.

```bash
$ ls
apt  configs  Documents  Fonts    Music      Programming  Templates  workspace
bin  Desktop  eclipse    git      Pictures   Public       Videos

```

## List Files Without Using `ls`

Use the Bash shell's [filename expansion](https://www.gnu.org/software/bash/manual/bashref.html#Filename-Expansion) and [brace expansion](https://www.gnu.org/software/bash/manual/bashref.html#Brace-Expansion) capabilities to obtain the filenames:

```bash
# display the files and directories that are in the current directory
printf "%s\n" *

# display only the directories in the current directory
printf "%s\n" */

# display only (some) image files
printf "%s\n" *.{gif,jpg,png}

```

To capture a list of files into a variable for processing, it is typically good practice to use a [bash array](https://www.gnu.org/software/bash/manual/bashref.html#Arrays):

```bash
files=( * )

# iterate over them
for file in "${files[@]}"; do
    echo "$file"
done

```

## List Files in a Tree-Like Format

The `tree` command lists the contents of a specified directory in a tree-like format. If no directory is specified then, by default, the contents of the current directory are listed.

Example Output:

```bash
$ tree /tmp
/tmp
├── 5037
├── adb.log
└── evince-20965
    └── image.FPWTJY.png

```

Use the `tree` command's `-L` option to limit the display depth and the `-d` option to only list directories.

Example Output:

```bash
$ tree -L 1 -d /tmp
/tmp
└── evince-20965

```

## List Files Sorted by Size

The `ls` command's `-S` option sorts the files in descending order of file size.

```bash
$ ls -l -S ./Fruits
total 444
-rw-rw-rw- 1 root root 295303 Jul 28 19:19 apples.jpg
-rw-rw-rw- 1 root root 102283 Jul 28 19:19 kiwis.jpg
-rw-rw-rw- 1 root root  50197 Jul 28 19:19 bananas.jpg

```

When used with the `-r` option the sort order is reversed.

```bash
$ ls -l -S -r /Fruits
total 444
-rw-rw-rw- 1 root root  50197 Jul 28 19:19 bananas.jpg
-rw-rw-rw- 1 root root 102283 Jul 28 19:19 kiwis.jpg
-rw-rw-rw- 1 root root 295303 Jul 28 19:19 apples.jpg

```

#### Syntax

- ls [OPTION]... [FILE]...

#### Parameters

| Option                   | Description                                            |
| ------------------------ | ------------------------------------------------------ |
| `-a`, `--all`            | List all entries including ones that start with a dot  |
| `-A`, `--almost-all`     | List all entries excluding `.` and `..`                |
| `-c`                     | Sort files by change time                              |
| `-d`, `--directory`      | List directory entries                                 |
| `-h`, `--human-readable` | Show sizes in human readable format (i.e. `K`, `M`)    |
| `-H`                     | Same as above only with powers of 1000 instead of 1024 |
| `-l`                     | Show contents in long-listing format                   |
| `-o`                     | Long -listing format without group info                |
| `-r`, `--reverse`        | Show contents in reverse order                         |
| `-s`, `--size`           | Print size of each file in blocks                      |
| `-S`                     | Sort by file size                                      |
| `--sort=WORD`            | Sort contents by a word. (i.e size, version, status)   |
| `-t`                     | Sort by modification time                              |
| `-u`                     | Sort by last access time                               |
| `-v`                     | Sort by version                                        |
| `-1`                     | List one file per line                                 |
