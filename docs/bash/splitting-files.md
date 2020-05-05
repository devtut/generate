---
metaTitle: "Bash - Splitting Files"
description: "Split a file, We can use sed with w option to split a file into mutiple files. Files can be split by specifying line address or pattern."
---

# Splitting Files


Sometimes it's useful to split a file into multiple separate files. If you have large files, it might be a good idea to break it into smaller chunks



## Split a file


Running the split command without any options will split a file into 1 or more separate files containing up to 1000 lines each.

```bash
split file

```

This will create files named `xaa`, `xab`, `xac`, etc, each containing up to 1000 lines. As you can see, all of them are prefixed with the letter `x` by default. If the initial file was less than 1000 lines, only one such file would be created.

To change the prefix, add your desired prefix to the end of the command line

```bash
split file customprefix

```

Now files named `customprefixaa`, `customprefixab`, `customprefixac` etc. will be created

To specify the number of lines to output per file, use the `-l` option. The following will split a file into a maximum of 5000 lines

```bash
split -l5000 file

```

OR

```bash
split --lines=5000 file

```

Alternatively, you can specify a maximum number of bytes instead of lines. This is done by using the `-b` or `--bytes` options. For example, to allow a maximum of 1MB

```bash
split --bytes=1MB file

```



## We can use sed with w option to split a file into mutiple files. Files can be split by specifying line address or pattern.


Suppose we have this source file that we would like to split:

```bash
cat -n sourcefile

```

<em>1  On the Ning Nang Nong<br />
2  Where the Cows go Bong!<br />
3  and the monkeys all say BOO!<br />
4  There's a Nong Nang Ning<br />
5  Where the trees go Ping!<br />
6  And the tea pots jibber jabber joo.<br />
7  On the Nong Ning Nang</em>

Command to split the file by line number:

```bash
sed '1,3w f1
> 4,7w f2' sourcefile

```

This writes line1 to line3 into file f1 and line4 to line7 into file f2, from the sourcefile.

```bash
cat -n f1

```

<em>1  On the Ning Nang Nong<br />
2  Where the Cows go Bong!<br />
3  and the monkeys all say BOO!</em>

```bash
cat -n f2 

```

1  There's a Nong Nang Ning<br />
2  Where the trees go Ping!<br />
3  And the tea pots jibber jabber joo.<br />
4  On the Nong Ning Nang

Command to split the file by context/pattern:

```bash
sed '/Ning/w file1
> /Ping/w file2' sourcefile

```

This splits the sourcefile into file1 and file2.
file1 contains all lines that match Ning, file2 contains lines that match Ping.

```bash
cat file1

```

<em>On the Ning Nang Nong<br />
There's a Nong Nang Ning<br />
On the Nong Ning Nang</em>

```bash
cat file2

```

**Where the trees go Ping!**

