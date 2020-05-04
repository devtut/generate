---
metaTitle: "Process substitution"
description: "Compare two files from the web, Feed a while loop with the output of a command, Concatenating files, With paste command, Stream a file through multiple programs at once, To avoid usage of a sub-shell"
---

# Process substitution




## Compare two files from the web


The following compares two files with `diff` using process substitution instead of creating temporary files.

```bash
diff <(curl http://www.example.com/page1) <(curl http://www.example.com/page2)

```



## Feed a while loop with the output of a command


This feeds a `while` loop with the output of a `grep` command:

```bash
while IFS=":" read -r user _
do
    # "$user" holds the username in /etc/passwd
done < <(grep "hello" /etc/passwd)

```



## Concatenating files


It is well known that you cannot use the same file for input and ouput in the same command. For instance,

```bash
$ cat header.txt body.txt >body.txt

```

doesn’t do what you want. By the time `cat` reads `body.txt`, it has already been truncated by the redirection and it is empty. The final result will be that `body.txt` will hold the contents of `header.txt` **only.**

One might think to avoid this with process substitution, that is, that the command

```bash
$ cat header.txt <(cat body.txt) > body.txt

```

will force the original contents of `body.txt` to be somehow saved in some buffer somewhere before the file is truncated by the redirection. It doesn’t work. The `cat` in parentheses begins reading the file only after all file descriptors have been set up, just like the outer one. There is no point in trying to use process substitution in this case.

The only way to prepend a file to another file is to create an intermediate one:

```bash
$ cat header.txt body.txt >body.txt.new
$ mv body.txt.new body.txt

```

which is what `sed` or `perl` or similar programs do under the carpet when called with an **edit-in-place** option (usually `-i`).



## With paste command


```bash
# Process substitution with paste command is common
# To compare the contents of two directories
paste <( ls /path/to/directory1 ) <( ls /path/to/directory1 )

```



## Stream a file through multiple programs at once


This counts the number of lines in a big file with `wc -l` while simultaneously compressing it with `gzip`.  Both run concurrently.

```bash
tee >(wc -l >&2) < bigfile | gzip > bigfile.gz

```

Normally `tee` writes its input to one or more files (and stdout).  We can write to commands instead of files with `tee >(command)`.

Here the command `wc -l >&2` counts the lines read from `tee` (which in turn is reading from `bigfile`).  (The line count is sent to stderr (`>&2`) to avoid mixing with the input to `gzip`.)  The stdout of `tee` is simultaneously fed into `gzip`.



## To avoid usage of a sub-shell


One major aspect of process substitution is that it lets us avoid usage of a sub-shell when piping commands from the shell.

This can be demonstrated with a simple example below. I have the following files in my current folder:

```bash
$ find . -maxdepth 1 -type f -print
foo bar zoo foobar foozoo barzoo 

```

If I pipe to a `while`/`read` loop that increments a counter as follows:

```bash
count=0
find . -maxdepth 1 -type f -print | while IFS= read -r _; do
    ((count++))
done

```

`$count` now does **not** contain `6`, because it was modified in the sub-shell context. Any of the commands shown below are run in a sub-shell context and the scope of the variables used within are lost after the sub-shell terminates.

```bash
command &
command | command 
( command )

```

Process substitution will solve the problem by avoiding use the of pipe `|` operator as in

```bash
count=0
while IFS= read -r _; do
    ((count++))
done < <(find . -maxdepth 1 -type f -print)

```

This will retain the `count` variable value as no sub-shells are invoked.



#### Remarks


Process substitution is a form of redirection where the input or output of a process (some sequence of commands) appear as a temporary file.

