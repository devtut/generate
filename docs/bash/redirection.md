---
metaTitle: "Redirection"
description: "Append vs Truncate, Redirecting both STDOUT and STDERR, Redirecting standard output, Using named pipes, Redirecting multiple commands to the same file, Print error messages to stderr, Redirection to network addresses, Redirecting STDIN, Redirecting STDERR, STDIN, STDOUT and STDERR explained"
---

# Redirection




## Append vs Truncate


### Truncate `>`

1. Create specified file if it does not exist.
1. Truncate (remove file's content)
1. Write to file

```bash
$ echo "first line" > /tmp/lines
$ echo "second line" > /tmp/lines

$ cat /tmp/lines
second line

```

### Append `>>`

1. Create specified file if it does not exist.
1. Append file (writing at end of file).

```bash
# Overwrite existing file
$ echo "first line" > /tmp/lines

# Append a second line
$ echo "second line" >> /tmp/lines

$ cat /tmp/lines
first line
second line

```



## Redirecting both STDOUT and STDERR


File descriptors like `0` and `1` are pointers. We change what file descriptors point to with redirection. `>/dev/null` means `1` points to `/dev/null`.

First we point `1` (`STDOUT`) to `/dev/null` then point `2` (`STDERR`) to whatever `1` points to.

```bash
# STDERR is redirect to STDOUT: redirected to /dev/null,
# effectually redirecting both STDERR and STDOUT to /dev/null
echo 'hello' > /dev/null 2>&1

```

This **can** be further shortened to the following:

```bash
echo 'hello' &> /dev/null

```

However, this form may be undesirable in production if shell compatibility is a concern as it conflicts with POSIX, introduces parsing ambiguity, and shells without this feature will misinterpret it:

```bash
# Actual code
echo 'hello' &> /dev/null
echo 'hello' &> /dev/null 'goodbye'

# Desired behavior
echo 'hello' > /dev/null 2>&1
echo 'hello' 'goodbye' > /dev/null 2>&1

# Actual behavior
echo 'hello' &
echo 'hello' & goodbye > /dev/null

```

NOTE: `&>` is known to work as desired in both Bash and Zsh.



## Redirecting standard output


`>` redirect the standard output (aka `STDOUT`) of the current command into a file or another descriptor.

These examples write the output of the `ls` command into the file `file.txt`

```bash
ls >file.txt
> file.txt ls

```

The target file is created if it doesn't exists, otherwise this file is truncated.

The default redirection descriptor is the standard output or `1` when none is specified.
This command is equivalent to the previous examples with the standard output explicitly indicated:

```bash
ls 1>file.txt

```

Note: the redirection is initialized by the executed shell and not by the executed command, therefore it is done before the command execution.



## Using named pipes


Sometimes you may want to output something by one program and input it into another program, but can't use a standard pipe.

```bash
ls -l | grep ".log"

```

You could simply write to a temporary file:

```bash
touch tempFile.txt
ls -l > tempFile.txt
grep ".log" < tempFile.txt

```

This works fine for most applications, however, nobody will know what `tempFile` does and someone might remove it if it contains the output of `ls -l` in that directory. This is where a named pipe comes into play:

```bash
mkfifo myPipe
ls -l > myPipe
grep ".log" < myPipe

```

`myPipe` is technically a file (everything is in Linux), so let's do `ls -l` in an empty directory that we just created a pipe in:

```bash
mkdir pipeFolder
cd pipeFolder
mkfifo myPipe
ls -l

```

The output is:

```bash
prw-r--r-- 1 root root 0 Jul 25 11:20 myPipe

```

Notice the first character in the permissions, it's listed as a pipe, not a file.

Now let's do something cool.

Open one terminal, and make note of the directory (or create one so that cleanup is easy), and make a pipe.

```bash
mkfifo myPipe

```

Now let's put something in the pipe.

```bash
echo "Hello from the other side" > myPipe

```

You'll notice this hangs, the other side of the pipe is still closed. Let's open up the other side of the pipe and let that stuff through.

Open another terminal and go to the directory that the pipe is in (or if you know it, prepend it to the pipe):

```bash
cat < myPipe

```

You'll notice that after `hello from the other side` is output, the program in the first terminal finishes, as does that in the second terminal.

Now run the commands in reverse. Start with `cat < myPipe` and then echo something into it. It still works, because a program will wait until something is put into the pipe before terminating, because it knows it has to get something.

Named pipes can be useful for moving information between terminals or between programs.

Pipes are small. Once full, the writer blocks until some reader reads the contents, so you need to either run the reader and writer in different terminals or run one or the other in the background:

```

ls -l /tmp > myPipe &
 cat < myPipe

```

**More examples using named pipes:**

<li>
Example 1 - all commands on the same terminal / same shell

```bash
$ { ls -l && cat file3; } >mypipe &
$ cat <mypipe    
# Output: Prints ls -l data and then prints file3 contents on screen

```


</li>
<li>
Example 2 - all commands on the same terminal / same shell

```bash
$ ls -l >mypipe &
$ cat file3 >mypipe &
$ cat <mypipe
#Output: This prints on screen the contents of mypipe. 

```


Mind that first contents of `file3` are displayed and then the `ls -l` data is displayed (LIFO configuration).
</li>
<li>
Example 3 - all commands on the same terminal / same shell

```bash
$ { pipedata=$(<mypipe) && echo "$pipedata"; } &
$ ls >mypipe 
# Output: Prints the output of ls directly on screen 

```


Mind that the variable `$pipedata` is not available for usage in the main terminal / main shell since the use of `&` invokes a subshell and `$pipedata` was only available in this subshell.
</li>
<li>
Example 4 - all commands on the same terminal / same shell

```bash
$ export pipedata
$ pipedata=$(<mypipe) &
$ ls -l *.sh >mypipe
$ echo "$pipedata"   
#Output : Prints correctly the contents of mypipe

```


This prints correctly the value of `$pipedata` variable in the main shell due to the export declaration of the variable. The main terminal/main shell is not hanging due to the invocation of a background shell (`&`).
</li>



## Redirecting multiple commands to the same file


```bash
{
  echo "contents of home directory"
  ls ~
} > output.txt

```



## Print error messages to stderr


Error messages are generally included in a script for debugging purposes or for providing rich user experience. Simply writing error message like this:

```bash
cmd || echo 'cmd failed'

```

may work for simple cases but it's not the usual way. In this example, the error message will pollute the actual output of the script by mixing both errors and successful output in `stdout`.

In short, error message should go to `stderr` not `stdout`. It's pretty simple:

```bash
cmd || echo 'cmd failed' >/dev/stderr

```

Another example:

```bash
if cmd; then
    echo 'success'
else
    echo 'cmd failed' >/dev/stderr
fi

```

In the above example, the success message will be printed on `stdout` while the error message will be printed on `stderr`.

A better way to print error message is to define a function:

```bash
err(){
    echo "E: $*" >>/dev/stderr
}

```

Now, when you have to print an error:

```bash
err "My error message"

```



## Redirection to network addresses


Bash treats some paths as special and can do some network communication by writing to `/dev/{udp|tcp}/host/port`.  Bash cannot setup a listening server, but can initiate a connection, and for TCP can read the results at least.

For example, to send a simple web request one could do:

```bash
exec 3</dev/tcp/www.google.com/80
printf 'GET / HTTP/1.0\r\n\r\n' >&3
cat <&3

```

and the results of `www.google.com`'s default web page will be printed to `stdout`.

Similarly

```bash
printf 'HI\n' >/dev/udp/192.168.1.1/6666

```

would send a UDP message containing `HI\n` to a listener on `192.168.1.1:6666`



## Redirecting STDIN


`<` reads from its right argument and writes to its left argument.

To write a file into `STDIN` we should **read** `/tmp/a_file` and **write** into `STDIN` i.e `0</tmp/a_file`

Note: Internal file descriptor defaults to `0` (`STDIN`) for `<`

```bash
$ echo "b" > /tmp/list.txt
$ echo "a" >> /tmp/list.txt
$ echo "c" >> /tmp/list.txt
$ sort < /tmp/list.txt
a
b
c

```



## Redirecting STDERR


`2` is `STDERR`.

```bash
$ echo_to_stderr 2>/dev/null # echos nothing

```

Definitions:

`echo_to_stderr` is a command that writes `"stderr"` to `STDERR`

```bash
echo_to_stderr () {
    echo stderr >&2
}

$ echo_to_stderr
stderr

```



## STDIN, STDOUT and STDERR explained


Commands have one input (STDIN) and two kinds of outputs, standard output (STDOUT) and standard error (STDERR).

For example:

**STDIN**

```bash
root@server~# read
Type some text here

```

Standard input is used to provide input to a program.
(Here we're using the [`read` builtin](http://stackoverflow.com/documentation/bash/5473) to read a line from STDIN.)

**STDOUT**

```bash
root@server~# ls file
file

```

Standard output is generally used for "normal" output from a command. For example, `ls` lists files, so the files are sent to STDOUT.

**STDERR**

```bash
root@server~# ls anotherfile
ls: cannot access 'anotherfile': No such file or directory

```

Standard error is (as the name implies) used for error messages. Because this message is not a list of files, it is sent to STDERR.

STDIN, STDOUT and STDERR are the three **standard streams.** They are identified to the shell by a number rather than a name:

0 = Standard in<br />
1 = Standard out<br />
2 = Standard error

By default, STDIN is attached to the keyboard, and both STDOUT and STDERR appear in the terminal. However, we can redirect either STDOUT or STDERR to whatever we need. For example, let's say that you only need the standard out and all error messages printed on standard error should be suppressed. That's when we use the descriptors `1` and `2`.

**Redirecting STDERR to /dev/null**<br />
Taking the previous example,

```bash
root@server~# ls anotherfile 2>/dev/null
root@server~#

```

In this case, if there is any STDERR, it will be redirected to /dev/null (a special file which ignores anything put into it), so you won't get any error output on the shell.



#### Syntax


- command </path/to/file # Redirect standard input to file
- command >/path/to/file # Redirect standard output to flie
- command file_descriptor>/path/to/file # Redirect output of file_descriptor to file
- command >&file_descriptor # Redirect output to file_descriptor
- command file_descriptor>&another_file_descriptor # Redirect file_descriptor to another_file_descriptor
- command <&file_descriptor # Redirect file_descriptor to standard input
- command &>/path/to/file # Redirect standard output and standard error to file



#### Parameters


|Parameter|Details
|------
|internal file descriptor|An integer.
|direction|One of `>`, `<` or `<>`
|external file descriptor or path|`&` followed by an integer for file descriptor or a path.



#### Remarks


UNIX console programs have an input file and two output files (input and output streams, as well as devices, are treated as files by the OS.) These are typically the keyboard and screen, respectively, but any or all of them can be redirected to come from — or go to — a file or other program.

`STDIN` is standard input, and is how the program receives interactive input. `STDIN` is usually assigned file descriptor 0.

`STDOUT` is standard output. Whatever is emitted on `STDOUT` is considered the "result" of the program. `STDOUT` is usually assigned file descriptor 1.

`STDERR` is where error messages are displayed. Typically, when running a program from the console, `STDERR` is output on the screen and is indistinguishable from `STDOUT`. `STDERR` is usually assigned file descriptor 2.

**The order of redirection is important**

```bash
command > file 2>&1

```

Redirects both (`STDOUT` and `STDERR`) to the file.

```bash
command 2>&1 > file

```

Redirects only `STDOUT`, because the file descriptor 2 is redirected to the file pointed to by file descriptor 1 (which is not the file `file` yet when the statement is evaluated).

Each command in a pipeline has its own `STDERR` (and `STDOUT`) because each is a new process. This can create surprising results if you expect a redirect to affect the entire pipeline. For example this command (wrapped for legibility):

```bash
$ python -c 'import sys;print >> sys.stderr, "Python error!"' \
| cut -f1 2>> error.log

```

will print "Python error!" to the console rather than the log file. Instead, attach the error to the command you want to capture:

```bash
$ python -c 'import sys;print >> sys.stderr, "Python error!"' 2>> error.log \
| cut -f1 

```

