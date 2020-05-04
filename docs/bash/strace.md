---
metaTitle: "strace"
description: "How to observe the system calls of a program"
---

# strace




## How to observe the system calls of a program


For an **executable file <em>or** command</em> exec, running this will list all system calls:

```bash
$ ptrace exec

```

To display specific system calls use -e option:

```bash
$ strace -e open exec

```

To save the output to a file use the -o option:

```bash
$ strace -o output exec

```

To find the system calls an active program uses, use the -p option while specifying the pid [[how to get pid]](https://stackoverflow.com/questions/31676071/how-to-get-process-id-of-specific-process) :

```bash
$ sudo strace -p 1115

```

To generate a statistics report of all the system calls used, use option -c:

```bash
$ strace -c exec 

```



#### Syntax


<li>strace  -c[df]  [-In]  [-bexecve]  [-eexpr]...  [-Ooverhead] [-Ssortby]
-ppid... / [-D] [-Evar[=val]]... [-uusername] command [args]</li>

