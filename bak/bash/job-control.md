---
metaTitle: "Bash - Job Control"
description: "List background processes, Bring a background process to the foreground, Restart stopped background process, Run command in background, Stop a foreground process"
---

# Job Control



## List background processes


```bash
$ jobs
[1]   Running                 sleep 500 &  (wd: ~)
[2]-  Running                 sleep 600 &  (wd: ~)
[3]+  Running                 ./Fritzing &

```

First field shows the job ids. The + and - sign that follows the job id for two jobs denote the default job and next candidate default job when the current default job ends respectively. The default job is used when the `fg` or `bg` commands are used without any argument.

Second field gives the status of the job. Third field is the command used to start the process.

The last field (wd: ~) says that the sleep commands were started from the working directory ~ (Home).



## Bring a background process to the foreground


```bash
$ fg %2
sleep 600

```

%2 specifies job no. 2. If fg is used without any arguments if brings the last process put in background to the foreground.

```bash
$ fg %?sle
sleep 500

```

`?sle` refers to the baground process command containing "sle". If multiple background commands contain the string, it will produce an error.



## Restart stopped background process


```bash
$ bg
[8]+ sleep 600 &

```



## Run command in background


```bash
$ sleep 500 &
[1] 7582

```

Puts the sleep command in background. 7582 is the process id of the background process.



## Stop a foreground process


Press Ctrl + Z to stop a foreground process and put it in background

```bash
$ sleep 600
^Z
[8]+  Stopped                 sleep 600

```



#### Syntax


- long_cmd &
- jobs
- fg %JOB_ID
- fg %?PATTERN
- fg %JOB_ID

