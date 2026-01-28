---
metaTitle: "Bash - Jobs and Processes"
description: "Job handling, Check which process running on specific port, Disowning background job, List Current Jobs, List all processes, Finding information about a  running process "
---

# Jobs and Processes

## Job handling

### Creating jobs

To create an job, just append a single `&` after the command:

```bash
$ sleep 10 &
[1] 20024

```

You can also make a running process a job by pressing <kbd>Ctrl</kbd> + <kbd>z</kbd>:

```bash
$ sleep 10
^Z
[1]+  Stopped                 sleep 10

```

### Background and foreground a process

To bring the Process to the foreground, the command `fg` is used together with `%`

```bash
$ sleep 10 &
[1] 20024

$ fg %1
sleep 10

```

Now you can interact with the process. To bring it back to the background you can use the `bg` command. Due to the occupied terminal session, you need to stop the process first by pressing <kbd>Ctrl</kbd> + <kbd>z</kbd>.

```bash
$ sleep 10
^Z
[1]+  Stopped              sleep 10

$ bg %1
[1]+ sleep 10 &

```

Due to the laziness of some Programmers, all these commands also work with a single `%` if there is only one process, or for the first process in the list. For Example:

```bash
$ sleep 10 &
[1] 20024

$ fg %        # to bring a process to foreground 'fg %' is also working.
sleep 10

```

or just

```bash
$ %           # laziness knows no boundaries, '%' is also working.
sleep 10

```

Additionally, just typing `fg` or `bg` without any argument handles the last job:

```bash
$ sleep 20 &
$ sleep 10 &
$ fg
sleep 10
^C
$ fg
sleep 20

```

### Killing running jobs

```bash
$ sleep 10 &
[1] 20024

$ kill %1
[1]+  Terminated              sleep 10

```

The sleep process runs in the background with process id (pid) `20024` and job number `1`. In order to reference the process, you can use either the pid or the job number. If you use the job number, you must prefix it with `%`. The default kill signal sent by `kill` is `SIGTERM`, which allows the target process to exit gracefully.

Some common kill signals are shown below. To see a full list, run `kill -l`.

| Signal name | Signal value | Effect                  |
| ----------- | ------------ | ----------------------- |
| `SIGHUP`    | `1`          | Hangup                  |
| `SIGINT`    | `2`          | Interrupt from keyboard |
| `SIGKILL`   | `9`          | Kill signal             |
| `SIGTERM`   | `15`         | Termination signal      |

### Start and kill specific processes

Probably the easiest way of killing a running process is by selecting it through the process name as in the following example using `pkill` command as

```bash
pkill -f test.py

```

(or) a more fool-proof way using `pgrep` to search for the actual process-id

```bash
kill $(pgrep -f 'python test.py')

```

The same result can be obtained using `grep` over `ps -ef | grep name_of_process` then killing the process associated with the resulting pid (process id). Selecting a process using its name is convinient in a testing environment but can be really dangerous when the script is used in production: it is virtually impossible to be sure that the name will match the process you actually want to kill. In those cases, the following approach is actually much safe.

Start the script that will eventually killed with the following approach. Let's assume that the command you want to execute and eventually kill is `python test.py`.

```bash
#!/bin/bash

if [[ ! -e /tmp/test.py.pid ]]; then   # Check if the file already exists
    python test.py &                   #+and if so do not run another process.
    echo $! > /tmp/test.py.pid
else
    echo -n "ERROR: The process is already running with pid "
    cat /tmp/test.py.pid
    echo
fi

```

This will create a file in the `/tmp` directory containing the pid of the `python test.py` process. If the file already exists, we assume that the command is already running and the script return an error.

Then, when you want to kill it use the following script:

```bash
#!/bin/bash

if [[ -e /tmp/test.py.pid ]]; then   # If the file do not exists, then the
    kill `cat /tmp/test.py.pid`      #+the process is not running. Useless
    rm /tmp/test.py.pid              #+trying to kill it.
else
    echo "test.py is not running"
fi

```

that will kill exactly the process associated with your command, without relying on any volatile information (like the string used to run the command). Even in this case if the file does not exist, the script assume that you want to kill a non-running process.

This last example can be easily improved for running the same command multiple times (appending to the pid file instead of overwriting it, for example) and to manage cases where the process dies before being killed.

## Check which process running on specific port

To check which process running on port 8080

```bash
lsof -i :8080

```

## Disowning background job

```bash
$ gzip extremelylargefile.txt &
$ bg
$ disown %1

```

This allows a long running process to continue once your shell (terminal, ssh, etc) is closed.

## List Current Jobs

```bash
$ tail -f /var/log/syslog > log.txt
[1]+  Stopped                 tail -f /var/log/syslog > log.txt

$ sleep 10 &

$ jobs
[1]+  Stopped                 tail -f /var/log/syslog > log.txt
[2]-  Running                 sleep 10 &

```

## List all processes

There are two common ways to list all processes on a system. Both list all processes running by all users, though they differ in the format they output (the reason for the differences are historical).

```bash
ps -ef   # lists all processes
ps aux   # lists all processes in alternative format (BSD)

```

This can be used to check if a given application is running. For example, to check if the SSH server (sshd) is running:

```bash
ps -ef | grep sshd

```

## Finding information about a running process

`ps aux | grep <search-term>` shows processes matching **search-term**

Example:

```bash
root@server7:~# ps aux | grep nginx
root       315  0.0  0.3 144392  1020 ?        Ss   May28   0:00 nginx: master process /usr/sbin/nginx
www-data  5647  0.0  1.1 145124  3048 ?        S    Jul18   2:53 nginx: worker process
www-data  5648  0.0  0.1 144392   376 ?        S    Jul18   0:00 nginx: cache manager process
root     13134  0.0  0.3   4960   920 pts/0    S+   14:33   0:00 grep --color=auto nginx
root@server7:~#

```

Here, second column is the process id. For example, if you want to kill the nginx process, you can use the command `kill 5647`. It is always adviced to use the `kill` command with `SIGTERM` rather than `SIGKILL`.
