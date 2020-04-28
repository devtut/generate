---
metaTitle: "Multiprocessing"
description: "Multiprocessing using built-in fork functions, Creating child process using fork, Inter-Process Communication"
---

# Multiprocessing



## Multiprocessing using built-in fork functions


You can use built-in functions to run PHP processes as forks. This is the most simple way to achieve parallel work if you don't need your threads to talk to each other.

This allows you to put time intensive tasks (like uploading a file to another server or sending an email) to another thread so your script loads faster and can use multiple cores but be aware that this is not real multithreading and your main thread won't know what the children are up to.

Note that under Windows this will make another command prompt pop up for each fork you start.

master.php

```
$cmd = "php worker.php 10";
if(strtoupper(substr(PHP_OS, 0, 3)) === 'WIN') // for windows use popen and pclose
{
    pclose(popen($cmd,"r"));
}
else //for unix systems use shell exec with "&" in the end
{
    exec('bash -c "exec nohup setsid '.$cmd.' > /dev/null 2>&1 &"');
}

```

worker.php

```
//send emails, upload files, analyze logs, etc
$sleeptime = $argv[1];
sleep($sleeptime);

```



## Creating child process using fork


PHP has built in function `pcntl_fork` for creating child process. `pcntl_fork` is same as `fork` in unix. It does not take in any parameters and returns integer which can be used to differentiate between parent and child process. Consider the following code for explanation

```
<?php
    // $pid is the PID of child
    $pid = pcntl_fork();
    if ($pid == -1) {
         die('Error while creating child process');
    } else if ($pid) {
         // Parent process
    } else {
         // Child process
    }
?>

```

As you can see `-1` is an error in fork and the child was not created. On creation of child, we have two processes running with separate `PID`.

Another consideration here is a `zombie process` or `defunct process` when parent process finishes before child process. To prevent a zombie children process simply add `pcntl_wait($status)` at the end of parent process.

> 
**pnctl_wait** suspends execution of parent process until the child process has exited.


It is also worth noting that `zombie process` can't be killed using `SIGKILL` signal.



## Inter-Process Communication


Interprocess communication allows programmers to communicate between different processes. For example let us consider we need to write an PHP application that can run bash commands and print the output. We will be using `proc_open` , which will execute the command and return a resource that we can communicate with.
The following code shows a basic implementation that runs `pwd` in `bash` from `php`

```
<?php
    $descriptor = array(
        0 => array("pipe", "r"), // pipe for stdin of child
        1 => array("pipe", "w"), // pipe for stdout of child
    );
    $process = proc_open("bash", $descriptor, $pipes);
    if (is_resource($process)) {
        fwrite($pipes[0], "pwd" . "\n");
        fclose($pipes[0]);
        echo stream_get_contents($pipes[1]);
        fclose($pipes[1]);
        $return_value = proc_close($process);

    }
?>

```

`proc_open` runs `bash` command with `$descriptor` as descriptor specifications. After that we use `is_resource` to validate the process. Once done we can start interacting with the child process using **$pipes** which is generated according to descriptor specifications.

After that we can simply use `fwrite` to write to stdin of child process. In this case `pwd` followed by carriage return. Finally `stream_get_contents` is used to read stdout of child process.

> 
Always remember to close the child process by using proc_close() which will terminate the child and return the exit status code.


