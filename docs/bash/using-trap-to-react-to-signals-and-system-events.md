---
metaTitle: "Bash - Using trap to react to signals and system events"
description: "Introduction: clean up temporary files, Catching SIGINT or Ctl+C, Accumulate a list of trap work to run at exit., Killing Child Processes on Exit, react on change of terminals window size"
---

# Using "trap" to react to signals and system events

## Introduction: clean up temporary files

You can use the `trap` command to "trap" signals; this is the shell equivalent of the `signal()` or `sigaction()` call in C and most other programming languages to catch signals.

One of the most common uses of `trap` is to clean up temporary files on both an expected and unexpected exit.

Unfortunately not enough shell scripts do this :-(

```bash
#!/bin/sh

# Make a cleanup function
cleanup() {
  rm --force -- "${tmp}"
}

# Trap the special "EXIT" group, which is always run when the shell exits.
trap cleanup EXIT

# Create a temporary file
tmp="$(mktemp -p /tmp tmpfileXXXXXXX)"

echo "Hello, world!" >> "${tmp}"

# No rm -f "$tmp" needed. The advantage of using EXIT is that it still works
# even if there was an error or if you used exit.

```

## Catching SIGINT or Ctl+C

The trap is reset for subshells, so the `sleep` will still act on the `SIGINT` signal sent by `^C` (usually by quitting), but the parent process (i.e. the shell script) won't.

```bash
#!/bin/sh

# Run a command on signal 2 (SIGINT, which is what ^C sends)
sigint() {
    echo "Killed subshell!"
}
trap sigint INT

# Or use the no-op command for no output
#trap : INT

# This will be killed on the first ^C
echo "Sleeping..."
sleep 500

echo "Sleeping..."
sleep 500

```

And a variant which still allows you to quit the main program by pressing `^C` twice in a second:

```bash
last=0
allow_quit() {
    [ $(date +%s) -lt $(( $last + 1 )) ] && exit
    echo "Press ^C twice in a row to quit"
    last=$(date +%s)
}
trap allow_quit INT

```

## Accumulate a list of trap work to run at exit.

Have you ever forgotten to add a `trap` to clean up a temporary file or do other work at exit?

Have you ever set one trap which canceled another?

This code makes it easy to add things to be done on exit one item at a time, rather than having one large `trap` statement somewhere in your code, which may be easy to forget.

```bash
# on_exit and add_on_exit
# Usage:
#   add_on_exit rm -f /tmp/foo
#   add_on_exit echo "I am exiting"
#   tempfile=$(mktemp)
#   add_on_exit rm -f "$tempfile"
# Based on http://www.linuxjournal.com/content/use-bash-trap-statement-cleanup-temporary-files
function on_exit()
{
    for i in "${on_exit_items[@]}"
    do
        eval $i
    done
}
function add_on_exit()
{
    local n=${#on_exit_items[*]}
    on_exit_items[$n]="$*"
    if [[ $n -eq 0 ]]; then
        trap on_exit EXIT
    fi
}

```

## Killing Child Processes on Exit

Trap expressions don't have to be individual functions or programs, they can be more complex expressions as well.

By combining `jobs -p` and `kill`, we can kill all spawned child processes of the shell on exit:

```bash
trap 'jobs -p | xargs kill' EXIT

```

## react on change of terminals window size

There is a signal WINCH ( WINdowCHange), which is fired when one resizes a terminal window.

```bash
declare -x rows cols

update_size(){
  rows=$(tput lines) # get actual lines of term
  cols=$(tput cols)  # get actual columns of term
  echo DEBUG terminal window has no $rows lines and is $cols characters wide
}

trap update_size WINCH

```

#### Syntax

- trap action sigspec... # Run "action" on a list of signals
- trap sigspec... # Omitting action resets traps for signals

#### Parameters

| Parameter | Meaning                                     |
| --------- | ------------------------------------------- |
| -p        | List currently installed traps              |
| -l        | List signal names and corresponding numbers |

#### Remarks

The `trap` utility is a special shell built-in. It's [defined in POSIX](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#trap), but bash adds some useful extensions as well.

Examples that are POSIX-compatible start with `#!/bin/sh`, and examples that start with `#!/bin/bash` use a bash extension.

The signals can either be a signal number, a signal name (without the SIG prefix), or the special keyword `EXIT`.

Those guaranteed by POSIX are:

| Number | Name    | Notes                                             |
| ------ | ------- | ------------------------------------------------- |
| 0      | EXIT    | Always run on shell exit, regardless of exit code |
| 1      | SIGHUP  |
| 2      | SIGINT  | This is what `^C` sends                           |
| 3      | SIGQUIT |
| 6      | SIGABRT |
| 9      | SIGKILL |
| 14     | SIGALRM |
| 15     | SIGTERM | This is what `kill` sends by default              |
