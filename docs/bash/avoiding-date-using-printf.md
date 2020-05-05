---
metaTitle: "Bash - Avoiding date using printf"
description: "Get the current date, Set variable to current time"
---

# Avoiding date using printf


In Bash 4.2, a shell built-in time conversion for `printf` was introduced: the format specification `%(datefmt)T` makes `printf` output the date-time string corresponding to the format string `datefmt` as understood by [`strftime`](http://man7.org/linux/man-pages/man3/strftime.3.html).



## Get the current date


```bash
$ printf '%(%F)T\n'
2016-08-17

```



## Set variable to current time


```bash
$ printf -v now '%(%T)T'
$ echo "$now"
12:42:47

```



#### Syntax


- printf '%(dateFmt)T' # dateFmt can be any format string that strftime recognizes
- printf '%(dateFmt)T' -1 # -1 represents the current time (default for no argument)
- printf '%(dateFmt)T' -2 # -2 represents the time the shell was invoked



#### Remarks


Using `printf -v foo '%(...)T'` is identical to `foo=$(date +'...')` and saves a fork for the call to the external program `date`.

