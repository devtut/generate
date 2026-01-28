---
metaTitle: "Bash - Pipelines"
description: "Using |&, Show all processes paginated, Modify continuous output of a command"
---

# Pipelines



## Using |&


`|&` connects standard output and standard error of the first command to the second one while `|` only connects standard output of the first command to the second command.

In this example, the page is downloaded via `curl`. with `-v` option `curl` writes some info on `stderr` including , the downloaded page is written on `stdout`. Title of page can be found between `<title>` and `</title>`.

```bash
curl -vs 'http://www.google.com/' |& awk '/Host:/{print} /<title>/{match($0,/<title>(.*)<\/title>/,a);print a[1]}'

```

Output is:

```bash
> Host: www.google.com
Google

```

But with `|` a lot more information will be printed, i.e. those that are sent to `stderr` because only `stdout` is piped to the next command. In this example all lines except the last line (Google) were sent to `stderr` by `curl`:

```bash
* Hostname was NOT found in DNS cache
*   Trying 172.217.20.228...
* Connected to www.google.com (172.217.20.228) port 80 (#0)
> GET / HTTP/1.1
> User-Agent: curl/7.35.0
> Host: www.google.com
> Accept: */*
> 
* HTTP 1.0, assume close after body
< HTTP/1.0 200 OK
< Date: Sun, 24 Jul 2016 19:04:59 GMT
< Expires: -1
< Cache-Control: private, max-age=0
< Content-Type: text/html; charset=ISO-8859-1
< P3P: CP="This is not a P3P policy! See https://www.google.com/support/accounts/answer/151657?hl=en for more info."
< Server: gws
< X-XSS-Protection: 1; mode=block
< X-Frame-Options: SAMEORIGIN
< Set-Cookie: NID=82=jX0yZLPPUE7u13kKNevUCDg8yG9Ze_C03o0IM-EopOSKL0mMITEagIE816G55L2wrTlQwgXkhq4ApFvvYEoaWF-oEoq2T0sBTuQVdsIFULj9b2O8X35O0sAgUnc3a3JnTRBqelMcuS9QkQA; expires=Mon, 23-Jan-2017 19:04:59 GMT; path=/; domain=.google.com; HttpOnly
< Accept-Ranges: none
< Vary: Accept-Encoding
< X-Cache: MISS from jetsib_appliance
< X-Loop-Control: 5.202.190.157 81E4F9836653D5812995BA53992F8065
< Connection: close
< 
{ [data not shown]
* Closing connection 0
Google

```



## Show all processes paginated


```bash
ps -e | less

```

`ps -e` shows all the processes, its output is connected to the input of more via `|`, `less` paginates the results.



## Modify continuous output of a command


```bash
~$ ping -c 1 google.com # unmodified output
PING google.com (16.58.209.174) 56(84) bytes of data.
64 bytes from wk-in-f100.1e100.net (16.58.209.174): icmp_seq=1 ttl=53 time=47.4 ms
~$ ping google.com | grep -o '^[0-9]\+[^()]\+' # modified output
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
64 bytes from wk-in-f100.1e100.net 
...

```

The pipe (`|`) connects the `stdout` of `ping` to the `stdin` of `grep`, which processes it immediately. Some other commands like `sed` default to buffering their `stdin`, which means that it has to receive enough data, before it will print anything, potentially causing delays in further processing.



#### Syntax


- [time [-p]] [!] command1 [ | or |& command2 ] …



#### Remarks


A pipeline is a sequence of simple commands separated by one of the control operators `|` or `|&` ([source](https://www.gnu.org/software/bash/manual/html_node/Pipelines.html)).

`|` connects the output of `command1` to the input of `command2`.

`|&` connects standard output and standard error of `command1` to the standard input of `command2`.

