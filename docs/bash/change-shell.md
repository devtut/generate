---
metaTitle: "Change shell"
description: "Find the current shell, Change the shell, List available shells"
---

# Change shell



## Find the current shell


There are a few ways to determine the current shell

```bash
echo $0
ps -p $$
echo $SHELL

```



## Change the shell


To change the current bash run these commands

```bash
export SHELL=/bin/bash
exec /bin/bash

```

to change the bash that opens on startup edit `.profile` and add those lines



## List available shells


To list available login shells :

```bash
cat /etc/shells

```

Example:

```bash
$ cat /etc/shells
# /etc/shells: valid login shells
/bin/sh
/bin/dash
/bin/bash
/bin/rbash

```



#### Syntax


- echo $0
- ps -p $$
- echo $SHELL
- export SHELL=/bin/bash
- exec /bin/bash
- cat /etc/shells

