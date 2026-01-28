---
metaTitle: "Bash - co-processes"
description: "Hello World"
---

# co-processes



## Hello World


```bash
# create the co-process
coproc bash

# send a command to it (echo a)
echo 'echo Hello World' >&"${COPROC[1]}"

# read a line from its output
read line <&"${COPROC[0]}"

# show the line
echo "$line"

```

The output is "Hello World".

