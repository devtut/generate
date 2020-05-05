---
metaTitle: "Bash - true, false and : commands"
description: "Infinite Loop, Function Return, Code that will always/never be executed"
---

# true, false and : commands



## Infinite Loop


```bash
while true; do
    echo ok
done

```

or

```bash
while :; do
   echo ok
done

```

or

```bash
until false; do
    echo ok
done

```



## Function Return


```bash
function positive() {
    return 0
}

function negative() {
    return 1
}

```



## Code that will always/never be executed


```bash
if true; then
    echo Always executed
fi
if false; then
    echo Never executed
fi

```



#### Syntax


- true, : - always return 0 as exit code.
- false - always returns 1 as exit code.

