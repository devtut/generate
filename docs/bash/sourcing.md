---
metaTitle: "Sourcing"
description: "Sourcing a file, Sourcing a virtual environment"
---

# Sourcing



## Sourcing a file


Sourcing a file is different from execution, in that all commands are evaluated within the context of the current bash session - this means that any variables, function, or aliases defined will persist throughout your session.

Create the file you wish to source `sourceme.sh`

```bash
#!/bin/bash

export A="hello_world"
alias sayHi="echo Hi"
sayHello() {
    echo Hello
}

```

From your session, source the file

```bash
$ source sourceme.sh

```

From hencefourth, you have all the resources of the sourced file available

```bash
$ echo $A
hello_world

$ sayHi
Hi

$ sayHello
Hello

```

Note that the command `.` is synonymous to `source`, such that you can simply use

```bash
$ . sourceme.sh

```



## Sourcing a virtual environment


When developing several applications on one machine, it becomes useful to separate out dependencies into virtual environments.

With the use of [`virtualenv`](https://github.com/pypa/virtualenv/blob/master/README.rst), these environments are sourced into your shell so that when you run a command, it comes from that virtual environment.

This is most commonly installed using `pip`.

```bash
pip install https://github.com/pypa/virtualenv/tarball/15.0.2

```

Create a new environment

```bash
virtualenv --python=python3.5 my_env

```

Activate the environment

```bash
source my_env/bin/activate

```

