---
metaTitle: "Typing variables"
description: "declare weakly typed variables"
---

# Typing variables



## declare weakly typed variables


**declare** is an internal command of bash. (internal command use **help ** for displaying "manpage"). It is used to show and define variables or show function bodies.

Syntax: **declare [options] [name[=value]]...**

```bash
# options are used to define
# an integer
declare -i myInteger
declare -i anotherInt=10
# an array with values
declare -a anArray=( one two three)
# an assoc Array
declare -A assocArray=( [element1]="something" [second]=anotherthing )
# note that bash recognizes the string context within []

# some modifiers exist
# uppercase content
declare -u big='this will be uppercase'
# same for lower case
declare -l small='THIS WILL BE LOWERCASE'

# readonly array
declare -ra constarray=( eternal true and unchangeable )

# export integer to environment
declare -xi importantInt=42

```

You can use also the + which takes away the given attribute. Mostly useless, just for completness.

To display variables and/or functions there are some options too

```bash
# printing definded vars and functions
declare -f
# restrict output to functions only
declare -F # if debugging prints line number and filename defined in too

```

