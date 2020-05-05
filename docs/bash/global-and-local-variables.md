---
metaTitle: "Bash - global and local variables"
description: "Global variables, Local variables, Mixing the two together"
---

# global and local variables


By default, every variable in bash is **global** to every function, script and even the outside shell if you are declaring your variables inside a script.

If you want your variable to be local to a function, you can use `local` to have that variable a new variable that is independent to the global scope and whose value will only be accessible inside that function.



## Global variables


```bash
var="hello"

function foo(){
    echo $var
}

foo

```

Will obviously output "hello", but this works the other way around too:

```bash
function foo()  {
    var="hello"
}

foo
echo $var

```

Will also output `"hello"`



## Local variables


```bash
function foo() {
    local var
    var="hello"
}

foo
echo $var

```

Will output nothing, as var is a variable local to the function foo, and its value is not visible from outside of it.



## Mixing the two together


```bash
var="hello"

function foo(){
    local var="sup?"
    echo "inside function, var=$var"
}

foo
echo "outside function, var=$var"

```

Will output

```bash
inside function, var=sup?
outside function, var=hello

```

