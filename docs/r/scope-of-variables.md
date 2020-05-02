---
metaTitle: "Scope of variables"
description: "Environments and Functions, Function Exit, Sub functions, Global Assignment, Explicit Assignment of Environments and Variables, Packages and Masking"
---

# Scope of variables



## Environments and Functions


Variables declared inside a function only exist (unless passed) inside that function.

```r
x <- 1

foo <- function(x) {
    y <- 3
    z <- x + y
    return(z)
}

y

```

> 
Error: object 'y' not found


Variables passed into a function and then reassigned are overwritten, **but only inside the function**.

```r
foo <- function(x) {
    x <- 2
    y <- 3
    z <- x + y
    return(z)
}

foo(1)
x

```

> 
5
1


Variables assigned in a higher environment than a function exist within that function, without being passed.

```r
foo <- function() {
    y <- 3
    z <- x + y
    return(z)
}
    
foo()

```

> 
4




## Function Exit


The `on.exit()` function is handy for variable clean up if global variables must be assigned.

Some parameters, especially those for graphics, can only be set globally. This small function is common when creating more specialized plots.

```r
new_plot <- function(...) {
     
     old_pars <- par(mar = c(5,4,4,2) + .1, mfrow = c(1,1))
     on.exit(par(old_pars))
     plot(...)
 }

```



## Sub functions


Functions called within a function (ie subfunctions) must be defined within that function to access any variables defined in the local environment without being passed.

This fails:

```r
bar <- function() {
    z <- x + y
    return(z)
}
    
foo <- function() {
    y <- 3
    z <- bar()
    return(z)
}

foo()

```

> 
Error in bar() : object 'y' not found


This works:

```r
foo <- function() {
    
    bar <- function() {
        z <- x + y
        return(z)
    }
    
    y <- 3
    z <- bar()
    return(z)
}
    
foo()

```

> 
4




## Global Assignment


Variables can be assigned globally from any environment using `<<-`. `bar()` can now access `y`.

```r
bar <- function() {
    z <- x + y
    return(z)
}
    
foo <- function() {
    y <<- 3
    z <- bar()
    return(z)
}

foo()

```

> 
4


Global assignment is highly discouraged. Use of a wrapper function or explicitly calling variables from another local environment is greatly preferred.



## Explicit Assignment of Environments and Variables


Environments in R can be explicitly call and named. Variables can be explicitly assigned and call to or from those environments.

A commonly created environment is one which encloses `package:base` or a subenvironment within `package:base`.

```r
e1 <- new.env(parent = baseenv())
e2 <- new.env(parent = e1)

```

Variables can be explicitly assigned and call to or from those environments.

```r
assign("a", 3, envir = e1)
   get("a", envir = e1)
   get("a", envir = e2)

```

> 
3
3


Since `e2` inherits from `e1`, `a` is `3` in both `e1` and `e2`. However, assigning `a` within `e2` does not change the value of `a` in `e1`.

```r
assign("a", 2, envir = e2)
   get("a", envir = e2)
   get("a", envir = e1)

```

> 
3
2




## Packages and Masking


Functions and objects in different packages may have the same name. The package loaded later will 'mask' the earlier package and a warning message will be printed. When calling the function by name, the function from the most recently loaded package will be run. The earlier function can be accessed explicitly.

```r
library(plyr)
library(dplyr)

```

> 
Attaching package: ‘dplyr’
The following objects are masked from ‘package:plyr’:
arrange, count, desc, failwith, id, mutate, rename, summarise, summarize
The following objects are masked from ‘package:stats’:
filter, lag
The following objects are masked from ‘package:base’:
intersect, setdiff, setequal, union


When writing code, it is always best practice to call functions explicitly using `package::function()` specifically to avoid this issue.



#### Remarks


The most common pitfall with scope arises in parallelization. All variables and functions must be passed into a new environment that is run on each thread.

