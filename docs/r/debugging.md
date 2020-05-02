---
metaTitle: "Debugging"
description: "Using debug, Using browser"
---

# Debugging



## Using debug


You can set any function for debugging with `debug`.

```r
debug(mean)
mean(1:3)

```

All subsequent calls to the function will enter debugging mode.  You can disable this behavior with `undebug`.

```r
undebug(mean)
mean(1:3)

```

If you know you only want to enter the debugging mode of a function once, consider the use of `debugonce`.

```r
debugonce(mean)
mean(1:3)
mean(1:3)

```



## Using browser


The `browser` function can be used like a breakpoint: code execution will pause at the point it is called. Then user can then inspect variable values, execute arbitrary R code and step through the code line by line.

Once `browser()` is hit in the code the interactive interpreter will start. Any R code can be run as normal, and in addition the following commands are present,

|Command|Meaning
|------
|c|Exit browser and continue program
|f|Finish current loop or function \
|n|Step Over (evaluate next statement, stepping over function calls)
|s|Step Into (evaluate next statement, stepping into function calls)
|where|Print stack trace
|r|Invoke "resume" restart
|Q|Exit browser and quit

For example we might have a script like,

```r
toDebug <- function() {
    a = 1
    b = 2
    
    browser()
    
    for(i in 1:100) {
        a = a * b
    }
}

toDebug()

```

When running the above script we initially see something like,

```r
Called from: toDebug
Browser[1]>

```

We could then interact with the prompt as so,

```r
Called from: toDebug
Browser[1]> a
[1] 1
Browser[1]> b
[1] 2
Browse[1]> n
debug at #7: for (i in 1:100) {
    a = a * b
}
Browse[2]> n
debug at #8: a = a * b
Browse[2]> a
[1] 1
Browse[2]> n
debug at #8: a = a * b
Browse[2]> a
[1] 2
Browse[2]> Q

```

`browser()` can also be used as part of a functional chain, like so:

```r
mtcars %>% group_by(cyl) %>% {browser()}

```

