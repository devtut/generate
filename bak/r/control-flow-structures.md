---
metaTitle: "R - Control flow structures"
description: "Optimal Construction of a For Loop, Basic For Loop Construction, The Other Looping Constructs: while and repeat"
---

# Control flow structures



## Optimal Construction of a For Loop


To illustrate the effect of good for loop construction, we will calculate the mean of each column in four different ways:

1. Using a poorly optimized for loop
1. Using a well optimized for for loop
1. Using an `*apply` family of functions
1. Using the `colMeans` function

Each of these options will be shown in code; a comparison of the computational time to execute each option will be shown; and lastly a discussion of the differences will be given.

### Poorly optimized for loop

```r
column_mean_poor <- NULL
for (i in 1:length(mtcars)){
  column_mean_poor[i] <- mean(mtcars[[i]])
}

```

### Well optimized for loop

```r
column_mean_optimal <- vector("numeric", length(mtcars))
for (i in seq_along(mtcars)){
  column_mean_optimal <- mean(mtcars[[i]])
}

```

### `vapply` Function

```r
column_mean_vapply <- vapply(mtcars, mean, numeric(1))

```

### `colMeans` Function

```r
column_mean_colMeans <- colMeans(mtcars)

```

### Efficiency comparison

The results of benchmarking these four approaches is shown below (code not displayed)

```r
Unit: microseconds
     expr     min       lq     mean   median       uq     max neval  cld
     poor 240.986 262.0820 287.1125 275.8160 307.2485 442.609   100    d
  optimal 220.313 237.4455 258.8426 247.0735 280.9130 362.469   100   c 
   vapply 107.042 109.7320 124.4715 113.4130 132.6695 202.473   100 a   
 colMeans 155.183 161.6955 180.2067 175.0045 194.2605 259.958   100  b

```

Notice that the optimized `for` loop edged out the poorly constructed for loop.  The poorly constructed for loop is constantly increasing the length of the output object, and at each change of the length, R is reevaluating the class of the object.

Some of this overhead burden is removed by the optimized for loop by declaring the type of output object and its length before starting the loop.

In this example, however, the use of an `vapply` function doubles the computational efficiency, largely because we told R that the result had to be numeric (if any one result were not numeric, an error would be returned).

Use of the `colMeans` function is a touch slower than the `vapply` function. This difference is attributable to some error checks performed in `colMeans` and mainly to the `as.matrix` conversion (because `mtcars` is a `data.frame`) that weren't performed in the `vapply` function.



## Basic For Loop Construction


In this example we will calculate the squared deviance for each column in a data frame, in this case the `mtcars`.

Option A: integer index

```r
squared_deviance <- vector("list", length(mtcars))
for (i in seq_along(mtcars)){
  squared_deviance[[i]] <- (mtcars[[i]] - mean(mtcars[[i]]))^2
}

```

`squared_deviance` is an 11 elements list, as expected.

```r
class(squared_deviance)
length(squared_deviance)

```

Option B: character index

```r
squared_deviance <- vector("list", length(mtcars))
Squared_deviance <- setNames(squared_deviance, names(mtcars))
for (k in names(mtcars)){
  squared_deviance[[k]] <- (mtcars[[k]] - mean(mtcars[[k]]))^2
}

```

What if we want a `data.frame` as a result? Well, there are many options for transforming a list into other objects. However, and maybe the simplest in this case, will be to store the `for` results in a `data.frame`.

```r
squared_deviance <- mtcars #copy the original
squared_deviance[TRUE]<-NA  #replace with NA or do squared_deviance[,]<-NA
for (i in seq_along(mtcars)){
  squared_deviance[[i]] <- (mtcars[[i]] - mean(mtcars[[i]]))^2
}
dim(squared_deviance)
[1] 32 11

```

The result will be the same event though we use the character option (B).



## The Other Looping Constructs: while and repeat


R provides two additional looping constructs, `while` and `repeat`, which are typically used in situations where the number of iterations required is indeterminate.

### The `while` loop

The general form of a `while` loop is as follows,

```r
while (condition) {
    ## do something
    ## in loop body
}

```

where `condition` is evaluated prior to entering the loop body. If `condition` evaluates to `TRUE`, the code inside of the loop body is executed, and this process repeats until `condition` evaluates to `FALSE` (or a `break` statement is reached; see below). Unlike the `for` loop, if a `while` loop uses a variable to perform incremental iterations, the variable must be declared and initialized ahead of time, and must be updated within the loop body. For example, the following loops accomplish the same task:

```r
for (i in 0:4) {
    cat(i, "\n")
}
# 0 
# 1 
# 2 
# 3 
# 4 

i <- 0
while (i < 5) {
    cat(i, "\n")
    i <- i + 1
}
# 0 
# 1 
# 2 
# 3 
# 4 

```

In the `while` loop above, the line `i <- i + 1` is necessary to prevent an infinite loop.

Additionally, it is possible to terminate a `while` loop with a call to `break` from inside the loop body:

```r
iter <- 0
while (TRUE) {
    if (runif(1) < 0.25) {
        break
    } else {
        iter <- iter + 1
    }
}
iter
#[1] 4

```

In this example, `condition` is always `TRUE`, so the only way to terminate the loop is with a call to `break` inside the body. Note that the final value of `iter` will depend on the state of your PRNG when this example is run, and should produce different results (essentially) each time the code is executed.

### The `repeat` loop

The `repeat` construct is essentially the same as `while (TRUE) { ## something }`, and has the following form:

```r
repeat ({
    ## do something
    ## in loop body
})

```

The extra `{}` are not required, but the `()` are. Rewriting the previous example using `repeat`,

```r
iter <- 0
repeat ({
    if (runif(1) < 0.25) {
        break
    } else {
        iter <- iter + 1
    }
})
iter
#[1] 2 

```

### More on `break`

It's important to note that `break` will **only terminate the immediately enclosing loop**. That is, the following is an infinite loop:

```r
while (TRUE) {
    while (TRUE) {
        cat("inner loop\n")
        break
    }
    cat("outer loop\n")
}

```

With a little creativity, however, it is possible to break entirely from within a nested loop. As an example, consider the following expression, which, in its current state, will loop infinitely:

```r
while (TRUE) {
    cat("outer loop body\n")
    while (TRUE) {
        cat("inner loop body\n")
        x <- runif(1)
        if (x < .3) {
            break
        } else {
            cat(sprintf("x is %.5f\n", x))
        }
    }
}

```

One possibility is to recognize that, unlike `break`, the `return` expression **does** have the ability to return control across multiple levels of enclosing loops. However, since `return` is only valid when used within a function, we cannot simply replace `break` with `return()` above, but also need to wrap the entire expression as an anonymous function:

```r
(function() {
    while (TRUE) {
        cat("outer loop body\n")
        while (TRUE) {
            cat("inner loop body\n")
            x <- runif(1)
            if (x < .3) {
                return()
            } else {
                cat(sprintf("x is %.5f\n", x))
            }
        }
    }
})()

```

Alternatively, we can create a dummy variable (`exit`) prior to the expression, and activate it via `<<-` from the inner loop when we are ready to terminate:

```r
exit <- FALSE
while (TRUE) {
    cat("outer loop body\n")
    while (TRUE) {
        cat("inner loop body\n")
        x <- runif(1)
        if (x < .3) {
            exit <<- TRUE
            break
        } else {
            cat(sprintf("x is %.5f\n", x))
        }
    }
    if (exit) break
}

```



#### Remarks


For loops are a flow control method for repeating a task or set of tasks over a domain.  The core structure of a for loop is

```r
for ( [index] in [domain]){
  [body]
}

```

Where

1. `[index]` is a name takes exactly one value of `[domain]` over each iteration of the loop.
1. `[domain]` is a vector of values over which to iterate.
1. `[body]` is the set of instructions to apply on each iteration.

As a trivial example, consider the use of a for loop to obtain the cumulative sum of a vector of values.

```r
x <- 1:4
cumulative_sum <- 0
for (i in x){
  cumulative_sum <- cumulative_sum + x[i]
}
cumulative_sum

```

### Optimizing Structure of For Loops

For loops can be useful for conceptualizing and executing tasks to repeat. If not constructed carefully, however, they can be very slow to execute compared to the preferred used of the `apply` family of functions.  Nonetheless, there are a handful of elements you can include in your for loop construction to optimize the loop.  In many cases, good construction of the for loop will yield computational efficiency very close to that of an apply function.

A 'properly constructed' for loop builds on the core structure and includes a statement declaring the object that will capture each iteration of the loop.  This object should have both a class and a length declared.

```r
[output] <- [vector_of_length]
for ([index] in [length_safe_domain]){
  [output][index] <- [body]
}

```

To illustrate, let us write a loop to square each value in a numeric vector (this is a trivial example for illustration only.  The 'correct' way of completing this task would be `x_squared <- x^2`).

```r
x <- 1:100
x_squared <- vector("numeric", length = length(x))
for (i in seq_along(x)){
  x_squared[i] <- x[i]^2
}

```

Again, notice that we first declared a receptacle for the output `x_squared`, and gave it the class "numeric" with the same length as `x`.  Additionally, we declared a "length safe domain" using the `seq_along` function.  `seq_along` generates a vector of indices for an object that is suited for use in for loops.  While it seems intuitive to use `for (i in 1:length(x))`, if `x` has 0 length, the loop will attempt to iterate over the domain of `1:0`, resulting in an error (the 0th index is undefined in R).

Receptacle objects and length safe domains are handled internally by the `apply` family of functions and users are encouraged to adopt the `apply` approach in place of for loops as much as possible.  However, if properly constructed, a for loop may occasionally provide greater code clarity with minimal loss of efficiency.

### Vectorizing For Loops

For loops can often be a useful tool in conceptualizing the tasks that need to be completed within each iteration.  When the loop is completely developed and conceptualized, there may be advantages to turning the loop into a function.

In this example, we will develop a for loop to calculate the mean of each column in the `mtcars` dataset (again, a trivial example as it could be accomplished via the `colMeans` function).

```r
column_mean_loop <- vector("numeric", length(mtcars))
for (k in seq_along(mtcars)){
  column_mean_loop[k] <- mean(mtcars[[k]])
}

```

The for loop can be converted to an apply function by rewriting the body of the loop as a function.

```r
col_mean_fn <- function(x) mean(x)
column_mean_apply <- vapply(mtcars, col_mean_fn, numeric(1))

```

And to compare the results:

```r
identical(column_mean_loop, 
          unname(column_mean_apply)) #* vapply added names to the elements
                                     #* remove them for comparison

```

The advantages of the vectorized form is that we were able to eliminate a few lines of code.  The mechanics of determining the length and type of the output object and iterating over a length safe domain are handled for us by the apply function.  Additionally, the apply function is a little bit faster than the loop.  The difference of speed is often negligible in human terms depending on the number of iterations and the complexity of the body.

