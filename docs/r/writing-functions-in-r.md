---
metaTitle: "Writing functions in R"
description: "Anonymous functions, RStudio code snippets, Named functions, Passing column names as argument of a function"
---

# Writing functions in R




## Anonymous functions


An anonymous function is, as the name implies, not assigned a name. This can be useful when the function is a part of a larger operation, but in itself does not take much place.
One frequent use-case for anonymous functions is within the `*apply` family of Base functions.

Calculate the root mean square for each column in a `data.frame`:

```r
df <- data.frame(first=5:9, second=(0:4)^2, third=-1:3)

apply(df, 2, function(x) { sqrt(sum(x^2)) })
    first    second     third 
15.968719 18.814888  3.872983 

```

Create a sequence of step-length one from the smallest to the largest value for each row in a matrix.

```r
x <- sample(1:6, 12, replace=TRUE)
mat <- matrix(x, nrow=3)

apply(mat, 1, function(x) { seq(min(x), max(x)) })

```

An anonymous function can also stand on its own:

```r
(function() { 1 })()
[1] 1

```

is equivalent to

```r
f <- function() { 1 })
f()
[1] 1

```



## RStudio code snippets


This is just a small hack for those who use self-defined functions often.<br />
Type "fun" RStudio IDE and hit TAB.

[<img src="https://i.stack.imgur.com/gA8QV.png" alt="enter image description here" />](https://i.stack.imgur.com/gA8QV.png)

The result will be a skeleton of a new function.

```r
name <- function(variables) {
        
}

```

One can easily define their own snippet template, i.e. like the one below

```r
name <- function(df, x, y) {
        require(tidyverse)
        out <- 
        return(out)
}

```

The option is `Edit Snippets` in the `Global Options -> Code` menu.



## Named functions


R is full of functions, it is after all a [functional programming language](https://en.wikipedia.org/wiki/Functional_programming#R), but sometimes the precise function you need isn't provided in the Base resources. You could conceivably [install a package](http://stackoverflow.com/documentation/r/1719/installing-packages#t=201607251211199337159) containing the function, but maybe your requirements are just so specific that no pre-made function fits the bill? Then you're left with the option of making your own.

A function can be very simple, to the point of being being pretty much pointless.
It doesn't even need to take an argument:

```r
one <- function() { 1 }
one()
[1] 1

two <- function() { 1 + 1 }
two()
[1] 2

```

What's between the curly braces `{ }` is the function proper. As long as you can fit everything on a single line they aren't strictly needed, but can be useful to keep things organized.

A function can be very simple, yet highly specific. This function takes as input a vector (`vec` in this example) and outputs the same vector with the vector's length (6 in this case) subtracted from each of the vector's elements.

```r
vec <- 4:9
subtract.length <- function(x) { x - length(x) }
subtract.length(vec)
[1] -2 -1  0  1  2  3

```

Notice that `length()` is in itself a pre-supplied (i.e. **Base**) function. You can of course use a previously self-made function within another self-made function, as well as assign variables and perform other operations while spanning several lines:

```r
vec2 <- (4:7)/2

msdf <- function(x, multiplier=4) {
    mult <- x * multiplier
    subl <- subtract.length(x)
    data.frame(mult, subl)
}

msdf(vec2, 5)
  mult subl
1 10.0 -2.0
2 12.5 -1.5
3 15.0 -1.0
4 17.5 -0.5

```

`multiplier=4` makes sure that `4` is the default value of the argument `multiplier`, if no value is given when calling the function `4` is what will be used.

The above are all examples of **named** functions, so called simply because they have been given names (`one`, `two`, `subtract.length` etc.)



## Passing column names as argument of a function


Sometimes one would like to pass names of columns from a data frame to a function. They may be provided as strings and used in a function using `[[`. Let's take a look at the following example, which prints to R console basic stats of selected variables:

```r
basic.stats <- function(dset, vars){
    for(i in 1:length(vars)){
        print(vars[i])
        print(summary(dset[[vars[i]]]))
    }
}

basic.stats(iris, c("Sepal.Length", "Petal.Width"))

```

As a result of running above given code, names of selected variables and their basic summary statistics (minima, first quantiles, medians, means, third quantiles and maxima) are printed in R console. The code `dset[[vars[i]]]` selects i-th element from the argument `vars` and selects a corresponding column in declared input data set `dset`. For example, declaring `iris[["Sepal.Length"]]` alone would print the `Sepal.Length` column from the `iris` data set as a vector.

