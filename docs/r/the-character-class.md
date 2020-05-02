---
metaTitle: "The character class"
description: "Coercion"
---

# The character class


Characters are what other languages call 'string vectors.'



## Coercion


To check whether a value is a character use the `is.character()` function. To coerce a variable to a character use the `as.character()` function.

```r
x <- "The quick brown fox jumps over the lazy dog"
class(x)
[1] "character"
is.character(x)
[1] TRUE

```

Note that numerics can be coerced to characters, but attempting to coerce a character to numeric may result in `NA`.

```r
as.numeric("2")
[1] 2
as.numeric("fox")
[1] NA
Warning message:
NAs introduced by coercion 

```



#### Remarks


### Related topics

Patterns

- [Regular Expressions (regex)](http://stackoverflow.com/documentation/r/5748)
- [Pattern Matching and Replacement](http://stackoverflow.com/documentation/r/1123)
- [strsplit function](http://stackoverflow.com/documentation/r/2762/strsplit-function#t=201702151627471743408)

Input and output

- [Reading and writing strings](http://stackoverflow.com/documentation/r/5541/)

