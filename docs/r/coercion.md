---
metaTitle: "Coercion"
description: "Implicit Coercion"
---

# Coercion


Coercion happens in R when the type of objects are changed during computation either implicitly or by using functions for explicit coercion (such as as.numeric, as.data.frame, etc.).



## Implicit Coercion


Coercion happens with data types in R, often implicitly, so that the data can accommodate all the values. For example,

```r
x = 1:3
x
[1] 1 2 3
typeof(x)
#[1] "integer"

x[2] = "hi"
x
#[1] "1"  "hi" "3" 
typeof(x)
#[1] "character"

```

Notice that at first, `x` is of type `integer`. But when we assigned `x[2] = "hi"`, all the elements of `x` were coerced into `character` as vectors in R can only hold data of single type.

