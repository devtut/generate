---
metaTitle: "Introspection"
description: "Functions for Learning about Variables "
---

# Introspection



## Functions for Learning about Variables 


Often in `R` you'll want to know things about an object or variable you're working with.  This can be useful when reading someone else's code or even your own, especially when using packages that are new to you.

Suppose we create a variable `a`:

```r
a <- matrix(1:9, 3, 3)

```

What data type is this?  You can find out with

```r
> class(a)
[1] "matrix"

```

It's a matrix, so matrix operations will work on it:

```r
> a %*% t(a)
      [,1] [,2] [,3]
[1,]   66   78   90
[2,]   78   93  108
[3,]   90  108  126  

```

What are the dimensions of `a`?

```r
> dim(a)
[1] 3 3
> nrow(a)
[1] 3
> ncol(a)
[2] 3

```

Other useful functions that work for different data types are `head`, `tail`, and `str`:

```r
> head(a, 1)
     [,1] [,2] [,3]
[1,]    1    4    7
> tail(a, 1)
     [,1] [,2] [,3]
[3,]    3    6    9
> str(a)
int [1:3, 1:3] 1 2 3 4 5 6 7 8 9

```

These are much more useful for large objects (such as big datasets).  `str` is also great for learning about the nesting of lists.  Now reshape `a` like so:

```r
a <- c(a)

```

Does the class remain the same?

```r
> class(a)
[1] "integer"

```

No, `a` is not a matrix anymore.  I won't get a good answer if I ask for dimensions now:

```r
> dim(a)
NULL

```

Instead, I can ask for the length:

```r
> length(a)
[1] 9

```

What about now:

```r
> class(a * 1.0)
[1] "numeric"

```

Often you may work with `data.frames`:

```r
a <- as.data.frame(a)
names(a) <- c("var1", "var2", "var3")

```

See the variable names:

```r
> names(a)
[1] "var1" "var2" "var3"

```

These functions can help many ways when using `R`.

