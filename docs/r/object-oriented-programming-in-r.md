---
metaTitle: "Object-Oriented Programming in R"
description: "S3"
---

# Object-Oriented Programming in R


This documentation page describes the four object systems in R and their high-level similarities and differences.  Greater detail on each individual system can be found on its own topic page.

The four systems are: S3, S4, Reference Classes, and S6.



## S3


The S3 object system is a very simple OO system in R.

Every object has an S3 class.  It can be get (got?) with the function `class`.

```r
> class(3)
[1] "numeric"

```

It can also be set with the function `class`:

```r
> bicycle <- 2
> class(bicycle) <- 'vehicle'
> class(bicycle)
[1] "vehicle"

```

It can also be set with the function `attr`:

```r
> velocipede <- 2
> attr(velocipede, 'class') <- 'vehicle'
> class(velocipede)
[1] "vehicle"

```

An object can have many classes:

```r
> class(x = bicycle) <- c('human-powered vehicle', class(x = bicycle))
> class(x = bicycle)
[1] "human-powered vehicle" "vehicle" 

```

When using a generic function, R uses the first element of the class that has an available generic.

For example:

```r
> summary.vehicle <- function(object, ...) {
+   message('this is a vehicle')
+ }
> summary(object = my_bike)
this is a vehicle

```

But if we now define a `summary.bicycle`:

```r
> summary.bicycle <- function(object, ...) {
+   message('this is a bicycle')
+ }
> summary(object = my_bike)
this is a bicycle

```

