---
metaTitle: "R - Recycling"
description: "Recycling use in subsetting"
---

# Recycling



## Recycling use in subsetting


Recycling can be used in a clever way to simplify code.

**Subsetting**

If we want to keep every third element of a vector we can do the following:

```r
my_vec <-   c(1,2,3,4,5,6,7,8,9,10)
my_vec[c(TRUE, FALSE)]

[1] 1 3 5 7 9

```

Here the logical expression was expanded to the length of the vector.

We can also perform comparisons using recycling:

```r
my_vec <-   c("foo", "bar", "soap", "mix")
my_vec == "bar"

[1] FALSE  TRUE FALSE FALSE

```

Here "bar" gets recycled.



#### Remarks


**What is recycling in R**

[Recycling](https://cran.r-project.org/doc/manuals/r-release/R-intro.html#The-recycling-rule) is when an object is automatically extended in certain operations to match the length of another, longer object.

For example, the vectorised addition results in the following:

```r
c(1,2,3) + c(1,2,3,4,5,6)  
[1] 2 4 6 5 7 9

```

Because of the recycling, the operation that actually happened was:

```r
c(1,2,3,1,2,3) + c(1,2,3,4,5,6)

```

In cases where the longer object is not a multiple of the shorter one, a warning message is presented:

```r
c(1,2,3) + c(1,2,3,4,5,6,7)
[1] 2 4 6 5 7 9 8
Warning message:
In c(1, 2, 3) + c(1, 2, 3, 4, 5, 6, 7) :
  longer object length is not a multiple of shorter object length

```

Another example of recycling:

```r
matrix(nrow =5, ncol = 2, 1:5 )
     [,1] [,2]
[1,]    1    1
[2,]    2    2
[3,]    3    3
[4,]    4    4
[5,]    5    5

```

