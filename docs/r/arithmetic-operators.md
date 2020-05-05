---
metaTitle: "R - Arithmetic Operators"
description: "Range and addition, Addition and subtraction"
---

# Arithmetic Operators



## Range and addition


Let's take an example of adding a value to a range (as it could be done in a loop for example):

```r
3+1:5

```

Gives:

```r
[1] 4 5 6 7 8

```

This is because the range operator `:` has higher precedence than addition operator `+`.

What happens during evaluation is as follows:

- `3+1:5`
- `3+c(1, 2, 3, 4, 5)` expansion of the range operator to make a vector of integers.
- `c(4, 5, 6, 7, 8)` Addition of 3 to each member of the vector.

To avoid this behavior you have to tell the R interpreter how you want it to order the operations with `( )` like this:

```

 (3+1):5

```

Now R will compute what is inside the parentheses before expanding the range and gives:

```

[1] 4 5

```



## Addition and subtraction


The basic math operations are performed mainly on numbers or on vectors (lists of numbers).

**1. Using single numbers**

We can simple enter the numbers concatenated with `+` for **adding** and `-` for **subtracting**:

```r
> 3 + 4.5
# [1] 7.5
> 3 + 4.5 + 2
# [1] 9.5
> 3 + 4.5 + 2 - 3.8
# [1] 5.7
> 3 + NA
#[1] NA
> NA + NA
#[1] NA
> NA - NA
#[1] NA
> NaN - NA
#[1] NaN
> NaN + NA
#[1] NaN

```

We can assign the numbers to **variables** (constants in this case) and do the same operations:

```r
> a <- 3; B <- 4.5; cc <- 2; Dd <- 3.8 ;na<-NA;nan<-NaN
> a + B
# [1] 7.5
> a + B + cc
# [1] 9.5
> a + B + cc - Dd
# [1] 5.7
> B-nan
#[1] NaN
> a+na-na
#[1] NA
> a + na
#[1] NA
> B-nan
#[1] NaN
> a+na-na
#[1] NA

```

**2. Using vectors**

In this case we create vectors of numbers and do the operations using those vectors, or combinations with single numbers. In this case the operation is done considering each element of the vector:

```r
> A <- c(3, 4.5, 2, -3.8);
> A
# [1]  3.0  4.5  2.0 -3.8
> A + 2 # Adding a number 
# [1]  5.0  6.5  4.0 -1.8
> 8 - A # number less vector
# [1]  5.0  3.5  6.0 11.8
> n <- length(A) #number of elements of vector A
> n
# [1] 4
> A[-n] + A[n] # Add the last element to the same vector without the last element
# [1] -0.8  0.7 -1.8
> A[1:2] + 3 # vector with the first two elements plus a number
# [1] 6.0 7.5
> A[1:2] - A[3:4] # vector with the first two elements less the vector with elements 3 and 4
# [1] 1.0 8.3

```

We can also use the function `sum` to add all elements of a vector:

```r
> sum(A)
# [1] 5.7
> sum(-A)
# [1] -5.7
> sum(A[-n]) + A[n]
# [1] 5.7

```

We must take care with **recycling**, which is one of the characteristics of `R`, a behavior that happens when doing math operations where the length of vectors is different. **Shorter vectors in the expression are recycled as often as need be (perhaps fractionally) until they match the length of the longest vector. In particular a constant is simply repeated**. In this case a Warning is show.

```r
> B <- c(3, 5, -3, 2.7, 1.8)
> B
# [1]  3.0  5.0 -3.0  2.7  1.8
> A
# [1]  3.0  4.5  2.0 -3.8
> A + B # the first element of A is repeated
# [1]  6.0  9.5 -1.0 -1.1  4.8
Warning message:
In A + B : longer object length is not a multiple of shorter object length
> B - A # the first element of A is repeated
# [1]  0.0  0.5 -5.0  6.5 -1.2
Warning message:
In B - A : longer object length is not a multiple of shorter object length

```

In this case the correct procedure will be to consider only the elements of the shorter vector:

```r
> B[1:n] + A
# [1]  6.0  9.5 -1.0 -1.1
> B[1:n] - A
# [1]  0.0  0.5 -5.0  6.5

```

When using the `sum` function, again all the elements inside the function are added.

```r
> sum(A, B)
# [1] 15.2
> sum(A, -B)
# [1] -3.8
> sum(A)+sum(B)
# [1] 15.2
> sum(A)-sum(B)
# [1] -3.8

```



#### Remarks


Nearly all operators in R are really functions.  For example, `+` is a function defined as `function (e1, e2) .Primitive("+")` where e1 is the left-hand side of the operator and e2 is the right-hand side of the operator.  This means it is possible to accomplish rather counterintuitive effects by masking the `+` in base with a user defined function.

For example:

```r
`+` <- function(e1, e2) {e1-e2}

> 3+10
[1] -7

```

