---
metaTitle: "Creating vectors"
description: "Creating named vectors, Vectors from build in constants: Sequences of letters & month names, Sequence of numbers, seq(), Vectors, Expanding a vector with the rep() function"
---

# Creating vectors



## Creating named vectors


Named vector can be created in several ways. With `c`:

```r
xc <- c('a' = 5, 'b' = 6, 'c' = 7, 'd' = 8)

```

which results in:

```r
> xc
a b c d 
5 6 7 8

```

with `list`:

```r
xl <- list('a' = 5, 'b' = 6, 'c' = 7, 'd' = 8)

```

which results in:

```r
> xl
$a
[1] 5

$b
[1] 6

$c
[1] 7

$d
[1] 8

```

With the `setNames` function, two vectors of the same length can be used to create a named vector:

```r
x <- 5:8
y <- letters[1:4]

xy <- setNames(x, y)

```

which results in a named integer vector:

```r
> xy
a b c d 
5 6 7 8

```

As can be seen, this gives the same result as the `c` method.

You may also use the `names` function to get the same result:

```r
xy <- 5:8
names(xy) <- letters[1:4]

```

With such a vector it is also possibly to select elements by name:

```r
> xy["c"]
c 
7 

```

This feature makes it possible to use such a named vector as a look-up vector/table to match the values to values of another vector or column in  dataframe. Considering the following dataframe:

```r
mydf <- data.frame(let = c('c','a','b','d'))

> mydf
  let
1   c
2   a
3   b
4   d

```

Suppose you want to create a new variable in the `mydf` dataframe called `num` with the correct values from `xy` in the rows. Using the `match` function the appropriate values from `xy` can be selected:

```r
mydf$num <- xy[match(mydf$let, names(xy))]

```

which results in:

```r
> mydf
  let num
1   c   7
2   a   5
3   b   6
4   d   8

```



## Vectors from build in constants: Sequences of letters & month names


`R` has a number of build in constants. The following constants are available:

- `LETTERS`: the 26 upper-case letters of the Roman alphabet
- `letters`: the 26 lower-case letters of the Roman alphabet
<li>`month.abb`: the three-letter abbreviations for the English month
names</li>
- `month.name`: the English names for the months of the year
- `pi`: the ratio of the circumference of a circle to its diameter

From the letters and month constants, vectors can be created.

**1)** Sequences of letters:

```r
> letters
[1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"

> LETTERS[7:9]
[1] "G" "H" "I"

> letters[c(1,5,3,2,4)]
[1] "a" "e" "c" "b" "d"

```

**2)** Sequences of month abbreviations or month names:

```r
> month.abb
 [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

> month.name[1:4]
[1] "January"  "February" "March"    "April"   

> month.abb[c(3,6,9,12)]
[1] "Mar" "Jun" "Sep" "Dec"

```



## Sequence of numbers


Use the `:` operator to create sequences of numbers, such as for use in vectorizing larger chunks of your code:

```r
x <- 1:5
x
## [1] 1 2 3 4 5

```

This works both ways

```r
10:4
# [1] 10  9  8  7  6  5  4

```

and even with floating point numbers

```r
1.25:5
# [1] 1.25 2.25 3.25 4.25

```

or negatives

```r
-4:4
#[1] -4 -3 -2 -1  0  1  2  3  4

```



## seq()


`seq` is a more flexible function than the `:` operator allowing to specify steps other than 1.

The function creates a sequence from the `start` (default is 1) to the end including that number.

You can supply only the end (`to`) parameter

```r
seq(5)
# [1] 1 2 3 4 5

```

As well as the start

```r
seq(2, 5) # or seq(from=2, to=5)
# [1] 2 3 4 5

```

And finally the step (`by`)

```r
seq(2, 5, 0.5) # or seq(from=2, to=5, by=0.5)
# [1] 2.0 2.5 3.0 3.5 4.0 4.5 5.0

```

`seq` can optionally infer the (evenly spaced) steps when alternatively the desired length of the output (`length.out`) is supplied

```r
seq(2,5, length.out = 10)
# [1] 2.0 2.3 2.6 2.9 3.2 3.5 3.8 4.1 4.4 4.7 5.0

```

If the sequence needs to have the same length as another vector we can use the `along.with` as a shorthand for `length.out = length(x)`

```r
x = 1:8
seq(2,5,along.with = x)
# [1] 2.000000 2.428571 2.857143 3.285714 3.714286 4.142857 4.571429 5.000000

```

There are two useful simplified functions in the `seq` family: `seq_along`, `seq_len`, and `seq.int`. `seq_along` and `seq_len` functions construct the natural (counting) numbers from 1 through N where N is determined by the function argument, the length of a vector or list with `seq_along`, and the integer argument with `seq_len`.

```r
seq_along(x)
# [1] 1 2 3 4 5 6 7 8

```

Note that `seq_along` returns the indices of an existing object.

```r
# counting numbers 1 through 10
seq_len(10)
[1]  1  2  3  4  5  6  7  8  9 10
# indices of existing vector (or list) with seq_along
letters[1:10]
[1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
seq_along(letters[1:10])
[1]  1  2  3  4  5  6  7  8  9 10

```

`seq.int`is the same as `seq` maintained for ancient compatibility.

There is also an old function `sequence`that creates a vector of sequences from a non negative argument.

```r
sequence(4)
# [1] 1 2 3 4
sequence(c(3, 2))
# [1] 1 2 3 1 2
sequence(c(3, 2, 5))
# [1] 1 2 3 1 2 1 2 3 4 5

```



## Vectors


Vectors in R can have different types (e.g. integer, logical, character). The most general way of defining a vector is by using the function `vector()`.

```r
vector('integer',2) # creates a vector of integers of size 2.
vector('character',2) # creates a vector of characters of size 2.
vector('logical',2) # creates a vector of logicals of size 2.

```

However, in R, the shorthand functions are generally more popular.

```r
integer(2) # is the same as vector('integer',2) and creates an integer vector with two elements
character(2) # is the same as vector('integer',2) and creates an character vector with two elements
logical(2) # is the same as vector('logical',2) and creates an logical vector with two elements

```

Creating vectors with values, other than the default values, is also possible. Often the function `c()` is used for this. The c is short for combine or concatenate.

```r
c(1, 2) # creates a integer vector of two elements: 1 and 2.
c('a', 'b') # creates a character vector of two elements: a and b.
c(T,F) # creates a logical vector of two elements: TRUE and FALSE.

```

Important to note here is that R interprets any integer (e.g. 1) as an integer vector of size one. The same holds for numerics (e.g. 1.1), logicals (e.g. T or F), or characters (e.g. 'a'). Therefore, you are in essence combining vectors, which in turn are vectors.

Pay attention that you always have to combine similar vectors. Otherwise, R will try to convert the vectors in vectors of the same type.

```r
c(1,1.1,'a',T) # all types (integer, numeric, character and logical) are converted to the 'lowest' type which is character.

```

Finding elements in vectors can be done with the `[` operator.

```r
vec_int <- c(1,2,3)
vec_char <- c('a','b','c')
vec_int[2] # accessing the second element will return 2
vec_char[2] # accessing the second element will return 'b'

```

This can also be used to change values

```r
vec_int[2] <- 5 # change the second value from 2 to 5
vec_int # returns [1] 1 5 3

```

Finally, the `:` operator (short for the function `seq()`) can be used to quickly create a vector of numbers.

```r
vec_int <- 1:10
vec_int # returns [1] 1 2 3 4 5 6 7 8 9 10

```

This can also be used to subset vectors (from easy to more complex subsets)

```r
vec_char <- c('a','b','c','d','e')
vec_char[2:4] # returns [1] "b" "c" "d"
vec_char[c(1,3,5)] # returns [1] "a" "c" "e"

```



## Expanding a vector with the rep() function


The `rep` function can be used to repeat a vector in a fairly flexible manner.

```r
# repeat counting numbers, 1 through 5 twice
rep(1:5, 2)
[1] 1 2 3 4 5 1 2 3 4 5

# repeat vector with incomplete recycling
rep(1:5, 2, length.out=7)
[1] 1 2 3 4 5 1 2

```

The each argument is especially useful for expanding a vector of statistics of observational/experimental units into a vector of data.frame with repeated observations of these units.

```r
# same except repeat each integer next to each other
rep(1:5, each=2)
[1] 1 1 2 2 3 3 4 4 5 5

```

A nice feature of `rep` regarding involving expansion to such a data structure is that expansion of a vector to an unbalanced panel can be accomplished by replacing the length argument with a vector that dictates the number of times to repeat each element in the vector:

```r
# automated length repetition
rep(1:5, 1:5)
 [1] 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5
# hand-fed repetition length vector
rep(1:5, c(1,1,1,2,2))
[1] 1 2 3 4 4 5 5

```

This should expose the possibility of allowing an external function to feed the second argument of `rep` in order to dynamically construct a vector that expands according to the data.

As with `seq`, faster, simplified versions of `rep` are `rep_len` and `rep.int`. These drop some attributes that `rep` maintains and so may be most useful in situations where speed is a concern and additional aspects of the repeated vector are unnecessary.

```r
# repeat counting numbers, 1 through 5 twice
rep.int(1:5, 2)
[1] 1 2 3 4 5 1 2 3 4 5

# repeat vector with incomplete recycling
rep_len(1:5, length.out=7)
[1] 1 2 3 4 5 1 2

```

