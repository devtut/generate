---
metaTitle: "R - Missing values"
description: "Examining missing data, Reading and writing data with NA values, Using NAs of different classes, TRUE/FALSE and/or NA, Omitting or replacing missing values"
---

# Missing values


When we don't know the value a variable takes, we say its value is missing, indicated by `NA`.



## Examining missing data


`anyNA` reports whether any missing values are present; while `is.na` reports missing values elementwise:

```r
vec <- c(1, 2, 3, NA, 5)

anyNA(vec)
# [1] TRUE
is.na(vec)
# [1] FALSE FALSE FALSE  TRUE FALSE

```

`ìs.na` returns a logical vector that is coerced to integer values under arithmetic operations (with FALSE=0, TRUE=1). We can use this to find out how many missing values there are:

```r
sum(is.na(vec))
# [1] 1

```

Extending this approach, we can use `colSums` and `is.na` on a [data frame](http://stackoverflow.com/documentation/r/438) to count NAs per column:

```r
colSums(is.na(airquality))
#   Ozone Solar.R    Wind    Temp   Month     Day 
#      37       7       0       0       0       0 

```

The [naniar package](https://github.com/njtierney/naniar) (currently on github but not CRAN) offers further tools for exploring missing values.



## Reading and writing data with NA values


When reading tabular datasets with the `read.*` functions, R automatically looks for missing values that look like `"NA"`. However, missing values are not always represented by `NA`. Sometimes a dot (`.`), a hyphen(`-`) or a character-value (e.g.: `empty`) indicates that a value is `NA`. The `na.strings` parameter of the `read.*` function can be used to tell R which symbols/characters need to be treated as `NA` values:

```r
read.csv("name_of_csv_file.csv", na.strings = "-")

```

It is also possible to indicate that more than one symbol needs to be read as `NA`:

```r
read.csv('missing.csv', na.strings = c('.','-'))

```

Similarly, `NA`s can be written with customized strings using the `na` argument to `write.csv`. [Other tools for reading and writing tables](http://stackoverflow.com/documentation/r/5543/input-and-output/19700/reading-and-writing-data-frames#t=201702071913296746359) have similar options.



## Using NAs of different classes


The symbol `NA` is for a `logical` missing value:

```r
class(NA)
#[1] "logical"

```

This is convenient, since it can easily be coerced to other atomic vector types, and is therefore usually the only `NA` you will need:

```r
x <- c(1, NA, 1)
class(x[2])
#[1] "numeric"

```

If you do need a single `NA` value of another type, use `NA_character_`, `NA_integer_`, `NA_real_` or `NA_complex_`. For missing values of fancy classes, subsetting with `NA_integer_` usually works; for example, to get a missing-value Date:

```r
class(Sys.Date()[NA_integer_])
# [1] "Date"

```



## TRUE/FALSE and/or NA


`NA` is a logical type and a logical operator with an `NA` will return `NA` if the outcome is ambiguous. Below, `NA OR TRUE` evaluates to `TRUE` because at least one side evaluates to `TRUE`, however `NA OR FALSE` returns `NA` because we do not know whether `NA` would have been `TRUE` or `FALSE`

```r
NA | TRUE
# [1] TRUE  
# TRUE | TRUE is TRUE and FALSE | TRUE is also TRUE.

NA | FALSE
# [1] NA  
# TRUE | FALSE is TRUE but FALSE | FALSE is FALSE.

NA & TRUE
# [1] NA  
# TRUE & TRUE is TRUE but FALSE & TRUE is FALSE.

NA & FALSE
# [1] FALSE
# TRUE & FALSE is FALSE and FALSE & FALSE is also FALSE.

```

These properties are helpful if you want to subset a data set based on some columns that contain `NA`.

```r
df <- data.frame(v1=0:9, 
                 v2=c(rep(1:2, each=4), NA, NA), 
                 v3=c(NA, letters[2:10]))

df[df$v2 == 1 & !is.na(df$v2), ]
#  v1 v2   v3
#1  0  1 <NA>
#2  1  1    b
#3  2  1    c
#4  3  1    d

df[df$v2 == 1, ]
     v1 v2   v3
#1     0  1 <NA>
#2     1  1    b
#3     2  1    c
#4     3  1    d
#NA   NA NA <NA>
#NA.1 NA NA <NA>

```



## Omitting or replacing missing values


### Recoding missing values

Regularly, missing data isn't coded as `NA` in datasets. In SPSS for example, missing values are often represented by the value `99`.

```r
num.vec <- c(1, 2, 3, 99, 5)
num.vec
## [1]  1  2  3 99  5

```

It is possible to directly assign NA using subsetting

```r
num.vec[num.vec == 99] <- NA

```

However, the  preferred method is to use `is.na<-` as below. The help file (`?is.na`) states:

> 
`is.na<-` may provide a safer way to set missingness. It behaves differently for factors, for example.


```r
is.na(num.vec) <- num.vec == 99

```

Both methods return

```r
num.vec
## [1]  1  2  3 NA  5

```

### Removing missing values

Missing values can be removed in several ways from a vector:

```r
num.vec[!is.na(num.vec)]
num.vec[complete.cases(num.vec)]
na.omit(num.vec)
## [1] 1 2 3 5

```

### Excluding missing values from calculations

When using arithmetic functions on vectors with missing values, a missing value will be returned:

```r
mean(num.vec) # returns: [1] NA

```

The `na.rm` parameter tells the function to exclude the `NA` values from the calculation:

```r
mean(num.vec, na.rm = TRUE) # returns: [1] 2.75

# an alternative to using 'na.rm = TRUE':
mean(num.vec[!is.na(num.vec)]) # returns: [1] 2.75

```

Some R functions, like `lm`, have a `na.action` parameter. The default-value for this is `na.omit`, but with `options(na.action = 'na.exclude')` the default behavior of R can be changed.

If it is not necessary to change the default behavior, but for a specific situation another `na.action` is needed, the `na.action` parameter needs to be included in the function call, e.g.:

```

lm(y2 ~ y1, data = anscombe, na.action = 'na.exclude')

```



#### Remarks


Missing values are represented by the symbol `NA` (not available). Impossible values (e.g., as a result of `sqrt(-1)`) are represented by the symbol `NaN` (not a number).

