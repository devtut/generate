---
metaTitle: "Non-standard evaluation and standard evaluation"
description: "Examples with standard dplyr verbs"
---

# Non-standard evaluation and standard evaluation


Dplyr and many modern libraries in R use non-standard evaluation (NSE) for interactive programming and standard evaluation (SE) for programming[1](https://cran.r-project.org/web/packages/dplyr/vignettes/nse.html).

For instance, the `summarise()` function use non-standard evaluation but relies on the `summarise_()` which uses standard evaluation.

The lazyeval library makes it easy to turn standard evaluation function into NSE functions.



## Examples with standard dplyr verbs


NSE functions should be used in interactive programming. However, when developping new functions in a new package, it's better to use SE version.

Load dplyr and lazyeval :

```r
library(dplyr)
library(lazyeval)

```

**Filtering**

**NSE version**

```r
filter(mtcars, cyl == 8)
filter(mtcars, cyl < 6)
filter(mtcars, cyl < 6 & vs == 1)

```

**SE version (to be use when programming functions in a new package)**

```r
filter_(mtcars, .dots = list(~ cyl == 8))
filter_(mtcars, .dots = list(~ cyl < 6))
filter_(mtcars, .dots = list(~ cyl < 6, ~ vs == 1))

```

**Summarise**

**NSE version**

```r
summarise(mtcars,  mean(disp))
summarise(mtcars,  mean_disp = mean(disp))

```

**SE version**

```r
summarise_(mtcars, .dots = lazyeval::interp(~ mean(x), x = quote(disp)))
summarise_(mtcars, .dots = setNames(list(lazyeval::interp(~ mean(x), x = quote(disp))), "mean_disp"))
summarise_(mtcars, .dots = list("mean_disp" = lazyeval::interp(~ mean(x), x = quote(disp))))

```

**Mutate**

**NSE version**

```r
mutate(mtcars, displ_l = disp / 61.0237)

```

**SE version**

```r
mutate_(
    .data = mtcars, 
    .dots = list(
        "displ_l" = lazyeval::interp(
                        ~ x / 61.0237, x = quote(disp)
            )
         )
)

```

