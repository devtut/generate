---
metaTitle: "Column wise operation"
description: "sum of each column"
---

# Column wise operation



## sum of each column


Suppose we need to do the `sum` of each column in a dataset

```r
set.seed(20)
df1 <- data.frame(ID = rep(c("A", "B", "C"), each = 3), V1 = rnorm(9), V2 = rnorm(9))
m1 <- as.matrix(df1[-1])

```

There are many ways to do this.   Using `base R`, the best option would be `colSums`

```r
colSums(df1[-1], na.rm = TRUE)

```

Here, we removed the first column as it is non-numeric and did the `sum` of each column, specifying the `na.rm = TRUE` (in case there are any NAs in the dataset)

This also works with `matrix`

```r
colSums(m1, na.rm = TRUE)

```

This can be done in a loop with `lapply/sapply/vapply`

```

lapply(df1[-1], sum, na.rm = TRUE)

```

It should be noted that the output is a `list`.  If we need a `vector` output

```

sapply(df1[-1], sum, na.rm = TRUE)

```

Or

```

vapply(df1[-1], sum, na.rm = TRUE, numeric(1))

```

For matrices, if we want to loop through columns, then use `apply` with `MARGIN = 1`

```

apply(m1, 2, FUN = sum, na.rm = TRUE)

```

There are ways to do this with packages like `dplyr` or `data.table`

```

library(dplyr)
 df1 %>%
     summarise_at(vars(matches("^V\\d+")), sum, na.rm = TRUE)

```

Here, we are passing a regular expression to match the column names that we need to get the `sum` in `summarise_at`.  The regex will match all columns that start with `V` followed by one or more numbers (`\\d+`).

A `data.table` option is

```r
library(data.table)   
setDT(df1)[, lapply(.SD, sum, na.rm = TRUE), .SDcols = 2:ncol(df1)]

```

We convert the 'data.frame' to 'data.table' (`setDT(df1)`), specified the columns to be applied the function in `.SDcols` and loop through the Subset of Data.table (`.SD`) and get the `sum`.

If we need to use a group by operation, we can do this easily by specifying the group by column/columns

```

df1 %>%
   group_by(ID) %>%   
   summarise_at(vars(matches("^V\\d+")), sum, na.rm = TRUE)

```

In cases where we need the `sum` of all the columns, `summarise_each` can be used instead of `summarise_at`

```r
df1 %>%
    group_by(ID) %>%
    summarise_each(funs(sum(., na.rm = TRUE)))

```

The `data.table` option is

```r
setDT(df1)[, lapply(.SD, sum, na.rm = TRUE), by = ID]   

```

