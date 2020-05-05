---
metaTitle: "R - Cleaning data"
description: "Removing missing data from a vector, Removing incomplete rows"
---

# Cleaning data




## Removing missing data from a vector


First lets create a vector called Vector1:

```r
set.seed(123)
Vector1 <- rnorm(20)

```

And add missing data to it:

```r
set.seed(123)
Vector1[sample(1:length(Vector1), 5)] <- NA

```

Now we can use the is.na function to subset the Vector

```r
Vector1 <- Vector1[!is.na(Vector1)]

```

Now the resulting vector will have removed the NAs of the original Vector1



## Removing incomplete rows


There might be times where you have a data frame and you want to remove all the rows that might contain an NA value, for that the function **complete.cases** is the best option.

We will use the first 6 rows of the **airquality** dataset to make an example since it already has NAs

```r
x <- head(airquality)

```

This has two rows with NAs in the Solar.R column, to remove them we do the following

```r
x_no_NA <- x[complete.cases(x),]

```

The resulting dataframe **x_no_NA** will only have complete rows without NAs

