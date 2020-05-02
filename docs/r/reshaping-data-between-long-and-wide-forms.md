---
metaTitle: "Reshaping data between long and wide forms"
description: "Reshaping data, The reshape function"
---

# Reshaping data between long and wide forms


In R, tabular data is stored in [data frames](http://stackoverflow.com/documentation/r/438). This topic covers the various ways of transforming a single table.



## Reshaping data


Often data comes in tables. Generally one can divide this tabular data in wide and long formats. In a wide format, each variable has its own column.

|Person|Height [cm]|Age [yr]
|------
|Alison|178|20
|Bob|174|45
|Carl|182|31

However, sometimes it is more convenient to have a long format, in which all variables are in one column and the values are in a second column.

|Person|Variable|Value
|------
|Alison|Height [cm]|178
|Bob|Height [cm]|174
|Carl|Height [cm]|182
|Alison|Age [yr]|20
|Bob|Age [yr]|45
|Carl|Age [yr]|31

Base R, as well as third party packages can be used to simplify this process. For each of the options, the `mtcars` dataset will be used. By default, this dataset is in a long format. In order for the packages to work, we will insert the row names as the first column.

```r
mtcars # shows the dataset
data <- data.frame(observation=row.names(mtcars),mtcars)

```

### Base R

There are two functions in base R that can be used to convert between wide and long format: `stack()` and `unstack()`.

```r
long <- stack(data)
long # this shows the long format
wide <- unstack(long)    
wide # this shows the wide format

```

However, these functions can become very complex for more advanced use cases. Luckily, there are other options using third party packages.

### The tidyr package

This package uses `gather()` to convert from wide to long and `spread()` to convert from long to wide.

```r
library(tidyr)
long <- gather(data, variable, value, 2:12) # where variable is the name of the 
# variable column, value indicates the name of the value column and 2:12 refers to
# the columns to be converted.
long # shows the long result
wide <- spread(long,variable,value)
wide # shows the wide result (~data)

```

### The data.table package

The data.table package extends the `reshape2` functions and uses the function `melt()` to go from wide to long and `dcast()` to go from long to wide.

```r
library(data.table)
long <- melt(data,'observation',2:12,'variable', 'value')
long # shows the long result
wide <- dcast(long, observation ~ variable)
wide # shows the wide result (~data)

```



## The reshape function


The most flexible base R function for reshaping data is `reshape`. See `?reshape` for its syntax.

```r
# create unbalanced longitudinal (panel) data set
set.seed(1234)
df <- data.frame(identifier=rep(1:5, each=3),
                 location=rep(c("up", "down", "left", "up", "center"), each=3),
                 period=rep(1:3, 5), counts=sample(35, 15, replace=TRUE),
                 values=runif(15, 5, 10))[-c(4,8,11),]
df

   identifier location period counts   values
1           1       up      1      4 9.186478
2           1       up      2     22 6.431116
3           1       up      3     22 6.334104
5           2     down      2     31 6.161130
6           2     down      3     23 6.583062
7           3     left      1      1 6.513467
9           3     left      3     24 5.199980
10          4       up      1     18 6.093998
12          4       up      3     20 7.628488
13          5   center      1     10 9.573291
14          5   center      2     33 9.156725
15          5   center      3     11 5.228851

```

Note that the data.frame is unbalanced, that is, unit 2 is missing an observation in the first period, while units 3 and 4 are missing observations in the second period. Also, note that there are two variables that vary over the periods: counts and values, and two that do not vary: identifier and location.

### Long to Wide

To reshape the data.frame to wide format,

```r
# reshape wide on time variable
df.wide <- reshape(df, idvar="identifier", timevar="period",
                   v.names=c("values", "counts"), direction="wide")
df.wide
   identifier location values.1 counts.1 values.2 counts.2 values.3 counts.3
1           1       up 9.186478        4 6.431116       22 6.334104       22
5           2     down       NA       NA 6.161130       31 6.583062       23
7           3     left 6.513467        1       NA       NA 5.199980       24
10          4       up 6.093998       18       NA       NA 7.628488       20
13          5   center 9.573291       10 9.156725       33 5.228851       11

```

Notice that the missing time periods are filled in with NAs.

In reshaping wide, the "v.names"  argument specifies the columns that vary over time. If the location variable is not necessary, it can be dropped prior to reshaping with the "drop" argument. In dropping the only non-varying / non-id column from the data.frame, the v.names argument becomes unnecessary.

```r
reshape(df, idvar="identifier", timevar="period", direction="wide",
        drop="location")

```

### Wide to Long

To reshape long with the current df.wide, a minimal syntax is

```r
reshape(df.wide, direction="long")

```

However, this is typically trickier:

```r
# remove "." separator in df.wide names for counts and values
names(df.wide)[grep("\\.", names(df.wide))] <-
              gsub("\\.", "", names(df.wide)[grep("\\.", names(df.wide))])

```

Now the simple syntax will produce an error about undefined columns.

With column names that are more difficult for the `reshape` function to automatically parse, it is sometimes necessary to add the "varying" argument which tells `reshape` to group particular variables in wide format for the transformation into long format. This argument takes a list of vectors of variable names or indices.

```r
reshape(df.wide, idvar="identifier",
        varying=list(c(3,5,7), c(4,6,8)), direction="long")

```

In reshaping long, the "v.names" argument can be provided to rename the resulting varying variables.

Sometimes the specification of "varying" can be avoided by use of the "sep" argument which tells `reshape` what part of the variable name specifies the value argument and which specifies the time argument.



#### Remarks


### Helpful packages

- [Reshaping, stacking and splitting](http://stackoverflow.com/documentation/data.table/4117/) with data.table
- [Reshape using tidyr](http://stackoverflow.com/documentation/r/9195)
- splitstackshape

