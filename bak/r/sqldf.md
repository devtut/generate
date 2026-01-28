---
metaTitle: "R - sqldf"
description: "Basic Usage Examples"
---

# sqldf



## Basic Usage Examples


`sqldf()` from the package `sqldf` allows the use of SQLite queries to select and manipulate data in R. SQL queries are entered as character strings.

To select the first 10 rows of the "diamonds" dataset from the package `ggplot2`, for example:

```r
data("diamonds")
head(diamonds)

```

> 

```r
# A tibble: 6 x 10
  carat       cut color clarity depth table price     x     y     z
  <dbl>     <ord> <ord>   <ord> <dbl> <dbl> <int> <dbl> <dbl> <dbl>
1  0.23     Ideal     E     SI2  61.5    55   326  3.95  3.98  2.43
2  0.21   Premium     E     SI1  59.8    61   326  3.89  3.84  2.31
3  0.23      Good     E     VS1  56.9    65   327  4.05  4.07  2.31
4  0.29   Premium     I     VS2  62.4    58   334  4.20  4.23  2.63
5  0.31      Good     J     SI2  63.3    58   335  4.34  4.35  2.75
6  0.24 Very Good     J    VVS2  62.8    57   336  3.94  3.96  2.48

```




```r
require(sqldf)
sqldf("select * from diamonds limit 10")

```

> 

```r
   carat       cut color clarity depth table price    x    y    z
1   0.23     Ideal     E     SI2  61.5    55   326 3.95 3.98 2.43
2   0.21   Premium     E     SI1  59.8    61   326 3.89 3.84 2.31
3   0.23      Good     E     VS1  56.9    65   327 4.05 4.07 2.31
4   0.29   Premium     I     VS2  62.4    58   334 4.20 4.23 2.63
5   0.31      Good     J     SI2  63.3    58   335 4.34 4.35 2.75
6   0.24 Very Good     J    VVS2  62.8    57   336 3.94 3.96 2.48
7   0.24 Very Good     I    VVS1  62.3    57   336 3.95 3.98 2.47
8   0.26 Very Good     H     SI1  61.9    55   337 4.07 4.11 2.53
9   0.22      Fair     E     VS2  65.1    61   337 3.87 3.78 2.49
10  0.23 Very Good     H     VS1  59.4    61   338 4.00 4.05 2.39

```




To select the first 10 rows where for the color "E":

```r
sqldf("select * from diamonds where color = 'E' limit 10")

```

> 

```r
   carat       cut color clarity depth table price    x    y    z
1   0.23     Ideal     E     SI2  61.5    55   326 3.95 3.98 2.43
2   0.21   Premium     E     SI1  59.8    61   326 3.89 3.84 2.31
3   0.23      Good     E     VS1  56.9    65   327 4.05 4.07 2.31
4   0.22      Fair     E     VS2  65.1    61   337 3.87 3.78 2.49
5   0.20   Premium     E     SI2  60.2    62   345 3.79 3.75 2.27
6   0.32   Premium     E      I1  60.9    58   345 4.38 4.42 2.68
7   0.23 Very Good     E     VS2  63.8    55   352 3.85 3.92 2.48
8   0.23 Very Good     E     VS1  60.7    59   402 3.97 4.01 2.42
9   0.23 Very Good     E     VS1  59.5    58   402 4.01 4.06 2.40
10  0.23      Good     E     VS1  64.1    59   402 3.83 3.85 2.46

```




Notice in the example above that quoted strings within the SQL query are quoted using '' if the overall query is quoted with "" (this also works in reverse).

Suppose that we wish to add a new column to count the number of Premium cut diamonds over 1 carat:

```r
sqldf("select count(*) from diamonds where carat > 1 and color = 'E'")

```

> 

```r
  count(*)
1     1892

```




Results of created values can also be returned as new columns:

```r
sqldf("select *, count(*) as cnt_big_E_colored_stones from diamonds where carat > 1 and color = 'E' group by clarity")

```

> 

```r
  carat       cut color clarity depth table price    x    y    z cnt_big_E_colored_stones
1  1.30      Fair     E      I1  66.5    58  2571 6.79 6.75 4.50                       65
2  1.28     Ideal     E      IF  60.7    57 18700 7.09 6.99 4.27                       28
3  2.02 Very Good     E     SI1  59.8    59 18731 8.11 8.20 4.88                      499
4  2.03   Premium     E     SI2  61.5    59 18477 8.24 8.16 5.04                      666
5  1.51     Ideal     E     VS1  61.5    57 18729 7.34 7.40 4.53                      158
6  1.72 Very Good     E     VS2  63.4    56 18557 7.65 7.55 4.82                      318
7  1.20     Ideal     E    VVS1  61.8    56 16256 6.78 6.87 4.22                       52
8  1.55     Ideal     E    VVS2  62.5    55 18188 7.38 7.40 4.62                      106

```




If one would be interested what is the **max** `price` of the diamond **according** to the `cut`:

```r
sqldf("select cut,  max(price) from diamonds group by cut")

        cut max(price)
1      Fair      18574
2      Good      18788
3     Ideal      18806
4   Premium      18823
5 Very Good      18818

```

