---
metaTitle: "Reshape using tidyr"
description: "Reshape from long to wide format with spread(), Reshape from wide to long format with gather()"
---

# Reshape using tidyr


tidyr has two tools for reshaping data: `gather` (wide to long) and `spread` (long to wide).

See [Reshaping data](http://stackoverflow.com/documentation/r/2904) for other options.



## Reshape from long to wide format with spread()


```r
library(tidyr)

## example data
set.seed(123)
df <- data.frame(
  name = rep(c("firstName", "secondName"), each=4),
  numbers = rep(1:4, 2),
  value = rnorm(8)
)
df
#         name numbers       value
# 1  firstName       1 -0.56047565
# 2  firstName       2 -0.23017749
# 3  firstName       3  1.55870831
# 4  firstName       4  0.07050839
# 5 secondName       1  0.12928774
# 6 secondName       2  1.71506499
# 7 secondName       3  0.46091621
# 8 secondName       4 -1.26506123

```

We can "spread" the 'numbers' column, into separate columns:

```r
spread(data = df,
       key = numbers,
       value = value)
#      name          1          2         3           4
# 1  firstName -0.5604756 -0.2301775 1.5587083  0.07050839
# 2 secondName  0.1292877  1.7150650 0.4609162 -1.26506123

```

Or spread the 'name' column into separate columns:

```r
spread(data = df, 
       key = name,
       value = value)
#   numbers   firstName secondName
# 1       1 -0.56047565  0.1292877
# 2       2 -0.23017749  1.7150650
# 3       3  1.55870831  0.4609162
# 4       4  0.07050839 -1.2650612

```



## Reshape from wide to long format with gather()


### 

```r
library(tidyr)

## example data
df <- read.table(text ="  numbers  firstName secondName
1       1  1.5862639  0.4087477
2       2  0.1499581  0.9963923
3       3  0.4117353  0.3740009
4       4 -0.4926862  0.4437916", header = T)
df
#   numbers  firstName secondName
# 1       1  1.5862639  0.4087477
# 2       2  0.1499581  0.9963923
# 3       3  0.4117353  0.3740009
# 4       4 -0.4926862  0.4437916

```

We can gather the columns together using 'numbers' as the key column:

```r
gather(data = df,
       key = numbers,
       value = myValue)
#   numbers    numbers    myValue
# 1       1  firstName  1.5862639
# 2       2  firstName  0.1499581
# 3       3  firstName  0.4117353
# 4       4  firstName -0.4926862
# 5       1 secondName  0.4087477
# 6       2 secondName  0.9963923
# 7       3 secondName  0.3740009
# 8       4 secondName  0.4437916

```

