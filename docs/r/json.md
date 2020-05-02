---
metaTitle: "JSON"
description: "JSON to / from R objects"
---

# JSON



## JSON to / from R objects


The [`jsonlite` package](https://cran.r-project.org/web/packages/jsonlite/index.html) is a fast JSON parser and generator optimized for statistical data and the web. The two main functions used to read and write JSON are `fromJSON()` and `toJSON()` respecitively, and are designed to work with `vectors`, `matrices` and `data.frames`, and streams of JSON from the web.

Create a JSON array from a vector, and vice versa

```r
library(jsonlite)

## vector to JSON
toJSON(c(1,2,3))
# [1,2,3]

fromJSON('[1,2,3]')
# [1] 1 2 3

```

Create a named JSON array from a list, and vice versa

```r
toJSON(list(myVec = c(1,2,3)))
# {"myVec":[1,2,3]}

fromJSON('{"myVec":[1,2,3]}')
# $myVec
# [1] 1 2 3

```

More complex list structures

```r
## list structures
lst <- list(a = c(1,2,3),
            b = list(letters[1:6]))

toJSON(lst)
# {"a":[1,2,3],"b":[["a","b","c","d","e","f"]]} 


fromJSON('{"a":[1,2,3],"b":[["a","b","c","d","e","f"]]} ')
# $a
# [1] 1 2 3
# 
# $b
# [,1] [,2] [,3] [,4] [,5] [,6]
# [1,] "a"  "b"  "c"  "d"  "e"  "f" 

```

Create JSON from a `data.frame`, and vice versa

```r
## converting a data.frame to JSON
df <- data.frame(id = seq_along(1:10),
                 val = letters[1:10])

toJSON(df)
# [{"id":1,"val":"a"},{"id":2,"val":"b"},{"id":3,"val":"c"},{"id":4,"val":"d"},{"id":5,"val":"e"},{"id":6,"val":"f"},{"id":7,"val":"g"},{"id":8,"val":"h"},{"id":9,"val":"i"},{"id":10,"val":"j"}] 

## reading a JSON string
fromJSON('[{"id":1,"val":"a"},{"id":2,"val":"b"},{"id":3,"val":"c"},{"id":4,"val":"d"},{"id":5,"val":"e"},{"id":6,"val":"f"},{"id":7,"val":"g"},{"id":8,"val":"h"},{"id":9,"val":"i"},{"id":10,"val":"j"}]')
#     id val
# 1   1   a
# 2   2   b
# 3   3   c
# 4   4   d
# 5   5   e
# 6   6   f
# 7   7   g
# 8   8   h
# 9   9   i
# 10 10   j

```

Read JSON direct from the internet

```r
## Reading JSON from URL
googleway_issues <- fromJSON("https://api.github.com/repos/SymbolixAU/googleway/issues")

googleway_issues$url
# [1] "https://api.github.com/repos/SymbolixAU/googleway/issues/20" "https://api.github.com/repos/SymbolixAU/googleway/issues/19"
# [3] "https://api.github.com/repos/SymbolixAU/googleway/issues/14" "https://api.github.com/repos/SymbolixAU/googleway/issues/11"
# [5] "https://api.github.com/repos/SymbolixAU/googleway/issues/9"  "https://api.github.com/repos/SymbolixAU/googleway/issues/5" 
# [7] "https://api.github.com/repos/SymbolixAU/googleway/issues/2"

```

