---
metaTitle: "R - strsplit function"
description: "Introduction"
---

# strsplit function



## Introduction


`strsplit` is a useful function for breaking up a vector into an list on some character pattern. With typical R tools, the whole list can be reincorporated to a data.frame or part of the list might be used in a graphing exercise.

Here is a common usage of `strsplit`: break a character vector along a comma separator:

```r
temp <- c("this,that,other", "hat,scarf,food", "woman,man,child")
# get a list split by commas
myList <- strsplit(temp, split=",")
# print myList
myList
[[1]]
[1] "this"  "that"  "other"

[[2]]
[1] "hat"   "scarf" "food" 

[[3]]
[1] "woman" "man"   "child"

```

As hinted above, the split argument is not limited to characters, but may follow a pattern dictated by a regular expression. For example, temp2 is identical to temp above except that the separators have been altered for each item. We can take advantage of the fact that the split argument accepts regular expressions to alleviate the irregularity in the vector.

```r
temp2 <- c("this, that, other", "hat,scarf ,food", "woman; man ; child")
myList2 <- strsplit(temp2, split=" ?[,;] ?")
myList2
[[1]]
[1] "this"  "that"  "other"

[[2]]
[1] "hat"   "scarf" "food" 

[[3]]
[1] "woman" "man"   "child"

```

**Notes**:

1. breaking down the regular expression syntax is out of scope for this example.
1. Sometimes matching regular expressions can slow down a process. As with many R functions that allow the use of regular expressions, the fixed argument is available to tell R to match on the split characters literally.



#### Syntax


- strsplit(
- x
- split
- fixed = FALSE
- perl = FALSE
- useBytes = FALSE)

