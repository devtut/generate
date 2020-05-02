---
metaTitle: "String manipulation with stringi package"
description: "Count pattern inside string, Duplicating strings, Paste vectors, Splitting text by some fixed pattern"
---

# String manipulation with stringi package



## Count pattern inside string


With fixed pattern

```r
stri_count_fixed("babab", "b")
# [1] 3
stri_count_fixed("babab", "ba")
# [1] 2
stri_count_fixed("babab", "bab")
# [1] 1

```

**Natively:**

```r
length(gregexpr("b","babab")[[1]])
# [1] 3
length(gregexpr("ba","babab")[[1]])
# [1] 2
length(gregexpr("bab","babab")[[1]])
# [1] 1

```

function is vectorized over string and pattern:

```r
stri_count_fixed("babab", c("b","ba"))
# [1] 3 2
stri_count_fixed(c("babab","bbb","bca","abc"), c("b","ba"))
# [1] 3 0 1 0

```

**A base R solution**:

```r
sapply(c("b","ba"),function(x)length(gregexpr(x,"babab")[[1]]))
# b ba 
# 3  2

```

With regex

First example - find `a` and any character after

Second example - find `a` and any digit after

```r
stri_count_regex("a1 b2 a3 b4 aa", "a.")
# [1] 3
stri_count_regex("a1 b2 a3 b4 aa", "a\\d")
# [1] 2

```



## Duplicating strings


```r
stri_dup("abc",3)
# [1] "abcabcabc"

```

**A base R solution that does the same would look like this:**

```r
paste0(rep("abc",3),collapse = "")
# [1] "abcabcabc"

```



## Paste vectors


```r
stri_paste(LETTERS,"-", 1:13)
# [1] "A-1"  "B-2"  "C-3"  "D-4"  "E-5"  "F-6"  "G-7"  "H-8"  "I-9"  "J-10" "K-11" "L-12" "M-13" 
# [14] "N-1"  "O-2"  "P-3"  "Q-4"  "R-5"  "S-6"  "T-7"  "U-8"  "V-9"  "W-10" "X-11" "Y-12" "Z-13"

```

**Natively, we could do this in R via:**

```r
> paste(LETTERS,1:13,sep="-")
 #[1] "A-1"  "B-2"  "C-3"  "D-4"  "E-5"  "F-6"  "G-7"  "H-8"  "I-9"  "J-10" "K-11" "L-12" "M-13"
 #[14] "N-1"  "O-2" "P-3"  "Q-4"  "R-5"  "S-6"  "T-7"  "U-8"  "V-9"  "W-10" "X-11" "Y-12" "Z-13"

```



## Splitting text by some fixed pattern


Split vector of texts using one pattern:

```r
stri_split_fixed(c("To be or not to be.", "This is very short sentence.")," ")
# [[1]]
# [1] "To"  "be"  "or"  "not" "to"  "be."
# 
# [[2]]
# [1] "This"      "is"        "very"      "short"     "sentence."

```

Split one text using many patterns:

```r
stri_split_fixed("Apples, oranges and pineaplles.",c(" ", ",", "s"))
# [[1]]
# [1] "Apples,"     "oranges"     "and"         "pineaplles."
# 
# [[2]]
# [1] "Apples"                   " oranges and pineaplles."
# 
# [[3]]
# [1] "Apple"          ", orange"       " and pineaplle" "."     

```



#### Remarks


To install package simply run:

```r
install.packages("stringi")

```

to load it:

```r
require("stringi")

```

