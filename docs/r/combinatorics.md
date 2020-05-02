---
metaTitle: "Combinatorics"
description: "Enumerating combinations of a specified length, Counting combinations of a specified length"
---

# Combinatorics



## Enumerating combinations of a specified length


### Without replacement

With `combn`, each vector appears in a column:

```r
combn(LETTERS, 3)

# Showing only first 10.
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
[1,] "A"  "A"  "A"  "A"  "A"  "A"  "A"  "A"  "A"  "A"  
[2,] "B"  "B"  "B"  "B"  "B"  "B"  "B"  "B"  "B"  "B"  
[3,] "C"  "D"  "E"  "F"  "G"  "H"  "I"  "J"  "K"  "L"

```

### With replacement

With `expand.grid`, each vector appears in a row:

```r
expand.grid(LETTERS, LETTERS, LETTERS)
# or 
do.call(expand.grid, rep(list(LETTERS), 3))

# Showing only first 10.
   Var1 Var2 Var3
1     A    A    A
2     B    A    A
3     C    A    A
4     D    A    A
5     E    A    A
6     F    A    A
7     G    A    A
8     H    A    A
9     I    A    A
10    J    A    A

```

For the special case of pairs, `outer` can be used, putting each vector into a cell:

```r
# FUN here is used as a function executed on each resulting pair.
# in this case it's string concatenation.
outer(LETTERS, LETTERS, FUN=paste0)

# Showing only first 10 rows and columns
      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
 [1,] "AA" "AB" "AC" "AD" "AE" "AF" "AG" "AH" "AI" "AJ"
 [2,] "BA" "BB" "BC" "BD" "BE" "BF" "BG" "BH" "BI" "BJ"
 [3,] "CA" "CB" "CC" "CD" "CE" "CF" "CG" "CH" "CI" "CJ"
 [4,] "DA" "DB" "DC" "DD" "DE" "DF" "DG" "DH" "DI" "DJ"
 [5,] "EA" "EB" "EC" "ED" "EE" "EF" "EG" "EH" "EI" "EJ"
 [6,] "FA" "FB" "FC" "FD" "FE" "FF" "FG" "FH" "FI" "FJ"
 [7,] "GA" "GB" "GC" "GD" "GE" "GF" "GG" "GH" "GI" "GJ"
 [8,] "HA" "HB" "HC" "HD" "HE" "HF" "HG" "HH" "HI" "HJ"
 [9,] "IA" "IB" "IC" "ID" "IE" "IF" "IG" "IH" "II" "IJ"
[10,] "JA" "JB" "JC" "JD" "JE" "JF" "JG" "JH" "JI" "JJ"

```



## Counting combinations of a specified length


### Without replacement

```r
choose(length(LETTERS), 5)
[1] 65780

```

### With replacement

```r
length(letters)^5
[1] 11881376

```

