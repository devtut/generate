---
metaTitle: "Regular Expressions (regex)"
description: "Differences between Perl and POSIX regex, Validate a date in a YYYYMMDD format, Validate US States postal abbreviations, Validate US phone numbers, Escaping characters in R regex patterns, Eliminating Whitespace"
---

# Regular Expressions (regex)


Regular expressions (also called "regex" or "regexp") define patterns that can be [matched against a string](http://stackoverflow.com/documentation/r/1123). Type `?regex` for the official R documentation and see the [Regex Docs](http://stackoverflow.com/documentation/regex/topics) for more details. The most important 'gotcha' that will not be learned in the SO regex/topics is that most R-regex functions need the use of paired backslashes to escape in a `pattern` parameter.



## Differences between Perl and POSIX regex


There are two ever-so-slightly different engines of regular expressions implemented in R. The default is called POSIX-consistent; all regex functions in R are also equipped with an option to turn on the latter type: `perl = TRUE`.

### Look-ahead/look-behind

`perl = TRUE` enables look-ahead and look-behind in regular expressions.

- `"(?<=A)B"` matches an appearance of the letter `B` **only if** it's preceded by `A`, i.e. `"ABACADABRA"` would be matched, but `"abacadabra"` and `"aBacadabra"` would not.



## Validate a date in a "YYYYMMDD" format


It is a common practice to name files using the date as prefix in the following format: `YYYYMMDD`, for example: `20170101_results.csv`. A date in such string format can be verified using the following regular expression:

```r
\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])

```

The above expression considers dates from year: `0000-9999`, months between: `01-12` and days `01-31`.

For example:

```r
> grepl("\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])", "20170101")
[1] TRUE
> grepl("\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])", "20171206")
[1] TRUE
> grepl("\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])", "29991231")
[1] TRUE

```

**Note**: It validates the date syntax, but we can have a wrong date with a valid syntax, for example: `20170229` (2017 it is not a leap year).

```r
> grepl("\\d{4}(0[1-9]|1[012])(0[1-9]|[12][0-9]|3[01])", "20170229")
[1] TRUE

```

If you want to validate a date, it can be done via this user defined function:

```r
is.Date <- function(x) {return(!is.na(as.Date(as.character(x), format = '%Y%m%d')))}

```

Then

```r
> is.Date(c("20170229", "20170101", 20170101))
[1] FALSE  TRUE  TRUE

```



## Validate US States postal abbreviations


The following `regex` includes 50 states and also Commonwealth/Territory (see [www.50states.com](http://www.50states.com/abbreviations.htm)):

`regex <- "(A[LKSZR])|(C[AOT])|(D[EC])|(F[ML])|(G[AU])|(HI)|(I[DLNA])|(K[SY])|(LA)|(M[EHDAINSOT])|(N[EVHJMYCD])|(MP)|(O[HKR])|(P[WAR])|(RI)|(S[CD])|(T[NX])|(UT)|(V[TIA])|(W[AVIY])"`

For example:

```r
> test <- c("AL", "AZ", "AR", "AJ", "AS", "DC", "FM", "GU","PW", "FL", "AJ", "AP")
> grepl(us.states.pattern, test)
 [1]  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE
> 

```

**Note**:

If you want to verify only the 50 States, then we recommend to use the R-dataset: `state.abb` from `state`, for example:

```r
> data(state)
> test %in% state.abb
[1]  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE    

```

We get `TRUE` only for 50-States abbreviations: `AL, AZ, AR, FL`.



## Validate US phone numbers


The following regular expression:

```r
us.phones.regex <- "^\\s*(\\+\\s*1(-?|\\s+))*[0-9]{3}\\s*-?\\s*[0-9]{3}\\s*-?\\s*[0-9]{4}$" 

```

Validates a phone number in the form of: `+1-xxx-xxx-xxxx`, including optional leading/trailing blanks at the beginning/end of each group of numbers, but not in the middle, for example: `+1-xxx-xxx-xx xx` is not valid. The `-` delimiter can be replaced by blanks: `xxx xxx xxx` or without delimiter: `xxxxxxxxxx`. The `+1` prefix is optional.

Let's check it:

```r
us.phones.regex <- "^\\s*(\\+\\s*1(-?|\\s+))*[0-9]{3}\\s*-?\\s*[0-9]{3}\\s*-?\\s*[0-9]{4}$"

phones.OK <- c("305-123-4567", "305 123 4567", "+1-786-123-4567", 
    "+1 786 123 4567", "7861234567", "786 - 123   4567", "+ 1 786 - 123   4567")

phones.NOK <- c("124-456-78901", "124-456-789", "124-456-78 90", 
    "124-45 6-7890", "12 4-456-7890")

```

Valid cases:

```r
> grepl(us.phones.regex, phones.OK)
[1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE
> 

```

Invalid cases:

```

> grepl(us.phones.regex, phones.NOK)
[1] FALSE FALSE FALSE FALSE FALSE
> 

```

**Note**:

- `\\s` Matches any space, tab or newline character



## Escaping characters in R regex patterns


Since both R and regex share the escape character ,`"\"`, building correct patterns for `grep`, `sub`, `gsub` or any other function that accepts a pattern argument will often need pairing of backslashes. If you build a three item character vector in which one items has a linefeed, another a tab character and one neither, and hte desire is to turn either the linefeed or the tab into 4-spaces then a single backslash is need for the construction, but tpaired backslashes for matching:

```r
x <- c( "a\nb", "c\td", "e    f")
x  # how it's stored
   #  [1] "a\nb"   "c\td"   "e    f"
cat(x)   # how it will be seen with cat
#a
#b c    d e    f

gsub(patt="\\n|\\t", repl="    ", x)
#[1] "a    b" "c    d" "e    f"

```

Note that the pattern argument (which is optional if it appears first and only needs partial spelling) is the only argument to require this doubling or pairing. The replacement argument does not require the doubling of characters needing to be escaped. If you wanted all the linefeeds and 4-space occurrences replaces with tabs it would be:

```r
gsub("\\n|    ", "\t", x)
#[1] "a\tb" "c\td" "e\tf"

```



## Eliminating Whitespace


```r
string <- '    some text on line one; 
and then some text on line two     '

```

### Trimming Whitespace

"Trimming" whitespace typically refers to removing both leading and trailing whitespace from a string.  This may be done using a combination of the previous examples. `gsub` is used to force the replacement over both the leading and trailing matches.

Prior to R 3.2.0

```r
gsub(pattern = "(^ +| +$)",
     replacement = "",
     x = string)

[1] "some text on line one; \nand then some text on line two"

```

R 3.2.0 and higher

```r
trimws(x = string)

[1] "some text on line one; \nand then some text on line two"

```

### Removing Leading Whitespace

Prior to R 3.2.0

```r
sub(pattern = "^ +", 
    replacement = "",
    x = string)

[1] "some text on line one; \nand then some text on line two     "

```

R 3.2.0 and higher

```r
trimws(x = string,
       which = "left")

[1] "some text on line one; \nand then some text on line two     "

```

### Removing Trailing Whitespace

Prior to R 3.2.0

```r
sub(pattern = " +$",
    replacement = "",
    x = string)

[1] "    some text on line one; \nand then some text on line two"

```

R 3.2.0 and higher

```r
trimws(x = string,
       which = "right")

[1] "    some text on line one; \nand then some text on line two"

```

### Removing All Whitespace

```r
gsub(pattern = "\\s",   
     replacement = "",
     x = string)

[1] "sometextonlineone;andthensometextonlinetwo"

```

Note that this will also remove white space characterse such as tabs (`\t`), newlines (`\r` and `\n`), and spaces.



#### Remarks


### Character classes

- `"[AB]"` could be A or B
- `"[[:alpha:]]"` could be any letter
- `"[[:lower:]]"` stands for any lower-case letter. Note that `"[a-z]"` is close but doesn't match, e.g.,  `ú`.
- `"[[:upper:]]"` stands for any upper-case letter. Note that `"[A-Z]"` is close but doesn't match, e.g., `Ú`.
- `"[[:digit:]]"` stands for any digit : 0, 1, 2, ..., or 9 and is equivalent to `"[0-9]"`.

### Quantifiers

`+`, `*` and `?` apply as usual in regex. -- `+` matches at least once, `*` matches 0 or more times, and `?` matches 0 or 1 time.

### Start and end of line indicators

You can specify the position of the regex in the string :

- `"^..."` forces the regular expression to be at the beginning of the string
- `"...$"` forces the regular expression to be at the end of the string

### Differences from other languages

Please note that regular expressions in R often look **ever-so-slightly** different from regular expressions used in other languages.

<li>
R requires double-backslash escapes (because `"\"` already implies escaping in general in R strings), so, for example, to capture whitespace in most regular expression engines, one simply needs to type `\s`, vs. `\\s` in R.
</li>
<li>
UTF-8 characters in R should be escaped with a capital U, e.g. `[\U{1F600}]` and `[\U1F600]` match 😀, whereas in, e.g., Ruby, this would be matched with a lower-case u.
</li>

### Additional Resources

The following site [reg101](https://regex101.com) is a good place for checking online regex before using it R-script.

The [R Programmming wikibook](https://en.wikibooks.org/wiki/R_Programming/Text_Processing) has a page dedicated to text processing with many examples using regular expressions.

