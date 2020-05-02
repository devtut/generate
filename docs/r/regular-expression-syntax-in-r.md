---
metaTitle: "Regular Expression Syntax in R"
description: "Use `grep` to find a string in a character vector"
---

# Regular Expression Syntax in R


This document introduces the basics of regular expressions as used in R. For more information about R's regular expression syntax, see [`?regex`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html). For a comprehensive list of regular expression operators, see [this ICU guide on regular expressions](http://userguide.icu-project.org/strings/regexp).



## Use `grep` to find a string in a character vector


```r
# General syntax:   
# grep(<pattern>, <character vector>)

mystring <- c('The number 5',
              'The number 8',
              '1 is the loneliest number',
              'Company, 3 is',
              'Git SSH tag is git@github.com',
              'My personal site is www.personal.org',
              'path/to/my/file')

grep('5', mystring)
# [1] 1
grep('@', mystring)
# [1] 5
grep('number', mystring)
# [1] 1 2 3

```

`x|y` means look for "x" or "y"

```r
grep('5|8', mystring)
# [1] 1 2
grep('com|org', mystring)
# [1] 5 6

```

`.` is a special character in Regex. It means "match any character"

```r
grep('The number .', mystring)
# [1] 1 2

```

Be careful when trying to match dots!

```r
tricky <- c('www.personal.org', 'My friend is a cyborg')
grep('.org', tricky)
# [1] 1 2

```

To match a literal character, you have to escape the string with a backslash (`\`). However, R tries to look for escape characters when creating strings, so you actually need to escape the backslash itself (i.e. you need to **double escape** regular expression characters.)

```r
grep('\.org', tricky)
# Error: '\.' is an unrecognized escape in character string starting "'\."
grep('\\.org', tricky)
# [1] 1

```

If you want to match one of several characters, you can wrap those characters in brackets (`[]`)

```r
grep('[13]', mystring)
# [1] 3 4
grep('[@/]', mystring)
# [1] 5 7

```

It may be useful to indicate character sequences. E.g. `[0-4]` will match 0, 1, 2, 3, or 4, `[A-Z]` will match any uppercase letter, `[A-z]` will match any uppercase or lowercase letter, and `[A-z0-9]` will match any letter or number (i.e. all alphanumeric characters)

```r
grep('[0-4]', mystring)
# [1] 3 4
grep('[A-Z]', mystring)
# [1] 1 2 4 5 6

```

R also has several shortcut classes that can be used in brackets. For instance, `[:lower:]` is short for `a-z`, `[:upper:]` is short for `A-Z`, `[:alpha:]` is `A-z`, `[:digit:]` is `0-9`, and `[:alnum:]` is `A-z0-9`. Note that these **whole expressions** must be used inside brackets; for instance, to match a single digit, you can use `[[:digit:]]` (note the double brackets). As another example, `[@[:digit:]/]` will match the characters `@`, `/` or `0-9`.

```r
grep('[[:digit:]]', mystring)
# [1] 1 2 3 4
grep('[@[:digit:]/]', mystring)
# [1] 1 2 3 4 5 7

```

Brackets can also be used to negate a match with a carat (`^`). For instance, `[^5]` will match any character other than "5".

```r
grep('The number [^5]', mystring)
# [1] 2

```

