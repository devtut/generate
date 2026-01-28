---
metaTitle: "R - Publishing"
description: "Formatting tables, Formatting entire documents"
---

# Publishing


There are many ways of formatting R code, tables and graphs for publishing.



## Formatting tables


Here, "table" is meant broadly (covering `data.frame`, `table`,

### Printing to plain text

Printing (as seen in the console) might suffice for a plain-text document to be viewed in monospaced font:

**Note: Before making the example data below, make sure you're in an empty folder you can write to. Run `getwd()` and read `?setwd` if you need to change folders.**

```r
..w = options()$width
options(width = 500) # reduce text wrapping
sink(file = "mytab.txt")
   summary(mtcars)
sink()
options(width = ..w) 
rm(..w)

```

### Printing delimited tables

Writing to CSV (or another common format) and then opening in a spreadsheet editor to apply finishing touches is another option:

**Note: Before making the example data below, make sure you're in an empty folder you can write to. Run `getwd()` and read `?setwd` if you need to change folders.**

```r
write.csv(mtcars, file="mytab.csv")

```

### Further resources

- `knitr::kable`
- stargazer
- `tables::tabular`
- [texreg](http://stackoverflow.com/documentation/r/9037)
- xtable



## Formatting entire documents


`Sweave` from the `utils` package allows for formatting code, prose, graphs and tables together in a LaTeX document.

### Further Resources

- Knitr and RMarkdown



#### Remarks


R users often want to publish analysis and results in a reproducible way. See [Reproducible R](http://stackoverflow.com/documentation/r/4087) for details.

