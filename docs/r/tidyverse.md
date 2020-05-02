---
metaTitle: "tidyverse"
description: "tidyverse: an overview, Creating tbl_df’s"
---

# tidyverse



## tidyverse: an overview


### What is `tidyverse`?

[`tidyverse`](https://github.com/tidyverse) is the fast and elegant way to turn basic `R` into an enhanced tool, redesigned by Hadley/Rstudio. The development of all packages included in `tidyverse` follow the principle rules of [The tidy tools manifesto](https://mran.microsoft.com/web/packages/tidyverse/vignettes/manifesto.html). But first, let the authors describe their masterpiece:

> 
<p>The tidyverse is a set of packages that work in harmony because they
share common data representations and API design. The tidyverse
package is designed to make it easy to install and load core packages
from the tidyverse in a single command.</p>
<p>The best place to learn about all the packages in the tidyverse and
how they fit together is R for Data Science. Expect to hear more about
the tidyverse in the coming months as I work on improved package
websites, making citation easier, and providing a common home for
discussions about data analysis with the tidyverse.</p>
([source](https://blog.rstudio.org/2016/09/15/tidyverse-1-0-0/)))


### How to use it?

Just with the ordinary `R` packages, you need to install and load the package.

```r
install.package("tidyverse")
library("tidyverse")

```

The difference is, on a single command a couple of dozens of packages are installed/loaded. As a bonus, one may rest assured that all the installed/loaded packages are of compatible versions.

### What are those packages?

The commonly known and widely used packages:

- [ggplot2](http://ggplot2.org/): advanced data visualisation [SO_doc](http://stackoverflow.com/documentation/r/1334)
- [dplyr](https://github.com/hadley/dplyr): fast ([Rcpp](http://stackoverflow.com/documentation/r/1404)) and coherent approach to data manipulation [SO_doc](http://stackoverflow.com/documentation/r/4250)
- [tidyr](https://github.com/tidyverse/tidyr): tools for data tidying [SO_doc](http://stackoverflow.com/documentation/r/2904)
- [readr](https://github.com/tidyverse/readr): for data import.
- [purrr](https://github.com/hadley/purrr): makes your pure functions purr by completing R's functional programming tools with important features from other languages, in the style of the JS packages underscore.js, lodash and lazy.js.
- [tibble](https://github.com/tidyverse/tibble): a modern re-imagining of data frames.
- [magrittr](https://github.com/tidyverse/magrittr): piping to make code more readable [SO_doc](http://stackoverflow.com/documentation/r/652)

Packages for manipulating specific data formats:

- [hms](https://github.com/rstats-db/hms): easily read times
- [stringr](https://github.com/tidyverse/stringr): provide a cohesive set of functions designed to make working with strings as easy as posssible
- [lubridate](https://github.com/hadley/lubridate): advanced date/times manipulations [SO_doc](http://stackoverflow.com/documentation/r/2496)
- [forcats](https://github.com/tidyverse/forcats): advanced work with [factors](http://stackoverflow.com/documentation/r/1104).

Data import:

- [DBI](https://github.com/rstats-db/DBI): defines a common interface between the R and database management systems (DBMS)
- [haven](https://github.com/tidyverse/haven): easily import SPSS, SAS and Stata files [SO_doc](http://stackoverflow.com/documentation/r/5536/i-o-for-foreign-tables-excel-sas-spss-stata/4824/read-and-write-stata-spss-and-sas-files#t=201611211701173991879)
- [httr](https://github.com/hadley/httr/): the aim of httr is to provide a wrapper for the curl package, customised to the demands of modern web APIs
- [jsonlite](https://github.com/jeroenooms/jsonlite): a fast JSON parser and generator optimized for statistical data and the web
- [readxl](https://github.com/hadley/readxl): read.xls and .xlsx files without need for dependency packages [SO_doc](http://stackoverflow.com/documentation/r/5536/i-o-for-foreign-tables-excel-sas-spss-stata/4445/importing-excel-files#t=201611211701173991879)
- [rvest](https://github.com/hadley/rvest): rvest helps you scrape information from web pages [SO_doc](http://stackoverflow.com/documentation/r/2890)
- [xml2](https://github.com/hadley/xml2): for XML

And modelling:

- [modelr](https://github.com/hadley/modelr): provides functions that help you create elegant pipelines when modelling
- [broom](https://github.com/tidyverse/broom): easily extract the  models into tidy data

Finally, `tidyverse` suggest the use of:

- [knitr](https://github.com/yihui/knitr): the amazing general-purpose literate programming engine, with lightweight API's designed to give users full control of the output without heavy coding work. SO_docs: [one](http://stackoverflow.com/documentation/r/5894), [two](http://stackoverflow.com/documentation/r/4334)
- [rmarkdown](http://rmarkdown.rstudio.com/): Rstudio's package for reproducible programming. SO_docs: [one](http://stackoverflow.com/documentation/r/589), [two](http://stackoverflow.com/documentation/r/7606), [three](http://stackoverflow.com/documentation/r/4087), [four](http://stackoverflow.com/documentation/r/4572)



## Creating tbl_df’s


A tbl_df (pronounced **tibble diff**) is a variation of a [data frame](http://stackoverflow.com/documentation/r/438/data-frames#t=201607211323113518979) that is often used in tidyverse packages. It is implemented in the [tibble](https://cran.r-project.org/package=tibble) package.

Use the `as_data_frame` function to turn a data frame into a tbl_df:

```r
library(tibble)
mtcars_tbl <- as_data_frame(mtcars)

```

One of the most notable differences between data.frames and tbl_dfs is how they print:

```r
# A tibble: 32 x 11
     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
*  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
1   21.0     6 160.0   110  3.90 2.620 16.46     0     1     4     4
2   21.0     6 160.0   110  3.90 2.875 17.02     0     1     4     4
3   22.8     4 108.0    93  3.85 2.320 18.61     1     1     4     1
4   21.4     6 258.0   110  3.08 3.215 19.44     1     0     3     1
5   18.7     8 360.0   175  3.15 3.440 17.02     0     0     3     2
6   18.1     6 225.0   105  2.76 3.460 20.22     1     0     3     1
7   14.3     8 360.0   245  3.21 3.570 15.84     0     0     3     4
8   24.4     4 146.7    62  3.69 3.190 20.00     1     0     4     2
9   22.8     4 140.8    95  3.92 3.150 22.90     1     0     4     2
10  19.2     6 167.6   123  3.92 3.440 18.30     1     0     4     4
# ... with 22 more rows

```


- The printed output includes a summary of the dimensions of the table (`32 x 11`)
- It includes the type of each column (`dbl`)
- It prints a limited number of rows. (To change this use `options(tibble.print_max = [number])`).

Many functions in the dplyr package work naturally with tbl_dfs, such as `group_by()`.

