---
metaTitle: "Reproducible R"
description: "Data reproducibility, Package reproducibility"
---

# Reproducible R


With 'Reproducibility' we mean that someone else (perhaps you in the future) can repeat the steps you performed and get the same result. See the [Reproducible Research Task View](https://cran.r-project.org/web/views/ReproducibleResearch.html).



## Data reproducibility


### `dput()` and `dget()`

The easiest way to share a (preferable small) data frame is to use a basic function `dput()`. It will export an R object in a plain text form.

**Note: Before making the example data below, make sure you're in an empty folder you can write to. Run `getwd()` and read `?setwd` if you need to change folders.**

```r
dput(mtcars, file = 'df.txt')

```

Then, anyone can load the precise R object to their GlobalEnvironment using the `dget()` function.

```r
df <- dget('df.txt')

```

For larger R objects, there are a number of ways of saving them reproducibly. See [Input and output](http://stackoverflow.com/documentation/r/5543) .



## Package reproducibility


Package reproducibility is a very common issue in reproducing some R code. When various packages get updated, some interconnections between them may break. The ideal solution for the problem is to reproduce the image of the R code writer's machine on your computer at the date when the code was written. And here comes `checkpoint` package.

Starting from 2014-09-17, the authors of the package make daily copies of the whole CRAN package repository to their own mirror repository -- Microsoft R Archived Network. So, to avoid package reproduciblity issues when creating a reproducible R project, all you need is to:

1. Make sure that all your packages (and R version) are up-to-date.
1. Include `checkpoint::checkpoint('YYYY-MM-DD')` line in your code.

`checkpoint` will create a directory `.checkpoint` in your R_home directory (`"~/"`). To this technical directory it will install all the packages, that are used in your project. That means, `checkpoint` looks through all the `.R` files in your project directory to pick up all the `library()` or `require()` calls and install all the required packages in the form they existed at CRAN on the specified date.

**PRO** You are freed from the package reproducibility issue.<br />
**CONTRA** For each specified date you have to download and install all the packages that are used in a certain project that you aim to reproduce. That may take quite a while.



#### Remarks


To create reproducible results, all sources of variation need to be fixed. For instance, if a (pseudo)random number generator is used, the seed needs to be fixed if you want to recreate the same results. Another way to reduce variation is to combine text and computation in the same document.

### References

<li>
Peng, R. D. (2011). Reproducible Research in Computational. Science, 334(6060), 1226–1227. [http://doi.org/10.1126/science.1213847](http://doi.org/10.1126/science.1213847)
</li>
<li>
Peng, Roger D. Report Writing for Data Science in R. Leanpub, 2015. [https://leanpub.com/reportwriting](https://leanpub.com/reportwriting).
</li>

