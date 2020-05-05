---
metaTitle: "R - Updating R and the package library"
description: "On Windows"
---

# Updating R and the package library



## On Windows


Default installation of R on Windows stored files (and thus library) on a dedicated folder per R version on program files.

That means that by default, you would work with several versions of R in parallel and thus separate libraries.

If this not what you want and you prefer to always work with a single R instance you wan't to gradually update, it is recommended to modify the R installation folder. In wizard, just specify this folder (I personally use `c:\stats\R`). Then, for any upgrade, one possibility is to overwrite this R. Whether you also want to upgrade (all) packages is a delicate choice as it may break some of your code (this appeared for me with the `tm`package). You may:

- First make a copy of all your library before upgrading packages
- Maintain your own source packages repository, for instance using package `miniCRAN`

If you want to upgrade all packages - without any check, you can call use `packageStatus` as in:

```r
pkgs <- packageStatus()  # choose mirror
upgrade(pkgs)

```

Finally, there exists a very convenient package to perform all operations, namely `installr`, even coming with a dedicated gui. If you want to use gui, you must use Rgui and not load the package in RStudio. Using the package with code is as simple as:

```r
install.packages("installr") # install 
setInternet2(TRUE) # only for R versions older than 3.3.0
installr::updateR() # updating R.

```

I refer to the great documentation [https://www.r-statistics.com/tag/installr/](https://www.r-statistics.com/tag/installr/) and specifically the step by step process with screenshots on Windows:[https://www.r-statistics.com/2015/06/a-step-by-step-screenshots-tutorial-for-upgrading-r-on-windows/](https://www.r-statistics.com/2015/06/a-step-by-step-screenshots-tutorial-for-upgrading-r-on-windows/)

Note that still I advocate using a single directory, ie. removing reference to the R version in installation folder name.

