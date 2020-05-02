---
metaTitle: ".Rprofile"
description: ".Rprofile - the first chunk of code executed, .Rprofile example"
---

# .Rprofile



## .Rprofile - the first chunk of code executed


`.Rprofile` is a file containing R code that is executed when you launch R from the directory containing the `.Rprofile` file. The similarly named `Rprofile.site`, located in R's home directory, is executed by default every time you load R from any directory. `Rprofile.site` and to a greater extend `.Rprofile` can be used to initialize an R session with personal preferences and various utility functions that you have defined.

> 
Important note: if you use RStudio, you can have a separate `.Rprofile` in every RStudio project directory.


Here are some examples of code that you might include in an .Rprofile file.

### Setting your R home directory

```r
# set R_home
Sys.setenv(R_USER="c:/R_home") # just an example directory
# but don't confuse this with the $R_HOME environment variable.

```

### Setting page size options

```r
options(papersize="a4")
options(editor="notepad")
options(pager="internal")

```

### set the default help type

```r
options(help_type="html")

```

### set a site library

```r
.Library.site <- file.path(chartr("\\", "/", R.home()), "site-library")

```

### Set a CRAN mirror

```r
local({r <- getOption("repos")
    r["CRAN"] <- "http://my.local.cran"
    options(repos=r)})

```

### Setting the location of your library

This will allow you to not have to install all the packages again with each R version update.

```r
# library location
.libPaths("c:/R_home/Rpackages/win")

```

### Custom shortcuts or functions

Sometimes it is useful to have a shortcut for a long R expression. A common example of this setting an active binding to access the last top-level expression result without having to type out `.Last.value`:

```r
makeActiveBinding(".", function(){.Last.value}, .GlobalEnv)

```

Because .Rprofile is just an R file, it can contain any arbitrary R code.

### Pre-loading the most useful packages

This is bad practice and should generally be avoided because it separates package loading code from the scripts where those packages are actually used.

### See Also

See `help(Startup)` for all the different startup scripts, and further aspects.  In particular, two system-wide `Profile` files can be loaded as well.  The first, `Rprofile`,  may contain global settings, the other file `Profile.site` may contain local choices the system administrator can make for all users.  Both files are found in the `${RHOME}/etc` directory of the R installation.  This directory also contains global files `Renviron` and `Renviron.site` which both can be completemented with a local file `~/.Renviron` in the user's home directory.



## .Rprofile example


### Startup

```r
# Load library setwidth on start - to set the width automatically.
.First <- function() {
  library(setwidth)
  # If 256 color terminal - use library colorout.
  if (Sys.getenv("TERM") %in% c("xterm-256color", "screen-256color")) {
    library("colorout")
  }
}

```

### Options

```r
# Select default CRAN mirror for package installation.
options(repos=c(CRAN="https://cran.gis-lab.info/"))

# Print maximum 1000 elements.
options(max.print=1000)

# No scientific notation.
options(scipen=10)

# No graphics in menus.
options(menu.graphics=FALSE)

# Auto-completion for package names.
utils::rc.settings(ipck=TRUE)

```

### Custom Functions

```r
# Invisible environment to mask defined functions
.env = new.env()

# Quit R without asking to save.
.env$q <- function (save="no", ...) {
  quit(save=save, ...)
}

# Attach the environment to enable functions.
attach(.env, warn.conflicts=FALSE)

```



#### Remarks


There is a nice chapter on the matter in [Efficient R programming](https://bookdown.org/csgillespie/efficientR/set-up.html#r-startup)

