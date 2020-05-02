---
metaTitle: "roxygen2"
description: "Documenting a package with roxygen2"
---

# roxygen2



## Documenting a package with roxygen2


### Writing with roxygen2

[roxygen2](https://cran.r-project.org/package=roxygen2) is a package created by Hadley Wickham to facilitate documentation.

It allows to include the documentation inside the R script, in lines starting by `#'`. The different parameters passed to the documentation start with an `@`, for example the creator of a package will by written as follow:

```r
#' @author The Author

```

For example, if we wanted to document the following function:

```r
mean<-function(x) sum(x)/length(x)

```

We will want to write a small description to this function, and explain the parameters with the following (each line will be explained and detailed after):

```r
#' Mean
#'
#' A function to compute the mean of a vector
#' @param x A numeric vector
#' @keyword mean
#' @importFrom base sum
#' @export
#' @examples
#' mean(1:3)
#' \dontrun{ mean(1:1e99) }
mean<-function(x) sum(x)/length(x)

```


- The first line `#' Mean` is the title of the documentation, the following lines make the corpus.
- Each parameter of a function must be detailed through a relevant `@param`. `@export` indicated that this function name should be exported, and thus can be called when the package is loaded.
- `@keyword` provides relevant keywords when looking for help
- `@importFrom` lists all functions to import from a package that will be used in this function or in you package. Note that importing the complete namespace of a package can be done with `@import`
<li>The examples are then written below the `@example` tag.
<ul>
- The first one will be evaluated when the package is built;
- The second one will not - usually to prevent long computations - due to the `\dontrun` command.

### Building the documentation

The documentation can be created using `devtools::document()`. Note also that `devtools::check()` will automatically create a documentation and will report missing arguments in the documentation of functions as warnings.



#### Parameters


|Parameter|details
|------
|author|Author of the package
|examples|The following lines will be examples on how to use the documented function
|export|To export the function - i.e. make it callable by users of the package
|import|Package(s) namespace(s) to import
|importFrom|Functions to import from the package (first name of the list)
|param|Parameter of the function to document

