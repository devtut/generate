---
metaTitle: "R - Using pipe assignment in your own package %<>%: How to ?"
description: "Putting the pipe in a utility-functions file"
---

# Using pipe assignment in your own package %<>%: How to ?


In order to use the pipe in a user-created package, it must be listed in the NAMESPACE like any other function you choose to import.



## Putting the pipe in a utility-functions file


One option for doing this is to export the pipe from within the package itself. This may be done in the 'traditional' `zzz.R` or `utils.R` files that many packages utilise for useful little functions that are not exported as part of the package. For example, putting:

```r
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

```

