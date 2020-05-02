---
metaTitle: "RESTful R Services"
description: "opencpu Apps"
---

# RESTful R Services




## opencpu Apps


The official website contain good exemple of apps:
[https://www.opencpu.org/apps.html](https://www.opencpu.org/apps.html)

The following code is used to serve a R session:

```r
library(opencpu)
opencpu$start(port = 5936)

```

After this code is executed, you can use URLs to access the functions of the R session.
The result could be XML, html, JSON or some other defined formats.

For exemple, the previous R session can be accessed by a cURL call:

```r
#curl uses http post method for -X POST or -d "arg=value"
curl http://localhost:5936/ocpu/library/MASS/scripts/ch01.R -X POST
curl http://localhost:5936/ocpu/library/stats/R/rnorm -d "n=10&mean=5"

```

The call is asynchronous, meaning that the R session is not blocked while waiting for the call to finish (contrary to shiny).

The call result is kept in a temporary session stored in `/ocpu/tmp/`

An exemple of how to retrieve the temporary session:

```r
curl https://public.opencpu.org/ocpu/library/stats/R/rnorm -d n=5
/ocpu/tmp/x009f9e7630/R/.val
/ocpu/tmp/x009f9e7630/stdout
/ocpu/tmp/x009f9e7630/source
/ocpu/tmp/x009f9e7630/console
/ocpu/tmp/x009f9e7630/info

```

`x009f9e7630` is the name of the session.

Pointing to `/ocpu/tmp/x009f9e7630/R/.val` will return the value resulting of `rnorm(5)`, `/ocpu/tmp/x009f9e7630/R/console` will return the content of the console of `rnorm(5)`, etc..

