---
metaTitle: "Fault-tolerant/resilient code"
description: "Using tryCatch()"
---

# Fault-tolerant/resilient code



## Using tryCatch()


We're defining a robust version of a function that reads the HTML code from a given URL. **Robust** in the sense that we want it to handle situations where something either goes wrong (error) or not quite the way we planned it to (warning). The umbrella term for errors and warnings is **condition**

### Function definition using `tryCatch`

```r
readUrl <- function(url) {
    out <- tryCatch(

        ########################################################
        # Try part: define the expression(s) you want to "try" #
        ########################################################

        {
            # Just to highlight: 
            # If you want to use more than one R expression in the "try part" 
            # then you'll have to use curly brackets. 
            # Otherwise, just write the single expression you want to try and 

            message("This is the 'try' part")
            readLines(con = url, warn = FALSE) 
        },

        ########################################################################
        # Condition handler part: define how you want conditions to be handled #
        ########################################################################

        # Handler when a warning occurs:
        warning = function(cond) {
            message(paste("Reading the URL caused a warning:", url))
            message("Here's the original warning message:")
            message(cond)

            # Choose a return value when such a type of condition occurs
            return(NULL)
        },

        # Handler when an error occurs:
        error = function(cond) {
            message(paste("This seems to be an invalid URL:", url))
            message("Here's the original error message:")
            message(cond)

            # Choose a return value when such a type of condition occurs
            return(NA)
        },

        ###############################################
        # Final part: define what should happen AFTER #
        # everything has been tried and/or handled    #
        ###############################################

        finally = {
            message(paste("Processed URL:", url))
            message("Some message at the end\n")
        }
    )    
    return(out)
}

```

### Testing things out

Let's define a vector of URLs where one element isn't a valid URL

```r
urls <- c(
    "http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html",
    "http://en.wikipedia.org/wiki/Xz",
    "I'm no URL"
)

```

And pass this as input to the function we defined above

```r
y <- lapply(urls, readUrl)
# Processed URL: http://stat.ethz.ch/R-manual/R-devel/library/base/html/connections.html
# Some message at the end
#
# Processed URL: http://en.wikipedia.org/wiki/Xz
# Some message at the end
#
# URL does not seem to exist: I'm no URL 
# Here's the original error message:
# cannot open the connection
# Processed URL: I'm no URL
# Some message at the end
#
# Warning message:
# In file(con, "r") : cannot open file 'I'm no URL': No such file or directory

```

### Investigating the output

```r
length(y)
# [1] 3

head(y[[1]])
# [1] "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">"      
# [2] "<html><head><title>R: Functions to Manipulate Connections</title>"      
# [3] "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">"
# [4] "<link rel=\"stylesheet\" type=\"text/css\" href=\"R.css\">"             
# [5] "</head><body>"                                                          
# [6] ""    

y[[3]]
# [1] NA

```



#### Parameters


|Parameter|Details
|---|---|---
|expr|In case the "try part" was completed successfully `tryCatch` will return the **last evaluated expression**. Hence, the actual value being returned in case everything went well and there is no condition (i.e. a **warning** or an **error**) is the return value of `readLines`. Note that you don't need to explicilty state the return value via `return` as code in the "try part" is not wrapped insided a function environment (unlike that for the condition handlers for warnings and error below)
|warning/error/etc|Provide/define a handler function for all the conditions that you want to handle explicitly. AFAIU, you can provide handlers for **any** type of conditions (not just **warnings** and **errors**, but also **custom** conditions; see `simpleCondition` and friends for that) as long as the **name of the respective handler function matches the class of the respective condition** (see the **Details** part of the doc for `tryCatch`).
|finally|Here goes everything that should be executed at the very end, **regardless** if the expression in the "try part" succeeded or if there was any condition. If you want more than one expression to be executed, then you need to wrap them in curly brackets, otherwise you could just have written `finally = <expression>` (i.e. the same logic as for "try part".



#### Remarks


### `tryCatch`

`tryCatch` returns the value associated to executing `expr` unless there's a condition: a warning or an error. If that's the case, specific return values (e.g.  `return(NA)` above) can be specified by supplying a handler function for the respective conditions (see arguments `warning` and `error` in `?tryCatch`). These can be functions that already exist, but you can also define them within `tryCatch` (as we did above).

### Implications of choosing specific return values of the handler functions

As we've specified that `NA` should be returned in case of an error in the "try part", the third element in `y` is `NA`. If we'd have chosen `NULL` to be the return value, the length of `y` would just have been `2` instead of `3` as `lapply` will simply "ignore/drop" return values that are `NULL`. Also note that if you don't specify an **explicit** return value via `return`, the handler functions will return `NULL` (i.e. in case of an **error** or a **warning** condition).

### "Undesired" warning message

When the third element of our `urls` vector hits our function, we get the following warning **in addition** to the fact that an error occurs (`readLines` first complains that it can't open the connection via a **warning** before actually failing with an **error**):

```r
Warning message:
    In file(con, "r") : cannot open file 'I'm no URL': No such file or directory

```

An **error** "wins" over a **warning**, so we're not really interested in the warning in this particular case. Thus we have set `warn = FALSE` in `readLines`, but that doesn't seem to have any effect. An alternative way to suppress the warning is to use

```r
suppressWarnings(readLines(con = url))

```

instead of

```r
readLines(con = url, warn = FALSE)

```

