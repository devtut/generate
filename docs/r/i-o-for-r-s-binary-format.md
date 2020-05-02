---
metaTitle: "I/O for R's binary format"
description: "Rds and RData (Rda) files, Enviromments"
---

# I/O for R's binary format




## Rds and RData (Rda) files


`.rds` and `.Rdata` (also known as `.rda`) files can be used to store R objects in a format native to R. There are multiple advantages of saving this way when contrasted with non-native storage approaches, e.g. `write.table`:

- It is faster to restore the data to R
- It keeps R specific information encoded in the data (e.g., attributes, variable types, etc).

`saveRDS`/`readRDS` only handle a single R object.  However, they are more flexible than the multi-object storage approach in that the object name of the restored object need not be the same as the object name when the object was stored.

Using an .rds file, for example, saving the `iris` dataset we would use:

```r
saveRDS(object = iris, file = "my_data_frame.rds")

```

To load it data back in:

```r
iris2 <- readRDS(file = "my_data_frame.rds")

```

To save a multiple objects we can use `save()` and output as `.Rdata`.

Example, to save 2 dataframes: iris and cars

```r
save(iris, cars, file = "myIrisAndCarsData.Rdata")

```

To load:

```r
load("myIrisAndCarsData.Rdata")

```



## Enviromments


The functions `save` and `load` allow us to specify the environment where the object will be hosted:

```r
save(iris, cars, file = "myIrisAndCarsData.Rdata", envir = foo  <- new.env())
load("myIrisAndCarsData.Rdata", envir = foo)
foo$cars

save(iris, cars, file = "myIrisAndCarsData.Rdata", envir = foo  <- new.env())
load("myIrisAndCarsData.Rdata", envir = foo)
foo$cars

```

