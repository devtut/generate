---
metaTitle: "R - I/O for raster images"
description: "Load a multilayer raster"
---

# I/O for raster images


See also [Raster and Image Analysis](http://stackoverflow.com/documentation/r/3726) and [Input and Output](http://stackoverflow.com/documentation/r/5543)



## Load a multilayer raster


The R-Logo is a multilayer raster file (red, green, blue)

```r
library(raster)
r <- stack("C:/Program Files/R/R-3.2.3/doc/html/logo.jpg")
plot(r)

```

[<img src="http://i.stack.imgur.com/9jVrN.png" alt="" />](http://i.stack.imgur.com/9jVrN.png)

The individual layers of the `RasterStack` object can be adressed by `[[`.

```r
plot(r[[1]])

```

[<img src="http://i.stack.imgur.com/tOgqL.png" alt="" />](http://i.stack.imgur.com/tOgqL.png)

