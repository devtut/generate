---
metaTitle: "Raster and Image Analysis"
description: "Calculating GLCM Texture, Mathematical Morphologies"
---

# Raster and Image Analysis


See also [I/O for Raster Images](http://stackoverflow.com/documentation/r/5539)



## Calculating GLCM Texture


[Gray Level Co-Occurrence Matrix](https://en.wikipedia.org/wiki/Co-occurrence_matrix) (Haralick et al. 1973) texture is a powerful image feature for image analysis. The `glcm` package provides a easy-to-use function to calculate such texutral features for `RasterLayer` objects in R.

```r
library(glcm)
library(raster)

r <- raster("C:/Program Files/R/R-3.2.3/doc/html/logo.jpg")
plot(r)

```

[<img src="http://i.stack.imgur.com/yBLGi.png" alt="enter image description here" />](http://i.stack.imgur.com/yBLGi.png)

**Calculating GLCM textures in one direction**

```r
rglcm <- glcm(r, 
              window = c(9,9), 
              shift = c(1,1), 
              statistics = c("mean", "variance", "homogeneity", "contrast", 
                             "dissimilarity", "entropy", "second_moment")
              )

plot(rglcm)

```

[<img src="http://i.stack.imgur.com/YBnub.png" alt="enter image description here" />](http://i.stack.imgur.com/YBnub.png)

**Calculation rotation-invariant texture features**

The textural features can also be calculated in all 4 directions (0°, 45°, 90° and 135°) and then combined to one rotation-invariant texture. The key for this is the `shift` parameter:

```r
rglcm1 <- glcm(r, 
              window = c(9,9), 
              shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
              statistics = c("mean", "variance", "homogeneity", "contrast", 
                             "dissimilarity", "entropy", "second_moment")
              )

plot(rglcm1)

```

[<img src="http://i.stack.imgur.com/U0SHY.png" alt="enter image description here" />](http://i.stack.imgur.com/U0SHY.png)



## Mathematical Morphologies


The package `mmand` provides functions for the calculation of Mathematical Morphologies for n-dimensional arrays. With a little workaround, these can also be calculated for raster images.

```r
library(raster)
library(mmand)

r <- raster("C:/Program Files/R/R-3.2.3/doc/html/logo.jpg")
plot(r)

```

[<img src="http://i.stack.imgur.com/cuCPz.png" alt="" />](http://i.stack.imgur.com/cuCPz.png)

At first, a kernel (moving window) has to be set with a size (e.g. 9x9) and a shape type (e.g. `disc`, `box` or `diamond`)

```r
sk <- shapeKernel(c(9,9), type="disc")

```

Afterwards, the raster layer has to be converted into an array wich is used as input for the `erode()` function.

```r
rArr <- as.array(r, transpose = TRUE)
rErode <- erode(rArr, sk)
rErode <- setValues(r, as.vector(aperm(rErode)))

```

Besides `erode()`, also the morphological functions `dilate()`, `opening()` and `closing()` can be applied like this.

```r
plot(rErode)

```

[<img src="http://i.stack.imgur.com/mAuCt.png" alt="Eroded R Logo" />](http://i.stack.imgur.com/mAuCt.png)

