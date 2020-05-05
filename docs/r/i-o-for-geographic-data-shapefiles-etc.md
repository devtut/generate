---
metaTitle: "R - I/O for geographic data (shapefiles, etc.)"
description: "Import and Export Shapefiles"
---

# I/O for geographic data (shapefiles, etc.)


See also [Introduction to Geographical Maps](http://stackoverflow.com/documentation/r/1372) and [Input and Output](http://stackoverflow.com/documentation/r/5543)



## Import and Export Shapefiles


With the `rgdal` package it is possible to import and export shapfiles with R.
The function `readOGR` can be used to imports shapfiles. If you want to import a file from e.g. ArcGIS the first argument `dsn` is the path to the folder which contains the shapefile. `layer` is the name of the shapefile without the file ending (just `map` and not `map.shp`).

```r
library(rgdal)
readOGR(dsn = "path\to\the\folder\containing\the\shapefile", layer = "map") 

```

To export a shapefile use the`writeOGR` function. The first argument is the spatial object produced in R. `dsn` and `layer` are the same as above. The obligatory 4. argument is the driver used to generate the shapefile. The function `ogrDrivers()` lists all available drivers. If you want to export a shapfile to ArcGis or QGis you could use `driver = "ESRI Shapefile"`.

```r
writeOGR(Rmap, dsn = "path\to\the\folder\containing\the\shapefile", layer = "map",
         driver = "ESRI Shapefile" )

```

`tmap` package has a very convenient function `read_shape()`, which is a wrapper for `rgdal::reagOGR()`. The `read_shape()` function simplifies the process of importing a shapefile a lot. On the downside, `tmap` is quite heavy.

