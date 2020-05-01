---
metaTitle: "Spatial Data"
description: "POINT"
---

# Spatial Data


There are 2 spatial data types

**Geometry**
X/Y coordinate system for a flat surface

**Geography**
Latitude/Longitude coordinate system for a curved surface (the earth). There are multiple projections of curved surfaces so each geography spatial must let SQL Server know which projection to use. The usual Spatial Reference ID (SRID) is 4326, which is measuring distances in Kilometers. This is the default SRID used in most web maps



## POINT


Creates a single Point. This will be a geometry or geography point depending on the class used.

|Parameter|Detail
|---|---|---|---
|Lat or X|Is a float expression representing the x-coordinate of the Point being generated
|Long or Y|Is a float expression representing the y-coordinate of the Point being generated
|String|Well Known Text (WKB) of a geometry/geography shape
|Binary|Well Known Binary (WKB) of a geometry/geography shape
|SRID|Is an int expression representing the spatial reference ID (SRID) of the geometry/geography instance you wish to return

```sql
--Explicit constructor 
DECLARE @gm1 GEOMETRY = GEOMETRY::Point(10,5,0)

DECLARE @gg1 GEOGRAPHY = GEOGRAPHY::Point(51.511601,-0.096600,4326)

--Implicit constructor (using WKT - Well Known Text)
DECLARE @gm1 GEOMETRY = GEOMETRY::STGeomFromText('POINT(5 10)', 0)

DECLARE @gg1 GEOGRAPHY= GEOGRAPHY::STGeomFromText('POINT(-0.096600 51.511601)', 4326)

--Implicit constructor (using WKB - Well Known Binary)
DECLARE @gm1 GEOMETRY = GEOMETRY::STGeomFromWKB(0x010100000000000000000014400000000000002440, 0)

DECLARE @gg1 GEOGRAPHY= GEOGRAPHY::STGeomFromWKB(0x01010000005F29CB10C7BAB8BFEACC3D247CC14940, 4326)

```

