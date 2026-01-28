---
metaTitle: "iOS - Simulating Location Using GPX files iOS"
description: "Your .gpx file: MPS_HQ.gpx, To set this location:"
---

# Simulating Location Using GPX files iOS




## Your .gpx file: MPS_HQ.gpx


```swift
<gpx xmlns="http://www.topografix.com/GPX/1/1"
 xmlns:gpxx = "http://www.garmin.com/xmlschemas/GpxExtensions/v3"
 xmlns:xsi = "http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://www.topografix.com/GPX/1/1 
 http://www.topografix.com/GPX/1/1/gpx.xsd
 http://www.garmin.com/xmlschemas/GpxExtensions/v3
 http://www8.garmin.com/xmlschemas/GpxExtensions/v3/GpxExtensionsv3.xsd"
 version="1.1"
 creator="gpx-poi.com">
 <wpt lat="38.9072" lon="77.0369">38.9072/-77.0369
 <time>2015-04-16T22:20:29Z</time>
  <name>Washington, DC</name>
  <extensions>
     <gpxx:WaypointExtension>
        <gpxx:Proximity>10</gpxx:Proximity>
        <gpxx:Address>
           <gpxx:StreetAddress>Washington DC</gpxx:StreetAddress>
           <gpxx:City>Washington</gpxx:City>
           <gpxx:State>DC</gpxx:State>
           <gpxx:Country>United States</gpxx:Country>
           <gpxx:PostalCode> 20005 </gpxx:PostalCode>
        </gpxx:Address>
     </gpxx:WaypointExtension>
  </extensions>

```



## To set this location:


1. Go to Edit Scheme.
1. Select Run -> Options.
1. Check "Allow Location Simulation".
1. Select the *.GPX File Name from the "Default Location" drop down list.

[<img src="https://i.stack.imgur.com/NxJyr.png" alt="Allow Location Simulation" />](https://i.stack.imgur.com/NxJyr.png)

