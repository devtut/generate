---
metaTitle: "HTML - Embed"
description: "Basic usage, Defining the MIME type"
---

# Embed




## Basic usage


The `embed` tag is new in HTML5. This element provides an integration point for an external (typically non-HTML) application or interactive content.

`<embed src="myflash.swf">`



## Defining the MIME type


The [MIME](https://tools.ietf.org/html/rfc2046) type must be defined using the `type` attribute.

`<embed type="video/mp4" src="video.mp4" width="640" height="480">`



#### Parameters


|Parameters|Details
|---|---|---|---|---|---|---|---|---|---
|`src`|Address of the resource
|`type`|Type of embedded resource
|`width`|Horizontal dimension
|`height`|Vertical dimension

