---
metaTitle: "HTML - Image Maps"
description: "Introduction to Image Maps"
---

# Image Maps



## Introduction to Image Maps


### Description

An image maps is an image with clickable areas that usually act as hyperlinks.

The image is defined by the [`<img>`](http://stackoverflow.com/documentation/html/587/images) tag, and the map is defined by a [`<map>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map) tag with [`<area>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area) tags to denote each clickable area. Use the `usemap` and `name` attributes to bind the image and the map.

### Basic Example

To create an image map so that each of the shapes in the image below are clickable:

<img src="http://jaced.com/blogpix/2007/trisquarecircle/002.gif" alt="triangle, square, circle image" />

The code would be as follows:

```html
<img src="http://jaced.com/blogpix/2007/trisquarecircle/002.gif" usemap="#shapes">
<map name="shapes">
    <area shape="polygon" coords="79,6,5,134,153,134">
    <area shape="rectangle" coords="177,6,306,134">
    <area shape="circle" coords="397,71,65">
</map>

```

You should see that the browser recognizes the areas when the cursor becomes a pointer. See a [live demo](https://jsfiddle.net/jlam55555/xvpdbk6u/) on JSFiddle, or see a demonstration below:

<img src="http://media.giphy.com/media/l46CsHpT9BRuvKwco/giphy.gif" alt="demonstration of image map and cursor" />



#### Syntax


- `<img usemap="#[map-name]">`
- `<map name="[map-name]"></map>`
- `<area>`



#### Parameters


|Tag/Attribute|Value
|---|---|---|---|---|---|---|---|---|---
|**`<img>`**|Below are the image map-specific attributes to use with `<img>`. Regular `<img>` attributes apply.
|`usemap`|The `name` of the map with a hash symbol prepended to it. For example, for a map with `name="map"`, the image should have `usemap="#map"`.
| | 
|**`<map>`**| 
|`name`|The name of the map to identify it. To be used with the image's `usemap` attribute.
| | 
|**`<area>`**|Below are `<area>`-specific attributes. When `href` is specified, making the  `<area>` a link, `<area>` also supports all of the attributes of the anchor tag (`<a>`) except `ping`. See them at the [MDN docs](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a).
|`alt`|The alternate text to display if images are not supported. This is only necessary if `href` is also set on the `<area>`.
|`coords`|The coordinates outlining the selectable area. When `shape="polygon"`, this should be set to a list of "x, y" pairs separated by commas (i.e., `shape="polygon" coords="x1, y1, x2, y2, x3, y3, ..."`). When `shape="rectangle"`, this should be set to `left, top, right, bottom`. When `shape="circle"`, this should be set to `centerX, centerY, radius`.
|`href`|The URL of the hyperlink, if specified. If it is omitted, then the `<area>` will not represent a hyperlink.
|`shape`|The shape of the `<area>`. Can be set to `default` to select the entire image (no `coords` attribute necessary), `circle` or `circ` for a circle, `rectangle` or `rect` for a rectangle, and `polygon` or `poly` for a polygonal area specified by corner points.



#### Remarks


<li>
The above parameters list is modified from the MDN docs: [`<map>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/map) and [`<area>`](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/area).
</li>
<li>
It is feasible to create an image map's coordinates with for an image with simpler shapes (such as in the introductory example above) with an image editor that shows coordinates (such as GIMP). However, it might be easier in general to use an image map generator, such as [this one](http://imagemap-generator.dariodomi.de/).
</li>

