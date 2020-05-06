---
metaTitle: "HTML - Data Attributes"
description: "Data Attribute Use, Older browsers support"
---

# Data Attributes




## Data Attribute Use


HTML5 `data-*` attributes provide a convenient way to store data in HTML elements. The stored data can be read or modified using JavaScript

```html
<div data-submitted="yes" class="user_profile">
  … some content …
</div>

```


<li>
Data attribute structure is `data-*`, i.e. the name of the data attribute comes after the `data-` part. Using this name, the attribute can be accessed.
</li>
<li>
Data in string format (including `json`) can be stored using `data-*` attribute.
</li>



## Older browsers support


Data attributes were introduced in HTML5 which is supported by all modern browsers, but older browsers before HTML5 don't recognize the data attributes.

However, in HTML specifications, attributes that are not recognized by the browser must be left alone and the browser will simply ignore them when rendering the page.

Web developers have utilized this fact to create non-standard attributes which are any attributes not part of the HTML specifications. For example, the `value` attribute in the line bellow is considered a non-standard attribute because the specifications for the `<img>` tag don't have a `value` attribute and it is not a global attribute:

```html
<img src="sample.jpg" value="test" />

```

This means that although data attributes are not supported in older browsers, they still work and you can set and retrieve them using the same generic JavaScript `setAttribute` and `getAttribute` methods, but you cannot use the new `dataset` property which is only supported in modern browsers.



#### Syntax


- `<element data-custom-name="somevalue">`



#### Parameters


|Value|Description
|---|---|---|---|---|---|---|---|---|---
|somevalue|Specifies the value of the attribute (as a string)

