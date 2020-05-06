---
metaTitle: "jQuery - Attributes"
description: "Differece between attr() and prop(), Get the attribute value of a HTML element, Setting value of HTML attribute, Removing attribute"
---

# Attributes




## Differece between attr() and prop()


[`attr()`](http://stackoverflow.com/documentation/jquery/4429/attributes/15471/getting-value-of-html-attribute#t=201608020513212065116) gets/sets the HTML attribute using the DOM functions `getAttribute()` and `setAttribute()`. [`prop()`](https://api.jquery.com/prop/) works by setting the DOM property without changing the attribute. In many cases the two are interchangeable, but occasionally one is needed over the other.

To set a checkbox as checked:

```js
$('#tosAccept').prop('checked', true); // using attr() won't work properly here

```

To remove a property you can use the [`removeProp()`](https://api.jquery.com/removeProp/) method. Similarly [`removeAttr()`](http://stackoverflow.com/documentation/jquery/4429/attributes/15473/removing-attribute#t=201608020514117105889) removes attributes.



## Get the attribute value of a HTML element


When a single parameter is passed to the [`.attr()`](https://api.jquery.com/attr/) function it returns the value of passed attribute on the selected element.

Syntax:

`$([selector]).attr([attribute name]);`

Example:

HTML:

`<a href="/home">Home</a>`

jQuery:

`$('a').attr('href');`

**Fetching `data` attributes:**

jQuery offers [`.data()`](https://api.jquery.com/data/) function in order to deal with data attributes. `.data` function returns the value of the data attribute on the selected element.

Syntax:

`$([selector]).data([attribute name]);`

Example:

Html:

`<article data-column="3"></article>`

jQuery:

`$("article").data("column")`

**Note:**

> 
jQuery's data() method will give you access to data-* attributes, BUT, it clobbers the case of the attribute name. [Reference](http://stackoverflow.com/questions/17351282/jquery-cant-get-data-attribute-value)




## Setting value of HTML attribute


If you want to add an attribute to some element you can use the [`attr(attributeName, attributeValue)`](https://api.jquery.com/attr/) function. For example:

```js
$('a').attr('title', 'Click me');

```

This example will add mouseover text `"Click me"` to all links on the page.

The same function is used to change attributes' values.



## Removing attribute


To remove an attribute from an element you can use the function [`.removeAttr(attributeName)`](https://api.jquery.com/removeAttr/). For example:

```js
$('#home').removeAttr('title');

```

This will remove `title` attribute from the element with ID `home`.



#### Remarks


The jQuery function `.attr()`, gets the value of an attribute for the **first** element in the set of matched elements or set one or more attributes for **every** matched element.

It is worth noting that when getting the value of an attribute, it only gets it from the first element that matches the selector (i.e. `$("input").attr("type");` would only get the type of the first input, if there are more than one)

However, when setting an attribute, it will apply it to all matching elements.

