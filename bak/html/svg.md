---
metaTitle: "HTML - SVG"
description: "Inline SVG, Embedding external SVG files in HTML, Embedding SVG using CSS"
---

# SVG


SVG stands for Scalable Vector Graphics. SVG is used to define graphics for the Web

The HTML `<svg>` element is a container for SVG graphics.

SVG has several methods for drawing paths, boxes, circles, text, and graphic images.



## Inline SVG


SVG can be written directly into a HTML document. Inline SVG can be styled and manipulated using CSS and JavaScript.

```html
<body>
    <svg class="attention" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 1000 1000" >
        <path id="attention" d="m571,767l0,-106q0,-8,-5,-13t-12,-5l-108,0q-7,0,-12,5t-5,13l0,106q0,8,5,13t12,6l108,0q7,0,12,-6t5,-13Zm-1,-208l10,-257q0,-6,-5,-10q-7,-6,-14,-6l-122,0q-7,0,-14,6q-5,4,-5,12l9,255q0,5,6,9t13,3l103,0q8,0,13,-3t6,-9Zm-7,-522l428,786q20,35,-1,70q-10,17,-26,26t-35,10l-858,0q-18,0,-35,-10t-26,-26q-21,-35,-1,-70l429,-786q9,-17,26,-27t36,-10t36,10t27,27Z" />
    </svg>
</body>

```

The above inline SVG can then be styled using the corresponding CSS class:

```html
.attention {
    fill: red;
    width: 50px;
    height: 50px;
}

```

The result looks like this:

[<img src="http://i.stack.imgur.com/8Kxaa.png" alt="enter image description here" />](http://i.stack.imgur.com/8Kxaa.png)



## Embedding external SVG files in HTML


You can use the `<img>` or `<object>` elements to embed external SVG elements. Setting the height and width is optional but is highly recommended.

### Using the image element

```html
<img src="attention.svg" width="50" height="50">

```

Using `<img>` does not allow you to style the SVG using CSS or manipulate it using JavaScript.

### Using the object element

```html
<object type="image/svg+xml" data="attention.svg" width="50" height="50">

```

Unlike `<img>`, `<object>` directly imports the SVG into the document and therefore it can be manipulated using Javascript and CSS.



## Embedding SVG using CSS


You can add external SVG files using the `background-image` property, just as you would do with any other image.

HTML:

```html
<div class="attention"></div>

```

CSS:

```css
.attention {
    background-image: url(attention.svg);
    background-size: 100% 100%;
    width: 50px;
    height: 50px;
}

```

You can also embed the image directly into a css file using a data url:

```css
background-image: url(data:image/svg+xml,%3Csvg%20xmlns%3D%22http%3A%2F%2Fwww.w3.org%2F2000%2Fsvg%22%20xmlns%3Axlink%3D%22http%3A%2F%2Fwww.w3.org%2F1999%2Fxlink%22%20viewBox%3D%220%200%201000%201000%22%20%3E%0D%0A%3Cpath%20id%3D%22attention%22%20d%3D%22m571%2C767l0%2C-106q0%2C-8%2C-5%2C-13t-12%2C-5l-108%2C0q-7%2C0%2C-12%2C5t-5%2C13l0%2C106q0%2C8%2C5%2C13t12%2C6l108%2C0q7%2C0%2C12%2C-6t5%2C-13Zm-1%2C-208l10%2C-257q0%2C-6%2C-5%2C-10q-7%2C-6%2C-14%2C-6l-122%2C0q-7%2C0%2C-14%2C6q-5%2C4%2C-5%2C12l9%2C255q0%2C5%2C6%2C9t13%2C3l103%2C0q8%2C0%2C13%2C-3t6%2C-9Zm-7%2C-522l428%2C786q20%2C35%2C-1%2C70q-10%2C17%2C-26%2C26t-35%2C10l-858%2C0q-18%2C0%2C-35%2C-10t-26%2C-26q-21%2C-35%2C-1%2C-70l429%2C-786q9%2C-17%2C26%2C-27t36%2C-10t36%2C10t27%2C27Z%22%20%2F%3E%0D%0A%3C%2Fsvg%3E);

```



#### Remarks


SVG is an XML-based language for creating scalable vector images. It can be written directly into an HTML document or embedded from external SVG files. Inline SVG can be restyled and modified using CSS and JavaScript respectively.

Browser support for SVG varies, but can be ascertained [here](http://caniuse.com/#cats=SVG).

For more comprehensive information, see the [SVG documentation](http://stackoverflow.com/documentation/svg/topics).

