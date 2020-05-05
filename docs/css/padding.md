---
metaTitle: "Padding"
description: "Padding Shorthand, Padding on a given side"
---

# Padding



## Padding Shorthand


The padding property sets the padding space on all sides of an element. The padding area is the space between the content of the element and its border. Negative values are not allowed.

To save adding padding to each side individually (using `padding-top`, `padding-left` etc) can you write it as a shorthand, as below:

**Four values**:

```html
<style>
    .myDiv {
        padding: 25px 50px 75px 100px; /* top right bottom left; */
    }
</style>
<div class="myDiv"></div>

```

[<img src="https://i.stack.imgur.com/xWS9v.png" alt="enter image description here" />](https://i.stack.imgur.com/xWS9v.png)

**Three values**:

```html
<style>
    .myDiv {
        padding: 25px 50px 75px; /* top left/right bottom */
    }
</style>
<div class="myDiv"></div>

```

[<img src="https://i.stack.imgur.com/Qrs3R.png" alt="enter image description here" />](https://i.stack.imgur.com/Qrs3R.png)

**Two values**:

```html
<style>
    .myDiv {
        padding: 25px 50px; /* top/bottom left/right */
    }
</style>
<div class="myDiv"></div>

```

[<img src="https://i.stack.imgur.com/LiW8C.png" alt="enter image description here" />](https://i.stack.imgur.com/LiW8C.png)

**One value**:

```html
<style>
    .myDiv {
        padding: 25px; /* top/right/bottom/left */
    }
</style>
<div class="myDiv"></div>

```

[<img src="https://i.stack.imgur.com/GdRZW.png" alt="enter image description here" />](https://i.stack.imgur.com/GdRZW.png)



## Padding on a given side


The padding property sets the padding space on all sides of an element. The padding area is the space between the content of the element and its border. Negative values are not allowed.

You can specify a side individually:

- `padding-top`
- `padding-right`
- `padding-bottom`
- `padding-left`

The following code would add a padding of `5px` to the top of the div:

```html
<style>
.myClass {
    padding-top: 5px;
}
</style>

<div class="myClass"></div>

```



#### Syntax


- padding: **length**|initial|inherit|unset;
- padding-top: **length**|initial|inherit|unset;
- padding-right: **length**|initial|inherit|unset;
- padding-bottom: **length**|initial|inherit|unset;
- padding-left: **length**|initial|inherit|unset;



#### Remarks


> 
<p><a href="http://stackoverflow.com/questions/4973988/why-does-css-not-support-negative-padding">The padding property sets the padding space on all sides of an
element. The padding area is the space between the content of the
element and its border. **Negative values are not allowed**.</a></p>


[1](http://stackoverflow.com/questions/4973988/why-does-css-not-support-negative-padding): [https://developer.mozilla.org/en/docs/Web/CSS/padding](https://developer.mozilla.org/en/docs/Web/CSS/padding) MDN

Also see this [question](http://stackoverflow.com/questions/4973988/why-does-css-not-support-negative-padding), ["Why does CSS not support negative padding?"](http://stackoverflow.com/questions/4973988/why-does-css-not-support-negative-padding) and his answers.

Also please consider [The Box Model](http://stackoverflow.com/documentation/css/646/the-box-model#t=201704122142044217481) when using padding. Depending on the box-sizing value, padding on an element can either add to the previously defined height/width of an element or not.

Related Properties:

[margin](http://stackoverflow.com/documentation/css/305/margins#t=201608021521171797931)

Padding on inline elements will only apply to the left and right of the element, and not the top and bottom, due to the inherent display properties of inline elements.

