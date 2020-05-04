---
metaTitle: "Filter Property"
description: "Blur, Drop Shadow (use box-shadow instead if possible), Hue Rotate, Multiple Filter Values, Invert Color"
---

# Filter Property



## Blur


**HTML**

```css
<img src='donald-duck.png' alt='Donald Duck' title='Donald Duck' />

```

**CSS**

```css
img {
    -webkit-filter: blur(1px);
    filter: blur(1px);
}

```

**Result**

[<img src="http://i.stack.imgur.com/XYAHi.png" alt="enter image description here" />](http://i.stack.imgur.com/XYAHi.png)

Makes you wanna rub your glasses.



## Drop Shadow (use box-shadow instead if possible)


**HTML**

```css
<p>My shadow always follows me.</p>

```

**CSS**

```css
p {
    -webkit-filter: drop-shadow(10px 10px 1px green);
    filter: drop-shadow(10px 10px 1px green);
}

```

**Result**

[<img src="http://i.stack.imgur.com/70t2C.png" alt="enter image description here" />](http://i.stack.imgur.com/70t2C.png)



## Hue Rotate


**HTML**

```css
<img src='donald-duck.png' alt='Donald Duck' title='Donald Duck' />

```

**CSS**

```css
img {
    -webkit-filter: hue-rotate(120deg);
    filter: hue-rotate(120deg);
}

```

**Result**

[<img src="http://i.stack.imgur.com/CYvur.png" alt="enter image description here" />](http://i.stack.imgur.com/CYvur.png)



## Multiple Filter Values


To use multiple filters, separate each value with a space.

**HTML**

```css
<img src='donald-duck.png' alt='Donald Duck' title='Donald Duck' />

```

**CSS**

```css
img {
    -webkit-filter: brightness(200%) grayscale(100%) sepia(100%) invert(100%);
    filter: brightness(200%) grayscale(100%) sepia(100%) invert(100%);
}

```

**Result**

[<img src="http://i.stack.imgur.com/pxMPC.png" alt="enter image description here" />](http://i.stack.imgur.com/pxMPC.png)



## Invert Color


**HTML**

```css
<div></div>

```

**CSS**

```css
div {
    width: 100px;
    height: 100px;
    background-color: white;
    -webkit-filter: invert(100%);
    filter: invert(100%);
}

```

**Result**

[<img src="http://i.stack.imgur.com/tO8fB.png" alt="enter image description here" />](http://i.stack.imgur.com/tO8fB.png)

Turns from white to black.



#### Syntax


- filter: none (default value)
- filter: initial (defaults to none);
- filter: inherit (defaults to parent value);
- filter: blur(px)
- filter: brightness(number | %)
- filter: contrast(number | %)
- filter: drop-shadow(horizontal-shadow-px  vertical-shadow-px shadow-blur-px shadow- - spread color)
- filter: greyscale(number | %)
- filter: hue-rotate(deg)
- filter: invert(number | %)
- filter: opacity(number | %)
- filter: saturate(number | %)
- filter: sepia(number | %)



#### Parameters


|Value|Description
|------
|blur(x)|Blurs the image by x pixels.
|brightness(x)|Brightens the image at any value above 1.0 or 100%. Below that, the image will be darkened.
|contrast(x)|Provides more contrast to the image at any value above 1.0 or 100%. Below that, the image will get less saturated.
|drop-shadow(h, v, x, y, z)|Gives the image a drop-shadow. h and v can have negative values. x, y, and z are optional.
|greyscale(x)|Shows the image in greyscale, with a maximum value of 1.0 or 100%.
|hue-rotate(x)|Applies a hue-rotation to the image.
|invert(x)|Inverts the color of the image with a maximum value of 1.0 or 100%.
|opacity(x)|Sets how opaque/transparent the image is with a maximum value of 1.0 or 100%.
|saturate(x)|Saturates the image at any value above 1.0 or 100%. Below that, the image will start to de-saturate.
|sepia(x)|Converts the image to sepia with a maximum value of 1.0 or 100%.



#### Remarks


<li>
Since filter is an experimental feature, you should use the -webkit prefix. It may change in syntax and behavior, but the changes are probably going to be small.
</li>
<li>
It might not be supported in older versions of major browsers. It might be entirely unsupported in mobile browsers.
</li>
<li>
Due to its relatively limited support, try to use `box-shadow` instead of `filter: drop-shadow()`. Use `opacity` instead of `filter: opacity()`.
</li>
<li>
It can be animated through Javascript/jQuery. For Javascript, use `object.style.WebkitFilter`.
</li>
<li>
Check [W3Schools](http://www.w3schools.com/cssref/css3_pr_filter.asp) or [MDN](https://developer.mozilla.org/en/docs/Web/CSS/filter) for more info.
</li>
<li>
W3Schools also has a [demo page](http://www.w3schools.com/cssref/playit.asp?filename=playcss_filter&preval=hue-rotate(90deg)) for all the different type of filter values.
</li>

