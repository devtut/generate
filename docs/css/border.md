---
metaTitle: "Border"
description: "border-radius, border-style, Multiple Borders, border (shorthands), border-image, border-collapse, Creating a multi-colored border using border-image, border-[left|right|top|bottom]"
---

# Border




## border-radius


The border-radius property allows you to change the shape of the basic box model.

Every corner of an element can have up to two values, for the vertical and horizontal radius of that corner (for a maximum of 8 values).

[<img src="https://i.stack.imgur.com/FnIqF.png" alt="border-radius" />](https://i.stack.imgur.com/FnIqF.png)

The first set of values defines the horizontal radius. The optional second set of values, preceded by a ‘/’ , defines the vertical radius. If only one set of values is supplied, it is used for both the vertical and horizontal radius.

```css
border-radius: 10px 5% / 20px 25em 30px 35em;

```

The `10px` is the horizontal radius of the top-left-and-bottom-right. And the `5%` is the horizontal radius of the top-right-and-bottom-left. The other four values after '/' are the vertical radii for top-left, top-right, bottom-right and bottom-left.

As with many CSS properties, shorthands can be used for any or all
possible values. You can therefore specify anything from one to eight values. The following shorthand allows you to set the horizontal and vertical radius of every corner to the same value:

HTML:

```css
<div class='box'></div>

```

CSS:

```css
.box {
    width: 250px;
    height: 250px;
    background-color: black;
    border-radius: 10px;
}

```

Border-radius is most commonly used to convert box elements into circles. By setting the border-radius to half of the length of a square element, a circular element is created:

```css
.circle {
    width: 200px;
    height: 200px;
    border-radius: 100px;
}

```

Because border-radius accepts percentages, it is common to use 50% to avoid manually calculating the border-radius value:

```css
.circle {
    width: 150px;
    height: 150px;
    border-radius: 50%;
}

```

If the width and height properties are not equal, the resulting shape will be an oval rather than a circle.

Browser specific border-radius example:

```

 -webkit-border-top-right-radius: 4px;
  -webkit-border-bottom-right-radius: 4px;
  -webkit-border-bottom-left-radius: 0;
  -webkit-border-top-left-radius: 0;
  -moz-border-radius-topright: 4px;
  -moz-border-radius-bottomright: 4px;
  -moz-border-radius-bottomleft: 0;
  -moz-border-radius-topleft: 0;
  border-top-right-radius: 4px;
  border-bottom-right-radius: 4px;
  border-bottom-left-radius: 0;
  border-top-left-radius: 0;

```



## border-style


The `border-style` property sets the style of an element's border.
This property can have from one to four values (for every side of the element one value.)

Examples:

```css
border-style: dotted;

```

```css
border-style: dotted solid double dashed;

```

[<img src="https://i.stack.imgur.com/gieW6.png" alt="border-styles" />](https://i.stack.imgur.com/gieW6.png)

`border-style` can also have the values `none` and `hidden`. They have the same effect, except `hidden` works for border conflict resolution for `<table>` elements. In a `<table>` with multiple borders, `none` has the lowest priority (meaning in a conflict, the border would show), and `hidden` has the highest priority (meaning in a conflict, the border would not show).



## Multiple Borders


Using outline:

```css
.div1{
  border: 3px solid black; 
  outline: 6px solid blue;
  width: 100px;
  height: 100px;
  margin: 20px;
}

```

Using box-shadow:

```css
.div2{
  border: 5px solid green; 
  box-shadow: 0px 0px 0px 4px #000;
  width: 100px;
  height: 100px;
  margin: 20px;
}

```

Using a pseudo element:

```css
.div3 {
  position: relative;
  border: 5px solid #000;
  width: 100px;
  height: 100px;
  margin: 20px;
}
.div3:before {
  content: " ";
  position: absolute;
  border: 5px solid blue;
  z-index: -1;
  top: 5px;
  left: 5px;
  right: 5px;
  bottom: 5px;
}

```

[<img src="https://i.stack.imgur.com/lta3P.png" alt="multiple borders" />](https://i.stack.imgur.com/lta3P.png)

[http://jsfiddle.net/MadalinaTn/bvqpcohm/2/](http://jsfiddle.net/MadalinaTn/bvqpcohm/2/)



## border (shorthands)


In most cases you want to define several border properties (`border-width`, `border-style` and `border-color`) for all sides of an element.

Instead of writing:

```css
border-width: 1px;
border-style: solid;
border-color: #000;

```

You can simply write:

```css
border: 1px solid #000;

```

These shorthands are also available for every side of an element:
`border-top`, `border-left`, `border-right` and `border-bottom`. So you can do:

```css
border-top: 2px double #aaaaaa;

```



## border-image


With the `border-image` property you have the possibility to set an image to be used instead of normal border styles.

A `border-image` essentially consist of a

- `border-image-source`: The path to the image to be used
- `border-image-slice`: Specifies the offset that is used to divide the image into **nine regions** (four **corners**, four **edges** and a **middle**)
- `border-image-repeat`: Specifies how the images for the sides and the middle of the border image are scaled

Consider the following example wheras border.png is a image of 90x90 pixels:

`border-image: url("border.png") 30 stretch;`

The image will be split into nine regions with 30x30 pixels. The edges will be used as the corners of the border while the side will be used in between. If the element is higher / wider than 30px this part of the image will be **stretched**. The middle part of the image defaults to be transparent.



## border-collapse


The `border-collapse` property applies only to `table`s (and elements displayed as `display: table` or `inline-table`) and sets whether the table borders are collapsed into a single border or detached as in standard HTML.

```css
table {
  border-collapse: separate; /* default */
  border-spacing: 2px; /* Only works if border-collapse is separate */
}

```

Also see [Tables - border-collapse](http://stackoverflow.com/documentation/css/1074/tables/3456/border-collapse) documentation entry



## Creating a multi-colored border using border-image


### CSS

```css
.bordered {
  border-image: linear-gradient(to right, red 20%, green 20%, green 40%, blue 40%, blue 60%, maroon 60%, maroon 80%, chocolate 80%); /* gradient with required colors */
  border-image-slice: 1;
}

```

### HTML

```css
<div class='bordered'>Border on all sides</div>

```

The above example would produce a border that comprises of 5 different colors. The colors are defined through a `linear-gradient` (you can find more information about gradients in the [docs](http://stackoverflow.com/documentation/css/296/backgrounds/2478/css-gradients#t=201607291511493190436)). You can find more information about `border-image-slice` property in the [`border-image` example](http://stackoverflow.com/documentation/css/2160/border/9699/border-image) in same page.

[<img src="https://i.stack.imgur.com/YWzek.png" alt="enter image description here" />](https://i.stack.imgur.com/YWzek.png)

(**Note: Additional properties were added to the element for presentational purpose.**)

You'd have noticed that the left border has only a single color (the start color of the gradient) while the right border also has only a single color (the  gradient's end color). This is because of the way that border image property works. It is as though the gradient is applied to the entire box and then the colors are masked from the padding and content areas, thus making it look as though only the border has the gradient.

Which border(s) have a single color is dependant on the gradient definition. If the gradient is a `to right` gradient, the left border would be the start color of the gradient and right border would be the end color. If it was a `to bottom` gradient the top border would be the gradient's start color and bottom border would be end color. Below is the output of a `to bottom` 5 colored gradient.

[<img src="https://i.stack.imgur.com/6ihGO.png" alt="enter image description here" />](https://i.stack.imgur.com/6ihGO.png)

If the border is required only on specific sides of the element then the `border-width` property can be used just like with any other normal border. For example, adding the below code would produce a border only on the top of the element.

```css
border-width: 5px 0px 0px 0px;

```

[<img src="https://i.stack.imgur.com/wU0fE.png" alt="enter image description here" />](https://i.stack.imgur.com/wU0fE.png)

Note that, any element that has `border-image` property **won't respect the** `border-radius` (that is the border won't curve). This is based on the below statement in the spec:

> 
A box's backgrounds, but not its border-image, are clipped to the appropriate curve (as determined by ‘background-clip’).




## border-[left|right|top|bottom]


The `border-[left|right|top|bottom]` property is used to add a border to a specific side of an element.

For example if you wanted to add a border to the left side of an element, you could do:

```css
#element {
    border-left: 1px solid black;
}

```



#### Syntax


<li>
**border**
</li>
<li>
border: border-width border-style border-color | initial | inherit;
</li>
<li>
border-top: border-width border-style border-color | initial | inherit;
</li>
<li>
border-bottom: border-width border-style border-color | initial | inherit;
</li>
<li>
border-left: border-width border-style border-color | initial | inherit;
</li>
<li>
border-right: border-width border-style border-color | initial | inherit;
</li>
<li>
**border-style**
</li>
<li>
border-style: 1-4 none | hidden | dotted | dashed | solid | double | groove | ridge | inset | outset | initial | inherit;
</li>
<li>
**border-radius**
</li>
<li>
border-radius: 1-4 length | % / 1-4 length | % | initial | inherit;
</li>
<li>
border-top-left-radius: length | %  [length | %] | initial | inherit;
</li>
<li>
border-top-right-radius: length | %  [length | %] | initial | inherit;
</li>
<li>
border-bottom-left-radius: length | %  [length | %] | initial | inherit;
</li>
<li>
border-bottom-right-radius: length | %  [length | %] | initial | inherit;
</li>
<li>
**border-image**
</li>
<li>
border-image: border-image-source border-image-slice [ border-image-width [ border-image-outset ] ] border-image-repeat
</li>
<li>
border-image-source: none | image;
</li>
<li>
border-image-slice: 1-4 number | percentage [fill]
</li>
<li>
border-image-repeat: 1-2 stretch | repeat | round | space
</li>
<li>
**border-collapse**
</li>
<li>
border-collapse: separate | collapse | initial | inherit
</li>



#### Remarks


Related [properties](https://developer.mozilla.org/en-US/docs/Web/CSS/border):

<li>
border
</li>
<li>
border-bottom
</li>
<li>
border-bottom-color
</li>
<li>
border-bottom-left-radius
</li>
<li>
border-bottom-right-radius
</li>
<li>
border-bottom-style
</li>
<li>
border-bottom-width
</li>
<li>
border-color
</li>
<li>
border-image
</li>
<li>
border-image-outset
</li>
<li>
border-image-repeat
</li>
<li>
border-image-slice
</li>
<li>
border-image-source
</li>
<li>
border-image-width
</li>
<li>
border-left
</li>
<li>
border-left-color
</li>
<li>
border-left-style
</li>
<li>
border-left-width
</li>
<li>
border-radius
</li>
<li>
border-right
</li>
<li>
border-right-color
</li>
<li>
border-right-style
</li>
<li>
border-right-width
</li>
<li>
border-style
</li>
<li>
border-top
</li>
<li>
border-top-color
</li>
<li>
border-top-left-radius
</li>
<li>
border-top-right-radius
</li>
<li>
border-top-style
</li>
<li>
border-top-width
</li>
<li>
border-width
</li>

