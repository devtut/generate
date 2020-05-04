---
metaTitle: "Custom Properties (Variables)"
description: "Variable Color, Variable Dimensions, Variable Cascading, Valid/Invalids, With media queries"
---

# Custom Properties (Variables)


CSS Variables allow authors to create reusable values which can be used throughout a CSS document.

For example, it's common in CSS to reuse a single color throughout a document. Prior to CSS Variables this would mean reusing the same color value many times throughout a document. With CSS Variables the color value can be assigned to a variable and referenced in multiple places. This makes changing values easier and is more semantic than using traditional CSS values.



## Variable Color


```css
:root {
  --red: #b00;
  --blue: #4679bd;
  --grey: #ddd;
}
.Bx1 {
  color: var(--red);
  background: var(--grey);
  border: 1px solid var(--red);
}

```



## Variable Dimensions


```css
:root {
  --W200: 200px;
  --W10: 10px;
}
.Bx2 {
  width: var(--W200);
  height: var(--W200);
  margin: var(--W10);
}

```



## Variable Cascading


CSS variables cascade in much the same way as other properties, and can be restated safely.

You can define variables multiple times and only the definition with the highest specificity will apply to the element selected.

Assuming this HTML:

```css
<a class="button">Button Green</a>
<a class="button button_red">Button Red</a>
<a class="button">Button Hovered On</a>

```

We can write this CSS:

```css
.button {
  --color: green;
  padding: .5rem;
  border: 1px solid var(--color);
  color: var(--color);
}

.button:hover {
  --color: blue;
}

.button_red {
  --color: red;
}

```

And get this result:

[<img src="https://i.stack.imgur.com/49U74.png" alt="Three button variations" />](https://i.stack.imgur.com/49U74.png)



## Valid/Invalids


**Naming**
When naming CSS variables, it contains only letters and dashes just like other CSS properties (eg: line-height, -moz-box-sizing) but it should start with double dashes (--)

```css
//These are Invalids variable names
--123color: blue;
--#color: red;
--bg_color: yellow
--$width: 100px;

//Valid variable names
--color: red;
--bg-color: yellow
--width: 100px;

```

**CSS Variables are case sensitive.**

```css
/* The variable names below are all different variables */
--pcolor: ;
--Pcolor: ;
--pColor: ;

```

**Empty Vs Space**

```css
/* Invalid */
    --color:;

/* Valid */
  --color: ; /* space is assigned */

```

**Concatenations**

```

   /* Invalid - CSS doesn't support concatenation*/
    .logo{
        --logo-url: 'logo';
        background: url('assets/img/' var(--logo-url) '.png');
    }

    /* Invalid - CSS bug */
    .logo{
        --logo-url: 'assets/img/logo.png';
        background: url(var(--logo-url));
    }

    /* Valid */
    .logo{
        --logo-url: url('assets/img/logo.png');
        background: var(--logo-url);
    }

```

**Careful when using Units**

```

   /* Invalid */
    --width: 10;
    width: var(--width)px;

    /* Valid */
    --width: 10px;
    width: var(--width);

    /* Valid */
    --width: 10;
    width: calc(1px * var(--width)); /* multiply by 1 unit to convert */
    width: calc(1em * var(--width));

```



## With media queries


You can re-set variables within media queries and have those new values cascade wherever they are used, something that isn't possible with pre-processor variables.

Here, a media query changes the variables used to set up a very simple grid:

**HTML**

```css
<div></div>
<div></div>
<div></div>
<div></div>

```

**CSS**

```css
:root{
    --width: 25%;
    --content: 'This is desktop';
}
@media only screen and (max-width: 767px){
    :root{
        --width:50%;
        --content: 'This is mobile';
    }
}
@media only screen and (max-width: 480px){
    :root{
        --width:100%;
    }
}

div{
    width: calc(var(--width) - 20px);
    height: 100px;
}
div:before{
    content: var(--content);
}

/* Other Styles */
body {
    padding: 10px;
}

div{
    display: flex;
    align-items: center;
    justify-content: center;
    font-weight:bold;
    float:left;
    margin: 10px;
    border: 4px solid black;
    background: red;
}

```

You can try resizing the window in this [CodePen Demo](https://codepen.io/mkumaran/pen/BZaXvz)

Here's an animated screenshot of the resizing in action:

[<img src="https://i.stack.imgur.com/GXWRP.gif" alt="Resizing the screen" />](https://i.stack.imgur.com/GXWRP.gif)



#### Syntax


- :root {} /* pseudo-class that allows for more global definition of variables */
- --variable-name: **value**; /* define variable */
- var(--variable-name, **default-value**) /* use defined variable with default value fallback */



#### Remarks


**CSS Variables are currently considered an experimental technology.**

<strong><a class="remarks-subsection-anchor" name="remarks-browser-support-/-compatibility-0"></a>
<h3>BROWSER SUPPORT / COMPATIBILITY</h3></strong>

**Firefox:** Version **31**+ **(Enabled by default)**

[More info from Mozilla](https://developer.mozilla.org/en-US/docs/Web/CSS/var())

**Chrome:** Version **49**+ **(Enabled by default)**.

**"This feature can be enabled in Chrome Version 48 for testing by enabling the `experimental Web Platform` feature. Enter `chrome://flags/` in your Chrome address bar to access this setting."**

**IE:** Not Supported.

**Edge:** [Under Development](https://developer.microsoft.com/en-us/microsoft-edge/platform/status/csscustompropertiesakacssvariables/)

**Safari:** Version **9.1+**

