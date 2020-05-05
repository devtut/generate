---
metaTitle: "CSS - CSS Image Sprites"
description: "A Basic Implementation"
---

# CSS Image Sprites



## A Basic Implementation


**What's an image sprite?**

An image sprite is a single asset located within an image sprite sheet.
An image sprite sheet is an image file that contains more than one asset that can be extracted from it.

For example:

[<img src="https://i.stack.imgur.com/XuyVW.png" alt="a basic image sprite sheet" />](https://i.stack.imgur.com/XuyVW.png)

The image above is an image sprite sheet, and each one of those stars is a sprite within the sprite sheet. These sprite sheets are useful because they improve performance by reducing the number of HTTP requests a browser might have to make.

So how do you implement one? Here's some example code.

**HTML**

```css
<div class="icon icon1"></div>
<div class="icon icon2"></div>
<div class="icon icon3"></div>

```

**CSS**

```css
.icon {
    background: url(“icons-sprite.png”);
    display: inline-block;
    height: 20px;
    width: 20px;
}
.icon1 {
      background-position: 0px 0px;
}
.icon2 {
      background-position: -20px 0px;
}
.icon3 {
      background-position: -40px 0px;
}

```

By using setting the sprite's width and height and by using the background-position property in CSS (with an x and y value) you can easily extract sprites from a sprite sheet using CSS.



#### Syntax


<li>**//Using background-position**<br />
background: url("sprite-image.png");<br />
background-position: -20px 50px;</li>
<li>**//Background property shorthand**<br />
background: url("sprite-image.png") -20px 50px;</li>



#### Remarks


For some use cases, sprites are slowly falling out of favor, being replaced by icon webfonts or [SVG images](http://stackoverflow.com/documentation/svg/963/introduction-to-svg#t=201607281322046213028).

