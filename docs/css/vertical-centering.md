---
metaTitle: "Vertical Centering"
description: "Centering with display: table, Centering with Transform, Centering with Flexbox, Centering Text with Line Height, Centering with Position: absolute, Centering with pseudo element"
---

# Vertical Centering




## Centering with display: table


HTML:

```css
<div class="wrapper">
    <div class="outer">
        <div class="inner">
            centered
        </div>
    </div>
</div>

```

CSS:

```css
.wrapper {
  height: 600px;
  text-align: center;
}
.outer {
  display: table;
  height: 100%;
  width: 100%;
}
.outer .inner {
  display: table-cell;
  text-align: center;
  vertical-align: middle;
}

```



## Centering with Transform


HTML:

```css
<div class="wrapper">
    <div class="centered">
        centered
    </div>
</div>

```

CSS:

```css
.wrapper {
  position: relative;
  height: 600px;
}
.centered {
  position: absolute;
  z-index: 999;
  transform: translate(-50%, -50%);
  top: 50%;
  left: 50%;
}

```



## Centering with Flexbox


HTML:

```css
<div class="container">
    <div class="child"></div>
</div>

```

CSS:

```css
.container {
  height: 500px;
  width: 500px;
  display: flex;              // Use Flexbox
  align-items: center;        // This centers children vertically in the parent.
  justify-content: center;    // This centers children horizontally.
  background: white;
}

.child {
  width: 100px;              
  height: 100px;
  background: blue;
}

```



## Centering Text with Line Height


HTML:

```css
<div class="container">
    <span>vertically centered</span>
</div>

```

CSS:

```css
.container{
    height: 50px;           /* set height */
    line-height: 50px;      /* set line-height equal to the height */
    vertical-align: middle; /* works without this rule, but it is good having it explicitly set */ 
}

```

**Note:** This method will only vertically center a **single line of text**. It will not center block elements correctly and if the text breaks onto a new line, you will have two very tall lines of text.



## Centering with Position: absolute


HTML:

```css
<div class="wrapper">
  <img src="http://cdn.sstatic.net/Sites/stackoverflow/company/img/logos/so/so-icon.png?v=c78bd457575a">
</div>

```

CSS:

```css
.wrapper{
   position:relative;
   height: 600px;
}
.wrapper img {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  margin: auto;
}

```

If you want to center other then images, then you must give height and width to that element.

HTML:

```css
<div class="wrapper">
  <div class="child">
     make me center
  </div>
</div>

```

CSS:

```css
.wrapper{
   position:relative;
   height: 600px;
}
.wrapper .child {
  position: absolute;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  margin: auto;
  width: 200px;
  height: 30px;
  border: 1px solid #f00;
}

```



## Centering with pseudo element


HTML:

```css
<div class="wrapper">
  <div class="content"></div>
</div>

```

CSS:

```css
.wrapper{
   min-height: 600px;
}

.wrapper:before{
  content: "";
  display: inline-block;
  height: 100%;
  vertical-align: middle;
}

.content {
  display: inline-block;
  height: 80px;
  vertical-align: middle;
}

```

This method is best used in cases where you have a varied-height `.content` centered inside `.wrapper`; and you want `.wrapper`'s height to expand when `.content`'s height exceed `.wrapper`'s min-height.



#### Remarks


This is used when the element's dimensions (`width` and `height`) are not known or dynamic.

Prefer to use **Flexbox** over all other options as it is optimized for user interface design.

