---
metaTitle: "CSS - Block Formatting Contexts"
description: "Using the overflow property with a value different to visible"
---

# Block Formatting Contexts



## Using the overflow property with a value different to visible


```css
img{
  float:left;
  width:100px;
  margin:0 10px;
}
.div1{
  background:#f1f1f1;
  /* does not create block formatting context */
}
.div2{
  background:#f1f1f1;
  overflow:hidden;
  /* creates block formatting context */
}

```

[<img src="https://i.stack.imgur.com/ceEkU.png" alt="enter image description here" />](https://i.stack.imgur.com/ceEkU.png)

[https://jsfiddle.net/MadalinaTn/qkwwmu6m/2/](https://jsfiddle.net/MadalinaTn/qkwwmu6m/2/)

> 
<p><a href="https://css-tricks.com/almanac/properties/o/overflow/" rel="nofollow noreferrer">Using the overflow property with a value different to visible (its
default) will create a new block formatting context. This is
technically necessary — if a float intersected with the scrolling
element it would forcibly rewrap the content.</a></p>


This example that show how a number of paragraphs will interact with a floated image is similar to [this example](https://css-tricks.com/almanac/properties/o/overflow/), on css-tricks.com.

[2](https://css-tricks.com/almanac/properties/o/overflow/): [https://developer.mozilla.org/en-US/docs/Web/CSS/overflow](https://developer.mozilla.org/en-US/docs/Web/CSS/overflow) MDN



#### Remarks


> 
<p>[A block formatting context is a part of a visual CSS rendering of a
Web page. It is the region in which the layout of block boxes occurs
and in which floats interact with each other.][1]</p>


[1]: [https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Block_formatting_context](https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Block_formatting_context) MDN

