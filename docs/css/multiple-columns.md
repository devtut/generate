---
metaTitle: "Multiple columns"
description: "Basic example, Create Multiple Columns"
---

# Multiple columns


CSS allows to define that element contents wrap into multiple columns with gaps and rules between them.



## Basic example


Consider the following HTML markup:

```css
<section>
  <p>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum.</p>
  <p> Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.</p>
  <p>Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet.</p>
</section>

```

With the following CSS applied the content is split into three columns separated by a gray column rule of two pixels.

```css
section {
  columns: 3;
  column-gap: 40px;
  column-rule: 2px solid gray;
}

```

See a [live sample of this on JSFiddle](https://jsfiddle.net/vjL9ewmb/).



## Create Multiple Columns


```css
<div class="content">
Lorem ipsum dolor sit amet, consectetuer adipiscing elit, sed diam nonummy nibh 
euismod tincidunt ut laoreet dolore magna aliquam erat volutpat. Ut wisi enim 
ad minim veniam, quis nostrud exerci tation ullamcorper suscipit lobortis nisl 
ut aliquip ex ea commodo consequat. Duis autem vel eum iriure dolor in 
hendrerit in vulputate velit esse molestie consequat, vel illum dolore eu 
feugiat nulla facilisis at vero eros et accumsan et iusto odio dignissim qui 
blandit praesent luptatum zzril delenit augue duis dolore te feugait nulla 
facilisi. Nam liber tempor cum soluta nobis eleifend option congue nihil 
imperdiet doming id quod mazim placerat facer possim assum.
</div>

```

Css

```css
.content {
-webkit-column-count: 3; /* Chrome, Safari, Opera */
-moz-column-count: 3; /* Firefox */
column-count: 3;
}

```



#### Remarks


[CSS Multi-column Layout Module Level 1](https://www.w3.org/TR/css3-multicol/) is, as of 12 April 2011, a W3C Candidate Recommendation. Since then, a few [smaller changes were made](https://drafts.csswg.org/css-multicol-1/#changes). It is considered to be in the [Stable stage](https://www.w3.org/Style/CSS/current-work).

As of 3 July 2017, Microsoft's Internet Explorer 10 and 11 and Edge browsers only support an older version of the specification using a vendor prefix.

