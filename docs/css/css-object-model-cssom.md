---
metaTitle: "CSS Object Model (CSSOM)"
description: "Adding a background-image rule via the CSSOM, Introduction"
---

# CSS Object Model (CSSOM)



## Adding a background-image rule via the CSSOM


To add a background-image rule via the CSSOM, first get a reference to the rules of the first stylesheet:

```css
var stylesheet = document.styleSheets[0].cssRules;

```

Then, get a reference to the end of the stylesheet:

```css
var end = stylesheet.length - 1;

```

Finally, insert a background-image rule for the body element at the end of the stylesheet:

```css
stylesheet.insertRule("body { background-image: url('http://cdn.sstatic.net/Sites/stackoverflow/img/favicon.ico'); }", end);

```



## Introduction


The browser identifies tokens from stylesheet and coverts them into nodes which are linked into a tree structure. The entire map of all the nodes with their associated styles of a page would be the CSS Object Model.

To display the webpage, a web browser takes following steps.

<li>The web browser examines your HTML and builds the DOM (Document
Object Model).</li>
<li>The web browser examines your CSS and builds the CSSOM (CSS Object
Model).</li>
<li>The web browser combines the DOM and the CSSOM to create a render
tree. The web browser displays your webpage.</li>

[<img src="http://i.stack.imgur.com/ZH4c7.png" alt="enter image description here" />](http://i.stack.imgur.com/ZH4c7.png)



#### Remarks


The CSS Object Model (CSSOM) is a specification on its own.

The current draft can be found here: [https://www.w3.org/TR/cssom-1/](https://www.w3.org/TR/cssom-1/)

