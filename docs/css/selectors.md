---
metaTitle: "Selectors"
description: "Attribute Selectors, Basic selectors, Combinators, Pseudo-classes, Child Pseudo Class, Class Name Selectors, Select element using its ID without the high specificity of the ID selector, Global boolean with checkbox:checked and ~ (general sibling combinator), CSS3 :in-range selector example, A. The :not pseudo-class example & B. :focus-within CSS pseudo-class, The :last-of-type selector, ID selectors, How to style a Range input, The :only-child pseudo-class selector example"
---

# Selectors


CSS selectors identify specific HTML elements as targets for CSS styles. This topic covers how CSS selectors target HTML elements. Selectors use a wide range of over 50 selection methods offered by the CSS language, including elements, classes, IDs, pseudo-elements and pseudo-classes, and patterns.



## Attribute Selectors


### Overview

Attribute selectors can be used with various types of operators that change the selection criteria accordingly. They select an element using the presence of a given attribute or attribute value.

<th align="right">Selector<sup>(1)</sup></th>|Matched element|Selects elements...<th align="center">CSS Version</th>
|------
<td align="right">`[attr]`</td>|`<div attr>`|With attribute `attr`<td align="center">2</td>
<td align="right">`[attr='val']`</td>|`<div attr="val">`|Where attribute `attr` has value `val`<td align="center">2</td>
<td align="right">`[attr~='val']`</td>|`<div attr="val val2 val3">`|Where `val` appears in the<br>whitespace-separated list of `attr`<td align="center">2</td>
<td align="right">`[attr^='val']`</td>|`<div attr="val1 val2">`|Where `attr`'s value **begins** with `val`<td align="center">3</td>
<td align="right">`[attr$='val']`</td>|`<div attr="sth aval">`|Where the `attr`'s value **ends** with `val`<td align="center">3</td>
<td align="right">`[attr*='val']`</td>|`<div attr="somevalhere">`|Where `attr` contains `val` anywhere<td align="center">3</td>
<td align="right">`[attr|='val']`</td>|`<div attr="val-sth etc">`|Where `attr`'s value is exactly `val`,<br>or starts with `val` and immediately<br>followed by `-` (U+002D)<td align="center">2</td>
<td align="right">`[attr='val' i]`</td>|`<div attr="val">`|Where `attr` has value `val`,<br>ignoring `val`'s letter casing.<td align="center">4<sup>(2)</sup></td>

****Notes:****

<li>
The attribute value can be surrounded by either single-quotes or double-quotes. No quotes at all may also work, but it's not valid according to the CSS standard, and is discouraged.
</li>
<li>
There is no single, integrated CSS4 specification, because it is split into separate modules. However, there are "level 4" modules. [See browser support](http://caniuse.com/#feat=css-case-insensitive).
</li>

### Details

### `[attribute]`

Selects elements with the given attribute.

```css
div[data-color] {
  color: red;
}

```

```css
<div data-color="red">This will be red</div>
<div data-color="green">This will be red</div>
<div data-background="red">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/cezale/1/edit?html,css,output)

### `[attribute="value"]`

Selects elements with the given attribute and value.

```css
div[data-color="red"] {
  color: red;
}

```

```css
<div data-color="red">This will be red</div>
<div data-color="green">This will NOT be red</div>
<div data-color="blue">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/waxoked/1/edit?html,css,output)

### `[attribute*="value"]`

Selects elements with the given attribute and value where the given attribute contains the given value anywhere (as a substring).

```css
[class*="foo"] {
  color: red;
}

```

```css
<div class="foo-123">This will be red</div>
<div class="foo123">This will be red</div>
<div class="bar123foo">This will be red</div>
<div class="barfooo123">This will be red</div>
<div class="barfo0">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/dazige/1/edit?html,css,output)

### `[attribute~="value"]`

Selects elements with the given attribute and value where the given value appears in a whitespace-separated list.

```css
[class~="color-red"] {
  color: red;
}

```

```css
<div class="color-red foo-bar the-div">This will be red</div>
<div class="color-blue foo-bar the-div">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/posuhim/1/edit?html,css,output)

### `[attribute^="value"]`

Selects elements with the given attribute and value where the given attribute begins with the value.

```css
[class^="foo-"] {
  color: red;
}

```

```css
<div class="foo-123">This will be red</div>
<div class="foo-234">This will be red</div>
<div class="bar-123">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/yowihi/1/edit?html,css,output)

### `[attribute$="value"]`

Selects elements with the given attribute and value where the given attribute ends with the given value.

```css
[class$="file"] {
  color: red;
}

```

```css
<div class="foobar-file">This will be red</div>
<div class="foobar-file">This will be red</div>
<div class="foobar-input">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/yowihi/2/edit?html,css,output)

### `[attribute|="value"]`

Selects elements with a given attribute and value where the attribute's value is exactly the given value or is exactly the given value followed by `-` (U+002D)

```css
[lang|="EN"] {
  color: red;
}

```

```css
<div lang="EN-us">This will be red</div>
<div lang="EN-gb">This will be red</div>
<div lang="PT-pt">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/yowihi/3/edit?html,css,output)

### `[attribute="value" i]`

Selects elements with a given attribute and value where the attribute's value can be represented as `Value`, `VALUE`, `vAlUe` or any other case-insensitive possibility.

```css
[lang="EN" i] {
  color: red;
}

```

```css
<div lang="EN">This will be red</div>
<div lang="en">This will be red</div>
<div lang="PT">This will NOT be red</div>

```

[Live Demo on JSBin](http://jsbin.com/yowihi/4/edit?html,css,output)

### Specificity of attribute selectors

### `0-1-0`

Same as class selector and pseudoclass.

```css
*[type=checkbox] // 0-1-0

```

Note that this means an attribute selector can be used to select an element by its ID at a lower level of specificity than if it was selected with an [ID selector](http://stackoverflow.com/documentation/css/611/selectors/2212/id-selectors): `[id="my-ID"]` targets the same element as `#my-ID` but with lower specificity.

See the [Syntax Section](http://stackoverflow.com/documentation/css/611/#syntax) for more details.



## Basic selectors


|Selector|Description
|------
|`*`|Universal selector (all elements)
|`div`|Tag selector (all `<div>` elements)
|`.blue`|Class selector (all elements with class `blue`)
|`.blue.red`|All elements with class `blue` **and** `red` (a type of Compound selector)
|`#headline`|ID selector (the element with "id" attribute set to `headline`)
|`:pseudo-class`|[All elements with pseudo-class](http://stackoverflow.com/documentation/css/611/selectors/2220/pseudo-classes)
|`::pseudo-element`|[Element that matches pseudo-element](http://stackoverflow.com/documentation/css/911/pseudo-elements/16491/pseudo-elements)
|`:lang(en)`|Element that matches :lang declaration, for example `<span lang="en">`
|`div > p`|child selector

> 
**Note:** The value of an ID must be unique in a web page. It is a violation of the [HTML standard](https://www.w3.org/TR/html/dom.html#the-id-attribute) to use the value of an ID more than once in the same document tree.


A complete list of selectors can be found in the [CSS Selectors Level 3 specification](https://www.w3.org/TR/css3-selectors/#selectors).



## Combinators


### Overview

|Selector|Description
|------
|`div span`|Descendant selector (all `<span>`s that are descendants of a `<div>`)
|`div > span`|Child selector (all `<span>`s that are a direct child of a `<div>`)
|`a ~ span`|General Sibling selector (all `<span>`s that are siblings after an `<a>`)
|`a + span`|Adjacent Sibling selector (all `<span>`s that are immediately after an `<a>`)

> 
**Note:** Sibling selectors target elements that come after them in the source document. CSS, by its nature (it cascades), cannot target **previous** or **parent** elements. However, using the flex `order` property, [a previous sibling selector can be simulated on visual media](http://stackoverflow.com/a/36118012/3597276).


### Descendant Combinator: `selector selector`

A descendant combinator, represented by at least one space character (``), selects elements that are a descendant of the defined element. This combinator selects **all** descendants of the element (from child elements on down).

```css
div p {
  color:red;
}

```

```css
<div>
  <p>My text is red</p>
  <section>
    <p>My text is red</p>
  </section>
</div>

<p>My text is not red</p>

```

[Live Demo on JSBin](http://jsbin.com/xonafuz/2/edit?html,css,output)

In the above example, the first two `<p>` elements are selected since they are both descendants of the `<div>`.

### Child Combinator: `selector > selector`

The child (`>`) combinator is used to select elements that are **children**, or **direct descendants**, of the specified element.

```css
div > p {
  color:red;
}

```

```css
<div>
  <p>My text is red</p>
  <section>
    <p>My text is not red</p>
  </section>
</div>

```

[Live Demo on JSBin](http://jsbin.com/xonafuz/3/edit?html,css,output)

The above CSS selects only the first `<p>` element, as it is the only paragraph directly descended from a `<div>`.

The second `<p>` element is not selected because it is not a direct child of the `<div>`.

### Adjacent Sibling Combinator: `selector + selector`

The adjacent sibling (`+`) combinator selects a sibling element that immediate follows a specified element.

```css
p + p {
  color:red;
}

```

```css
<p>My text is not red</p>
<p>My text is red</p>
<p>My text is red</p>
<hr>
<p>My text is not red</p>

```

[Live Demo on JSBin](http://jsbin.com/xonafuz/4/edit?html,css,output)

The above example selects only those `<p>` elements which are **directly preceded** by another `<p>` element.

### General Sibling Combinator: `selector ~ selector`

The general sibling (`~`) combinator selects **all** siblings that follow the specified element.

```css
p ~ p {
  color:red;
}

```

```css
<p>My text is not red</p>
<p>My text is red</p>
<hr>
<h1>And now a title</h1>
<p>My text is red</p>

```

[Live Demo on JSBin](http://jsbin.com/xonafuz/5/edit?html,css,output)

The above example selects all `<p>` elements that are **preceded** by another `<p>` element, whether or not they are immediately adjacent.



## Pseudo-classes


[Pseudo-classes](https://www.w3.org/TR/selectors/#pseudo-classes) are **keywords** which allow selection based on information that lies outside of the document tree or that cannot be expressed by other selectors or combinators. This information can be associated to a certain state ([state](https://www.w3.org/TR/selectors/#UIstates) and [dynamic](https://www.w3.org/TR/selectors/#dynamic-pseudos) pseudo-classes), to locations ([structural](https://www.w3.org/TR/selectors/#structural-pseudos) and [target](https://www.w3.org/TR/selectors/#target-pseudo) pseudo-classes), to negations of the former ([negation](https://www.w3.org/TR/selectors/#negation) pseudo-class) or to languages ([lang](https://www.w3.org/TR/selectors/#lang-pseudo) pseudo-class). Examples include whether or not a link has been followed (`:visited`), the mouse is over an element (`:hover`), a checkbox is checked (`:checked`), etc.

### Syntax

```css
selector:pseudo-class { 
    property: value;
}

```

### List of pseudo-classes:

|Name|Description
|------
|[`:active`](https://www.w3.org/TR/css3-selectors/#the-user-action-pseudo-classes-hover-act)|Applies to any element being activated (i.e. clicked) by the user.
|[`:any`](https://developer.mozilla.org/en-US/docs/Web/CSS/:any)|Allows you to build sets of related selectors by creating groups that the <br />included items will match. This is an alternative to repeating an entire selector.
|[`:target`](https://developer.mozilla.org/en-US/docs/Web/CSS/:target)|Selects the current active #news element (clicked on a URL<br />containing that anchor name)
|[`:checked`](https://www.w3.org/TR/css3-selectors/#checked)|Applies to radio, checkbox, or option elements that are checked<br />or toggled into an "on" state.
|[`:default`](https://developer.mozilla.org/en-US/docs/Web/CSS/:default)|Represents any user interface element that is the default among a group of<br />similar elements.
|[`:disabled`](https://www.w3.org/TR/css3-selectors/#enableddisabled)|Applies to any UI element which is in a disabled state.
|[`:empty`](https://www.w3.org/TR/selectors/#empty-pseudo)|Applies to any element which has no children.
|[`:enabled`](https://www.w3.org/TR/css3-selectors/#enableddisabled)|Applies to any UI element which is in an enabled state.
|[`:first`](http://tympanus.net/codrops/css_reference/first)|Used in conjunction with the `@page` rule, this selects the first page in a<br />printed document.
|[`:first-child`](https://developer.mozilla.org/en-US/docs/Web/CSS/:first-child)|Represents any element that is the first child element of its parent.
|[`:first-of-type`](https://www.w3.org/TR/css3-selectors/#first-of-type-pseudo)|Applies when an element is the first of the selected element type<br />inside its parent. This may or may not be the first-child.
|[`:focus`](https://www.w3.org/TR/css3-selectors/#the-user-action-pseudo-classes-hover-act)|Applies to any element which has the user's focus. This can be given by the<br />user's keyboard, mouse events, or other forms of input.
|[`:focus-within`](https://developer.mozilla.org/en-US/docs/Web/CSS/:focus-within)|Can be used to highlight a whole section when one element inside it is focused. It matches any element that the :focus pseudo-class matches or that has a descendant focused.
|[`:full-screen`](https://developer.mozilla.org/en-US/docs/Web/CSS/:fullscreen)|Applies to any element displayed in full-screen mode. It selects the whole stack<br />of elements and not just the top level element.
|[`:hover`](https://www.w3.org/TR/css3-selectors/#the-user-action-pseudo-classes-hover-act)|Applies to any element being hovered by the user's pointing device, but<br />not activated.
|[`:indeterminate`](https://www.w3.org/TR/css3-selectors/#indeterminate)|Applies radio or checkbox UI elements which are neither checked nor<br />unchecked, but are in an indeterminate state. This can be due to an<br />element's attribute or DOM manipulation.
|[`:in-range`](https://developer.mozilla.org/en-US/docs/Web/CSS/:in-range)|The `:in-range` CSS pseudo-class matches when an element has<br />its value attribute inside the specified range limitations for this element.<br />It allows the page to give a feedback that the value currently defined<br />using the element is inside the range limits.
|[`:invalid`](http://tympanus.net/codrops/css_reference/invalid/)|Applies to `<input>` elements whose values are invalid according to<br />the type specified in the `type=` attribute.
|[`:lang`](https://www.w3.org/TR/css3-selectors/#lang-pseudo)|Applies to any element who's wrapping `<body>` element has a properly<br />designated `lang=` attribute. For the pseudo-class to be valid, it must<br />contain a [valid two or three letter language code.](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes)
|[`:last-child`](https://developer.mozilla.org/en-US/docs/Web/CSS/:last-child)|Represents any element that is the last child element of its parent.
|[`:last-of-type`](https://www.w3.org/TR/css3-selectors/#last-of-type-pseudo)|Applies when an element is the last of the selected element type inside<br />its parent. This may or may not be the last-child.
|[`:left`](http://tympanus.net/codrops/css_reference/left_pseudo-class/)|Used in conjunction with the `@page` rule, this selects all the left<br />pages in a printed document.
|[`:link`](https://www.w3.org/TR/css3-selectors/#the-link-pseudo-classes-link-and-visited)|Applies to any links which haven't been visited by the user.
|[`:not()`](https://www.w3.org/wiki/CSS/Selectors/pseudo-classes/:not)|Applies to all elements which **do not** match the value passed to<br />(`:not(p)` or `:not(.class-name)` for example. It must have a value to be<br />valid and it can only contain one selector. However, you can chain multiple `:not` selectors together.
|[`:nth-child`](https://www.w3.org/TR/css3-selectors/#nth-child-pseudo)|Applies when an element is the `n`-th element of its parent, where `n`<br />can be an integer, a mathematical expression (e.g `n+3`) or the keywords<br />`odd` or `even`.
|[`:nth-of-type`](https://www.w3.org/TR/css3-selectors/#nth-of-type-pseudo)|Applies when an element is the `n`-th element of its parent of the<br />same element type, where `n` can be an integer, a mathematical<br />expression (e.g `n+3`) or the keywords `odd` or `even`.
|[`:only-child`](https://developer.mozilla.org/en-US/docs/Web/CSS/:only-child)|The `:only-child` CSS pseudo-class represents any element<br />which is the only child of its parent. This is the same as<br />`:first-child:last-child` or `:nth-child(1):nth-last-child(1)`,<br />but with a lower specificity.
|[`:optional`](https://developer.mozilla.org/en-US/docs/Web/CSS/:optional)|The `:optional` CSS pseudo-class represents any  element<br />that does not have the required attribute set on it. This allows<br />forms to easily indicate optional fields and to style them accordingly.
|[`:out-of-range`](https://developer.mozilla.org/en-US/docs/Web/CSS/:out-of-range)|The `:out-of-range` CSS pseudo-class matches when an element has its<br />value attribute outside the specified range limitations for this element.<br />It allows the page to give a feedback that the value currently defined using the<br />element is outside the range limits. A value can be outside of a range if it is<br />either smaller or larger than maximum and minimum set values.
|[`:placeholder-shown`](https://developer.mozilla.org/en-US/docs/Web/CSS/:placeholder-shown)|**Experimental.** Applies to any form element currently displaying placeholder text.
|[`:read-only`](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-only)|Applies to any element which is not editable by the user.
|[`:read-write`](https://developer.mozilla.org/en-US/docs/Web/CSS/:read-write)|Applies to any element that is editable by a user, such as `<input>` elements.
|[`:right`](http://tympanus.net/codrops/css_reference/right_pseudo-class)|Used in conjunction with the `@page` rule, this selects all the right pages in a<br />printed document.
|[`:root`](https://developer.mozilla.org/en-US/docs/Web/CSS/:root)|matches the root element of a tree representing the document.
|[`:scope`](https://developer.mozilla.org/en-US/docs/Web/CSS/:scope)|CSS pseudo-class matches the elements that are a reference<br />point for selectors to match against.
|[`:target`](https://developer.mozilla.org/en-US/docs/Web/CSS/:target)|Selects the current active #news element (clicked on a URL<br />containing that anchor name)
|[`:visited`](https://www.w3.org/TR/css3-selectors/#the-link-pseudo-classes-link-and-visited)|Applies to any links which have has been visited by the user.

> 
The `:visited` pseudoclass can't be used for most styling in a lot of modern browsers anymore because it's a security hole. See this [link](https://hacks.mozilla.org/2010/03/privacy-related-changes-coming-to-css-vistited/) for reference.




## Child Pseudo Class


> 
<p>"The :nth-child(an+b) CSS pseudo-class matches an element that has
an+b-1 siblings before it in the document tree, for a given positive
**or zero value** for n" - [MDN :nth-child](https://developer.mozilla.org/en-US/docs/Web/CSS/:nth-child)</p>


|pseudo-selector|1|2|3|4|5|6|7|8|9|10
|------
|`:first-child`|✔|||||||||
|`:nth-child(3)`|||✔|||||||
|`:nth-child(n+3)`|||✔|✔|✔|✔|✔|✔|✔|✔
|`:nth-child(3n)`|||✔|||✔|||✔|
|`:nth-child(3n+1)`|✔|||✔|||✔|||✔
|`:nth-child(-n+3)`|✔|✔|✔|||||||
|`:nth-child(odd)`|✔||✔||✔||✔||✔|
|`:nth-child(even)`||✔||✔||✔||✔||✔
|`:last-child`||||||||||✔
|`:nth-last-child(3)`||||||||✔||



## Class Name Selectors


The class name selector select all elements with the targeted class name. For example, the class name `.warning` would select the following `<div>` element:

```css
<div class="warning">
    <p>This would be some warning copy.</p>
</div>

```

You can also combine class names to target elements more specifically. Let's build on the example above to showcase a more complicated class selection.

**CSS**

```css
.important {
    color: orange;
}
.warning {
    color: blue;
}
.warning.important {
    color: red;
}

```

**HTML**

```css
<div class="warning">
    <p>This would be some warning copy.</p>
</div>

<div class="important warning">
    <p class="important">This is some really important warning copy.</p>
</div>

```

In this example, all elements with the `.warning` class will have a blue text color, elements with the `.important` class with have an orange text color, and all elements that have **both** the `.important` and `.warning` class name will have a red text color.

Notice that within the CSS, the `.warning.important` declaration did not have any spaces between the two class names. This means it will only find elements which contain both class names `warning` and `important` in their `class` attribute. Those class names could be in any order on the element.

If a space was included between the two classes in the CSS declaration, it would only select elements that have parent elements with a `.warning` class names and child elements with `.important` class names.



## Select element using its ID without the high specificity of the ID selector


This trick helps you select an element using the ID as a value for an attribute selector to avoid the high specificity of the ID selector.

HTML:

```css
<div id="element">...</div>  

```

CSS

```css
#element { ... } /* High specificity will override many selectors */

[id="element"] { ... } /* Low specificity, can be overridden easily */

```



## Global boolean with checkbox:checked and ~ (general sibling combinator)


With the ~ selector, you can easily implement a global accessible boolean without using JavaScript.

### Add boolean as a checkbox

To the very beginning of your document, add as much booleans as you want with a unique `id` and the `hidden` attribute set:

```css
<input type="checkbox" id="sidebarShown" hidden />
<input type="checkbox" id="darkThemeUsed" hidden />

<!-- here begins actual content, for example: -->
<div id="container">
    <div id="sidebar">
        <!-- Menu, Search, ... -->
    </div>

    <!-- Some more content ... -->
</div>

<div id="footer">
    <!-- ... -->
</div>

```

### Change the boolean's value

You can toggle the boolean by adding a `label` with the `for` attribute set:

```css
<label for="sidebarShown">Show/Hide the sidebar!</label>

```

### Accessing boolean value with CSS

The normal selector (like `.color-red`) specifies the default properties. They can be overridden by following `true` / `false` selectors:

```css
/* true: */
<checkbox>:checked ~ [sibling of checkbox & parent of target] <target>

/* false: */
<checkbox>:not(:checked) ~ [sibling of checkbox & parent of target] <target>

```

Note that `<checkbox>`, `[sibling ...]` and `<target>` should be replaced by the proper selectors. `[sibling ...]` can be a specific selector, (often if you're lazy) simply `*` or nothing if the target is already a sibling of the checkbox.

Examples for the above HTML structure would be:

```css
#sidebarShown:checked ~ #container #sidebar {
    margin-left: 300px;
}

#darkThemeUsed:checked ~ #container,
#darkThemeUsed:checked ~ #footer {
    background: #333;
}

```

### In action

See [this fiddle](https://jsfiddle.net/yokosbm0/1/) for a implementation of these global booleans.



## CSS3 :in-range selector example


```css
<style>
input:in-range {
    border: 1px solid blue;
}
</style>



<input type="number" min="10" max="20" value="15">
<p>The border for this value will be blue</p>

```

The `:in-range` CSS pseudo-class matches when an element has its value attribute inside the specified range limitations for this element. It allows the page to give a feedback that the value currently defined using the element is inside the range limits.[[1]](https://developer.mozilla.org/en-US/docs/Web/CSS/:in-range)



## A. The :not pseudo-class example & B. :focus-within CSS pseudo-class


A. The syntax is presented above.

The following selector matches all `<input>` elements in an HTML document that are not disabled and don't have the class `.example`:

HTML:

```css
<form>
    Phone: <input type="tel" class="example">
    E-mail: <input type="email" disabled="disabled">
    Password: <input type="password">
</form>

```

CSS:

```css
input:not([disabled]):not(.example){
   background-color: #ccc;
}

```

The `:not()` pseudo-class will also support comma-separated selectors in Selectors Level 4:

CSS:

```css
input:not([disabled], .example){
   background-color: #ccc;
}

```

[Live Demo on JSBin](http://jsbin.com/japere/edit?html,css,output)

See background syntax [here](http://stackoverflow.com/documentation/css/296/backgrounds/1055/background-color#t=201608060832050225611).

B. The :focus-within CSS pseudo-class

HTML:

```

 <h3>Background is blue if the input is focused .</p>
  <div>
    <input type="text">
  </div>

```

CSS:

```css
div {
  height: 80px;
}
input{
  margin:30px;
}
div:focus-within {
  background-color: #1565C0;
}

```

[<img src="https://i.stack.imgur.com/S4ke4.png" alt="enter image description here" />](https://i.stack.imgur.com/S4ke4.png)
[<img src="https://i.stack.imgur.com/YGn3H.png" alt="enter image description here" />](https://i.stack.imgur.com/YGn3H.png)



## The :last-of-type selector


The `:last-of-type` selects the element that is the last child, of a particular type, of its parent. In the example below, the css selects the last paragraph and the last heading `h1`.

```css
p:last-of-type { 
  background: #C5CAE9; 
}
h1:last-of-type { 
  background: #CDDC39; 
}

```

```css
<div class="container">
    <p>First paragraph</p>
    <p>Second paragraph</p>
    <p>Last paragraph</p>
    <h1>Heading 1</h1>
    <h2>First heading 2</h2>
    <h2>Last heading 2</h2>
</div>

```

[<img src="http://i.stack.imgur.com/8RYda.png" alt="enter image description here" />](http://i.stack.imgur.com/8RYda.png)

[jsFiddle](http://jsfiddle.net/MadalinaTn/YmMZZ/113/)



## ID selectors


ID selectors select DOM elements with the targeted ID. To select an element by a specific ID in CSS, the `#` prefix is used.

For example, the following HTML `div` element…

```css
<div id="exampleID">
    <p>Example</p>
</div>

```

…can be selected by `#exampleID` in CSS as shown below:

```css
#exampleID {
    width: 20px;
}

```

> 
**Note**: The HTML specs do not allow multiple elements with the same ID




## How to style a Range input


HTML

```css
<input type="range"></input>

```

CSS

|Effect|Pseudo Selector
|------
|Thumb|`input[type=range]::-webkit-slider-thumb, input[type=range]::-moz-range-thumb, input[type=range]::-ms-thumb`
|Track|`input[type=range]::-webkit-slider-runnable-track, input[type=range]::-moz-range-track, input[type=range]::-ms-track`
|OnFocus|`input[type=range]:focus`
|Lower part of the track|`input[type=range]::-moz-range-progress, input[type=range]::-ms-fill-lower` (not possible in WebKit browsers currently - JS needed)



## The :only-child pseudo-class selector example


The `:only-child` `CSS` pseudo-class represents any element which is the only child of its parent.

HTML:

```css
<div>
  <p>This paragraph is the only child of the div, it will have the color blue</p>
</div>

<div>
  <p>This paragraph is one of the two children of the div</p>
  <p>This paragraph is one of the two children of its parent</p>
</div>

```

CSS:

```css
p:only-child {
  color: blue;
}

```

The above example selects the `<p>` element that is the unique child from its parent, in this case a `<div>`.

[Live Demo on JSBin](https://jsbin.com/dizosi/edit?html,css)



#### Syntax


- #**id**
- .**classname**
- :**pseudo-classname**
- ::**pseudo-elementname**
- [**attr**] /* has the **attr** attribute. */
- [**attr**="**value**"] /* has the **attr** attribute, and its value is exactly "**value**". */
- [**attr**~="**value**"] /* has the **attr** attribute, and its value, when split on **whitespace**, contains "**value**". */
- [**attr**|="**value**"] /* has the **attr** attribute, and its value is exactly "**value**", **or** its value begins with "**value****-**". */
- [**attr**^="**value**"] /* has the **attr** attribute, and its value begins with "**value**". */
- [**attr**$="**value**"] /* has the **attr** attribute, and its value ends with "**value**". */
- [**attr***="**value**"] /* has the **attr** attribute, and its value contains "**value**". */
- **element-name**
- *​



#### Remarks


- Sometimes you will see double colons (`::`) instead of just one (`:`).  This is a way to separate [pseudo-classes](https://www.w3.org/TR/selectors/#pseudo-classes) from [pseudo-elements](https://www.w3.org/TR/selectors/#pseudo-elements).
- Old browsers, like Internet Explorer 8, **only** support a single colon (`:`) for defining pseudo-elements.
- Unlike pseudo-classes, only one pseudo-element may be used per selector, if present it must appear after the sequence of simple selectors that represents the subjects of the selector (a future version of the [W3C specification](https://www.w3.org/TR/selectors/#pseudo-elements) may allow multiple pseudo-elements per selector).

