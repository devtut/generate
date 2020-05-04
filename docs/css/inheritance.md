---
metaTitle: "Inheritance"
description: "Automatic inheritance, Enforced inheritance"
---

# Inheritance



## Automatic inheritance


Inheritance the a fundamental mechanism of CSS by which the computed values of some properties of an element are applied to its' children. This is particularly useful when you want to set a global style to your elements rather than having to set said properties to each and every element in your markup.

Common properties that are automatically inherited are: `font`, `color`, `text-align`, `line-height`.

Assume the following stylesheet:

```css
#myContainer {
  color: red;
  padding: 5px;
}

```

This will apply `color: red` not only to the `<div>` element but also to the `<h3>` and `<p>` elements. However, due to the nature of `padding` its value will ****not**** be inherited to those elements.

```css
<div id="myContainer">
  <h3>Some header</h3>
  <p>Some paragraph</p>
</div>

```



## Enforced inheritance


Some properties are not automatically inherited from an element down to its' children. This is because those properties are typically desired to be unique to the element (or selection of elements) to which the property is applied to. Common such properties are `margin`, `padding`, `background`, `display`, etc.

However, sometimes inheritance is desired anyway. To achieve this, we can apply the `inherit` value to the property that should be inherited. The `inherit` value can be appied to **any** CSS property and **any** HTML element.

Assume the following stylesheet:

```css
#myContainer {
  color: red;
  padding: 5px;
}
#myContainer p {
  padding: inherit;
}

```

This will apply `color: red` to both the `<h3>` and `<p>` elements due to the inheritance nature of the `color` property. However, the `<p>` element will also inherit the `padding` value from its' parent because this was specified.

```css
<div id="myContainer">
  <h3>Some header</h3>
  <p>Some paragraph</p>
</div>

```



#### Syntax


- **property:** inherit;

