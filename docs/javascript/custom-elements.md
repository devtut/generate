---
metaTitle: "JavsScript - Custom Elements"
description: "Extending Native Elements, Registering New Elements"
---

# Custom Elements



## Extending Native Elements


It's possible to extent native elements, but their descendants don't get to have their own tag names. Instead, the `is` attribute is used to specify which subclass an element is supposed to use. For example, here's an extension of the `<img>` element which logs a message to the console when it's loaded.

```js
const prototype = Object.create(HTMLImageElement.prototype);
prototype.createdCallback = function() {
  this.addEventListener('load', event => {
      console.log("Image loaded successfully.");
  });
};

document.registerElement('ex-image', { extends: 'img', prototype: prototype });

```

```js
<img is="ex-image" src="http://cdn.sstatic.net/Sites/stackoverflow/img/apple-touch-icon.png" />

```



## Registering New Elements


Defines an `<initially-hidden>` custom element which hides its contents until a specified number of seconds have elapsed.

```js
const InitiallyHiddenElement = document.registerElement('initially-hidden', class extends HTMLElement {
  createdCallback() {
    this.revealTimeoutId = null;
  }

  attachedCallback() {
    const seconds = Number(this.getAttribute('for'));
    this.style.display = 'none';
    this.revealTimeoutId = setTimeout(() => {
      this.style.display = 'block';
    }, seconds * 1000);
  }

  detachedCallback() {
    if (this.revealTimeoutId) {
      clearTimeout(this.revealTimeoutId);
      this.revealTimeoutId = null;
    }
  }
});

```

```js
<initially-hidden for="2">Hello</initially-hidden>
<initially-hidden for="5">World</initially-hidden>

```



#### Syntax


- .prototype.createdCallback()
- .prototype.attachedCallback()
- .prototype.detachedCallback()
- .prototype.attributeChangedCallback(name, oldValue, newValue)
- document.registerElement(name, [options])



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|name|The name of the new custom element.
|options.extends|The name of the native element being extended, if any.
|options.prototype|The custom prototype to use for the custom element, if any.



#### Remarks


> 
**Note that the Custom Elements specification has not yet been standardized, and is subject to change. The documentation describes the version that's been shipped in Chrome stable at this time.**


Custom Elements is an HTML5 feature allowing developers to use JavaScript to define custom HTML tags that can be used in their pages, with associated styles and behaviours. They are often used with [shadow-dom](/questions/tagged/shadow-dom).

