---
metaTitle: "JavsScript - Using javascript to get/set CSS custom variables"
description: "How to get and set CSS variable property values."
---

# Using javascript to get/set CSS custom variables




## How to get and set CSS variable property values.


To get a value use the .getPropertyValue() method

```js
element.style.getPropertyValue("--var")

```

To set a value use the .setProperty() method.

```js
element.style.setProperty("--var", "NEW_VALUE")

```

