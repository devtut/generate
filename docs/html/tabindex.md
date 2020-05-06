---
metaTitle: "HTML - Tabindex"
description: "Add an element to the tabbing order, Remove an element from the tabbing order, Define a custom tabbing order (not recommended)"
---

# Tabindex



## Add an element to the tabbing order


```html
<div tabindex="0">Some button</div>

```

**Note**: Try to use a native HTML `button` or an `a` tag where appropriate.



## Remove an element from the tabbing order


```html
<button tabindex="-1">This button will not be reachable by tab</button>

```

The element will be removed from the tabbing order but will still be focusable.



## Define a custom tabbing order (not recommended)


```html
<div tabindex="2">Second</div>
<div tabindex="1">First</div>

```

Positive values will insert the element at the tabbing order position of its respective value. Elements without preference (i.e. `tabindex="0"` or native elements such as `button` and `a`) will be appended after those with preference.

Positive values are **not recommended** as they disrupt the expected behavior of tabbing and might confuse people who rely on screenreaders. Try to create a natural order by rearranging your DOM structure.



#### Parameters


|Value|Meaning
|---|---|---|---|---|---|---|---|---|---
|negative|element will be focusable, but it should not be reachable via sequential keyboard navigation
|0|element will be focusable and reachable through keyboard sequential navigation, but it's relative order is defined by the platform convention
|positive|element must be focusable and accessible via sequential keyboard navigation; it's relative order will be defined by the attribute value: the sequential follow the increasing number of the `tabindex`



#### Remarks


The maximum value for `tabindex` should not exceed 32767 as per W3C section 17.11.1
Unless specified default value is -1

An element with a value of 0, an invalid value, or no `tabindex` value should be placed after the elements with a positive index in the sequential order of keyboard navigation.

