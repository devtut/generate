---
metaTitle: "JavsScript - Selection API"
description: "Get the text of the selection, Deselect everything that is selected, Select the contents of an element"
---

# Selection API



## Get the text of the selection


```js
let sel = document.getSelection();
let text = sel.toString();
console.log(text); // logs what the user selected

```

Alternatively, since the `toString` member function is called automatically by some functions when converting the object to a string, you don't always have to call it yourself.

```js
console.log(document.getSelection());

```



## Deselect everything that is selected


```js
let sel = document.getSelection();
sel.removeAllRanges();

```



## Select the contents of an element


```js
let sel = document.getSelection();

let myNode = document.getElementById('element-to-select');

let range = document.createRange();
range.selectNodeContents(myNode);

sel.addRange(range);

```

It may be necessary to first remove all the ranges of the previous selection, as most browsers don't support multiple ranges.



#### Syntax


- Selection sel = window.getSelection();
- Selection sel = document.getSelection(); // equivalent to the above
- Range range = document.createRange();
- range.setStart(startNode, startOffset);
- range.setEnd(endNode, endOffset);



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|startOffset|If the node is a Text node, it is the number of characters from the beginning of `startNode` to where the range begins. Otherwise, it is the number of child nodes between the beginning of `startNode` to where the range begins.
|endOffset|If the node is a Text node, it is the number of characters from the beginning of `startNode` to where the range ends. Otherwise, it is the number of child nodes between the beginning of `startNode` to where the range ends.



#### Remarks


The Selection API allows you to view and change the elements and text that are selected (highlighted) in the document.

It is implemented as a singleton `Selection` instance that applies to the document, and holds a collection of `Range` objects, each representing one contiguous selected area.

Practically speaking, no browser except Mozilla Firefox supports multiple ranges in selections, and this is not encouraged by the spec either. Additionally, most users are not familiar with the concept of multiple ranges. As such, a developer can usually only concern themselves with one range.

