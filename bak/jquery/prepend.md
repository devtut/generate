---
metaTitle: "jQuery - Prepend"
description: "Prepending an element to a container, Prepend method"
---

# Prepend



## Prepending an element to a container


**Solution 1:**

```

$('#parent').prepend($('#child')); 

```

**Solution 2:**

```

$('#child').prependTo($('#parent'));

```

Both solutions are prepending the element `#child` (adding at the beginning) to the element `#parent`.

Before:

```js
<div id="parent">
  <span>other content</span>
</div>
<div id="child">

</div>

```

After:

```js
<div id="parent">
  <div id="child">

  </div>
  <span>other content</span>
</div>

```



## Prepend method


[`prepend()`](http://api.jquery.com/prepend/) - Insert content, specified by the parameter, to the beginning of each element in the set of matched elements.

**1.** [`prepend( content [, content ] )`](http://api.jquery.com/prepend/)

```js
// with html string
jQuery('#parent').prepend('<span>child</span>');
// or you can use jQuery object
jQuery('#parent').prepend($('#child'));
// or you can use comma seperated multiple elements to prepend
jQuery('#parent').prepend($('#child1'),$('#child2'));

```

**2.** [`prepend(function)`](http://api.jquery.com/prepend/)

JQuery **`version: 1.4`**  onwards you can use callback function as the argument. Where you can  get arguments as index position of the element in the set and the old HTML value of the element. Within the function, `this` refers to the current element in the set.

```js
jQuery('#parent').prepend(function(i,oldHTML){      
     // return the value to be prepend
     return  '<span>child</span>';
});

```

