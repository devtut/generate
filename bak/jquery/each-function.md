---
metaTitle: "jQuery - Each function"
description: "jQuery each function, Basic use"
---

# Each function




## jQuery each function


HTML:

```js
<ul>
  <li>Mango</li>
  <li>Book</li>
</ul>

```

Script:

```js
$( "li" ).each(function( index ) {
  console.log( index + ": " + $( this ).text() );
});

```

A message is thus logged for each item in the list:

0: Mango

1: Book



## Basic use


```js
// array
var arr = [
   'one',
   'two',
   'three',
   'four'
];
$.each(arr, function (index, value) {
  console.log(value);
  
  // Will stop running after "three"
  return (value !== 'three');
});
// Outputs: one two three

```

