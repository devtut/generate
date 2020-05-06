---
metaTitle: "jQuery - Getting and setting width and height of an element"
description: "Getting and setting width and height (ignoring border), Getting and setting innerWidth and innerHeight (ignoring padding and border),  Getting and setting outerWidth and outerHeight (including padding and border)"
---

# Getting and setting width and height of an element



## Getting and setting width and height (ignoring border)


Get width and height:

```js
var width = $('#target-element').width();
var height = $('#target-element').height();

```

Set width and height:

```js
$('#target-element').width(50);
$('#target-element').height(100);

```



## Getting and setting innerWidth and innerHeight (ignoring padding and border)


Get width and height:

```js
var width = $('#target-element').innerWidth();
var height = $('#target-element').innerHeight();

```

Set width and height:

```js
$('#target-element').innerWidth(50);
$('#target-element').innerHeight(100);

```



##  Getting and setting outerWidth and outerHeight (including padding and border)


Get width and height (excluding margin):

```js
var width = $('#target-element').outerWidth();
var height = $('#target-element').outerHeight();

```

Get width and height (including margin):

```js
var width = $('#target-element').outerWidth(true);
var height = $('#target-element').outerHeight(true);

```

Set width and height:

```js
$('#target-element').outerWidth(50);
$('#target-element').outerHeight(100);

```

