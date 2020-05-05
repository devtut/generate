---
metaTitle: "JavsScript - Events"
description: "Page, DOM and Browser loading"
---

# Events




## Page, DOM and Browser loading


This is an example to explain the variations of load events.

1. **onload event**

```js
<body onload="someFunction()">
<img src="image1" />
<img src="image2" />
</body>

<script>
    function someFunction() {
    console.log("Hi! I am loaded");
}
</script>

```

In this case, the message is logged once **all the contents of the page including the images and stylesheets(if any)** are completely loaded.

<li>
**DOMContentLoaded event**

```js
document.addEventListener("DOMContentLoaded", function(event) {
    console.log("Hello! I am loaded");
});

```


</li>

In the above code, the message is logged only after the DOM/document is loaded (**ie:once the DOM is constructed**).

<li>
**Self-invoking anonymous function**

```js
(function(){
    console.log("Hi I am an anonymous function! I am loaded");
})();

```


</li>

Here, the message gets logged as soon as the browser interprets the anonymous function. It means, this function can get executed even before the DOM is loaded.

