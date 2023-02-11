---
metaTitle: "JavaScript - Memory efficiency"
description: "Drawback of creating true private method"
---

# Memory efficiency



## Drawback of creating true private method


One drawback of creating private method in Javascript is memory-inefficient because a copy of the private method will be created every time a new instance is created. See this simple example.

```js
function contact(first, last) {
    this.firstName = first;
    this.lastName = last;
    this.mobile;

    // private method
    var formatPhoneNumber = function(number) {
        // format phone number based on input
    };

    // public method
    this.setMobileNumber = function(number) {
        this.mobile = formatPhoneNumber(number);
    };
}

```

When you create few instances, they all have a copy of `formatPhoneNumber` method

```js
var rob = new contact('Rob', 'Sanderson');
var don = new contact('Donald', 'Trump');
var andy = new contact('Andy', 'Whitehall');

```

Thus, would be great to avoid using private method only if it's necessary.

