---
metaTitle: "AngularJS - Distinguishing Service vs Factory"
description: "Factory VS Service once-and-for-all"
---

# Distinguishing Service vs Factory



## Factory VS Service once-and-for-all


**By definition:**

Services are basically constructor functions. They use ‘this’ keyword.

Factories are simple functions hence return an object.

**Under the hood:**

Factories internally calls provider function.

Services internally calls Factory function.

**Debate:**

Factories can run code before we return our object literal.

But at the same time, Services can also be written to return an object literal and to run code before returning. Though that is contra productive as services are designed to act as constructor function.

In fact, constructor functions in JavaScript can return whatever they want.

**So which one is better?**

The constructor syntax of services is more close to class syntax of ES6. So migration will be easy.

**Summary**

So in summary, provider, factory, and service are all providers.

A factory is a special case of a provider when all you need in your provider is a $get() function. It allows you to write it with less code.

A service is a special case of a factory when you want to return an instance of a new object, with the same benefit of writing less code.

[<img src="http://i.stack.imgur.com/vQy9B.png" alt="enter image description here" />](http://i.stack.imgur.com/vQy9B.png)

