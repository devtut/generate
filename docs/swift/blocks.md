---
metaTitle: "Swift - Blocks"
description: "Non-escaping closure, Escaping closure"
---

# Blocks


From Swift Documentarion

A closure is said to escape a function when the closure is passed as an argument to the function, but is called after the function returns. When you declare a function that takes a closure as one of its parameters, you can write @escaping before the parameter’s type to indicate that the closure is allowed to escape.



## Non-escaping closure


In Swift 1 and 2, closure parameters were escaping by default. If you knew your closure wouldn’t escape the function body, you could mark the parameter with the @noescape attribute.

In Swift 3, it’s the other way around: closure parameters are non-escaping by default. If you intend for it to escape the function, you have to mark it with the @escaping attribute.

```swift
class ClassOne {
  // @noescape is applied here as default
  func methodOne(completion: () -> Void) {
    // 
  }
}

class ClassTwo {
  let obj = ClassOne()
  var greeting = "Hello, World!"

  func methodTwo() {
    obj.methodOne() {
      // self.greeting is required
      print(greeting)
    }
  }
}

```



## Escaping closure


From Swift Documentarion

> 
@escaping


> 
<p>Apply this attribute to a parameter’s type in a method or function
declaration to indicate that the parameter’s value can be stored for
later execution. This means that the value is allowed to outlive the
lifetime of the call. Function type parameters with the escaping type
attribute require explicit use of self. for properties or methods.</p>


```swift
class ClassThree {

    var closure: (() -> ())?

    func doSomething(completion: @escaping () -> ()) {
        closure = finishBlock
    }
}

```

In the above example the completion block is saved to closure and will literally live beyond the function call. So complier will force to mark completion block as @escaping.

