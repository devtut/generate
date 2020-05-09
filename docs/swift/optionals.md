---
metaTitle: "Swift - Optionals"
description: "Types of Optionals, Unwrapping an Optional, Nil Coalescing Operator, Optional Chaining, Overview - Why Optionals?"
---

# Optionals


“ An optional value either contains a value or contains nil to indicate that a value is missing”

Excerpt From: Apple Inc. “The Swift Programming Language (Swift 3.1 Edition).” iBooks. [https://itun.es/us/k5SW7.l](https://itun.es/us/k5SW7.l)

Basic optional use cases include: for a constant (let), use of an optional within a loop (if-let), safely unwrapping an optional value within a method (guard-let), and as part of switch loops (case-let), defaulting to a value if nil, using the coalesce operator (??)



## Types of Optionals


Optionals are a generic enum type that acts as a wrapper. This wrapper allows a variable to have one of two states: the value of the user-defined type or `nil`, which represents the absence of a value.

This ability is particularly important in Swift because one of the stated design objectives of the language is to work well with Apple's frameworks. Many (most) of Apple's frameworks utilize `nil` due to its ease of use and significance to programming patterns and API design within Objective-C.

In Swift, for a variable to have a `nil` value, it must be an optional. Optionals can be created by appending either a `!` or a `?` to the variable type. For example, to make an `Int` optional, you could use

```swift
var numberOne: Int! = nil
var numberTwo: Int? = nil

```

`?` optionals must be explicitly unwrapped, and should be used if you aren't certain whether or not the variable will have a value when you access it. For example, when turning a string into an `Int`, the result is an optional `Int?`, because nil will be returned if the string is not a valid number

```swift
let str1 = "42"
let num1: Int? = Int(str1) // 42

let str2 = "Hello, World!"
let num2: Int? = Int(str2) // nil

```

`!` optionals are automatically unwrapped, and should **only** be used when you are **certain** that the variable will have a value when you access it. For example, a global `UIButton!` variable that is initialized in `viewDidLoad()`

```swift
//myButton will not be accessed until viewDidLoad is called,
//so a ! optional can be used here
var myButton: UIButton!

override func viewDidLoad(){
    self.myButton = UIButton(frame: self.view.frame)
    self.myButton.backgroundColor = UIColor.redColor()
    self.view.addSubview(self.myButton)
}

```



## Unwrapping an Optional


In order to access the value of an Optional, it needs to be unwrapped.

You can **conditionally unwrap** an Optional using optional binding and **force unwrap** an Optional using the `!` operator.

Conditionally unwrapping effectively asks "Does this variable have a value?" while force unwrapping says "This variable has a value!".

If you force unwrap a variable that is `nil`, your program will throw an **unexpectedly found nil while unwrapping an optional** exception and crash, so you need to consider carefully if using `!` is appropriate.

```swift
var text: String? = nil
var unwrapped: String = text! //crashes with "unexpectedly found nil while unwrapping an Optional value"

```

For safe unwrapping, you can use an `if-let` statement, which will not throw an exception or crash if the wrapped value is `nil`:

```swift
var number: Int?
if let unwrappedNumber = number {       // Has `number` been assigned a value?
    print("number: \(unwrappedNumber)") // Will not enter this line
} else {
    print("number was not assigned a value")
}

```

Or, [a guard statement](https://stackoverflow.com/documentation/swift/475/conditionals/1558/using-guard#t=20170526044611473436):

```swift
var number: Int?
guard let unwrappedNumber = number else {
    return
}
print("number: \(unwrappedNumber)")

```

Note that the scope of the `unwrappedNumber` variable is inside the `if-let` statement and outside of the `guard`  block.

You can chain unwrapping of many optionals, this is mainly useful in cases that your code requires more then variable to run correctly:

```swift
var firstName:String?
var lastName:String?

if let fn = firstName, let ln = lastName {
    print("\(fn) + \(ln)")//pay attention that the condition will be true only if both optionals are not nil.
}

```

Note that all the variables have to be unwrapped in order to pass successfully the test, otherwise you would have no way to determine which variables were unwrapped and which weren't.

You can chain conditional statements using your  optionals immediately after it is unwrapped. This means no nested if - else statements!

```swift
var firstName:String? = "Bob"
var myBool:Bool? = false

if let fn = firstName, fn == "Bob", let bool = myBool, !bool {
    print("firstName is bob and myBool was false!")
}

```



## Nil Coalescing Operator


You can use the [**nil coalescing operator**](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html#//apple_ref/doc/uid/TP40014097-CH6-ID72) to unwrap a value if it is non-nil, otherwise provide a different value:

```swift
func fallbackIfNil(str: String?) -> String {
    return str ?? "Fallback String"
}
print(fallbackIfNil("Hi")) // Prints "Hi"
print(fallbackIfNil(nil)) // Prints "Fallback String"

```

This operator is able to [short-circuit](https://en.wikipedia.org/wiki/Short-circuit_evaluation), meaning that if the left operand is non-nil, the right operand will not be evaluated:

```swift
func someExpensiveComputation() -> String { ... }

var foo : String? = "a string"
let str = foo ?? someExpensiveComputation()

```

In this example, as `foo` is non-nil, `someExpensiveComputation()` will not be called.

You can also chain multiple nil coalescing statements together:

```swift
var foo : String?
var bar : String?

let baz = foo ?? bar ?? "fallback string"

```

In this example `baz` will be assigned the unwrapped value of `foo` if it is non-nil, otherwise it will be assigned the unwrapped value of `bar` if it is non-nil, otherwise it will be assigned the fallback value.



## Optional Chaining


You can use [Optional Chaining](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/OptionalChaining.html#//apple_ref/doc/uid/TP40014097-CH21-ID245) in order to call a [method](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Methods.html#//apple_ref/doc/uid/TP40014097-CH15-ID234), access a [property](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Properties.html#//apple_ref/doc/uid/TP40014097-CH14-ID254) or [subscript](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Subscripts.html#//apple_ref/doc/uid/TP40014097-CH16-ID305) an optional. This is done by placing a `?` between the given optional variable and the given member (method, property or subscript).

```swift
struct Foo {
    func doSomething() {
        print("Hello World!")
    }
}

var foo : Foo? = Foo()

foo?.doSomething() // prints "Hello World!" as foo is non-nil

```

If `foo` contains a value, `doSomething()` will be called on it. If `foo` is `nil`, then nothing bad will happen – the code will simply fail silently and continue executing.

```swift
var foo : Foo? = nil

foo?.doSomething() // will not be called as foo is nil

```

<sup> (This is similar behaviour to sending messages to `nil` in Objective-C) </sup>

The reason that Optional Chaining is named as such is because ‘optionality’ will be propagated through the members you call/access. What this means is that the return values of any members used with optional chaining will be optional, regardless of whether they are typed as optional or not.

```swift
struct Foo {
    var bar : Int
    func doSomething() { ... }
}

let foo : Foo? = Foo(bar: 5)
print(foo?.bar) // Optional(5)

```

Here `foo?.bar` is returning an `Int?` even though `bar` is non-optional, as `foo` itself is optional.

As optionality is propagated, methods returning `Void` will return `Void?` when called with optional chaining. This can be useful in order to determine whether the method was called or not (and therefore if the optional has a value).

```swift
let foo : Foo? = Foo()

if foo?.doSomething() != nil {
    print("foo is non-nil, and doSomething() was called")
} else {
    print("foo is nil, therefore doSomething() wasn't called")
}

```

Here we’re comparing the `Void?` return value with `nil` in order to determine whether the method was called (and therefore whether `foo` is non-nil).



## Overview - Why Optionals?


Often when programming it is necessary to make some distinction between a variable that has a value and one that does not.  For reference types, such as C Pointers, a special value such as `null` can be used to indicate that the variable has no value.  For intrinsic types, such as an integer, it is more difficult.  A nominated value, such as -1 can be used, but this relies on interpretation of the value.  It also eliminates that "special" value from normal use.

To address this, Swift allows any variable to be declared as an optional.  This is indicated by the use of a ? or !  after the type (See [Types of optionals](http://stackoverflow.com/documentation/swift/247/optionals/889/types-of-optionals#t=201607220020034058369))

For example,

```swift
var possiblyInt: Int?

```

declares a variable that may or may not contain an integer value.

The special value `nil` indicates that no value is currently assigned to this variable.

```swift
possiblyInt = 5      // PossiblyInt is now 5
possiblyInt = nil    // PossiblyInt is now unassigned

```

`nil` can also be used to test for an assigned value:

```swift
if possiblyInt != nil {
    print("possiblyInt has the value \(possiblyInt!)")
}

```

Note the use of `!` in the print statement to [**unwrap**](http://stackoverflow.com/documentation/swift/247/optionals/913/unwrapping-an-optional#t=20160722002622952402) the optional value.

As an example of a common use of optionals, consider a function that returns an integer from a string containing digits;  It is possible that the string may contain non-digit characters, or may even be empty.

How can a function that returns a simple `Int` indicate failure?  It cannot do so by returning any specific value as this would preclude that value from being parsed from the string.

```swift
var someInt
someInt = parseInt("not an integer") // How would this function indicate failure?

```

In Swift, however, that function can simply return an **optional** Int.  Then failure is indicated by return value of `nil`.

```swift
var someInt?
someInt = parseInt("not an integer")  // This function returns nil if parsing fails
if someInt == nil {
    print("That isn't a valid integer")
}

```



#### Syntax


- var optionalName: optionalType? // declare an optional type, defaults to nil
- var optionalName: optionalType? = value // declare an optional with a value
- var optionalName: optionalType! // declare an implicitly unwrapped optional
- optional! // force unwrap an optional



#### Remarks


For more information about optionals, see [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-ID330).

