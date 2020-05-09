---
metaTitle: "Swift - Booleans"
description: "Booleans and Inline Conditionals, What is Bool?, Negate a Bool with the prefix ! operator, Boolean Logical Operators"
---

# Booleans



## Booleans and Inline Conditionals


A clean way to handle booleans is using an inline conditional with the a ? b : c ternary operator, which is part of Swift's [Basic Operations](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/BasicOperators.html).

The inline conditional is made up of 3 components:

```swift
question ? answerIfTrue : answerIfFalse

```

where question is a boolean that is evaluated and answerIfTrue is the value returned if the question is true, and answerIfFalse is the value returned if the question is false.

The expression above is the same as:

```swift
if question {
    answerIfTrue
} else {
    answerIfFalse
}

```

With inline conditionals you return a value based on a boolean:

```swift
func isTurtle(_ value: Bool) {
    let color = value ? "green" : "red"
    print("The animal is \(color)")
}

isTurtle(true) // outputs 'The animal is green'
isTurtle(false) // outputs 'The animal is red'

```

You can also call methods based on a boolean value:

```swift
func actionDark() {
    print("Welcome to the dark side")
}

func actionJedi() {
    print("Welcome to the Jedi order")
}

func welcome(_ isJedi: Bool) {
    isJedi ? actionJedi() : actionDark()
}

welcome(true) // outputs 'Welcome to the Jedi order'
welcome(false) // outputs 'Welcome to the dark side'

```

Inline conditionals allow for clean one-liner boolean evaluations



## What is Bool?


[Bool](https://developer.apple.com/reference/swift/bool) is a [Boolean](https://en.wikipedia.org/wiki/Boolean_data_type) type with two possible values: `true` and `false`.

```swift
let aTrueBool = true
let aFalseBool = false

```

Bools are used in control-flow statements as conditions.
The [`if` statement](http://stackoverflow.com/documentation/swift/475/conditionals/1559/basic-conditionals-if-statements#t=201607182137080572886) uses a Boolean condition to determine which block of code to run:

```swift
func test(_ someBoolean: Bool) {
    if someBoolean {
        print("IT'S TRUE!")
    }
    else {
        print("IT'S FALSE!")
    }
}
test(aTrueBool)  // prints "IT'S TRUE!"

```



## Negate a Bool with the prefix ! operator


The [prefix `!` operator](https://developer.apple.com/reference/swift/1540994) returns the [**logical negation**](https://en.wikipedia.org/wiki/Negation) of its argument. That is, `!true` returns `false`, and `!false` returns `true`.

```swift
print(!true)  // prints "false"
print(!false) // prints "true"

func test(_ someBoolean: Bool) {
    if !someBoolean {
        print("someBoolean is false")
    }
}

```



## Boolean Logical Operators


The OR (||) operator returns true if one of its two operands evaluates to true, otherwise it returns false. For example, the following code evaluates to true because at least one of the expressions either side of the OR operator is true:

```swift
if (10 < 20) || (20 < 10) {
    print("Expression is true")
}

```

The AND (&&) operator returns true only if both operands evaluate to be true. The following example will return false because only one of the two operand expressions evaluates to true:

```swift
if (10 < 20) && (20 < 10) {
    print("Expression is true")
}

```

The XOR (^) operator returns true if one and only one of the two operands evaluates to true. For example, the following code will return true since only one operator evaluates to be true:

```swift
if (10 < 20) ^ (20 < 10) {
    print("Expression is true")
}

```

