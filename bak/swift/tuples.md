---
metaTitle: "Swift - Tuples"
description: "What are Tuples?, Decomposing into individual variables, Tuples as the Return Value of Functions, Using a typealias to name your tuple type, Swapping values, Tuples as Case in Switch"
---

# Tuples


A tuple type is a comma-separated list of types, enclosed in parentheses.

This list of types also can have name of the elements and use those names to refer to the values of the individual elements.

An element name consists of an identifier followed immediately by a colon (:).

Common use -

We can use a tuple type as the return type of a function to enable the function to return a single tuple containing multiple values



## What are Tuples?


Tuples group multiple values into a single compound value. The values within a tuple can be of any type and do not have to be of the same type as each other.

Tuples are created by grouping any amount of values:

```swift
let tuple = ("one", 2, "three")

// Values are read using index numbers starting at zero
print(tuple.0) // one
print(tuple.1) // 2
print(tuple.2) // three

```

Also individual values can be named when the tuple is defined:

```swift
let namedTuple = (first: 1, middle: "dos", last: 3)

// Values can be read with the named property
print(namedTuple.first)  // 1
print(namedTuple.middle) // dos

// And still with the index number
print(namedTuple.2)      // 3

```

They can also be named when being used as a variable and even have the ability to have optional values inside:

```swift
var numbers: (optionalFirst: Int?, middle: String, last: Int)?

//Later On
numbers = (nil, "dos", 3)

print(numbers.optionalFirst)// nil
print(numbers.middle)//"dos"
print(numbers.last)//3

```



## Decomposing into individual variables


Tuples can be decomposed into individual variables with the following syntax:

```swift
let myTuple = (name: "Some Name", age: 26)
let (first, second) = myTuple

print(first) // "Some Name"
print(second)  // 26

```

This syntax can be used regardless of if the tuple has unnamed properties:

```swift
let unnamedTuple = ("uno", "dos")
let (one, two) = unnamedTuple
print(one) // "uno"
print(two) // "dos"

```

Specific properties can be ignored by using underscore (`_`):

```swift
let longTuple = ("ichi", "ni", "san")
let (_, _, third) = longTuple
print(third) // "san"

```



## Tuples as the Return Value of Functions


Functions can return tuples:

```swift
func tupleReturner() -> (Int, String) {
    return (3, "Hello")
}

let myTuple = tupleReturner()
print(myTuple.0) // 3
print(myTuple.1) // "Hello"

```

If you assign parameter names, they can be used from the return value:

```swift
func tupleReturner() -> (anInteger: Int, aString: String) {
    return (3, "Hello")
}

let myTuple = tupleReturner()
print(myTuple.anInteger) // 3
print(myTuple.aString) // "Hello"

```



## Using a typealias to name your tuple type


Occasionally you may want to use the same tuple type in multiple places throughout your code. This can quickly get messy, especially if your tuple is complex:

```swift
// Define a circle tuple by its center point and radius
let unitCircle: (center: (x: CGFloat, y: CGFloat), radius: CGFloat) = ((0.0, 0.0), 1.0)

func doubleRadius(ofCircle circle: (center: (x: CGFloat, y: CGFloat), radius: CGFloat)) -> (center: (x: CGFloat, y: CGFloat), radius: CGFloat) {
    return (circle.center, circle.radius * 2.0)
}


```

If you use a certain tuple type in more than one place, you can use the [`typealias`](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Declarations.html#//apple_ref/doc/uid/TP40014097-CH34-ID361) keyword to name your tuple type.

```swift
// Define a circle tuple by its center point and radius
typealias Circle = (center: (x: CGFloat, y: CGFloat), radius: CGFloat)

let unitCircle: Circle = ((0.0, 0.0), 1)

func doubleRadius(ofCircle circle: Circle) -> Circle {
    // Aliased tuples also have access to value labels in the original tuple type.
    return (circle.center, circle.radius * 2.0)
}

```

If you find yourself doing this too often, however, you should consider using a [`struct`](http://stackoverflow.com/documentation/swift/255/structs) instead.



## Swapping values


Tuples are useful to swap values between 2 (or more) variables without using temporary variables.

### Example with 2 variables

Given 2 variables

```swift
var a = "Marty McFly"
var b = "Emmett Brown"

```

we can easily swap the values

```swift
(a, b) = (b, a)

```

Result:

```swift
print(a) // "Emmett Brown" 
print(b) // "Marty McFly"

```

### Example with 4 variables

```swift
var a = 0
var b = 1
var c = 2
var d = 3

(a, b, c, d) = (d, c, b, a)

print(a, b, c, d) // 3, 2, 1, 0

```



## Tuples as Case in Switch


Use tuples in a switch

```swift
let switchTuple = (firstCase: true, secondCase: false)
   
switch switchTuple {   
 case (true, false):
    // do something
 case (true, true):
    // do something
 case (false, true):
    // do something
 case (false, false):
    // do something
}

```

Or in combination with an Enum
For example with Size Classes:

```

   let switchTuple = (UIUserInterfaceSizeClass.Compact, UIUserInterfaceSizeClass.Regular)
    
    switch switchTuple {
    case (.Regular, .Compact):
        //statement
    case (.Regular, .Regular):
        //statement
    case (.Compact, .Regular):
        //statement
    case (.Compact, .Compact):
        //statement
    }

```



#### Remarks


Tuples are considered value types. More info on tuples could be found in the documentation:
[developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/TheBasics.html#//apple_ref/doc/uid/TP40014097-CH5-ID329)

