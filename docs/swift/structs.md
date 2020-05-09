---
metaTitle: "Swift - Structs"
description: "Structs are value types, Basics of Structs, Mutating a Struct, Structs cannot inherit, Accessing members of struct"
---

# Structs




## Structs are value types


Unlike classes, which are passed by reference, structures are passed through copying:

```swift
first = "Hello"
second = first
first += " World!"
// first == "Hello World!"
// second == "Hello"

```

String is a structure, therefore it's copied on assignment.

Structures also cannot be compared using identity operator:

```swift
window0 === window1 // works because a window is a class instance
"hello" === "hello" // error: binary operator '===' cannot be applied to two 'String' operands

```

Every two structure instances are deemed identical if they compare equal.

Collectively, these traits that differentiate structures from classes are what makes structures value types.



## Basics of Structs


```swift
struct Repository {
    let identifier: Int
    let name: String
    var description: String?
}

```

This defines a `Repository` struct with three stored properties, an integer `identifier`, a string `name`, and an optional string `description`. The `identifier` and `name` are constants, as they've been declared using the `let`-keyword. Once set during initialization, they cannot be modified. The description is a variable. Modifying it updates the value of the structure.

Structure types automatically receive a memberwise initializer if they do not define any of their own custom initializers. The structure receives a memberwise initializer even if it has stored properties that do not have default values.

`Repository` contains three stored properties of which only `description` has a default value (`nil`). Further it defines no initializers of its own, so it receives a memberwise initializer for free:

```swift
let newRepository = Repository(identifier: 0, name: "New Repository", description: "Brand New Repository")

```



## Mutating a Struct


A method of a struct that change the value of the struct itself must be prefixed with the `mutating` keyword

```swift
struct Counter {
    private var value = 0
    
    mutating func next() {
        value += 1
    }
}

```

### When you can use mutating methods

The `mutating` methods are only available on struct values inside variables.

```swift
var counter = Counter()
counter.next()

```

### When you can NOT use mutating methods

On the other hand, `mutating` methods are NOT available on struct values inside constants

```swift
let counter = Counter()
counter.next()
//  error: cannot use mutating member on immutable value: 'counter' is a 'let' constant

```



## Structs cannot inherit


Unlike classes, structures cannot inherit:

```swift
class MyView: NSView { }  // works

struct MyInt: Int { } // error: inheritance from non-protocol type 'Int'

```

Structures, however, can adopt protocols:

```swift
struct Vector: Hashable { ... }  // works

```



## Accessing members of struct


In Swift, structures use a simple “dot syntax” to access their members.

For example:

```swift
struct DeliveryRange {
  var range: Double
  let center: Location
}
let storeLocation = Location(latitude: 44.9871,
                             longitude: -93.2758)
var pizzaRange = DeliveryRange(range: 200,
                               center: storeLocation)

```

You can access(print) the range like this:

```

   print(pizzaRange.range) // 200

```

You can even access members of members using dot syntax:

```

print(pizzaRange.center.latitude) // 44.9871

```

Similar to how you can read values with dot syntax, you can also assign them.

```swift
pizzaRange.range = 250

```

