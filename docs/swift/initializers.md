---
metaTitle: "Swift - Initializers"
description: "Setting default property values, Customizing initialization with paramaters, Convenience init, Throwable Initilizer"
---

# Initializers



## Setting default property values


You can use an initializer to set default property values:

```swift
struct Example {
    var upvotes: Int
    init() {
        upvotes = 42
    }
}
let myExample = Example() // call the initializer
print(myExample.upvotes) // prints: 42

```

Or, specify default property values as a part of the property's declaration:

```swift
struct Example {
    var upvotes = 42 // the type 'Int' is inferred here
}

```

Classes and structs **must** set all stored properties to an appropriate initial value by the time an instance is created. This example will not compile, because the initializer did not give an initial value for `downvotes`:

```swift
struct Example {
    var upvotes: Int
    var downvotes: Int
    init() {
         upvotes = 0
    } // error: Return from initializer without initializing all stored properties
}

```



## Customizing initialization with paramaters


```swift
struct MetricDistance {
    var distanceInMeters: Double

    init(fromCentimeters centimeters: Double) {
        distanceInMeters = centimeters / 100
    }
    init(fromKilometers kilos: Double) {
        distanceInMeters = kilos * 1000
    }
}

let myDistance = MetricDistance(fromCentimeters: 42)
// myDistance.distanceInMeters is 0.42
let myOtherDistance = MetricDistance(fromKilometers: 42)
// myOtherDistance.distanceInMeters is 42000

```

Note that you cannot omit the parameter labels:

```swift
let myBadDistance = MetricDistance(42) // error: argument labels do not match any available overloads

```

In order to allow omission of parameter labels, use an underscore `_` as the label:

```swift
struct MetricDistance {
    var distanceInMeters: Double
    init(_ meters: Double) {
        distanceInMeters = meters
    }
}
let myDistance = MetricDistance(42) // distanceInMeters = 42

```

If your argument labels share names with one or more properties, use `self` to explicitly set the property values:

```swift
struct Color {
    var red, green, blue: Double
    init(red: Double, green: Double, blue: Double) {
        self.red = red
        self.green = green
        self.blue = blue
    }
}

```



## Convenience init


Swift classes supports having multiple ways of being initialized.
Following Apple's specs this 3 rules must be respected:

<li>A designated initializer must call a designated initializer from its immediate superclass.
[<img src="https://i.stack.imgur.com/7ie2O.png" alt="For First Rule" />](https://i.stack.imgur.com/7ie2O.png)</li>
1. A convenience initializer must call another initializer from the same class.
<li>A convenience initializer must ultimately call a designated initializer.
[<img src="https://i.stack.imgur.com/y411s.png" alt="For Second and Third Rule" />](https://i.stack.imgur.com/y411s.png)</li>

```swift
class Foo {

    var someString: String
    var someValue: Int
    var someBool: Bool

    // Designated Initializer
    init(someString: String, someValue: Int, someBool: Bool)
    {
        self.someString = someString
        self.someValue = someValue
        self.someBool = someBool
    }

    // A convenience initializer must call another initializer from the same class.
    convenience init()
    {
        self.init(otherString: "")
    }
    
    // A convenience initializer must ultimately call a designated initializer.
convenience init(otherString: String)
    {
        self.init(someString: otherString, someValue:  0, someBool: false)
    }
}


class Baz: Foo
{
    var someFloat: Float
    
    // Designed initializer
    init(someFloat: Float)
    {
        self.someFloat = someFloat
        
        // A designated initializer must call a designated initializer from its immediate superclass.
        super.init(someString: "", someValue: 0, someBool: false)
    }
    
    // A convenience initializer must call another initializer from the same class.
    convenience init()
    {
        self.init(someFloat: 0)
    }
}

```

### Designated Initializer

```swift
let c = Foo(someString: "Some string", someValue: 10, someBool: true)

```

### Convenience init()

```swift
let a = Foo()

```

### Convenience init(otherString: String)

```swift
let b = Foo(otherString: "Some string")

```

### Designated Initializer (will call the superclass Designated Initializer)

```swift
let d = Baz(someFloat: 3)

```

### Convenience init()

```swift
let e = Baz()

```

**Image source: [The Swift Programming Languag](https://developer.apple.com/library/content/documentation/Swift/Conceptual/Swift_Programming_Language/Initialization.html)e**



## Throwable Initilizer


Using Error Handling to make Struct(or class) initializer as throwable initializer:

Example Error Handling enum:

```swift
enum ValidationError: Error {
    case invalid
}

```

You can use Error Handling enum to check the parameter for the Struct(or class)  meet expected requirement

```swift
struct User {
    let name: String

    init(name: String?) throws {

        guard let name = name else { 
           ValidationError.invalid
        }

        self.name = name
    }
}

```

Now, you can use throwable initializer by:

```swift
do {
   let user = try User(name: "Sample name")
            
   // success
}
catch ValidationError.invalid {
     // handle error
}

```

