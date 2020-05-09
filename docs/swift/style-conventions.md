---
metaTitle: "Swift - Style Conventions"
description: "Fluent Usage, Clear Usage, Capitalization"
---

# Style Conventions




## Fluent Usage


### Using natural language

Example:

```swift
list.insert(element, at: index) 

```

instead of

```swift
list.insert(element, position: index)

```

### Naming Factory Methods

Example:

```swift
factory.makeObject()

```

### Naming Parameters in Initializers and Factory Methods

Example:

```swift
factory.makeObject(key: value)

```

Instead of:

```swift
factory.makeObject(havingProperty: value)

```

### Naming according to side effects
- Functions with side effects (mutating functions) should be named using verbs or nouns prefixed with `form-` .
<li>Functions without side effects (nonmutating functions) should be named using nouns or verbs with the suffix `-ing` or `-ed`.

```swift
print(value)
array.sort()                 // in place sorting
list.add(value)              // mutates list
set.formUnion(anotherSet)    // set is now the union of set and anotherSet

```

Nonmutating functions:

```swift
let sortedArray = array.sorted()     // out of place sorting
let union = set.union(anotherSet)    // union is now the union of set and another set

```

### Boolean functions or variables

Example:

```swift
set.isEmpty
line.intersects(anotherLine)

```

### Naming Protocols

- Protocols describing what something is should be named using nouns.
- Protocols describing capabilities should have `-able`, `-ible` or `-ing` as suffix.

```swift
Collection        // describes that something is a collection
ProgressReporting // describes that something has the capability of reporting progress
Equatable         // describes that something has the capability of being equal to something

```

### Types and Properties

Example:

```swift
let factory = ...
let list = [1, 2, 3, 4]

```



## Clear Usage


### Avoid Ambiguity

Example:

```swift
extension List {
    public mutating func remove(at position: Index) -> Element {
        // implementation
    }
}

```

The function call to this function will then look like this:

```swift
list.remove(at: 42)

```

This way, ambiguity is avoided. If the function call would be just `list.remove(42)` it would be unclear, if an Element equal to 42 would be removed or if the Element at Index 42 would be removed.

### Avoid Redundancy

A bad example would be:

```swift
extension List {
    public mutating func removeElement(element: Element) -> Element? {
        // implementation
    }
}

```

A call to the function may look like `list.removeElement(someObject)`. The variable `someObject` already indicates, that an Element is removed. It would be better for the function signature to look like this:

```swift
extension List {
    public mutating func remove(_ member: Element) -> Element? {
        // implementation
    }
}

```

The call to this function looks like this: `list.remove(someObject)`.

### Naming variables according to their role

### High coupling between Protocol Name and Variable Names

### Provide additional details when using weakly typed parameters

```swift
func addObserver(_ observer: NSObject, forKeyPath path: String)

```

to which a call would look like `object.addObserver(self, forKeyPath: path)

instead of

```swift
func add(_ observer: NSObject, for keyPath: String)

```

to which a call would look like `object.add(self, for: path)`



## Capitalization


### Types & Protocols

Type and protocol names should start with an uppercase letter.

Example:

```swift
protocol Collection {}
struct String {}
class UIView {}
struct Int {}
enum Color {}

```

### Everything else...

Variables, constants, functions and enumeration cases should start with a lowercase letter.

Example:

```swift
let greeting = "Hello"
let height = 42.0

enum Color {
    case red
    case green
    case blue
}

func print(_ string: String) {
    ...
}

```

### Camel Case:

All naming should use the appropriate camel case. Upper camel case for type/protocol names and lower camel case for everything else.

Upper Camel Case:

```swift
protocol IteratorType { ... }

```

Lower Camel Case:

```swift
let inputView = ...

```

### Abbreviations

Abbreviations should be avoided unless commonly used (e.g. URL, ID).
If an abbreviation is used, all letters should have the same case.

Example:

```swift
let userID: UserID = ...
let urlString: URLString = ...

```



#### Remarks


Swift has an official style guide: [Swift.org API Design Guidelines](https://swift.org/documentation/api-design-guidelines/). Another popular guide is [The Official raywenderlich.com Swift Style Guide.](https://github.com/raywenderlich/swift-style-guide)

