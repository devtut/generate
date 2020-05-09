---
metaTitle: "Swift - Enums"
description: "Basic enumerations, Enums with associated values, Indirect payloads, Raw and Hash values, Initializers, Enumerations share many features with classes and structures, Nested Enumerations"
---

# Enums




## Basic enumerations


An [enum](https://developer.apple.com/library/prerelease/content/documentation/Swift/Conceptual/Swift_Programming_Language/Enumerations.html) provides a set of related values:

```swift
enum Direction {
    case up
    case down
    case left
    case right
}

enum Direction { case up, down, left, right }

```

Enum values can be used by their fully-qualified name, but you can omit the type name when it can be inferred:

```swift
let dir = Direction.up
let dir: Direction = Direction.up
let dir: Direction = .up

// func move(dir: Direction)...
move(Direction.up)
move(.up)

obj.dir = Direction.up
obj.dir = .up

```

The most fundamental way of comparing/extracting enum values is with a [`switch`](http://stackoverflow.com/documentation/swift/207/switch#t=201604251548528035104) statement:

```swift
switch dir {
case .up:
    // handle the up case
case .down:
    // handle the down case
case .left:
    // handle the left case
case .right:
    // handle the right case
}

```

Simple enums are automatically [`Hashable`](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_Hashable_Protocol/index.html#//apple_ref/swift/intf/s:Ps8Hashable), [`Equatable`](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_Equatable_Protocol/index.html#//apple_ref/swift/intf/s:Ps9Equatable) and have string conversions:

```swift
if dir == .down { ... }

let dirs: Set<Direction> = [.right, .left]

print(Direction.up)  // prints "up"
debugPrint(Direction.up)  // prints "Direction.up"

```



## Enums with associated values


Enum cases can contain one or more **payloads** (**associated values**):

```swift
enum Action {
    case jump
    case kick
    case move(distance: Float)  // The "move" case has an associated distance
}

```

The payload must be provided when instantiating the enum value:

```swift
performAction(.jump)
performAction(.kick)
performAction(.move(distance: 3.3))
performAction(.move(distance: 0.5))

```

The `switch` statement can extract the associated value:

```swift
switch action {
case .jump:
    ...
case .kick:
    ...
case .move(let distance):  // or case let .move(distance):
    print("Moving: \(distance)") 
}

```

A single case extraction can be done using `if case`:

```swift
if case .move(let distance) = action {
    print("Moving: \(distance)") 
}

```

The `guard case` syntax can be used for later use extraction:

```swift
guard case .move(let distance) = action else {
    print("Action is not move")
    return
}

```

Enums with associated values are not `Equatable` by default. Implementation of the `==` operator must be done manually:

```swift
extension Action: Equatable { }

func ==(lhs: Action, rhs: Action) -> Bool {
    switch lhs {
    case .jump: if case .jump = rhs { return true }
    case .kick: if case .kick = rhs { return true }
    case .move(let lhsDistance): if case .move (let rhsDistance) = rhs { return lhsDistance == rhsDistance }
    }
    return false
}

```



## Indirect payloads


Normally, enums can't be recursive (because they would require infinite storage):

```swift
enum Tree<T> {
    case leaf(T)
    case branch(Tree<T>, Tree<T>)  // error: recursive enum 'Tree<T>' is not marked 'indirect'
}

```

The **`indirect`** keyword makes the enum store its payload with a layer of indirection, rather than storing it inline. You can use this keyword on a single case:

```swift
enum Tree<T> {
    case leaf(T)
    indirect case branch(Tree<T>, Tree<T>)
}

let tree = Tree.branch(.leaf(1), .branch(.leaf(2), .leaf(3)))

```

`indirect` also works on the whole enum, making any case indirect when necessary:

```swift
indirect enum Tree<T> {
    case leaf(T)
    case branch(Tree<T>, Tree<T>)
}

```



## Raw and Hash values


Enums without payloads can have **raw values** of any literal type:

```swift
enum Rotation: Int {
    case up = 0
    case left = 90
    case upsideDown = 180
    case right = 270
}

```

Enums without any specific type do not expose the rawValue property

```swift
enum Rotation {
    case up
    case right
    case down
    case left
}

let foo = Rotation.up
foo.rawValue //error

```

Integer raw values are assumed to start at 0 and increase monotonically:

```swift
enum MetasyntacticVariable: Int {
    case foo  // rawValue is automatically 0
    case bar  // rawValue is automatically 1
    case baz = 7
    case quux  // rawValue is automatically 8
}

```

String raw values can be synthesized automatically:

```swift
enum MarsMoon: String {
    case phobos  // rawValue is automatically "phobos"
    case deimos  // rawValue is automatically "deimos"
}

```

A raw-valued enum automatically conforms to [RawRepresentable](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_RawRepresentable_Protocol/index.html). You can get an enum value's corresponding raw value with `.rawValue`:

```swift
func rotate(rotation: Rotation) {
    let degrees = rotation.rawValue
    ...
}

```

You can also create an enum **from** a raw value using `init?(rawValue:)`:

```swift
let rotation = Rotation(rawValue: 0)  // returns Rotation.Up
let otherRotation = Rotation(rawValue: 45)  // returns nil (there is no Rotation with rawValue 45)

if let moon = MarsMoon(rawValue: str) {
    print("Mars has a moon named \(str)")
} else {
    print("Mars doesn't have a moon named \(str)")
}

```

If you wish to get the hash value of a specific enum you can access its hashValue, The hash value will return the index of the enum starting from zero.

```swift
let quux = MetasyntacticVariable(rawValue: 8)// rawValue is 8
quux?.hashValue //hashValue is 3

```



## Initializers


Enums can have custom init methods that can be more useful than the default `init?(rawValue:)`. Enums can also store values as well. This can be useful for storing the values they where initialized with and retrieving that value later.

```swift
enum CompassDirection {
    case north(Int)
    case south(Int)
    case east(Int)
    case west(Int)

    init?(degrees: Int) {
        switch degrees {
        case 0...45:
            self = .north(degrees)
        case 46...135:
            self = .east(degrees)
        case 136...225:
            self = .south(degrees)
        case 226...315:
            self = .west(degrees)
        case 316...360:
            self = .north(degrees)
        default:
            return nil
        }
    }
    
    var value: Int = {
        switch self {
            case north(let degrees):
                return degrees
            case south(let degrees):
                return degrees
            case east(let degrees):
                return degrees
            case west(let degrees):
                return degrees
        }    
    }
}

```

Using that initializer we can do this:

```swift
var direction = CompassDirection(degrees: 0) // Returns CompassDirection.north
direction = CompassDirection(degrees: 90) // Returns CompassDirection.east
print(direction.value) //prints 90
direction = CompassDirection(degrees: 500) // Returns nil

```



## Enumerations share many features with classes and structures


Enums in Swift are much more powerful than some of their counterparts in other languages, such as [C](http://stackoverflow.com/documentation/c/1119/structs-unions-and-enums/6563/simple-enums#t=201607221735315605089). They share many features with [classes](http://stackoverflow.com/documentation/swift/459/classes) and [structs](http://stackoverflow.com/documentation/swift/255/structs), such as defining [initialisers](http://stackoverflow.com/documentation/swift/1778/initializers), [computed properties](http://stackoverflow.com/documentation/swift/536/variables-properties/1751/computed-properties#t=201607221720176485844), [instance methods](http://stackoverflow.com/documentation/swift/432/functions/4084/methods#t=201607221721587119854), [protocol conformances](http://stackoverflow.com/documentation/swift/241/protocols) and [extensions](http://stackoverflow.com/documentation/swift/324/extensions).

```swift
protocol ChangesDirection {
    mutating func changeDirection()
}

enum Direction {
    
    // enumeration cases
    case up, down, left, right
    
    // initialise the enum instance with a case
    // that's in the opposite direction to another
    init(oppositeTo otherDirection: Direction) {
        self = otherDirection.opposite
    }
    
    // computed property that returns the opposite direction
    var opposite: Direction {
        switch self {
        case .up:
            return .down
        case .down:
            return .up
        case .left:
            return .right
        case .right:
            return .left
        }
    }
}

// extension to Direction that adds conformance to the ChangesDirection protocol
extension Direction: ChangesDirection {
    mutating func changeDirection() {
        self = .left
    }
}

```

****

```swift
var dir = Direction(oppositeTo: .down) // Direction.up

dir.changeDirection() // Direction.left

let opposite = dir.opposite // Direction.right

```



## Nested Enumerations


You can nest enumerations one inside an other, this allows you to structure hierarchical enums to be more organized and clear.

```swift
enum Orchestra {
    enum Strings {
        case violin
        case viola
        case cello
        case doubleBasse
    }
    
    enum Keyboards {
        case piano
        case celesta
        case harp
    }
    
    enum Woodwinds {
        case flute
        case oboe
        case clarinet
        case bassoon
        case contrabassoon
    }
}

```

And you can use it like that:

```swift
let instrment1 = Orchestra.Strings.viola
let instrment2 = Orchestra.Keyboards.piano

```



#### Remarks


Like structs and unlike classes, enums are value types and are copied instead of referenced when passed around.

For more information about enums, see [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Enumerations.html).

