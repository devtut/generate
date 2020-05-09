---
metaTitle: "Swift - Generics"
description: "The Basics of Generics, Constraining Generic Placeholder Types, Generic Class Examples, Using Generics to Simplify Array Functions, Generic Class Inheritance, Use generics to enhance type-safety, Advanced Type Constraints"
---

# Generics



## The Basics of Generics


[Generics](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html#//apple_ref/doc/uid/TP40014097-CH26-ID179) are placeholders for types, allowing you to write flexible code that can be applied across multiple types. The advantage of using generics over [`Any`](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/TypeCasting.html#//apple_ref/doc/uid/TP40014097-CH22-ID342) is that they still allow the compiler to enforce strong type-safety.

A generic placeholder is defined within angle brackets `<>`.

### Generic Functions

For [functions](http://stackoverflow.com/documentation/swift/432/functions), this placeholder is placed after the function name:

```swift
/// Picks one of the inputs at random, and returns it
func pickRandom**&#60;T&#62;**(_ a:**T**, _ b:**T**) -> **T** {
    return arc4random_uniform(2) == 0 ? a : b
}
```

In this case, the generic placeholder is `T`. When you come to call the function, Swift can infer the type of `T` for you (as it simply acts as a placeholder for an actual type).

```swift
let randomOutput = pickRandom(5, 7) // returns an Int (that's either 5 or 7)

```

Here we’re passing two integers to the function. Therefore Swift is inferring `T == Int` – thus the function signature is inferred to be `(Int, Int) -> Int`.

Because of the strong type safety that generics offer – both the arguments and return of the function must be the **same** type. Therefore the following will not compile:

```swift
struct Foo {}

let foo = Foo()

let randomOutput = pickRandom(foo, 5) // error: cannot convert value of type 'Int' to expected argument type 'Foo'

```

### Generic Types

In order to use generics with [classes](http://stackoverflow.com/documentation/swift/459/classes), [structs](http://stackoverflow.com/documentation/swift/255/structs) or [enums](http://stackoverflow.com/documentation/swift/224/enums), you can define the generic placeholder after the type name.

```swift
class Bar**&#60;T&#62;** {
    var baz : **T**
    
    init(baz:**T**) {
        self.baz = baz
    }
}
```

This generic placeholder will require a type when you come to use the class `Bar`. In this case, it can be inferred from the initialiser `init(baz:T)`.

```swift
let bar = Bar(baz: "a string") // bar's type is Bar<String>

```

Here the generic placeholder `T` is inferred to be of type `String`, thus creating a `Bar<String>` instance. You can also specify the type explicitly:

```swift
let bar = Bar**&#60;String&#62;**(baz: "a string")
```

When used with a type, the given generic placeholder will keep its type for the entire lifetime of the given instance, and cannot be changed after initialisation. Therefore when you access the property `baz`, it will always be of type `String` for this given instance.

```swift
let str = bar.baz // of type String

```

### Passing Around Generic Types

When you come to pass around generic types, in most cases you have to be explicit about the generic placeholder type you expect. For example, as a function input:

```swift
func takeABarInt(bar:Bar**&#60;Int&#62;**) {
    ...
}
```

This function will only accept a `Bar<Int>`. Attempting to pass in a `Bar` instance where the generic placeholder type is not `Int` will result in a compiler error.

### Generic Placeholder Naming

Generic placeholder names are not just limited to single letters. If a given placeholder represents a meaningful concept, you should give it a descriptive name. For example, Swift’s [`Array`](https://developer.apple.com/library/watchos/documentation/Swift/Reference/Swift_Array_Structure/index.html) has a generic placeholder called `Element`, which defines the element type of a given `Array` instance.

```swift
public struct Array**&#60;Element&#62;** : RandomAccessCollection, MutableCollection {
    ...
}
```



## Constraining Generic Placeholder Types


It is possible to force the type parameters of a generic class to [implement a protocol](http://stackoverflow.com/documentation/swift/241/protocols), for example, [`Equatable`](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_Equatable_Protocol/index.html)

```swift
class MyGenericClass**<Type: Equatable>**{
    
    var value: **Type**
    init(value: **Type**){
        self.value = value
    }
    
    func getValue() -> **Type**{
        return self.value
    }

    func valueEquals(anotherValue: **Type**) -> Bool{
        return self.value == anotherValue
    }
}
```

Whenever we create a new `MyGenericClass`, the type parameter has to implement the `Equatable` protocol (ensuring the type parameter can be compared to another variable of the same type using `==`)

```swift
let myFloatGeneric = MyGenericClass<**Double**>(value: 2.71828) // valid
let myStringGeneric = MyGenericClass<**String**>(value: "My String") // valid

// "Type [Int] does not conform to protocol 'Equatable'"
let myInvalidGeneric = MyGenericClass<**[Int]**>(value: [2]) 

let myIntGeneric = MyGenericClass<**Int**>(value: 72)
print(myIntGeneric.valueEquals(72)) // true
print(myIntGeneric.valueEquals(-274)) // false

// "Cannot convert value of type 'String' to expected argument type 'Int'"
print(myIntGeneric.valueEquals("My String"))

```



## Generic Class Examples


A generic class with the type parameter `Type`

```swift
class MyGenericClass**<Type>**{
 
    var value: **Type**
    init(value: **Type**){
        self.value = value
    }
 
    func getValue() -> **Type**{
        return self.value
    }
 
    func setValue(value: **Type**){
        self.value = value
    }
}
```

We can now create new objects using a type parameter

```swift
let myStringGeneric = MyGenericClass**<String>**(value: "My String Value")
let myIntGeneric = MyGenericClass**<Int>**(value: 42)
 
print(myStringGeneric.getValue()) // "My String Value"
print(myIntGeneric.getValue()) // 42
 
myStringGeneric.setValue("Another String")
myIntGeneric.setValue(1024)
 
print(myStringGeneric.getValue()) // "Another String"
print(myIntGeneric.getValue()) // 1024
```

Generics can also be created with multiple type parameters

```swift
class AnotherGenericClass**<TypeOne, TypeTwo, TypeThree>**{
 
    var value1: **TypeOne**
    var value2: **TypeTwo**
    var value3: **TypeThree**
    init(value1: **TypeOne**, value2: **TypeTwo**, value3: **TypeThree**){
        self.value1 = value1
        self.value2 = value2
        self.value3 = value3
    }
 
    func getValueOne() -> **TypeOne**{return self.value1}
    func getValueTwo() -> **TypeTwo**{return self.value2}
    func getValueThree() -> **TypeThree**{return self.value3}
}
```

And used in the same way

```swift
let myGeneric = AnotherGenericClass**<String, Int, Double>**(value1: "Value of pi", value2: 3, value3: 3.14159)
 
print(myGeneric.getValueOne() is String) // true
print(myGeneric.getValueTwo() is Int) // true
print(myGeneric.getValueThree() is Double) // true
print(myGeneric.getValueTwo() is String) // false
 
print(myGeneric.getValueOne()) // "Value of pi"
print(myGeneric.getValueTwo()) // 3
print(myGeneric.getValueThree()) // 3.14159
```



## Using Generics to Simplify Array Functions


A function that extends the functionality of the array by creating an object oriented remove function.

```swift
// Need to restrict the extension to elements that can be compared.
// The `Element` is the generics name defined by Array for its item types.
// This restriction also gives us access to `index(of:_)` which is also
// defined in an Array extension with `where Element: Equatable`.
public extension Array where Element: Equatable {
    /// Removes the given object from the array.
    mutating func remove(_ element: Element) {
        if let index = self.index(of: element ) {
            self.remove(at: index)
        } else {
            fatalError("Removal error, no such element:\"\(element)\" in array.\n")
        }
    }
}

```

**Usage**

```swift
var myArray = [1,2,3]
print(myArray)

// Prints [1,2,3]

```

Use the function to remove an element without need for an index. Just pass the object to remove.

```swift
myArray.remove(2)
print(myArray)

// Prints [1,3]

```



## Generic Class Inheritance


Generic classes can be inherited:

```swift
// Models
class MyFirstModel {
}

class MySecondModel: MyFirstModel {
}

// Generic classes
class MyFirstGenericClass<T: MyFirstModel> {
    
    func doSomethingWithModel(model: T) {
        // Do something here
    }
    
}

class MySecondGenericClass<T: MySecondModel>: MyFirstGenericClass<T> {
    
    override func doSomethingWithModel(model: T) {
        super.doSomethingWithModel(model)
        
        // Do more things here
    }
    
}

```



## Use generics to enhance type-safety


Let's take this example without using generics

```swift
protocol JSONDecodable {
    static func from(_ json: [String: Any]) -> Any?
}

```

The protocol declaration seems fine unless you actually use it.

```swift
let myTestObject = TestObject.from(myJson) as? TestObject

```

Why do you have to cast the result to `TestObject`? Because of the `Any` return type in the protocol declaration.

By using generics you can avoid this problem that can cause runtime errors (and we don't want to have them!)

```swift
protocol JSONDecodable {
    associatedtype Element 
    static func from(_ json: [String: Any]) -> Element?
}

struct TestObject: JSONDecodable {
    static func from(_ json: [String: Any]) -> TestObject? {
    }
}

let testObject = TestObject.from(myJson) // testObject is now automatically `TestObject?`

```



## Advanced Type Constraints


It's possible to specify several type constraints for generics using the `where` clause:

```swift
func doSomething<T where T: Comparable, T: Hashable>(first: T, second: T) {
    // Access hashable function
    guard first.hashValue == second.hashValue else {
        return
    }
    // Access comparable function
    if first == second {
        print("\(first) and \(second) are equal.")
    }
}

```

It's also valid to write the `where` clause after the argument list:

```swift
func doSomething<T>(first: T, second: T) where T: Comparable, T: Hashable {
    // Access hashable function
    guard first.hashValue == second.hashValue else {
        return
    }
    // Access comparable function
    if first == second {
        print("\(first) and \(second) are equal.")
    }
}

```

Extensions can be restricted to types that satisfy conditions. The function is only available to instances which satisfy the type conditions:

```swift
// "Element" is the generics type defined by "Array". For this example, we
// want to add a function that requires that "Element" can be compared, that
// is: it needs to adhere to the Equatable protocol.
public extension Array where Element: Equatable {
    /// Removes the given object from the array.
    mutating func remove(_ element: Element) {
        // We could also use "self.index(of: element)" here, as "index(of:_)"
        // is also defined in an extension with "where Element: Equatable".
        // For the sake of this example, explicitly make use of the Equatable.
        if let index = self.index(where: { $0 == element }) {
            self.remove(at: index)
        } else {
            fatalError("Removal error, no such element:\"\(element)\" in array.\n")
        }
    }
}

```



#### Remarks


> 
Generic code enables you to write flexible, reusable functions and types that can work with any type, subject to requirements that you define. You can write code that avoids duplication and expresses its intent in a clear, abstracted manner.
Generics are one of the most powerful features of Swift, and much of the Swift standard library is built with generic code. For example, Swift's `Array` and `Dictionary` types are both generic collections. You can create an array that holds `Int` values, or an array that holds `String` values, or indeed an array for any other type that can be created in Swift. Similarly, you can create a dictionary to store values of any specified type, and there are no limitations on what that type can be.
<sub>**Source:** [Apple's Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Generics.html)</sub>


