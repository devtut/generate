---
metaTitle: "Swift - Performance"
description: "Allocation Performance"
---

# Performance



## Allocation Performance


In Swift, memory management is done for you automatically using Automatic Reference Counting. (See [Memory Management](http://stackoverflow.com/documentation/swift/745/memory-management)) Allocation is the process of reserving a spot in memory for an object, and in Swift understanding the performance of such requires some understanding of the **heap** and the **stack**. The heap is a memory location where most objects get placed, and you may think of it as a storage shed. The stack, on the other hand, is a call stack of functions that have led to the current execution. (Hence, a stack trace is a sort of printout of the functions on the call stack.)

Allocating and deallocating from the stack is a very efficient operation, however in comparison heap allocation is costly. When designing for performance, you should keep this in mind.

Classes:

```swift
class MyClass {

    let myProperty: String

}

```

Classes in Swift are reference types and therefore several things happen. First, the actual object will be allocated onto the heap. Then, any references to that object must be added to the stack. This makes classes a more expensive object for allocation.

Structs:

```swift
struct MyStruct {

    let myProperty: Int

}

```

Because structs are value types and therefore copied when passed around, they are allocated on the stack. This makes structs more efficient than classes, however, if you do need a notion of identity and/or reference semantics, a struct cannot provide you with those things.

### Warning about structs with Strings and properties that are classes

While structs are generally cheeper than  classes, you should be careful about structs with properties that are classes:

```swift
struct MyStruct {

    let myProperty: MyClass

}

```

Here, due to reference counting and other factors, the performance is now more similar to a class. Further, if more than one property in the struct is a class, the performance impact may be even more negative than if the struct were a class instead.

Also, while Strings are structs, they internally store their characters on the heap, so are more expensive than most structs.

