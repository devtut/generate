---
metaTitle: "Swift - (Unsafe) Buffer Pointers"
description: "UnsafeMutablePointer, Practical Use-Case for Buffer Pointers"
---

# (Unsafe) Buffer Pointers


“A buffer pointer is used for low-level access to a region of memory. For example, you can use a buffer pointer for efficent processing and communication of data between apps and services.”

Excerpt From: Apple Inc. “Using Swift with Cocoa and Objective-C (Swift 3.1 Edition).” iBooks. [https://itun.es/us/utTW7.l](https://itun.es/us/utTW7.l)

You are responsible for handling the life cycle of any memory you work with through buffer pointers, to avoid leaks or undefined behavior.



## UnsafeMutablePointer


```swift
struct UnsafeMutablePointer<Pointee>

```

> 
A pointer for accessing and manipulating data of a specific type.


You use instances of the UnsafeMutablePointer type to access data of a specific type in memory. The type of data that a pointer can access is the pointer's Pointee type. UnsafeMutablePointer provides no automated memory management or alignment guarantees. You are responsible for handling the life cycle of any memory you work with through unsafe pointers to avoid leaks or undefined behavior.

Memory that you manually manage can be either untyped or bound to a specific type. You use the UnsafeMutablePointer type to access and manage memory that has been bound to a specific type. ([Source](http://swiftdoc.org/v3.1/type/UnsafeMutablePointer/))

```swift
import Foundation

let arr = [1,5,7,8]

let pointer = UnsafeMutablePointer<[Int]>.allocate(capacity: 4)
pointer.initialize(to: arr)

let x = pointer.pointee[3]

print(x)

pointer.deinitialize()
pointer.deallocate(capacity: 4)

class A {
  var x: String?
  
  convenience init (_ x: String) {
    self.init()
    self.x = x
  }
  
  func description() -> String {
    return x ?? ""
  }
}


let arr2 = [A("OK"), A("OK 2")]
let pointer2 = UnsafeMutablePointer<[A]>.allocate(capacity: 2)
pointer2.initialize(to: arr2)

pointer2.pointee
let y = pointer2.pointee[1]

print(y)

pointer2.deinitialize()
pointer2.deallocate(capacity: 2)

```

Converted to Swift 3.0 from original [source](https://gist.github.com/Ben-G/4bc238d243f56a8354d4)



## Practical Use-Case for Buffer Pointers


Deconstructing the use of an unsafe pointer in the Swift library method;

```swift
public init?(validatingUTF8 cString: UnsafePointer<CChar>)

```

Purpose:

Creates a new string by copying and validating the null-terminated UTF-8 data referenced by the given pointer.

This initializer does not try to repair ill-formed UTF-8 code unit sequences. If any are found, the result of the initializer is `nil`. The following example calls this initializer with pointers to the contents of two different `CChar` arrays---the first with well-formed UTF-8 code unit sequences and the second with an ill-formed sequence at
the end.

> 
<p>**Source**, **Apple Inc., Swift 3 header file**
(For header access: In Playground, Cmd+Click on the word Swift) in the line of code:</p>


> 
`import Swift`


```swift
let validUTF8: [CChar] = [67, 97, 102, -61, -87, 0]
     validUTF8.withUnsafeBufferPointer { ptr in
         let s = String(validatingUTF8: ptr.baseAddress!)
         print(s as Any)
     }
     // Prints "Optional(Café)"

     let invalidUTF8: [CChar] = [67, 97, 102, -61, 0]
     invalidUTF8.withUnsafeBufferPointer { ptr in
      let s = String(validatingUTF8: ptr.baseAddress!)
      print(s as Any)
     }
// Prints "nil"

```

> 
(Source, Apple Inc., Swift Header File Example)




#### Remarks


Closely aligned concepts **required** to complete one's understanding of (Unsafe) BufferPointers.

- MemoryLayout (**The memory layout of a type, describing its size, stride, and alignment**.)
- Unmanaged (**A type for propagating an unmanaged object reference**.)
- UnsafeBufferPointer (**A non-owning collection interface to a buffer of elements stored contiguously in memory**.)
- UnsafeBufferPointerIterator (**An iterator for the elements in the buffer referenced by an UnsafeBufferPointer or UnsafeMutableBufferPointer instance**.)
- UnsafeMutableBufferPointer (**A non-owning collection interface to a buffer of mutable elements stored contiguously in memory.**)
- UnsafeMutablePointer (**A pointer for accessing and manipulating data of a specific type.**)
- UnsafeMutableRawBufferPointer (**A mutable nonowning collection interface to the bytes in a region of memory.**)
- UnsafeMutableRawBufferPointer.Iterator (**An iterator over the bytes viewed by a raw buffer pointer.**)
- UnsafeMutableRawPointer (**A raw pointer for accessing and manipulating untyped data.**)
- UnsafePointer (**A pointer for accessing data of a specific type.**)
- UnsafeRawBufferPointer (**A nonowning collection interface to the bytes in a region of memory.**)
- UnsafeRawBufferPointer.Iterator (**An iterator over the bytes viewed by a raw buffer pointer.**)
- UnsafeRawPointer (**A raw pointer for accessing untyped data.**)

> 
(Source, [Swiftdoc.org](http://swiftdoc.org))


