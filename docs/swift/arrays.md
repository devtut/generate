---
metaTitle: "Swift - Arrays"
description: "Basics of Arrays, Extracting values of a given type from an Array with flatMap(_:), Flattening the result of an Array transformation with flatMap(_:), Combining an Array's elements with reduce(_:combine:), Filtering out nil from an Array transformation with flatMap(_:), Lazily flattening a multidimensional Array with flatten(), Subscripting an Array with a Range, Removing element from an array without knowing it's index, Filtering an Array, Sorting an Array of Strings, Accessing indices safely, Useful Methods, Sorting an Array, Transforming the elements of an Array with map(_:), Finding the minimum or maximum element of an Array, Value Semantics, Accessing Array Values, Modifying values in an array, Grouping Array values, Comparing 2 Arrays with zip"
---

# Arrays




## Basics of Arrays


[`Array`](https://developer.apple.com/reference/swift/array) is an ordered collection type in the Swift standard library. It provides O(1) random access and dynamic reallocation. Array is a [generic type](http://stackoverflow.com/documentation/swift/774/generics#t=201608180520342275711), so the type of values it contains are known at compile time.

As `Array` is a [value type](http://stackoverflow.com/documentation/swift/255/structs/1744/structs-are-value-types#t=201607261710016297343), its mutability is defined by whether it is annotated as a `var` (mutable) or `let` (immutable).

The type `[Int]` (meaning: an array containing `Int`s) is [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for `Array<T>`.

Read more about arrays in [The Swift Programming Language](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID107).

### Empty arrays

The following three declarations are equivalent:

```swift
// A mutable array of Strings, initially empty.

var arrayOfStrings: [String] = []      // type annotation + array literal
var arrayOfStrings = [String]()        // invoking the [String] initializer
var arrayOfStrings = Array<String>()   // without syntactic sugar

```

### Array literals

An array literal is written with square brackets surrounding comma-separated elements:

```swift
// Create an immutable array of type [Int] containing 2, 4, and 7
let arrayOfInts = [2, 4, 7]

```

The compiler can usually infer the type of an array based on the elements in the literal, but explicit **type annotations** can override the default:

```swift
let arrayOfUInt8s: [UInt8] = [2, 4, 7]   // type annotation on the variable
let arrayOfUInt8s = [2, 4, 7] as [UInt8] // type annotation on the initializer expression
let arrayOfUInt8s = [2 as UInt8, 4, 7]   // explicit for one element, inferred for the others

```

### Arrays with repeated values

```swift
// An immutable array of type [String], containing ["Example", "Example", "Example"]
let arrayOfStrings = Array(repeating: "Example",count: 3)

```

### Creating arrays from other sequences

```swift
let dictionary = ["foo" : 4, "bar" : 6]

// An immutable array of type [(String, Int)], containing [("bar", 6), ("foo", 4)]
let arrayOfKeyValuePairs = Array(dictionary)

```

### Multi-dimensional arrays

In Swift, a multidimensional array is created by nesting arrays: a 2-dimensional array of `Int` is `[[Int]]` (or `Array<Array<Int>>`).

```swift
let array2x3 = [
    [1, 2, 3],
    [4, 5, 6]
]
// array2x3[0][1] is 2, and array2x3[1][2] is 6.

```

To create a multidimensional array of repeated values, use nested calls of the array initializer:

```swift
var array3x4x5 = Array(repeating: Array(repeating: Array(repeating: 0,count: 5),count: 4),count: 3)

```



## Extracting values of a given type from an Array with flatMap(_:)


The `things` Array contains values of `Any` type.

```swift
let things: [Any] = [1, "Hello", 2, true, false, "World", 3]

```

We can extract values of a given type and create a new Array of that specific type. Let's say we want to extract all the `Int(s)` and put them into an `Int` Array in a safe way.

```swift
let numbers = things.flatMap { $0 as? Int }

```

Now `numbers` is defined as `[Int]`. The `flatMap` function discard all `nil` elements and the result thus contains only the following values:

```swift
[1, 2, 3]

```



## Flattening the result of an Array transformation with flatMap(_:)


As well as being able to create an array by [filtering out `nil`](http://stackoverflow.com/documentation/swift/284/arrays/3585/extracting-values-of-a-given-type-from-an-array-with-flatmap) from the transformed elements of a sequence, there is also a version of [`flatMap(_:)`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func--flatmap-s_-sequencetype_-self-generator-element-throws-s) that expects the transformation [closure](http://stackoverflow.com/documentation/swift/262/closures) to return a sequence `S`.

```swift
extension SequenceType {
    public func flatMap<S : SequenceType>(transform: (Self.Generator.Element) throws -> S) rethrows -> [S.Generator.Element]
}

```

Each sequence from the transformation will be concatenated, resulting in an array containing the combined elements of each sequence – `[S.Generator.Element]`.

### Combining the characters in an array of strings

For example, we can use it to take an array of prime strings and combine their characters into a single array:

```swift
let primes = ["2", "3", "5", "7", "11", "13", "17", "19"]
let allCharacters = primes.flatMap { $0.characters }
// => "["2", "3", "5", "7", "1", "1", "1", "3", "1", "7", "1", "9"]"

```

Breaking the above example down:

1. `primes` is a `[String]` (As an array is a sequence, we can call `flatMap(_:)` on it).
1. The transformation closure takes in one of the elements of `primes`, a [`String`](http://swiftdoc.org/v2.2/type/String/) (`Array<String>.Generator.Element`).
1. The closure then returns a sequence of type [`String.CharacterView`](http://swiftdoc.org/v2.2/type/String.CharacterView/).
1. The result is then an array containing the combined elements of all the sequences from each of the transformation closure calls – `[String.CharacterView.Generator.Element]`.

### Flattening a multidimensional array

As `flatMap(_:)` will concatenate the sequences returned from the transformation closure calls, it can be used to flatten a multidimensional array – such as a 2D array into a 1D array, a 3D array into a 2D array etc.

This can simply be done by returning the given element `$0` (a nested array) in the closure:

```swift
// A 2D array of type [[Int]]
let array2D = [[1, 3], [4], [6, 8, 10], [11]]

// A 1D array of type [Int]
let flattenedArray = array2D.flatMap { $0 }

print(flattenedArray) // [1, 3, 4, 6, 8, 10, 11]

```



## Combining an Array's elements with reduce(_:combine:)


[`reduce(_:combine:)`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func--reduce_combine_) can be used in order to combine the elements of a sequence into a single value. It takes an initial value for the result, as well as a [closure](http://stackoverflow.com/documentation/swift/262/closures) to apply to each element – which will return the new accumulated value.

For example, we can use it to sum an array of numbers:

```swift
let numbers = [2, 5, 7, 8, 10, 4]

let sum = numbers.reduce(0) {accumulator, element in
    return accumulator + element
}

print(sum) // 36

```

We're passing `0` into the initial value, as that's the logical initial value for a summation. If we passed in a value of `N`, the resulting `sum` would be `N + 36`. The closure passed to `reduce` has two arguments. `accumulator` is the current accumulated value, which is assigned the value that the closure returns at each iteration. `element` is the current element in the iteration.

As in this example, we're passing an `(Int, Int) -> Int` closure to `reduce`, which is simply outputting the addition of the two inputs – we can actually pass in the `+` operator directly, as operators are functions in Swift:

```swift
let sum = numbers.reduce(0, combine: +)

```



## Filtering out nil from an Array transformation with flatMap(_:)


You can use [`flatMap(_:)`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func--flatmap-t_-self-generator-element-throws-t) in a similar manner to [`map(_:)`](http://stackoverflow.com/documentation/swift/284/arrays/3584/transforming-the-elements-of-an-array-with-map) in order to create an array by applying a transform to a sequence's elements.

```swift
extension SequenceType {
    public func flatMap<T>(@noescape transform: (Self.Generator.Element) throws -> T?) rethrows -> [T]
}

```

The difference with this version of `flatMap(_:)` is that it expects the transform [closure](http://stackoverflow.com/documentation/swift/262/closures) to return an [Optional](http://stackoverflow.com/documentation/swift/247/optionals) value `T?` for each of the elements. It will then safely unwrap each of these optional values, filtering out `nil` – resulting in an array of `[T]`.

For example, you can this in order to transform a `[String]` into a `[Int]` using [`Int`'s failable `String` initializer](http://swiftdoc.org/v2.2/type/Int/#init_radix_), filtering out any elements that cannot be converted:

```swift
let strings = ["1", "foo", "3", "4", "bar", "6"]

let numbersThatCanBeConverted = strings.flatMap { Int($0) }

print(numbersThatCanBeConverted) // [1, 3, 4, 6]

```

You can also use `flatMap(_:)`'s ability to filter out `nil` in order to simply convert an array of optionals into an array of non-optionals:

```swift
let optionalNumbers : [Int?] = [nil, 1, nil, 2, nil, 3]

let numbers = optionalNumbers.flatMap { $0 }

print(numbers) // [1, 2, 3]

```



## Lazily flattening a multidimensional Array with flatten()


We can use [`flatten()`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-sequencetype-flatten) in order to [lazily](https://en.wikipedia.org/wiki/Lazy_evaluation) reduce the nesting of a multi-dimensional sequence.

For example, lazy flattening a 2D array into a 1D array:

```swift
// A 2D array of type [[Int]]
let array2D = [[1, 3], [4], [6, 8, 10], [11]]

// A FlattenBidirectionalCollection<[[Int]]>
let lazilyFlattenedArray = array2D.flatten()

print(lazilyFlattenedArray.contains(4)) // true

```

In the above example, `flatten()` will return a [`FlattenBidirectionalCollection`](http://swiftdoc.org/v2.2/type/FlattenBidirectionalCollection/), which will lazily apply the flattening of the array. Therefore [`contains(_:)`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-equatable-contains_) will only require the first two nested arrays of `array2D` to be flattened – as it will short-circuit upon finding the desired element.



## Subscripting an Array with a Range


One can extract a series of consecutive elements from an Array using a Range.

```swift
let words = ["Hey", "Hello", "Bonjour", "Welcome", "Hi", "Hola"]
let range = 2...4
let slice = words[range] // ["Bonjour", "Welcome", "Hi"]

```

Subscripting an Array with a Range returns an `ArraySlice`. It's a subsequence of the Array.

In our example, we have an Array of Strings, so we get back `ArraySlice<String>`.

Although an ArraySlice conforms to `CollectionType` and can be used with `sort`, `filter`, etc, its purpose is not for long-term storage but for transient computations: it should be converted back into an Array as soon as you've finished working with it.

For this, use the `Array()` initializer:

```swift
let result = Array(slice)

```

To sum up in a simple example without intermediary steps:

```swift
let words = ["Hey", "Hello", "Bonjour", "Welcome", "Hi", "Hola"]
let selectedWords = Array(words[2...4]) // ["Bonjour", "Welcome", "Hi"]

```



## Removing element from an array without knowing it's index


Generally, if we want to remove an element from an array, we need to know it's index so that we can remove it easily using `remove(at:)` function.

But what if we don't know the index but we know the value of element to be removed!

So here is the simple extension to an array which will allow us to remove an element from array easily without knowing it's index:

### **Swift3**

```swift
extension Array where Element: Equatable {

    mutating func remove(_ element: Element) {
        _ = index(of: element).flatMap {
            self.remove(at: $0)
        }
    }
}

```

e.g.

```

   var array = ["abc", "lmn", "pqr", "stu", "xyz"]
    array.remove("lmn")
    print("\(array)")    //["abc", "pqr", "stu", "xyz"]
    
    array.remove("nonexistent")
    print("\(array)")    //["abc", "pqr", "stu", "xyz"]
    //if provided element value is not present, then it will do nothing!

```

Also if, by mistake, we did something like this: `array.remove(25)`
i.e. we provided value with different data type, compiler will throw an error mentioning-<br />
`cannot convert value to expected argument type`



## Filtering an Array


You can use the [`filter(_:)`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-filter_) method on [`SequenceType`](http://swiftdoc.org/v2.2/protocol/SequenceType) in order to create a new array containing the elements of the sequence that satisfy a given predicate, which can be provided as a [closure](http://stackoverflow.com/documentation/swift/262/closures).

For example, filtering even numbers from an `[Int]`:

```swift
let numbers = [22, 41, 23, 30]

let evenNumbers = numbers.filter { $0 % 2 == 0 }

print(evenNumbers)  // [22, 30]

```

Filtering a `[Person]`, where their age is less than 30:

```swift
struct Person {
    var age : Int
}

let people = [Person(age: 22), Person(age: 41), Person(age: 23), Person(age: 30)]

let peopleYoungerThan30 = people.filter { $0.age < 30 }

print(peopleYoungerThan30) // [Person(age: 22), Person(age: 23)]

```



## Sorting an Array of Strings


The most simple way is to use `sorted()`:

```swift
let words = ["Hello", "Bonjour", "Salute", "Ahola"]
let sortedWords = words.sorted()
print(sortedWords) // ["Ahola", "Bonjour", "Hello", "Salute"]

```

or `sort()`

```swift
var mutableWords = ["Hello", "Bonjour", "Salute", "Ahola"]
mutableWords.sort()
print(mutableWords) // ["Ahola", "Bonjour", "Hello", "Salute"]

```

You can pass a closure as an argument for sorting:

```swift
let words = ["Hello", "Bonjour", "Salute", "Ahola"]
let sortedWords = words.sorted(isOrderedBefore: { $0 > $1 })
print(sortedWords) // ["Salute", "Hello", "Bonjour", "Ahola"]

```

Alternative syntax with a trailing closure:

```swift
let words = ["Hello", "Bonjour", "Salute", "Ahola"]
let sortedWords = words.sorted() { $0 > $1 }
print(sortedWords) // ["Salute", "Hello", "Bonjour", "Ahola"]

```

But there will be unexpected results if the elements in the array are not consistent:

```swift
let words = ["Hello", "bonjour", "Salute", "ahola"]
let unexpected = words.sorted()
print(unexpected) // ["Hello", "Salute", "ahola", "bonjour"]

```

To address this issue, either sort on a lowercase version of the elements:

```swift
let words = ["Hello", "bonjour", "Salute", "ahola"]
let sortedWords = words.sorted { $0.lowercased() < $1.lowercased() }
print(sortedWords) // ["ahola", "bonjour", "Hello", "Salute"]

```

Or `import Foundation` and use NSString's comparison methods like `caseInsensitiveCompare`:

```swift
let words = ["Hello", "bonjour", "Salute", "ahola"]
let sortedWords = words.sorted { $0.caseInsensitiveCompare($1) == .orderedAscending }
print(sortedWords) // ["ahola", "bonjour", "Hello", "Salute"]

```

**Alternatively, use `localizedCaseInsensitiveCompare`, which can manage diacritics.**

To properly sort Strings by the **numeric** value they contain, use `compare` with the `.numeric` option:

```swift
let files = ["File-42.txt", "File-01.txt", "File-5.txt", "File-007.txt", "File-10.txt"]
let sortedFiles = files.sorted() { $0.compare($1, options: .numeric) == .orderedAscending }
print(sortedFiles) // ["File-01.txt", "File-5.txt", "File-007.txt", "File-10.txt", "File-42.txt"]

```



## Accessing indices safely


By adding the following extension to array indices can be accessed without knowing if the index is inside bounds.

```swift
extension Array {
    subscript (safe index: Int) -> Element? {
        return indices ~= index ? self[index] : nil
    }
}

```

example:

```swift
if let thirdValue = array[safe: 2] {
    print(thirdValue)
}

```



## Useful Methods


Determine whether an array is empty or return its size

```swift
var exampleArray = [1,2,3,4,5]
exampleArray.isEmpty //false
exampleArray.count //5

```

Reverse an Array
**Note: The result is not performed on the array the method is called on and must be put into its own variable.**

```swift
exampleArray = exampleArray.reverse()
//exampleArray = [9, 8, 7, 6, 5, 3, 2]

```



## Sorting an Array


```swift
var array = [3, 2, 1]

```

### Creating a new sorted array

As [`Array`](http://swiftdoc.org/v2.2/type/Array/) conforms to [`SequenceType`](http://swiftdoc.org/v2.2/protocol/SequenceType), we can generate a new array of the sorted elements using a built in sort method.

In Swift 2, this is done with the [`sort()`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-comparable-sort) method.

```swift
let sorted = array.sort()  // [1, 2, 3]

```

As of Swift 3, it has been re-named to [`sorted()`](http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element_-comparable-sorted).

```swift
let sorted = array.sorted()  // [1, 2, 3]

```

### Sorting an existing array in place

As `Array` conforms to [`MutableCollectionType`](http://swiftdoc.org/v2.2/protocol/MutableCollectionType/), we can sort its elements in place.

In Swift 2, this is done using the [`sortInPlace()`](http://swiftdoc.org/v2.2/protocol/MutableCollectionType/#func-index_-randomaccessindextype-sortinplace_) method.

```swift
array.sortInPlace() // [1, 2, 3]

```

As of Swift 3, it has been renamed to [`sort()`](http://swiftdoc.org/v3.0/protocol/MutableCollection/#func-self_-randomaccesscollection-sort_).

```swift
array.sort() // [1, 2, 3]

```

> 
Note: In order to use the above methods, the elements must conform to the [`Comparable`](https://developer.apple.com/library/ios/documentation/Swift/Reference/Swift_Comparable_Protocol/index.html#//apple_ref/swift/intf/s:Ps10Comparable) protocol.


### Sorting an array with a custom ordering

You may also sort an array using a [closure](http://stackoverflow.com/documentation/swift/262/closures) to define whether one element should be ordered before another – which isn't restricted to arrays where the elements must be `Comparable`. For example, it doesn't make sense for a `Landmark` to be `Comparable` – but you can still sort an array of landmarks by height or name.

```swift
struct Landmark {
    let name : String
    let metersTall : Int
}

var landmarks = [Landmark(name: "Empire State Building", metersTall: 443),
                 Landmark(name: "Eifell Tower", metersTall: 300),
                 Landmark(name: "The Shard", metersTall: 310)]

```

```swift
// sort landmarks by height (ascending)
landmarks.sortInPlace {$0.metersTall < $1.metersTall}

print(landmarks) // [Landmark(name: "Eifell Tower", metersTall: 300), Landmark(name: "The Shard", metersTall: 310), Landmark(name: "Empire State Building", metersTall: 443)]

// create new array of landmarks sorted by name
let alphabeticalLandmarks = landmarks.sort {$0.name < $1.name}

print(alphabeticalLandmarks) // [Landmark(name: "Eifell Tower", metersTall: 300), Landmark(name: "Empire State Building", metersTall: 443), Landmark(name: "The Shard", metersTall: 310)]

```

```swift
// sort landmarks by height (ascending)
landmarks.sort {$0.metersTall < $1.metersTall}

// create new array of landmarks sorted by name
let alphabeticalLandmarks = landmarks.sorted {$0.name < $1.name}

```

> 
Note: String comparison can yield unexpected results if the strings are inconsistent, see [Sorting an Array of Strings](http://stackoverflow.com/documentation/swift/284/arrays/10786/sorting-an-array-of-strings).




## Transforming the elements of an Array with map(_:)


As [`Array`](http://swiftdoc.org/v2.2/type/Array/) conforms to [`SequenceType`](http://swiftdoc.org/v2.2/protocol/SequenceType/), we can use [`map(_:)`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-map_) to transform an array of `A` into an array of `B` using a [closure](http://stackoverflow.com/documentation/swift/262/closures) of type `(A) throws -> B`.

For example, we could use it to transform an array of [`Int`](http://swiftdoc.org/v2.2/type/Int/)s into an array of [`String`](http://swiftdoc.org/v2.2/type/String/)s like so:

```swift
let numbers = [1, 2, 3, 4, 5]
let words = numbers.map { String($0) }
print(words) // ["1", "2", "3", "4", "5"]

```

`map(_:)` will iterate through the array, applying the given closure to each element. The result of that closure will be used to populate a new array with the transformed elements.

Since `String` has an initialiser that receives an `Int` we can also use this clearer syntax:

```swift
let words = numbers.map(String.init)

```

A `map(_:)` transform need not change the type of the array – for example, it could also be used to multiply an array of `Int`s by two:

```swift
let numbers = [1, 2, 3, 4, 5]
let numbersTimes2 = numbers.map {$0 * 2}
print(numbersTimes2) // [2, 4, 6, 8, 10]

```



## Finding the minimum or maximum element of an Array


You can use the [`minElement()`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-comparable-minelement) and [`maxElement()`](http://swiftdoc.org/v2.2/protocol/SequenceType/#func-generator-element_-comparable-maxelement) methods to find the minimum or maximum element in a given sequence. For example, with an array of numbers:

```swift
let numbers = [2, 6, 1, 25, 13, 7, 9]

let minimumNumber = numbers.minElement() // Optional(1)
let maximumNumber = numbers.maxElement() // Optional(25)

```

As of Swift 3, the methods have been renamed to [`min()`](http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element_-comparable-min) and [`max()`](http://swiftdoc.org/v3.0/protocol/Sequence/#func-iterator-element_-comparable-max) respectively:

```swift
let minimumNumber = numbers.min() // Optional(1)
let maximumNumber = numbers.max() // Optional(25)

```

The returned values from these methods are [Optional](http://stackoverflow.com/documentation/swift/247/optionals) to reflect the fact that the array could be empty – if it is, `nil` will be returned.

> 
Note: The above methods require the elements to conform to the [`Comparable`](http://swiftdoc.org/v2.2/protocol/Comparable/) protocol.


### Finding the minimum or maximum element with a custom ordering

You may also use the above methods with a custom [closure](http://stackoverflow.com/documentation/swift/262/closures), defining whether one element should be ordered before another, allowing you to find the minimum or maximum element in an array where the elements aren't necessarily `Comparable`.

For example, with an array of vectors:

```swift
struct Vector2 {
    let dx : Double
    let dy : Double
    
    var magnitude : Double {return sqrt(dx*dx+dy*dy)}
}

let vectors = [Vector2(dx: 3, dy: 2), Vector2(dx: 1, dy: 1), Vector2(dx: 2, dy: 2)]

```

```swift
// Vector2(dx: 1.0, dy: 1.0)
let lowestMagnitudeVec2 = vectors.minElement { $0.magnitude < $1.magnitude } 

// Vector2(dx: 3.0, dy: 2.0)
let highestMagnitudeVec2 = vectors.maxElement { $0.magnitude < $1.magnitude } 

```

```swift
let lowestMagnitudeVec2 = vectors.min { $0.magnitude < $1.magnitude }
let highestMagnitudeVec2 = vectors.max { $0.magnitude < $1.magnitude }

```



## Value Semantics


Copying an array will copy all of the items inside the original array.

Changing the new array **will not change** the original array.

```swift
var originalArray = ["Swift", "is", "great!"]
var newArray = originalArray
newArray[2] = "awesome!"
//originalArray = ["Swift", "is", "great!"]
//newArray = ["Swift", "is", "awesome!"]

```

Copied arrays will share the same space in memory as the original until they are changed. As a result of this there is a performance hit when the copied array is given its own space in memory as it is changed for the first time.



## Accessing Array Values


The following examples will use this array to demonstrate accessing values

```swift
var exampleArray:[Int] = [1,2,3,4,5]
//exampleArray = [1, 2, 3, 4, 5]

```

To access a value at a known index use the following syntax:

```swift
let exampleOne = exampleArray[2]
//exampleOne = 3

```

**Note:** The value at **index two is the third value** in the `Array`. `Array`s use a **zero based index** which means the first element in the `Array` is at index 0.

```swift
let value0 = exampleArray[0]
let value1 = exampleArray[1]
let value2 = exampleArray[2]
let value3 = exampleArray[3]
let value4 = exampleArray[4]
//value0 = 1
//value1 = 2 
//value2 = 3
//value3 = 4
//value4 = 5

```

Access a subset of an `Array` using filter:

```swift
var filteredArray = exampleArray.filter({ $0 < 4 })
//filteredArray = [1, 2, 3]

```

Filters can have complex conditions like filtering only even numbers:

```swift
var evenArray = exampleArray.filter({ $0 % 2 == 0 })
//evenArray = [2, 4]

```

It is also possible to return the index of a given value, returning `nil` if the value wasn't found.

```swift
exampleArray.indexOf(3) // Optional(2)

```

There are methods for the first, last, maximum or minimum value in an `Array`. These methods will return `nil` if the `Array` is empty.

```swift
exampleArray.first // Optional(1)
exampleArray.last // Optional(5)
exampleArray.maxElement() // Optional(5)
exampleArray.minElement() // Optional(1)

```



## Modifying values in an array


There are multiple ways to append values onto an array

```swift
var exampleArray = [1,2,3,4,5]
exampleArray.append(6)
//exampleArray = [1, 2, 3, 4, 5, 6]
var sixOnwards = [7,8,9,10]
exampleArray += sixOnwards
//exampleArray = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

```

and remove values from an array

```swift
exampleArray.removeAtIndex(3)
//exampleArray = [1, 2, 3, 5, 6, 7, 8, 9, 10]
exampleArray.removeLast()
//exampleArray = [1, 2, 3, 5, 6, 7, 8, 9]
exampleArray.removeFirst()
//exampleArray = [2, 3, 5, 6, 7, 8, 9]

```



## Grouping Array values


If we have a struct like this

```swift
struct Box {
    let name: String
    let thingsInside: Int
}

```

and an array of `Box(es)`

```swift
let boxes = [
    Box(name: "Box 0", thingsInside: 1),
    Box(name: "Box 1", thingsInside: 2),
    Box(name: "Box 2", thingsInside: 3),
    Box(name: "Box 3", thingsInside: 1),
    Box(name: "Box 4", thingsInside: 2),
    Box(name: "Box 5", thingsInside: 3),
    Box(name: "Box 6", thingsInside: 1)
]

```

we can group the boxes by the `thingsInside` property in order to get a `Dictionary` where the `key` is the number of things and the value is an array of boxes.

```swift
let grouped = boxes.reduce([Int:[Box]]()) { (res, box) -> [Int:[Box]] in
    var res = res
    res[box.thingsInside] = (res[box.thingsInside] ?? []) + [box]
    return res
}

```

Now grouped is a `[Int:[Box]]` and has the following content

```swift
[
    2: [Box(name: "Box 1", thingsInside: 2), Box(name: "Box 4", thingsInside: 2)], 
    3: [Box(name: "Box 2", thingsInside: 3), Box(name: "Box 5", thingsInside: 3)],
    1: [Box(name: "Box 0", thingsInside: 1), Box(name: "Box 3", thingsInside: 1), Box(name: "Box 6", thingsInside: 1)]
]

```



## Comparing 2 Arrays with zip


The `zip` function accepts 2 parameters of type `SequenceType` and returns a `Zip2Sequence` where each element contains a value from the first sequence and one from the second sequence.

Example

```swift
let nums = [1, 2, 3]
let animals = ["Dog", "Cat", "Tiger"]
let numsAndAnimals = zip(nums, animals)

```

nomsAndAnimals now contains the following values

|sequence1|sequence1
|---|---|---|---|---|---|---|---|---|---
|`1`|`"Dog"`
|`2`|`"Cat"`
|`3`|`"Tiger"`

This is useful when you want to perform some kind of comparation between the n-th element of each Array.

**Example**

Given 2 Arrays of `Int(s)`

```swift
let list0 = [0, 2, 4]
let list1 = [0, 4, 8]

```

you want to check whether each value into `list1` is the double of the related value in `list0`.

```swift
let list1HasDoubleOfList0 = !zip(list0, list1).filter { $0 != (2 * $1)}.isEmpty

```



#### Syntax


- Array<Element> // The type of an array with elements of type Element
- [Element] // Syntactic sugar for the type of an array with elements of type Element
- [element0, element1, element2, ... elementN] // An array literal
- [[Element]()](http://swiftdoc.org/v2.2/type/Array/#init) // Creates a new empty array of type [Element]
- [Array(count:repeatedValue:)](http://swiftdoc.org/v2.2/type/Array/#init-count_repeatedvalue_) // Creates an array of `count` elements, each initialized to `repeatedValue`
- [Array(_:)](http://swiftdoc.org/v2.2/type/Array/#init_) // Creates an array from an arbitrary sequence



#### Remarks


Arrays are an **ordered collection** of values. Values may repeat but **must** be of the same type.

