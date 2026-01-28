---
metaTitle: "Kotlin - Arrays"
description: "Generic Arrays, Arrays of Primitives, Extensions, Iterate Array, Create an array, Create an array using a closure, Create an uninitialized array"
---

# Arrays



## Generic Arrays


Generic arrays in Kotlin are represented by `Array<T>`.

To create an empty array, use `emptyArray<T>()` factory function:

```kotlin
val empty = emptyArray<String>()

```

To create an array with given size and initial values, use the constructor:

```kotlin
var strings = Array<String>(size = 5, init = { index -> "Item #$index" })
print(Arrays.toString(a)) // prints "[Item #0, Item #1, Item #2, Item #3, Item #4]"
print(a.size) // prints 5

```

Arrays have `get(index: Int): T` and  `set(index: Int, value: T)` functions:

```kotlin
strings.set(2, "ChangedItem")
print(strings.get(2)) // prints "ChangedItem"

// You can use subscription as well:
strings[2] = "ChangedItem"
print(strings[2]) // prints "ChangedItem"

```



## Arrays of Primitives


These types **do not** inherit from `Array<T>` to avoid boxing, however, they have the same attributes and methods.

|Kotlin type|Factory function|JVM type
|---|---|---|---|---|---|---|---|---|---
|`BooleanArray`|`booleanArrayOf(true, false)`|`boolean[]`
|`ByteArray`|`byteArrayOf(1, 2, 3)`|`byte[]`
|`CharArray`|`charArrayOf('a', 'b', 'c')`|`char[]`
|`DoubleArray`|`doubleArrayOf(1.2, 5.0)`|`double[]`
|`FloatArray`|`floatArrayOf(1.2, 5.0)`|`float[]`
|`IntArray`|`intArrayOf(1, 2, 3)`|`int[]`
|`LongArray`|`longArrayOf(1, 2, 3)`|`long[]`
|`ShortArray`|`shortArrayOf(1, 2, 3)`|`short[]`



## Extensions


`average()` is defined for `Byte`, `Int`, `Long`, `Short`, `Double`, `Float` and always returns `Double`:

```kotlin
val doubles = doubleArrayOf(1.5, 3.0)
print(doubles.average()) // prints 2.25

val ints = intArrayOf(1, 4)
println(ints.average()) // prints 2.5

```

`component1()`, `component2()`, ... `component5()` return an item of the array

`getOrNull(index: Int)` returns null if index is out of bounds, otherwise an item of the array

`first()`, `last()`

`toHashSet()` returns a `HashSet<T>` of all elements

`sortedArray()`, `sortedArrayDescending()` creates and returns a new array with sorted elements of current

`sort()`, `sortDescending` sort the array in-place

`min()`, `max()`



## Iterate Array


You can print the array elements using the loop same as the Java enhanced loop, but you need to change keyword from `:` to `in`.

```kotlin
val asc = Array(5, { i -> (i * i).toString() })
for(s : String in asc){
    println(s);
}

```

You can also change data type in for loop.

```kotlin
val asc = Array(5, { i -> (i * i).toString() })
for(s in asc){
    println(s);
}

```



## Create an array


```kotlin
val a = arrayOf(1, 2, 3) // creates an Array<Int> of size 3 containing [1, 2, 3].

```



## Create an array using a closure


```kotlin
val a = Array(3) { i -> i * 2 } // creates an Array<Int> of size 3 containing [0, 2, 4]

```



## Create an uninitialized array


```kotlin
val a = arrayOfNulls<Int>(3) // creates an Array<Int?> of [null, null, null]

```

The returned array will always have a nullable type. Arrays of non-nullable items can't be created uninitialized.

