---
metaTitle: "Kotlin - Collections"
description: "Using list, Using map, Using set"
---

# Collections


Unlike many languages, Kotlin distinguishes between mutable and immutable collections (lists, sets, maps, etc). Precise control over exactly when collections can be edited is useful for eliminating bugs, and for designing good APIs.



## Using list


```kotlin
// Create a new read-only List<String>
val list = listOf("Item 1", "Item 2", "Item 3")
println(list) // prints "[Item 1, Item 2, Item 3]"

```



## Using map


```kotlin
// Create a new read-only Map<Integer, String>
val map = mapOf(Pair(1, "Item 1"), Pair(2, "Item 2"), Pair(3, "Item 3"))
println(map) // prints "{1=Item 1, 2=Item 2, 3=Item 3}"

```



## Using set


```kotlin
// Create a new read-only Set<String>
val set = setOf(1, 3, 5)
println(set) // prints "[1, 3, 5]"

```



#### Syntax


- listOf, mapOf and setOf returns read-only objects that you cannot add or remove items.
- If you want to add or remove items you have to use arrayListOf, hashMapOf, hashSetOf, linkedMapOf (LinkedHashMap), linkedSetOf (LinkedHashSet), mutableListOf (The Kotlin MultableList collection), mutableMapOf (The Kotlin MultableMap collection), mutableSetOf (The Kotlin MultableSet collection), sortedMapOf or sortedSetOf
- Each collection has methods like first(), last(), get() and lambda functions like filter, map, join, reduce and many others.

