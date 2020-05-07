---
metaTitle: "Kotlin - Ranges"
description: "Integral Type Ranges, downTo() function, step() function, until function"
---

# Ranges


Range expressions are formed with rangeTo functions that have the operator form .. which is complemented by in and !in.
Range is defined for any comparable type, but for integral primitive types it has an optimized implementation



## Integral Type Ranges


Integral type ranges ( IntRange , LongRange , CharRange ) have an extra feature: they can be iterated over. The compiler takes
care of converting this analogously to Java's indexed for-loop, without extra overhead

```kotlin
for (i in 1..4) print(i) // prints "1234"
for (i in 4..1) print(i) // prints nothing

```



## downTo() function


if you want to iterate over numbers in reverse order? It's simple. You can use the downTo() function defined in the
standard library

```kotlin
for (i in 4 downTo 1) print(i) // prints "4321"

```



## step() function


Is it possible to iterate over numbers with arbitrary step, not equal to 1? Sure, the step() function will help you

```kotlin
for (i in 1..4 step 2) print(i) // prints "13"
for (i in 4 downTo 1 step 2) print(i) // prints "42"

```



## until function


To create a range which does not include its end element, you can use the until function:

```kotlin
for (i in 1 until 10) { // i in [1, 10), 10 is excluded
println(i)
}

```

