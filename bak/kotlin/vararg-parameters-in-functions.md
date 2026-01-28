---
metaTitle: "Kotlin - Vararg Parameters in Functions"
description: "Basics: Using the vararg keyword, Spread Operator: Passing arrays into vararg functions"
---

# Vararg Parameters in Functions



## Basics: Using the vararg keyword


Define the function using the `vararg` keyword.

```kotlin
fun printNumbers(vararg numbers: Int) {
    for (number in numbers) {
        println(number)
    }
}

```

Now you can pass as many parameters (of the correct type) into the function as you want.

```kotlin
printNumbers(0, 1)                // Prints "0" "1"
printNumbers(10, 20, 30, 500)     // Prints "10" "20" "30" "500"

```

> 
**Notes:** Vararg parameters **must** be the last parameter in the parameter list.




## Spread Operator: Passing arrays into vararg functions


Arrays can be passed into vararg functions using the **Spread Operator**, `*`.

Assuming the following function exists...

```kotlin
fun printNumbers(vararg numbers: Int) {
    for (number in numbers) {
        println(number)
    }
}

```

You can **pass an array** into the function like so...

```kotlin
val numbers = intArrayOf(1, 2, 3)
printNumbers(*numbers)

// This is the same as passing in (1, 2, 3)

```

The spread operator can also be used **in the middle** of the parameters...

```kotlin
val numbers = intArrayOf(1, 2, 3)
printNumbers(10, 20, *numbers, 30, 40)

// This is the same as passing in (10, 20, 1, 2, 3, 30, 40)

```



#### Syntax


- **Vararg Keyword**: `vararg` is used in a method declaration to indicate that a variable number of parameters will be accepted.
- **Spread Operator**: An asterisk (`*`) before an array that is used in function calls to "unfold" the contents into individual parameters.

