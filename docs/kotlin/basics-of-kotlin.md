---
metaTitle: "Kotlin - Basics of Kotlin"
description: "Basic examples"
---

# Basics of Kotlin


This topic covers the basics of Kotlin for beginners.



## Basic examples


1.The Unit return type declaration is optional for functions. The following codes are equivalent.

```

   fun printHello(name: String?): Unit {
        if (name != null)
            println("Hello ${name}")
    }
    
    fun printHello(name: String?) {
        ...
    }

```

2.Single-Expression functions:When a function returns a single expression, the curly braces can be omitted and the body is specified after  = symbol

```

 fun double(x: Int): Int = x * 2

```

Explicitly declaring the return type is optional when this can be inferred by the compiler

```

 fun double(x: Int) = x * 2

```

3.String interpolation: Using string values is easy.

```kotlin
In java:
    int num=10
    String s  = "i =" + i;

In Kotlin
    val num = 10
    val s = "i = $num"

```

4.In Kotlin, the type system distinguishes between references that can hold null (nullable references) and those that can not (non-null references). For example, a regular variable of type String can not hold null:

```kotlin
var a: String = "abc"
a = null // compilation error

```

To allow nulls, we can declare a variable as nullable string, written String?:

```kotlin
var b: String? = "abc"
b = null // ok

```

5.In Kotlin,== actually checks for equality of values.By convention, an expression like
a == b is translated to

```

  a?.equals(b) ?: (b === null)

```



#### Remarks


1. Kotlin file has an extension .kt.
1. All classes in Kotlin have a common superclass Any, that is a default super for a class with no supertypes declared(similar to Object in Java).
1. Variables can be declared as val(immutable- assign once) or var(mutables- value can be changed)
1. Semicolon is not needed at end of statement.
<li>If a function does not return any useful value, its return type is Unit.It is also optional.
6.Referential equality is checked by the === operation. a === b evaluates to true if and only if a and b point to the same object.</li>

