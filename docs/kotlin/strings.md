---
metaTitle: "Kotlin - Strings"
description: "String Equality, String Literals, Elements of String, String Templates"
---

# Strings




## String Equality


In Kotlin strings are compared with `==` operator which chect for their structural equality.

```kotlin
val str1 = "Hello, World!"
val str2 = "Hello," + " World!"
println(str1 == str2) // Prints true

```

Referential equality is checked with `===` operator.

```kotlin
val str1 = """
    |Hello, World!
    """.trimMargin()

val str2 = """
    #Hello, World!
    """.trimMargin("#")

val str3 = str1

println(str1 == str2) // Prints true
println(str1 === str2) // Prints false
println(str1 === str3) // Prints true

```



## String Literals


Kotlin has two types of string literals:

- Escaped string
- Raw string

**Escaped string** handles special characters by escaping them. Escaping is done with a backslash. The following escape sequences are supported: `\t`, `\b`, `\n`, `\r`, `\'`, `\"`, `\\` and `\$`. To encode any other character, use the Unicode escape sequence syntax: `\uFF00`.

```kotlin
val s = "Hello, world!\n"

```

**Raw string** delimited by a triple quote `"""`, contains no escaping and can contain newlines and any other characters

```kotlin
val text = """
    for (c in "foo")
        print(c)
"""

```

Leading whitespace can be removed with [trimMargin()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/trim-margin.html) function.

```kotlin
val text = """
    |Tell me and I forget.
    |Teach me and I remember.
    |Involve me and I learn.
    |(Benjamin Franklin)
    """.trimMargin()

```

Default margin prefix is pipe character `|`, this can be set as a parameter to trimMargin; e.g. `trimMargin(">")`.



## Elements of String


Elements of String are characters that can be accessed by the indexing operation
`string[index]`.

```kotlin
val str = "Hello, World!"
println(str[1]) // Prints e

```

String elements can be iterated with a for-loop.

```kotlin
for (c in str) {
    println(c)
}

```



## String Templates


Both escaped strings and raw strings can contain template expressions. Template expression is a piece of code which is evaluated and its result is concatenated into string. It starts with a dollar sign `$` and consists of either a variable name:

```kotlin
val i = 10
val s = "i = $i" // evaluates to "i = 10"

```

Or an arbitrary expression in curly braces:

```kotlin
val s = "abc"
val str = "$s.length is ${s.length}" // evaluates to "abc.length is 3"

```

To include a literal dollar sign in a string, escape it using a backslash:

```kotlin
val str = "\$foo" // evaluates to "$foo"

```

The exception is raw strings, which do not support escaping. In raw strings you can use the following syntax to represent a dollar sign.

```kotlin
val price = """
${'$'}9.99
"""

```

