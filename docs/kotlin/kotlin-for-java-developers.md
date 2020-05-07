---
metaTitle: "Kotlin - Kotlin for Java Developers"
description: "Declaring Variables, Quick Facts, Equality & Identity, IF, TRY and others are expressions, not statements"
---

# Kotlin for Java Developers


Most people coming to Kotlin do have a programming background in Java.

This topic collects examples comparing Java to Kotlin, highlighting the most important differences and those gems Kotlin offers over Java.



## Declaring Variables


In Kotlin, variable declarations look a bit different than Java's:

```kotlin
val i : Int = 42

```


<li>
They start with either `val` or `var`, making the declaration `final` ("**val**ue") or **var**iable.
</li>
<li>
The type is noted after the name, separated by a `:`
</li>
<li>
Thanks to Kotlin's **type inference** the explicit type declaration can be obmitted if there is an assignment with a type the compiler is able to unambigously detect
</li>

|Java|Kotlin
|---|---|---|---|---|---|---|---|---|---
|`int i = 42;`|`var i = 42` (**or** `var i : Int = 42`)
|`final int i = 42;`|`val i = 42`



## Quick Facts


- Kotlin does not need **`;`** to end statements
- Kotlin is **null-safe**
- Kotlin is **100% Java interoperable**
- Kotlin has **no primitives** (but optimizes their object counterparts for the JVM, if possible)
- Kotlin classes have **properties, not fields**
- Kotlin offers **data classes** with auto-generated `equals`/`hashCode` methods and field accessors
- Kotlin only has runtime Exceptions, **no checked Exceptions**
- Kotlin has **no `new` keyword**. Creating objects is done just by calling the constructor like any other method.
- Kotlin supports (limited) **operator overloading**. For example, accessing a value of a map can be written like: `val a = someMap["key"]`
- Kotlin can not only be compiled to byte code for the JVM, but also into **Java Script**, enabling you to write both backend and frontend code in Kotlin
- Kotlin is **fully compatible with Java 6**, which is especially interesting in regards for support of (not so) old Android devices
- Kotlin is an **officially supported** language **for Android development**
- Kotlin's collections have built-in disctinction between **mutable and immutable collections**.
- Kotlin supports **Coroutines** (experimental)



## Equality & Identity


Kotlin uses `==` for equality (that is, calls `equals` internally) and `===` for referential identity.

|Java|Kotlin
|---|---|---|---|---|---|---|---|---|---
|`a.equals(b);`|`a == b`
|`a == b;`|`a === b`
|`a != b;`|`a !== b`

See: [https://kotlinlang.org/docs/reference/equality.html](https://kotlinlang.org/docs/reference/equality.html)



## IF, TRY and others are expressions, not statements


In Kotlin, `if`, `try` and others are expressions (so they do return a value) rather than (void) statements.

So, for example, Kotlin does not have Java's ternary **Elvis Operator**, but you can write something like this:

```kotlin
val i = if (someBoolean) 33 else 42

```

Even more unfamiliar, but equally expressive, is the `try` **expression**:

```kotlin
val i = try {
    Integer.parseInt(someString)
}
catch (ex : Exception)
{
    42
}

```

