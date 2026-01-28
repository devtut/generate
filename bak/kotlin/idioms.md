---
metaTitle: "Kotlin - Idioms"
description: "Serializable and serialVersionUid in Kotlin, Delegate to a class without providing it in the public constructor, Use let or also to simplify working with nullable objects, Filtering a list, Fluent methods in Kotlin, Use apply to initialize objects or to achieve method chaining, Creating DTOs (POJOs/POCOs)"
---

# Idioms



## Serializable and serialVersionUid in Kotlin


To create the `serialVersionUID` for a class in Kotlin you have a few options all involving adding a member to the companion object of the class.

**The most concise bytecode** comes from a `private const val` which will become a private static variable on the containing class, in this case `MySpecialCase`:

```kotlin
class MySpecialCase : Serializable {
    companion object {
        private const val serialVersionUID: Long = 123
    }
}

```

You can also use these forms, **each with a side effect of having getter/setter methods** which are not necessary for serialization...

```kotlin
class MySpecialCase : Serializable {
    companion object {
        private val serialVersionUID: Long = 123
    }
}

```

This creates the static field but also creates a getter as well `getSerialVersionUID` on the companion object which is unnecessary.

```kotlin
class MySpecialCase : Serializable {
    companion object {
        @JvmStatic private val serialVersionUID: Long = 123
    }
}  

```

This creates the static field but also creates a static getter as well `getSerialVersionUID` on the containing class `MySpecialCase` which is unnecessary.

But all work as a method of adding the `serialVersionUID` to a `Serializable` class.



## Delegate to a class without providing it in the public constructor


Assume you want to [delegate to a class](https://kotlinlang.org/docs/reference/delegation.html#class-delegation) but you do not want to provide the delegated-to class in the constructor parameter. Instead, you want to construct it privately, making the constructor caller unaware of it. At first this might seem impossible because class delegation allows to delegate only to constructor parameters. However, there is a way to do it, as given in [this answer](http://stackoverflow.com/a/37598292/986533):

```kotlin
class MyTable private constructor(table: Table<Int, Int, Int>) : Table<Int, Int, Int> by table {

    constructor() : this(TreeBasedTable.create()) // or a different type of table if desired

}

```

With this, you can just call the constructor of `MyTable` like that: `MyTable()`. The `Table<Int, Int, Int>` to which `MyTable` delegates will be created privately. Constructor caller knows nothing about it.

This example is based on [this SO question](http://stackoverflow.com/q/37593738/986533).



## Use let or also to simplify working with nullable objects


`let` in Kotlin creates a local binding from the object it was called upon.
Example:

```kotlin
val str = "foo"
str.let {
    println(it) // it
}  

```

This will print `"foo"` and will return `Unit`.

**The difference between `let` and `also` is that you can return any value from a `let` block. `also` in the other hand will always reutrn `Unit`.**

Now why this is useful, you ask? Because if you call a method which can return `null` and you want to run some code only when that return value is not `null` you can use `let` or `also` like this:

```kotlin
val str: String? = someFun()
str?.let {
    println(it)
}

```

This piece of code will only run the `let` block when `str` is not `null`. Note the `null` safety operator (`?`).



## Filtering a list


```kotlin
val list = listOf(1,2,3,4,5,6)

//filter out even numbers

val even = list.filter { it % 2 == 0 }

println(even) //returns [2,4]

```



## Fluent methods in Kotlin


Fluent methods in Kotlin can be the same as Java:

```kotlin
fun doSomething() {
   someOtherAction()
   return this
}

```

But you can also make them more functional by creating an extension function such as:

```kotlin
fun <T: Any> T.fluently(func: ()->Unit): T {
    func()
    return this
}

```

Which then allows more obviously fluent functions:

```kotlin
fun doSomething() {
   return fluently { someOtherAction() }
}

```



## Use apply to initialize objects or to achieve method chaining


The documentation of `apply` says the following:

> 
calls the specified function block with `this` value as its receiver and returns `this` value.


While the kdoc is not so helpful `apply` is indeed an useful function. In layman's terms `apply` establishes a scope in which `this` is bound to the object you called `apply` on. This enables you to spare some code when you need to call multiple methods on an object which you will then return later. Example:

```kotlin
File(dir).apply { mkdirs() }

```

This is the same as writing this:

```kotlin
fun makeDir(String path): File {
    val result = new File(path)
    result.mkdirs()
    return result
}

```



## Creating DTOs (POJOs/POCOs)


Data classes in kotlin are classes created to do nothing but hold data. Such classes are marked as `data`:

```kotlin
data class User(var firstname: String, var lastname: String, var age: Int)

```

The code above creates a `User` class with the following automatically generated:

- Getters and Setters for all properties (getters only for `val`s)
- `equals()`
- `hashcode()`
- `toString()`
- `copy()`
- `componentN()` (where `N` is the corresponding property in order of declaration)

Just as with a function, default values can also be specified:

```kotlin
data class User(var firstname: String = "Joe", var lastname: String = "Bloggs", var age: Int = 20)

```

More details can be found here [Data Classes](https://kotlinlang.org/docs/reference/data-classes.html).

