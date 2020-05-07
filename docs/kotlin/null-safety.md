---
metaTitle: "Kotlin - Null Safety"
description: "Smart casts, Assertion, Nullable and Non-Nullable types, Eliminate nulls from an Iterable and array, Null Coalescing / Elvis Operator, Safe call operator, Elvis Operator (?:)"
---

# Null Safety



## Smart casts


If the compiler can infer that an object can't be null at a certain point, you don't have to use the special operators anymore:

```kotlin
var string: String? = "Hello!"
print(string.length)  // Compile error
if(string != null) {
    // The compiler now knows that string can't be null
    print(string.length)  // It works now!
}

```

> 
**Note:** The compiler won't allow you to smart cast mutable variables that could potentially be modified between the null-check and the intended usage.
If a variable is accessible from outside the scope of the current block (because they are members of a non-local object, for example), you need to create a new, local reference which you can then smart cast and use.




## Assertion


`!!` suffixes ignore nullability and returns a non-null version of that type. `KotlinNullPointerException` will be thrown if the object is a `null`.

```kotlin
val message: String? = null
println(message!!) //KotlinNullPointerException thrown, app crashes

```



## Nullable and Non-Nullable types


Normal types, like `String`, are not nullable. To make them able to hold null values, you have to explicitly denote that by putting a `?` behind them: `String?`



## Eliminate nulls from an Iterable and array


Sometimes we need to change type from `Collection<T?>` to `Collections<T>`. In that case, `filterNotNull` is our solution.

```kotlin
val a: List<Int?> = listOf(1, 2, 3, null)
val b: List<Int> = a.filterNotNull()

```



## Null Coalescing / Elvis Operator


Sometimes it is desirable to evaluate a nullable expression in an if-else fashion. The elvis operator, `?:`, can be used in Kotlin for such a situation.

For instance:

```kotlin
val value: String = data?.first() ?: "Nothing here."

```

The expression above returns `"Nothing here"` if `data?.first()` or `data` itself yield a `null` value else the result of `data?.first()`.

It is also possible to throw exceptions using the same syntax to abort code execution.

```kotlin
val value: String = data?.second() 
    ?: throw IllegalArgumentException("Value can't be null!")

```

> 
Reminder: NullPointerExceptions can be thrown using the [assertion operator](http://stackoverflow.com/documentation/kotlin/2080/null-safety/12693/assertion) (e.g. `data!!.second()!!`)




## Safe call operator


To access functions and properties of nullable types, you have to use special operators.

The first one, `?.`, gives you the property or function you're trying to access, or it gives you null if the object is null:

```kotlin
apply</code></a> like this:</p>

```kotlin
obj?.apply { 
    foo()
    bar()
}

```

This will call `foo` and `bar` on `obj` (which is `this` in the `apply` block) only if `obj` is non-null, skipping the entire block otherwise.

To bring a nullable variable into scope as a non-nullable reference without making it the implicit receiver of function and property calls, you can use [`let`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/let.html) instead of `apply`:

```kotlin
nullable?.let { notnull ->
    notnull.foo()
    notnull.bar()
}

```

`notnull` could be named anything, or even left out and used through [the implicit lambda parameter `it`.](https://stackoverflow.com/documentation/kotlin/1280/functions/4199/lambda-functions)



## Elvis Operator (?:)


In Kotlin, we can declare variable which can hold `null reference`.
Suppose we have a nullable reference `a`, we can say "if `a` is not null, use it, otherwise use some non-null value `x`"

```kotlin
var a: String? = "Nullable String Value"

```

Now, `a` can be null. So when we need to access value of `a`, then we need to perform safety check, whether it contains value or not. We can perform this safety check by conventional `if...else` statement.

```kotlin
val b: Int = if (a != null) a.length else -1

```

But here comes advance operator `Elvis`(Operator Elvis : `?:`). Above `if...else` can be expressed with the Elvis operator as below:

```kotlin
val b = a?.length ?: -1

```

If the expression to the left of `?:` (here : `a?.length`) is not null, the elvis operator returns it, otherwise it returns the expression to the right (here: `-1`). Right-hand side expression is evaluated only if the left-hand side is null.

