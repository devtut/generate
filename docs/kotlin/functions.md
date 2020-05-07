---
metaTitle: "Kotlin - Functions"
description: "Function References, Basic Functions, Inline Functions, Lambda Functions, Functions Taking Other Functions, Operator functions, Shorthand Functions"
---

# Functions



## Function References


We can reference a function without actually calling it by prefixing the function's name with `::`. This can then be passed to a function which accepts some other function as a parameter.

```kotlin
fun addTwo(x: Int) = x + 2
listOf(1, 2, 3, 4).map(::addTwo) # => [3, 4, 5, 6]

```

Functions without a receiver will be converted to `(ParamTypeA, ParamTypeB, ...) -> ReturnType` where `ParamTypeA`, `ParamTypeB` ... are the type of the function parameters and `ReturnType1 is the type of function return value.

```kotlin
fun foo(p0: Foo0, p1: Foo1, p2: Foo2): Bar {
    //...
}
println(::foo::class.java.genericInterfaces[0]) 
// kotlin.jvm.functions.Function3<Foo0, Foo1, Foo2, Bar>
// Human readable type: (Foo0, Foo1, Foo2) -> Bar

```

Functions with a receiver (be it an extension function or a member function) has a different syntax. You have to add the type name of the receiver before the double colon:

```kotlin
class Foo
fun Foo.foo(p0: Foo0, p1: Foo1, p2: Foo2): Bar {
    //...
}
val ref = Foo::foo
println(ref::class.java.genericInterfaces[0]) 
// kotlin.jvm.functions.Function4<Foo, Foo0, Foo1, Foo2, Bar>
// Human readable type: (Foo, Foo0, Foo1, Foo2) -> Bar
// takes 4 parameters, with receiver as first and actual parameters following, in their order

// this function can't be called like an extension function, though
val ref = Foo::foo
Foo().ref(Foo0(), Foo1(), Foo2()) // compile error

class Bar {
    fun bar()
}
print(Bar::bar) // works on member functions, too.

```

However, when a function's receiver is an object, the receiver is omitted from parameter list, because these is and only is one instance of such type.

```kotlin
object Foo
fun Foo.foo(p0: Foo0, p1: Foo1, p2: Foo2): Bar {
    //...
}
val ref = Foo::foo
println(ref::class.java.genericInterfaces[0]) 
// kotlin.jvm.functions.Function3<Foo0, Foo1, Foo2, Bar>
// Human readable type: (Foo0, Foo1, Foo2) -> Bar
// takes 3 parameters, receiver not needed

object Bar {
    fun bar()
}
print(Bar::bar) // works on member functions, too.

```

Since kotlin 1.1,  function reference can also be **bounded** to a variable, which is then called a **bounded function reference**.

```kotlin
fun makeList(last: String?): List<String> {
    val list = mutableListOf("a", "b", "c")
    last?.let(list::add)
    return list
}

```

<sub>Note this example is given only to show how bounded function reference works. It's bad practice in all other senses.</sub>

There is a special case, though. An extension function declared as a member can't be referenced.

```kotlin
class Foo
class Bar {
    fun Foo.foo() {}
    val ref = Foo::foo // compile error
}

```



## Basic Functions


Functions are declared using the `fun` keyword, followed by a function name and any parameters. You can also specify the return type of a function, which defaults to `Unit`. The body of the function is enclosed in braces `{}`.  If the return type is other than `Unit`, the body must issue a return statement for every terminating branch within the body.

```kotlin
fun sayMyName(name: String): String {
    return "Your name is $name" 
} 

```

A shorthand version of the same:

```kotlin
fun sayMyName(name: String): String = "Your name is $name" 

```

And the type can be omitted since it can be inferred:

```kotlin
fun sayMyName(name: String) = "Your name is $name" 

```



## Inline Functions


Functions can be declared inline using the `inline` prefix, and in this case they act like macros in C - rather than being called, they are replaced by the function's body code at compile time. This can lead to performance benefits in some circumstances, mainly where lambdas are used as function parameters.

```kotlin
inline fun sayMyName(name: String) = "Your name is $name" 

```

One difference from C macros is that inline functions can't access the scope from which they're called:

```kotlin
inline fun sayMyName() = "Your name is $name"

fun main() {
    val name = "Foo"
    sayMyName() # => Unresolved reference: name
}

```



## Lambda Functions


Lambda functions are anonymous functions which are usually created during a function call to act as a function parameter. They are declared by surrounding expressions with {braces} - if arguments are needed, these are put before an arrow `->`.

```kotlin
{ name: String ->
    "Your name is $name" //This is returned
}

```

**The last statement inside a lambda function is automatically the return value.**

The type's are optional, if you put the lambda on a place where the compiler can infer the types.

Multiple arguments:

```kotlin
{ argumentOne:String, argumentTwo:String ->
    "$argumentOne - $argumentTwo"
}

```

If the lambda function only needs one argument, then the argument list can be omitted and the single argument be referred to using `it` instead.

```kotlin
{ "Your name is $it" }

```

If the only argument to a function is a lambda function, then parentheses can be completely omitted from the function call.

```kotlin
# These are identical
listOf(1, 2, 3, 4).map { it + 2 }
listOf(1, 2, 3, 4).map({ it + 2 })

```



## Functions Taking Other Functions


As seen in "Lambda Functions", functions can take other functions as a parameter. The "function type" which you'll need to declare functions which take other functions is as follows:

```kotlin
# Takes no parameters and returns anything
() -> Any?

# Takes a string and an integer and returns ReturnType
(arg1: String, arg2: Int) -> ReturnType

```

For example, you could use the vaguest type, `() -> Any?`, to declare a function which executes a lambda function twice:

```kotlin
fun twice(x: () -> Any?) {
    x(); x();
}

fun main() {
    twice {
        println("Foo")
    } # => Foo
      # => Foo
}

```



## Operator functions


Kotlin allows us to provide implementations for a predefined set of operators with fixed symbolic representation (like `+` or `*`) and fixed precedence. To implement an operator, we provide a member function or an extension function with a fixed name, for the corresponding type. Functions that overload operators need to be marked with the `operator` modifier:

```kotlin
data class IntListWrapper (val wrapped: List<Int>) {
    operator fun get(position: Int): Int = wrapped[position]
}

val a = IntListWrapper(listOf(1, 2, 3))
a[1] // == 2

```

More operator functions can be found in [here](https://kotlinlang.org/docs/reference/operator-overloading.html)



## Shorthand Functions


If a function contains just one expression, we can omit the brace brackets and use an equals instead, like a variable assignment. The result of the expression is returned automatically.

```kotlin
fun sayMyName(name: String): String = "Your name is $name" 

```



#### Syntax


- fun ****Name****(****Params****) = ...
- fun ****Name****(****Params****) {...}
- fun ****Name****(****Params****): ****Type**** {...}
- fun <****Type Argument****> ****Name****(****Params****): ****Type**** {...}
- inline fun ****Name****(****Params****): ****Type**** {...}
- { ****ArgName****: ****ArgType**** -> ... }
- { ****ArgName**** -> ... }
- { ****ArgNames**** -> ... }
- { (****ArgName****: ****ArgType****): ****Type**** -> ... }



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|Name|Name of the function
|Params|Values given to the function with a name and type: ****`Name`****`:`****`Type`****
|Type|Return type of the function
|Type Argument|Type parameter used in [generic programming](http://stackoverflow.com/documentation/kotlin/1147/generics) (not necessarily return type)
|ArgName|Name of value given to the function
|ArgType|Type specifier for ****ArgName****
|ArgNames|List of ArgName separated by commas

