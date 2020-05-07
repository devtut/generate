---
metaTitle: "Kotlin - Extension Methods"
description: "Potential Pitfall: Extensions are Resolved Statically, Top-Level Extensions, Lazy extension property workaround, Sample extending Java 7+ Path class, Sample extending long to render a human readable string, Sample extending Java 8 Temporal classes to render an ISO formatted string, Using extension functions to improve readability, Extension functions to Companion Objects (appearance of Static functions), Extensions for easier reference View from code"
---

# Extension Methods




## Potential Pitfall: Extensions are Resolved Statically


The extension method to be called is determined at compile-time based on the reference-type of the variable being accessed. It doesn't matter what the variable's type is at runtime, the same extension method will always be called.

```kotlin
open class Super

class Sub : Super()

fun Super.myExtension() = "Defined for Super"

fun Sub.myExtension() = "Defined for Sub"

fun callMyExtension(myVar: Super) {
    println(myVar.myExtension())
}

callMyExtension(Sub())

```

The above example will print `"Defined for Super"`, because the declared type of the variable `myVar` is `Super`.



## Top-Level Extensions


Top-level extension methods are not contained within a class.

```kotlin
fun IntArray.addTo(dest: IntArray) {
    for (i in 0 .. size - 1) {
        dest[i] += this[i]
    }
}

```

Above an extension method is defined for the type `IntArray`. Note that the object for which the extension method is defined (called the **receiver**) is accessed using the keyword `this`.

This extension can be called like so:

```kotlin
val myArray = intArrayOf(1, 2, 3)
intArrayOf(4, 5, 6).addTo(myArray)

```



## Lazy extension property workaround


Assume you want to create an extension property that is expensive to compute. Thus you would like to cache the computation, by using the [lazy property delegate](https://kotlinlang.org/docs/reference/delegated-properties.html#lazy) and refer to current instance (`this`), but you cannot do it, as explained in the Kotlin issues [KT-9686](https://youtrack.jetbrains.com/issue/KT-9686) and [KT-13053](https://youtrack.jetbrains.com/issue/KT-13053). However, there is an official workaround [provided here](https://youtrack.jetbrains.com/issue/KT-13053#comment=27-1510399).

In the example, the extension property is `color`. It uses an explicit `colorCache` which can be used with `this` as no `lazy` is necessary:

```kotlin
class KColor(val value: Int)

private val colorCache = mutableMapOf<KColor, Color>()

val KColor.color: Color
    get() = colorCache.getOrPut(this) { Color(value, true) }

```



## Sample extending Java 7+ Path class


A common use case for extension methods is to improve an existing API.  Here are examples of adding `exist`, `notExists` and `deleteRecursively` to the Java 7+ `Path` class:

```kotlin
fun Path.exists(): Boolean = Files.exists(this)
fun Path.notExists(): Boolean = !this.exists()
fun Path.deleteRecursively(): Boolean = this.toFile().deleteRecursively()

```

Which can now be invoked in this example:

```kotlin
val dir = Paths.get(dirName)
if (dir.exists()) dir.deleteRecursively()

```



## Sample extending long to render a human readable string


Given any value of type `Int` or `Long` to render a human readable string:

```kotlin
fun Long.humanReadable(): String {
    if (this <= 0) return "0"
    val units = arrayOf("B", "KB", "MB", "GB", "TB", "EB")
    val digitGroups = (Math.log10(this.toDouble())/Math.log10(1024.0)).toInt();
    return DecimalFormat("#,##0.#").format(this/Math.pow(1024.0, digitGroups.toDouble())) + " " + units[digitGroups];
}

fun Int.humanReadable(): String {
    return this.toLong().humanReadable()
}

```

Then easily used as:

```kotlin
println(1999549L.humanReadable())
println(someInt.humanReadable())

```



## Sample extending Java 8 Temporal classes to render an ISO formatted string


With this declaration:

```kotlin
fun Temporal.toIsoString(): String = DateTimeFormatter.ISO_INSTANT.format(this)

```

You can now simply:

```kotlin
val dateAsString = someInstant.toIsoString()

```



## Using extension functions to improve readability


In Kotlin you could write code like:

```kotlin
val x: Path = Paths.get("dirName").apply { 
    if (Files.notExists(this)) throw IllegalStateException("The important file does not exist")
}

```

But the use of `apply` is not that clear as to your intent.  Sometimes it is clearer to create a similar extension function to in effect rename the action and make it more self-evident.  This should not be allowed to get out of hand, but for very common actions such as verification:

```kotlin
infix inline fun <T> T.verifiedBy(verifyWith: (T) -> Unit): T {
    verifyWith(this)
    return this
}

infix inline fun <T: Any> T.verifiedWith(verifyWith: T.() -> Unit): T {
    this.verifyWith()
    return this
}

```

You could now write the code as:

```kotlin
val x: Path = Paths.get("dirName") verifiedWith {
    if (Files.notExists(this)) throw IllegalStateException("The important file does not exist")
}

```

Which now let's people know what to expect within the lambda parameter.

Note that the type parameter `T` for `verifiedBy` is same as `T: Any?` meaning that even nullable types will be able to use that version of the extension.  Although `verifiedWith` requires non-nullable.



## Extension functions to Companion Objects (appearance of Static functions)


If you want to extend a class as-if you are a static function, for example for class `Something` add static looking function `fromString`, this can only work if the class has a [companion object](https://kotlinlang.org/docs/reference/object-declarations.html#companion-objects) and that the extension function has been declared upon the companion object:

```kotlin
class Something {
    companion object {}
}

class SomethingElse {
}

fun Something.Companion.fromString(s: String): Something = ... 

fun SomethingElse.fromString(s: String): SomethingElse = ... 

fun main(args: Array<String>) {
    Something.fromString("") //valid as extension function declared upon the
                             //companion object

    SomethingElse().fromString("") //valid, function invoked on instance not
                                   //statically

    SomethingElse.fromString("") //invalid
}

```



## Extensions for easier reference View from code


You can use extensions for reference View, no more boilerplate after you created the views.

Original Idea is by [Anko Library](https://github.com/Kotlin/anko)

### Extensions

```kotlin
inline fun <reified T : View> View.find(id: Int): T = findViewById(id) as T
inline fun <reified T : View> Activity.find(id: Int): T = findViewById(id) as T
inline fun <reified T : View> Fragment.find(id: Int): T = view?.findViewById(id) as T
inline fun <reified T : View> RecyclerView.ViewHolder.find(id: Int): T = itemView?.findViewById(id) as T

inline fun <reified T : View> View.findOptional(id: Int): T? = findViewById(id) as? T
inline fun <reified T : View> Activity.findOptional(id: Int): T? = findViewById(id) as? T
inline fun <reified T : View> Fragment.findOptional(id: Int): T? = view?.findViewById(id) as? T
inline fun <reified T : View> RecyclerView.ViewHolder.findOptional(id: Int): T? = itemView?.findViewById(id) as? T

```

### Usage

```kotlin
val yourButton by lazy { find<Button>(R.id.yourButtonId) }
val yourText by lazy { find<TextView>(R.id.yourTextId) }
val yourEdittextOptional by lazy { findOptional<EditText>(R.id.yourOptionEdittextId) }

```



#### Syntax


- fun TypeName.extensionName(params, ...) { /* body */ } // Declaration
- fun <T: Any> TypeNameWithGenerics<T>.extensionName(params, ...) { /* body */ } // Declaration with Generics
- myObj.extensionName(args, ...) // invocation



#### Remarks


Extensions are resolved **statically**. This means that the extension method to be used is determined by the reference-type of the variable you are accessing; it doesn't matter what the variable's type is at runtime, the same extension method will always be called. This is because **declaring an extension method doesn't actually add a member to the receiver type**.

