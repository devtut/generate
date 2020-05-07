---
metaTitle: "Kotlin - Reflection"
description: "Referencing a class, Inter-operating with Java reflection, Referencing a function, Getting values of all properties of a class, Setting values of all properties of a class"
---

# Reflection


Reflection is a language's ability to inspect code at runtime instead of compile time.



## Referencing a class


To obtain a reference to a [`KClass`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.reflect/-k-class/index.html) object representing some class use double colons:

```kotlin
val c1 = String::class
val c2 = MyClass::class

```



## Inter-operating with Java reflection


To obtain a Java's [`Class`](https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html) object from Kotlin's [`KClass`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.reflect/-k-class/index.html) use the `.java` extension property:

```kotlin
val stringKClass: KClass<String> = String::class
val c1: Class<String> = stringKClass.java

val c2: Class<MyClass> = MyClass::class.java

```

The latter example will be optimized by the compiler to not allocate an intermediate `KClass` instance.



## Referencing a function


Functions are first-class citizens in Kotlin. You can obtain a reference on it using double colons and then pass it to another function:

```kotlin
fun isPositive(x: Int) = x > 0

val numbers = listOf(-2, -1, 0, 1, 2)
println(numbers.filter(::isPositive)) // [1, 2]

```



## Getting values of all properties of a class


Given `Example` class extending `BaseExample` class with some properties:

```kotlin
open class BaseExample(val baseField: String)

class Example(val field1: String, val field2: Int, baseField: String): 
    BaseExample(baseField) {
    
    val field3: String
        get() = "Property without backing field"

    val field4 by lazy { "Delegated value" }

    private val privateField: String = "Private value"
}

```

One can get hold of all properties of a class:

```kotlin
val example = Example(field1 = "abc", field2 = 1, baseField = "someText")

example::class.memberProperties.forEach { member ->
    println("${member.name} -> ${member.get(example)}")
}

```

Running this code will cause an exception to be thrown. Property `private val privateField` is declared private and calling `member.get(example)` on it will not succeed. One way to handle this it to filter out private properties. To do that we have to check the visibility modifier of a property's Java getter. In case of `private val` the getter does not exist so we can assume private access.

The helper function and it's usage might look like this:

```kotlin
fun isFieldAccessible(property: KProperty1<*, *>): Boolean {
    return property.javaGetter?.modifiers?.let { !Modifier.isPrivate(it) } ?: false
}

val example = Example(field1 = "abc", field2 = 1, baseField = "someText")

example::class.memberProperties.filter { isFieldAccessible(it) }.forEach { member ->
    println("${member.name} -> ${member.get(example)}")
}

```

Another approach is to make private properties accessible using reflection:

```kotlin
example::class.memberProperties.forEach { member ->
    member.isAccessible = true
    println("${member.name} -> ${member.get(example)}")
}

```



## Setting values of all properties of a class


As an example we want to set all string properties of a sample class

```kotlin
class TestClass {
    val readOnlyProperty: String
        get() = "Read only!"

    var readWriteString = "asd"
    var readWriteInt = 23

    var readWriteBackedStringProperty: String = ""
        get() = field + '5'
        set(value) { field = value + '5' }

    var readWriteBackedIntProperty: Int = 0
        get() = field + 1
        set(value) { field = value - 1 }

    var delegatedProperty: Int by TestDelegate()

    private var privateProperty = "This should be private"

    private class TestDelegate {
        private var backingField = 3

        operator fun getValue(thisRef: Any?, prop: KProperty<*>): Int {
            return backingField
        }

        operator fun setValue(thisRef: Any?, prop: KProperty<*>, value: Int) {
            backingField += value
        }
    }
}

```

Getting mutable properties builds on getting all properties, filtering mutable properties by type. We also need to check visibility, as reading private properties results in run time exception.

```kotlin
val instance = TestClass()
TestClass::class.memberProperties
        .filter{ prop.visibility == KVisibility.PUBLIC }
        .filterIsInstance<KMutableProperty<*>>()
        .forEach { prop ->
            System.out.println("${prop.name} -> ${prop.get(instance)")
        }

```

To set all `String` properties to `"Our Value"` we can additionally filter by the return type. Since Kotlin is based on Java VM, [Type Erasure](http://stackoverflow.com/questions/339699/java-generics-type-erasure-when-and-what-happens) is in effect, and thus Properties returning generic types such as `List<String>` will be the same as `List<Any>`. Sadly reflection is not a golden bullet and there is no sensible way to avoid this, so you need to watch out in your use-cases.

```kotlin
val instance = TestClass()
TestClass::class.memberProperties
        .filter{ prop.visibility == KVisibility.PUBLIC }
        // We only want strings
        .filter{ it.returnType.isSubtypeOf(String::class.starProjectedType) }
        .filterIsInstance<KMutableProperty<*>>()
        .forEach { prop ->
            // Instead of printing the property we set it to some value
            prop.setter.call(instance, "Our Value")
        }

```



#### Remarks


Reflection is a mechanism to introspect language constructs (classes and functions) at the runtime.

When targeting JVM platform, runtime reflection features are distributed in separate JAR: `kotlin-reflect.jar`. This is done to reduce runtime size, cut unused features and make it possible to target another (like JS) platforms.

