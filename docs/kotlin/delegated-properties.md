---
metaTitle: "Kotlin - Delegated properties"
description: "Observable properties, Custom delegation, Lazy initialization, Map-backed properties, Delegate Can be used as a layer to reduce boilerplate"
---

# Delegated properties


Kotlin can delegate the implementation of a property to a handler object. Some standard handlers are included, such as lazy initialization or observable properties. Custom handlers can also be created.



## Observable properties


```kotlin
var foo : Int by Delegates.observable("1") { property, oldValue, newValue ->
    println("${property.name} was changed from $oldValue to $newValue")
}
foo = 2

```

The example prints `foo was changed from 1 to 2`



## Custom delegation


```kotlin
class MyDelegate {
    operator fun getValue(owner: Any?, property: KProperty<*>): String {
        return "Delegated value"
    }
}

val foo : String by MyDelegate()
println(foo)

```

The example prints `Delegated value`



## Lazy initialization


```kotlin
val foo : Int by lazy { 1 + 1 }
println(foo)

```

The example prints `2`.



## Map-backed properties


```kotlin
val map = mapOf("foo" to 1)
val foo : String by map
println(foo)

```

The example prints `1`



## Delegate Can be used as a layer to reduce boilerplate


Consider Kotlin's Null Type system and `WeakReference<T>`.

So let's say we have to save some sort of reference and we wanted to avoid memory leaks, here is where `WeakReference` comes in.

take for example this:

```kotlin
class MyMemoryExpensiveClass {
    companion object {
        var reference: WeakReference<MyMemoryExpensiveClass>? = null

        fun doWithReference(block: (MyMemoryExpensiveClass) -> Unit) {
            reference?.let {
                it.get()?.let(block)
            }
        }
    }

    init {
        reference = WeakReference(this)
    }
}

```

Now this is just with one WeakReference. To Reduce this boilerplate, we can use a custom property delegate to help us like so:

```kotlin
class WeakReferenceDelegate<T>(initialValue: T? = null) : ReadWriteProperty<Any, T?> {
    var reference = WeakReference(initialValue)
        private set
    
    override fun getValue(thisRef: Any, property: KProperty<*>): T? = reference.get()
    
    override fun setValue(thisRef: Any, property: KProperty<*>, value: T?) {
        reference = WeakReference(value)
    }
}

```

So Now we can use variables that are wrapped with `WeakReference` just like normal nullable variables !

```kotlin
class MyMemoryExpensiveClass {
    companion object {
        var reference: MyMemoryExpensiveClass? by WeakReferenceDelegate<MyMemoryExpensiveClass>()

        fun doWithReference(block: (MyMemoryExpensiveClass) -> Unit) {
            reference?.let(block)
        }
    }

    init {
        reference = this
    }
}

```

