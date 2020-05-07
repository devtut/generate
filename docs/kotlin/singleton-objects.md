---
metaTitle: "Kotlin - Singleton objects"
description: "Use as repalcement of static methods/fields of java, Use as a singleton"
---

# Singleton objects


An **object** is a special kind of class, which can be declared using `object` keyword. Objects are similar to Singletons (a design pattern) in java. It also functions as the static part of java. Beginners who are switching from java to kotlin can vastly use this feature, in place of static, or singletons.



## Use as repalcement of static methods/fields of java


```kotlin
object CommonUtils {

    var anyname: String ="Hello"

    fun dispMsg(message: String) {
        println(message)
    }
}

```

From any other class, just invoke the variable and functions in this way:

```kotlin
CommonUtils.anyname
CommonUtils.dispMsg("like static call")

```



## Use as a singleton


Kotlin objects are actually just singletons. Its primary advantage is that you don't have to use `SomeSingleton.INSTANCE` to get the instance of the singleton.

In java your singleton looks like this:

```kotlin
public enum SharedRegistry {
    INSTANCE;
    public void register(String key, Object thing) {}
}

public static void main(String[] args) {
    SharedRegistry.INSTANCE.register("a", "apple");
    SharedRegistry.INSTANCE.register("b", "boy");
    SharedRegistry.INSTANCE.register("c", "cat");
    SharedRegistry.INSTANCE.register("d", "dog");
}

```

In kotlin, the equivalent code is

```kotlin
object SharedRegistry {
    fun register(key: String, thing: Object) {}
}

fun main(Array<String> args) {
    SharedRegistry.register("a", "apple")
    SharedRegistry.register("b", "boy")
    SharedRegistry.register("c", "cat")
    SharedRegistry.register("d", "dog")
}

```

It's obvoiusly less verbose to use.

