---
metaTitle: "Kotlin - Getting started with Kotlin"
description: "Hello World, Hello World using a Companion Object, Hello World using an Object Declaration, Main methods using varargs, Compile and Run Kotlin Code in Command Line, Reading input from Command Line"
---

# Getting started with Kotlin



## Hello World


All Kotlin programs start at the `main` function. Here is an example of a simple Kotlin "Hello World" program:

```kotlin
package my.program

fun main(args: Array<String>) {
    println("Hello, world!")
}

```

Place the above code into a file named `Main.kt` (this filename is entirely arbitrary)

When targeting the JVM, the function will be compiled as a static method in a class with a name derived from the filename. In the above example, the main class to run would be `my.program.MainKt`.

To change the name of the class that contains top-level functions for a particular file, place the following annotation at the top of the file above the package statement:

```kotlin
@file:JvmName("MyApp")

```

In this example, the main class to run would now be `my.program.MyApp`.

**See also:**

- [Package level functions](https://kotlinlang.org/docs/reference/java-to-kotlin-interop.html#package-level-functions) including `@JvmName` annotation.<br/>
- [Annotation use-site targets](https://kotlinlang.org/docs/reference/annotations.html#annotation-use-site-targets)



## Hello World using a Companion Object


Similar to using an Object Declaration, you can define the `main` function of a Kotlin program using a [Companion Object](https://kotlinlang.org/docs/reference/object-declarations.html#companion-objects) of a class.

```kotlin
package my.program

class App {
    companion object {
        @JvmStatic fun main(args: Array<String>) {
            println("Hello World")
        }
    }
}

```

The class name that you will run is the name of your class, in this case is `my.program.App`.

The advantage to this method over a top-level function is that the class name to run is more self-evident, and any other functions you add are scoped into the class `App`. This is similar to the `Object Declaration` example, other than you are in control of instantiating any classes to do further work.

A slight variation that instantiates the class to do the actual "hello":

```kotlin
class App {
    companion object {
        @JvmStatic fun main(args: Array<String>) {
            App().run()
        }
    }

    fun run() {
        println("Hello World")
    }
}

```

**See also:**

- [Static Methods](https://kotlinlang.org/docs/reference/java-to-kotlin-interop.html#static-methods) including the @JvmStatic annotation



## Hello World using an Object Declaration


You can alternatively use an [Object Declaration](https://kotlinlang.org/docs/reference/object-declarations.html#object-declarations) that contains the main function for a Kotlin program.

```kotlin
package my.program

object App {
    @JvmStatic fun main(args: Array<String>) {
        println("Hello World")
    }
}

```

The class name that you will run is the name of your object, in this case is `my.program.App`.

The advantage to this method over a top-level function is that the class name to run is more self-evident, and any other functions you add are scoped into the class `App`.  You then also have a singleton instance of `App` to store state and do other work.

**See also:**

- [Static Methods](https://kotlinlang.org/docs/reference/java-to-kotlin-interop.html#static-methods) including the `@JvmStatic` annotation



## Main methods using varargs


All of these main method styles can also be used with [varargs](https://kotlinlang.org/docs/reference/functions.html#variable-number-of-arguments-varargs):

```kotlin
package my.program

fun main(vararg args: String) {
    println("Hello, world!")
}

```



## Compile and Run Kotlin Code in Command Line


As java provide two different commands to compile and run Java code. Same as Kotlin also provide you different commands.

`javac` to compile java files.
`java` to run java files.

Same as
`kotlinc` to compile kotlin files
`kotlin` to run kotlin files.



## Reading input from Command Line


The arguments passed from the console can be received in the Kotlin program and it can be used as an input. You can pass N (1 2 3 and so on) numbers of arguments from the command prompt.

A simple example of a command-line argument in Kotlin.

```kotlin
fun main(args: Array<String>) {

    println("Enter Two number")
    var (a, b) = readLine()!!.split(' ') // !! this operator use for NPE(NullPointerException).

    println("Max number is : ${maxNum(a.toInt(), b.toInt())}")
}



fun maxNum(a: Int, b: Int): Int {

    var max = if (a > b) {
        println("The value of a is $a");
        a
    } else {
        println("The value of b is $b")
        b
    }

    return max;

}

```

Here, Enter two number from the command line to find the maximum number.
Output :

```kotlin
Enter Two number
71 89 // Enter two number from command line

The value of b is 89
Max number is: 89

```

For !! Operator Please check [Null Safety](https://kotlinlang.org/docs/reference/null-safety.html).

Note: Above example compile and run on Intellij.



#### Remarks


[Kotlin](http://kotlinlang.org) is a statically-typed object-oriented programming language developed by JetBrains primarily targeting the JVM. Kotlin is developed with the goals of being quick to compile, backwards-compatible, very type safe, and 100% interoperable with Java. Kotlin is also developed with the goal of providing many of the features wanted by Java developers. Kotlin's standard compiler allows it to be compiled both into Java bytecode for the JVM and into JavaScript.

### Compiling Kotlin

Kotlin has a standard IDE plugin for Eclipse and IntelliJ. Kotlin can also be compiled [using Maven](http://kotlinlang.org/docs/reference/using-maven.html), [using Ant](http://kotlinlang.org/docs/reference/using-ant.html), and [using Gradle](http://kotlinlang.org/docs/reference/using-gradle.html), or through the [command line](https://kotlinlang.org/docs/tutorials/command-line.html).

It is worth noting in `$ kotlinc Main.kt` will output a java class file, in this case `MainKt.class` (Note the Kt appended to the class name). However if one was to run the class file using `$ java MainKt` java will throw the following exception:

```kotlin
Exception in thread "main" java.lang.NoClassDefFoundError: kotlin/jvm/internal/Intrinsics
    at MainKt.main(Main.kt)
Caused by: java.lang.ClassNotFoundException: kotlin.jvm.internal.Intrinsics
    at java.net.URLClassLoader.findClass(URLClassLoader.java:381)
    at java.lang.ClassLoader.loadClass(ClassLoader.java:424)
    at sun.misc.Launcher$AppClassLoader.loadClass(Launcher.java:335)
    at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
    ... 1 more


```

In order to run the resulting class file using Java, one must include the Kotlin runt-time jar file to the current class path.

`java -cp .:/path/to/kotlin/runtime/jar/kotlin-runtime.jar MainKt`

