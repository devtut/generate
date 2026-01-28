---
metaTitle: "Kotlin - Interfaces"
description: "Interface with default implementations, Basic Interface, Properties in Interfaces, super keyword, Conflicts when Implementing Multiple Interfaces with Default Implementations"
---

# Interfaces




## Interface with default implementations


An interface in Kotlin can have default implementations for functions:

```kotlin
interface MyInterface {
    fun withImplementation() {
      print("withImplementation() was called")
    }
}

```

Classes implementing such interfaces will be able to use those functions without reimplementing

```kotlin
class MyClass: MyInterface {
    // No need to reimplement here
}
val instance = MyClass()
instance.withImplementation()

```

### Properties

Default implementations also work for property getters and setters:

```kotlin
interface MyInterface2 {
    val helloWorld
        get() = "Hello World!"
}

```

Interface accessors implementations can't use backing fields

```kotlin
interface MyInterface3 {
    // this property won't compile!
    var helloWorld: Int
        get() = field
        set(value) { field = value }
}

```

### Multiple implementations

When multiple interfaces implement the same function, or all of them define with one or more implementing, the derived class needs to manually resolve proper call

```kotlin
interface A {
    fun notImplemented()
    fun implementedOnlyInA() { print("only A") }
    fun implementedInBoth() { print("both, A") }
    fun implementedInOne() { print("implemented in A") }
}

interface B {
    fun implementedInBoth() { print("both, B") }
    fun implementedInOne() // only defined
}

class MyClass: A, B {
    override fun notImplemented() { print("Normal implementation") }

    // implementedOnlyInA() can by normally used in instances

    // class needs to define how to use interface functions
    override fun implementedInBoth() {
        super<B>.implementedInBoth()
        super<A>.implementedInBoth()
    }

    // even if there's only one implementation, there multiple definitions
    override fun implementedInOne() {
        super<A>.implementedInOne()
        print("implementedInOne class implementation")
    }
}

```



## Basic Interface


A Kotlin interface contains declarations of abstract methods, and default method implementations although they cannot store state.

```kotlin
interface MyInterface {
    fun bar()
}

```

This interface can now be implemented by a class as follows:

```kotlin
class Child : MyInterface {
   override fun bar() {
       print("bar() was called")
   }
}

```



## Properties in Interfaces


You can declare properties in interfaces.  Since an interface cannot have state you can only declare a property as abstract or by providing default implementation for the accessors.

```kotlin
interface MyInterface {
    val property: Int // abstract

    val propertyWithImplementation: String
        get() = "foo"

    fun foo() {
        print(property)
    }
}

class Child : MyInterface {
    override val property: Int = 29
}

```



## super keyword


```kotlin
interface MyInterface {
    fun funcOne() {
        //optional body
        print("Function with default implementation")
    }
}

```

If the method in the interface has its own default implementation, we can use super keyword to access it.

```kotlin
super.funcOne()

```



## Conflicts when Implementing Multiple Interfaces with Default Implementations


When implementing more than one interface that have methods of the same name that include default implementations, it is ambiguous to the compiler which implementation should be used.  In the case of a conflict, the developer must override the conflicting method and provide a custom implementation.  That implementation may chose to delegate to the default implementations or not.

```kotlin
interface FirstTrait {
    fun foo() { print("first") }
    fun bar()
}

interface SecondTrait {
    fun foo() { print("second") }
    fun bar() { print("bar") }
}

class ClassWithConflict : FirstTrait, SecondTrait {
    override fun foo() {
        super<FirstTrait>.foo()  // delegate to the default implementation of FirstTrait
        super<SecondTrait>.foo() // delegate to the default implementation of SecondTrait
    }

    // function bar() only has a default implementation in one interface and therefore is ok.
}

```



#### Remarks


**See also:**  Kotlin reference documentation for Interfaces: [Interfaces](https://kotlinlang.org/docs/reference/interfaces.html)

