---
metaTitle: "Default Methods"
description: "Basic usage of default methods, Accessing overridden default methods from implementing class, Why use Default Methods?, Accessing other interface methods within default method, Default method multiple inheritance collision, Class, Abstract class and Interface method precedence"
---

# Default Methods


**Default Method** introduced in Java 8, allows developers to add new methods to an interface without breaking the existing implementations of this interface. It provides flexibility to allow the interface to define an implementation which will be used as default when a class which implements that interface fails to provide an implementation of that method.



## Basic usage of default methods


```java
/**
 * Interface with default method
 */
public interface Printable {
    default void printString() {
        System.out.println( "default implementation" );
    }
}

/**
 * Class which falls back to default implementation of {@link #printString()}
 */
public class WithDefault
    implements Printable
{
}

/**
 * Custom implementation of {@link #printString()}
 */
public class OverrideDefault
    implements Printable {
    @Override
    public void printString() {
        System.out.println( "overridden implementation" );
    }
}

```

The following statements

```

   new WithDefault().printString();
    new OverrideDefault().printString();

```

Will produce this output:

`default implementation`<br />
`overridden implementation`



## Accessing overridden default methods from implementing class


In classes, `super.foo()` will look in superclasses only.  If you want to call a default implementation from a superinterface, you need to qualify `super` with the interface name: `Fooable.super.foo()`.

```java
public interface Fooable {
    default int foo() {return 3;}
}

public class A extends Object implements Fooable {
    @Override
    public int foo() {
        //return super.foo() + 1; //error: no method foo() in java.lang.Object
        return Fooable.super.foo() + 1; //okay, returns 4
    }
}

```



## Why use Default Methods?


The simple answer is that it allows you to evolve an existing interface without breaking existing implementations.

For example, you have `Swim` interface that you published 20 years ago.

```java
public interface Swim {
    void backStroke();
}

```

We did a great job, our interface is very popular, there are many implementation on that around the world and you don't have control over their source code.

```java
public class FooSwimmer implements Swim {
    public void backStroke() {
         System.out.println("Do backstroke");
    }
}

```

After 20 years, you've decided to add new functionality to the interface, but it looks like our interface is frozen because it will break existing implementations.

Luckily Java 8 introduces brand new feature called [Default method.](https://docs.oracle.com/javase/tutorial/java/IandI/defaultmethods.html)

We can now add new method to the `Swim` interface.

```java
public interface Swim {
    void backStroke();
    default void sideStroke() {
        System.out.println("Default sidestroke implementation. Can be overridden");
    }
}

```

Now all existing implementations of our interface can still work. But most importantly they can implement the newly added method in their own time.

One of the biggest reasons for this change, and one of its biggest uses, is in the Java Collections framework. Oracle could not add a `foreach` method to the existing Iterable interface without breaking all existing code which implemented Iterable. By adding default methods, existing Iterable implementation will inherit the default implementation.



## Accessing other interface methods within default method


You can as well access other interface methods from within your default method.

```java
public interface Summable {
    int getA();

    int getB();

    default int calculateSum() {
        return getA() + getB();
    }
}

public class Sum implements Summable {
    @Override
    public int getA() {
        return 1;
    }

    @Override
    public int getB() {
        return 2;
    }
}

```

The following statement will print **3**:

```java
System.out.println(new Sum().calculateSum());

```

Default methods could be used along with interface static methods as well:

```java
public interface Summable {
    static int getA() {
        return 1;
    }

    static int getB() {
        return 2;
    }

    default int calculateSum() {
        return getA() + getB();
    }
}

public class Sum implements Summable {}

```

The following statement will also print 3:

```java
System.out.println(new Sum().calculateSum());

```



## Default method multiple inheritance collision


Consider next example:

```java
public interface A {
    default void foo() { System.out.println("A.foo"); }
}

public interface B {
    default void foo() { System.out.println("B.foo"); }
}

```

Here are two interfaces declaring `default` method `foo` with the same signature.

If you will try to `extend` these both interfaces in the new interface you have to make choice of two, because Java forces you to resolve this collision explicitly.

**First**, you can declare method `foo` with the same signature as `abstract`, which will override `A` and `B` behaviour.

```java
public interface ABExtendsAbstract extends A, B {
    @Override
    void foo();
}

```

And when you will `implement` `ABExtendsAbstract` in the `class` you will have to provide `foo` implementation:

```java
public class ABExtendsAbstractImpl implements ABExtendsAbstract {
    @Override
    public void foo() { System.out.println("ABImpl.foo"); }
}

```

Or **second**, you can provide a completely new `default` implementation. You also may reuse code of `A` and `B` `foo` methods by [Accessing overridden default methods from implementing class](http://stackoverflow.com/documentation/java/113/default-methods/2442/accessing-overridden-default-methods-from-implementing-class).

```java
public interface ABExtends extends A, B {
    @Override
    default void foo() { System.out.println("ABExtends.foo"); }
}

```

And when you will `implement` `ABExtends` in the `class` you will `not` have to provide `foo` implementation:

```java
public class ABExtendsImpl implements ABExtends {}

```



## Class, Abstract class and Interface method precedence


Implementations in classes, including abstract declarations, take precedence over all interface defaults.

- Abstract class method takes precedence over [Interface Default Method](https://docs.oracle.com/javase/tutorial/java/IandI/defaultmethods.html).

```java
public interface Swim {
    default void backStroke() {
        System.out.println("Swim.backStroke");
    }
}

public abstract class AbstractSwimmer implements Swim {
    public void backStroke() {
        System.out.println("AbstractSwimmer.backStroke");
    }
}

public class FooSwimmer extends AbstractSwimmer {
}

```

The following statement

```java
new FooSwimmer().backStroke();

```

Will produce

```java
AbstractSwimmer.backStroke

```


- Class method takes precedence over [Interface Default Method](https://docs.oracle.com/javase/tutorial/java/IandI/defaultmethods.html)

```java
public interface Swim {
    default void backStroke() {
        System.out.println("Swim.backStroke");
    }
}

public abstract class AbstractSwimmer implements Swim {
}

public class FooSwimmer extends AbstractSwimmer {
    public void backStroke() {
        System.out.println("FooSwimmer.backStroke");
    }
}

```

The following statement

```java
new FooSwimmer().backStroke();

```

Will produce

```java
FooSwimmer.backStroke

```



#### Syntax


- public default void methodName() {/* method body */}



#### Remarks


### Default methods

<li>
Can be used within an interface, to introduce a behaviour without forcing existing subclasses to implement it. 
</li>
<li>
Can be overridden by subclasses or by a sub-interface.
</li>
<li>
Are not allowed to override methods in java.lang.Object class.
</li>
<li>
If a class implementing more than one interface, inherits default methods with identical method signatures from each of the intefaces, then it must override and provide its own interface as if they were not default methods (as part of resolving multiple inheritance).
</li>                
<li>
Although are intended to introduce a behaviour without breaking existing implementations, existing subclasses with a static method with same method signature as the newly introduced default method will still be broken. However this is true even in case of introducing an instance method in a superclass.     
</li>

<br><br>

### Static methods

<li>
    Can be used within an interface, primarily intended to be used as a utility method for default methods. 
</li>
<li>
    Cannot be overridden by subclasses or by a sub-interface (is hidden to them). 
    However as is the case with static methods even now, each class or interface can have its own.
</li>
<li>
    Are not allowed to override instance methods in java.lang.Object class (as is presently the case for subclasses as well).
</li>

<br><br>

**Below is a table summarizing the interaction between sub-class and super-class.**

|-|SUPER_CLASS-INSTANCE-METHOD|SUPER_CLASS-STATIC-METHOD
|------
|**SUB_CLASS-INSTANCE-METHOD**|**overrides**|**generates-compiletime-error**
|**SUB_CLASS-STATIC-METHOD**|**generates-compiletime-error**|**hides**

<br><br>

**Below is a table summarizing the interaction between interface and implementing-class.**

|-|INTERFACE-DEFAULT-METHOD|INTERFACE-STATIC-METHOD
|------
|**IMPL_CLASS-INSTANCE-METHOD**|**overrides**|**hides**
|**IMPL_CLASS-STATIC-METHOD**|**generates-compiletime-error**|**hides**

### References :

<li>
    http://www.journaldev.com/2752/java-8-interface-changes-static-method-default-method
</li>
<li>
    https://docs.oracle.com/javase/tutorial/java/IandI/override.html
</li>

