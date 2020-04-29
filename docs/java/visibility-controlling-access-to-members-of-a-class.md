---
metaTitle: "Visibility (controlling access to members of a class)"
description: "Private Visibility, Public Visibility, Package Visibility, Protected Visibility, Summary of Class Member Access Modifiers, Interface members"
---

# Visibility (controlling access to members of a class)




## Private Visibility


`private` visibility allows a variable to only be accessed by its class.  They are often used in **conjunction** with `public` getters and setters.

```java
class SomeClass {
    private int variable;

    public int getVariable() {
        return variable;
    }

    public void setVariable(int variable) {
        this.variable = variable;
    }
}

public class SomeOtherClass {
    public static void main(String[] args) {
        SomeClass sc = new SomeClass();
        
        // These statement won't compile because SomeClass#variable is private:
        sc.variable = 7;
        System.out.println(sc.variable);

        // Instead, you should use the public getter and setter:
        sc.setVariable(7);
        System.out.println(sc.getVariable());
    }
}

```



## Public Visibility


Visible to the class, package, and subclass.

Let's see an example with the class Test.

```java
public class Test{
    public int number = 2;

    public Test(){

    }
}

```

Now let's try to create an instance of the class.  In this example, **we can** access `number` because it is `public`.

```java
public class Other{
    
    public static void main(String[] args){
        Test t = new Test();
        System.out.println(t.number);
    }

}

```



## Package Visibility


With **no modifier**, the default is package visibility. **From the Java Documentation,** "[package visibility] indicates whether classes in the same package as the class (regardless of their parentage) have access to the member." In this example from [`javax.swing`](http://hg.openjdk.java.net/jdk8/jdk8/jdk/file/687fd7c7986d/src/share/classes/javax/swing),

```java
package javax.swing;
public abstract class JComponent extends Container … {
    …
    static boolean DEBUG_GRAPHICS_LOADED;
    …
}

```

`DebugGraphics` is in the same package, so `DEBUG_GRAPHICS_LOADED` is accessible.

```java
package javax.swing;
public class DebugGraphics extends Graphics {
    …
    static {
        JComponent.DEBUG_GRAPHICS_LOADED = true;
    }
    …
}

```

This [article](http://programmers.stackexchange.com/q/220053) gives some background on the topic.



## Protected Visibility


Protected visibility causes means that this member is visible to its package, along with any of its subclasses.

As an example:

```java
package com.stackexchange.docs;
public class MyClass{
    protected int variable; //This is the variable that we are trying to access
    public MyClass(){
        variable = 2;
    };
}

```

Now we'll extend this class and try to access one of its `protected` members.

```java
package some.other.pack;
import com.stackexchange.docs.MyClass;
public class SubClass extends MyClass{
    public SubClass(){
        super();
        System.out.println(super.variable);
    }
}

```

You would be also able to access a `protected` member without extending it if you are accessing it from the same package.

Note that this modifier only works on members of a class, not on the class itself.



## Summary of Class Member Access Modifiers


|Access Modifier|Visibility|Inheritance
|------
|Private|Class only|Can't be inherited
|**No modifier** / Package|In package|Available if subclass in package
|Protected|In package|Available in subclass
|Public|Everywhere|Available in subclass

There was once a `private protected` (both keywords at once) modifier that could be applied to methods or variables to make them accessible from a subclass outside the package, but make them private to the classes in that package. However, this was [removed in Java 1.0's release](http://stackoverflow.com/q/41431533/6754053).



## Interface members


```java
public interface MyInterface {
    public void foo();
    int bar();

    public String TEXT = "Hello";
    int ANSWER = 42;

    public class X {
    }

    class Y {
    }
}

```

Interface members always have public visibility, even if the `public` keyword is omitted. So both `foo()`, `bar()`, `TEXT`, `ANSWER`, `X`, and `Y` have public visibility. However, access may still be limited by the containing interface - since `MyInterface` has public visibility, its members may be accessed from anywhere, but if `MyInterface` had had package visibility, its members would only have been accessible from within the same package.



#### Syntax


- public type name[ = value];
- private type name[ = value];
- protected type name[ = value];
- type name[ = value];
- public class name{
- class name{



#### Remarks


**From the [Java tutorial](https://docs.oracle.com/javase/tutorial/java/javaOO/accesscontrol.html):**

Access level modifiers determine whether other classes can use a particular field or invoke a particular method. There are two levels of access control:

- At the top level—`public`, or **package-private** (no explicit modifier).
- At the member level—`public`, `private`, `protected`, or **package-private** (no explicit modifier).

A class may be declared with the modifier `public`, in which case that class is visible to all classes everywhere. If a class has no modifier (the default, also known as **package-private**), it is visible only within its own package.

At the member level, you can also use the `public` modifier or no modifier (**package-private**) just as with top-level classes, and with the same meaning. For members, there are two additional access modifiers: `private` and `protected`. The `private` modifier specifies that the member can only be accessed in its own class. The `protected` modifier specifies that the member can only be accessed within its own package (as with **package-private**) and, in addition, by a subclass of its class in another package.

The following table shows the access to members permitted by each modifier.

**Access Levels:**

|Modifier|Class|Package|Subclass|World
|------
|`public`|Y|Y|Y|Y
|`protected`|Y|Y|Y|N
|**no modifier**|Y|Y|N|N
|`private`|Y|N|N|N

