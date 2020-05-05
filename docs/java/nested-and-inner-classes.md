---
metaTitle: "Java - Nested and Inner Classes"
description: "A Simple Stack Using a Nested Class, Static vs Non Static Nested Classes, Access Modifiers for Inner Classes, Anonymous Inner Classes, Create instance of non-static inner class from outside, Method Local Inner Classes, Accessing the outer class from a non-static inner class"
---

# Nested and Inner Classes


Using Java, developers have the ability to define a class within another class. Such a class is called a [Nested Class](https://docs.oracle.com/javase/tutorial/java/javaOO/nested.html). Nested Classes are called Inner Classes if they were declared as non-static, if not, they are simply called Static Nested Classes. This page is to document and provide details with examples on how to use Java Nested and Inner Classes.



## A Simple Stack Using a Nested Class


```java
public class IntStack {

    private IntStackNode head;

    // IntStackNode is the inner class of the class IntStack
    // Each instance of this inner class functions as one link in the
    // Overall stack that it helps to represent
    private static class IntStackNode {

        private int val;
        private IntStackNode next;

        private IntStackNode(int v, IntStackNode n) {
            val = v;
            next = n;
        }
    }

    public IntStack push(int v) {
        head = new IntStackNode(v, head);
        return this;
    }

    public int pop() {
        int x = head.val;
        head = head.next;
        return x;
    }
}

```

And the use thereof, which (notably) does not at all acknowledge the existence of the nested class.

```java
public class Main {
    public static void main(String[] args) {
 
        IntStack s = new IntStack();
        s.push(4).push(3).push(2).push(1).push(0);

        //prints: 0, 1, 2, 3, 4, 
        for(int i = 0; i < 5; i++) {
            System.out.print(s.pop() + ", ");
        }            
    }
}

```



## Static vs Non Static Nested Classes


When creating a nested class, you face a choice of having that nested class static:

```java
public class OuterClass1 {

    private static class StaticNestedClass {

    }

}

```

Or non-static:

```java
public class OuterClass2 {

    private class NestedClass {

    }

}

```

At its core, static nested classes **do not have a surrounding **instance**** of the outer class, whereas non-static nested classes do. This affects both where/when one is allowed to instantiate a nested class, and what instances of those nested classes are allowed to access. Adding to the above example:

```java
public class OuterClass1 {

    private int aField;
    public void aMethod(){}

    private static class StaticNestedClass {
        private int innerField;

        private StaticNestedClass() {
             innerField = aField; //Illegal, can't access aField from static context 
             aMethod();           //Illegal, can't call aMethod from static context 
        }

        private StaticNestedClass(OuterClass1 instance) {
             innerField = instance.aField; //Legal
        }

    }

    public static void aStaticMethod() {
        StaticNestedClass s = new StaticNestedClass(); //Legal, able to construct in static context
        //Do stuff involving s...
    }

}

public class OuterClass2 {

    private int aField;

    public void aMethod() {}

    private class NestedClass {
        private int innerField;

        private NestedClass() {
             innerField = aField; //Legal   
             aMethod(); //Legal
        }
    }

    public void aNonStaticMethod() {
        NestedClass s = new NestedClass(); //Legal
    }

    public static void aStaticMethod() {
        NestedClass s = new NestedClass(); //Illegal. Can't construct without surrounding OuterClass2 instance.
                                         //As this is a static context, there is no surrounding OuterClass2 instance
    }
}

```

Thus, your decision of static vs non-static mainly depends on whether or not you need to be able to directly access fields and methods of the outer class, though it also has consequences for when and where you can construct the nested class.

As a rule of thumb, make your nested classes static unless you need to access fields and methods of the outer class. Similar to making your fields private unless you need them public, this decreases the visibility available to the nested class (by not allowing access to an outer instance), reducing the likelihood of error.



## Access Modifiers for Inner Classes


[A full explanation of Access Modifiers in Java can be found here](http://stackoverflow.com/documentation/java/134/visibility-controlling-access-to-members-of-a-class). But how do they interact with Inner classes?

`public`, as usual, gives unrestricted access to any scope able to access the type.

```java
public class OuterClass {

    public class InnerClass {

        public int x = 5;

    }

    public InnerClass createInner() {
        return new InnerClass();
    }
}

public class SomeOtherClass {

    public static void main(String[] args) {
        int x = new OuterClass().createInner().x; //Direct field access is legal
    }
}

```

both `protected` and the default modifier (of nothing) behave as expected as well, the same as they do for non-nested classes.

`private`, interestingly enough, does not restrict to the class it belongs to. Rather, it restricts to the compilation unit - the .java file. This means that Outer classes have full access to Inner class fields and methods, even if they are marked `private`.

```java
public class OuterClass {

    public class InnerClass {

        private int x;
        private void anInnerMethod() {}
    }

    public InnerClass aMethod() {
        InnerClass a = new InnerClass();
        a.x = 5; //Legal
        a.anInnerMethod(); //Legal
        return a;
    }
}

```

The Inner Class itself can have a visibility other than `public`. By marking it `private` or another restricted access modifier, other (external) classes will not be allowed to import and assign the type. They can still get references to objects of that type, however.

```java
public class OuterClass {

    private class InnerClass{}

    public InnerClass makeInnerClass() {
        return new InnerClass();
    }
}

public class AnotherClass {

    public static void main(String[] args) {
        OuterClass o = new OuterClass();
     
        InnerClass x = o.makeInnerClass(); //Illegal, can't find type
        OuterClass.InnerClass x = o.makeInnerClass(); //Illegal, InnerClass has visibility private
        Object x = o.makeInnerClass(); //Legal
    }
}

```



## Anonymous Inner Classes


An anonymous inner class is a form of inner class that is declared and instantiated with a single statement.  As a consequence, there is no name for the class that can be used elsewhere in the program; i.e. it is anonymous.

Anonymous classes are typically used in situations where you need to be able to create a light-weight class to be passed as a parameter.  This is typically done with an interface.  For example:

```java
public static Comparator<String> CASE_INSENSITIVE =
        new Comparator<String>() {
            @Override
            public int compare(String string1, String string2) {
                return string1.toUpperCase().compareTo(string2.toUpperCase());
            }
        };

```

This anonymous class defines a `Comparator<String>` object (`CASE_INSENSITIVE`) that compares two strings ignoring differences in case.

Other interfaces that are frequently implemented and instantiated using anonymous classes are `Runnable` and `Callable`.  For example:

```java
// An anonymous Runnable class is used to provide an instance that the Thread
// will run when started.
Thread t = new Thread(new Runnable() {
        @Override 
        public void run() {
              System.out.println("Hello world");
        }
    });
t.start();  // Prints "Hello world"

```

Anonymous inner classes can also be based on classes.  In this case, the anonymous class implicitly `extends` the existing class.  If the class being extended is abstract, then the anonymous class must implement all abstract methods.  It may also override non-abstract methods.

### Constructors

An anonymous class cannot have an explicit constructor.  Instead, an implicit constructor is defined that uses `super(...)` to pass any parameters to a constructor in the class that is being extended.  For example:

```java
SomeClass anon = new SomeClass(1, "happiness") {
            @Override
            public int someMethod(int arg) {
                // do something
            }
        };

```

The implicit constructor for our anonymous subclass of `SomeClass` will call a constructor of `SomeClass` that matches the call signature `SomeClass(int, String)`.  If no constructor is available, you will get a compilation error.  Any exceptions that are thrown by the matched constructor are also thrown by the implicit constructor.

Naturally, this does not work when extending an interface.  When you create an anonymous class from an interface, the classes superclass is `java.lang.Object` which only has a no-args constructor.



## Create instance of non-static inner class from outside


An inner class which is visible to any outside class can be created from this class as well.

The inner class depends on the outside class and requires a reference to an instance of it. To create an instance of the inner class, the `new` operator only needs to be called on an instance of the outer class.

```java
class OuterClass {

    class InnerClass {
    }
}

class OutsideClass {

    OuterClass outer = new OuterClass();
    
    OuterClass.InnerClass createInner() {
        return outer.new InnerClass();
    }
}

```

Note the usage as `outer.new`.



## Method Local Inner Classes


A class written within a method called **method local inner class**. In that case the scope of the inner class is restricted within the method.

A method-local inner class can be instantiated only within the method where the inner class is defined.

The example of using method local inner class:

```java
public class OuterClass {
    private void outerMethod() {
       final int outerInt = 1;
        // Method Local Inner Class
        class MethodLocalInnerClass {
            private void print() {
                System.out.println("Method local inner class " + outerInt);
            }
        }
        // Accessing the inner class
        MethodLocalInnerClass inner = new MethodLocalInnerClass();
        inner.print();
    }

    public static void main(String args[]) {
        OuterClass outer = new OuterClass();
        outer.outerMethod();
    }
}

```

Executing will give an output: `Method local inner class 1`.



## Accessing the outer class from a non-static inner class


The reference to the outer class uses the class name and `this`

```java
public class OuterClass {
    public class InnerClass {
        public void method() {
            System.out.println("I can access my enclosing class: " + OuterClass.this);
        }
    }
}

```

You can access fields and methods of the outer class directly.

```java
public class OuterClass {
    private int counter;

    public class InnerClass {
        public void method() {
            System.out.println("I can access " + counter);
        }
    }
}

```

But in case of name collision you can use the outer class reference.

```java
public class OuterClass {
    private int counter;

    public class InnerClass {
        private int counter;
        
        public void method() {
            System.out.println("My counter: " + counter);
            System.out.println("Outer counter: " + OuterClass.this.counter);
            
            // updating my counter
            counter = OuterClass.this.counter;
        }
    }
}

```



#### Syntax


- public class OuterClass { public class InnerClass { } } // Inner classes can also be private
- public class OuterClass { public static class StaticNestedClass { } } // Static nested classes can also be private
- public void method() { private class LocalClass { } } // Local classes are always private
- SomeClass anonymousClassInstance = new SomeClass() { }; // Anonymous inner classes cannot be named, hence access is moot. If 'SomeClass()' is abstract, the body must implement all abstract methods.
- SomeInterface anonymousClassInstance = new SomeInterface() { }; // The body must implement all interface methods.



#### Remarks


### Terminology and classification

The Java Language Specification (JLS) classifies the different kinds of Java class as follows:

> 
A **top level class** is a class that is not a nested class.


> 
A **nested class** is any class whose declaration occurs within the body of another class or interface.


> 
An **inner class** is a nested class that is not explicitly or implicitly declared static.


> 
An inner class may be a **non-static member class**, a **local class**, or an **anonymous class**. A member class of an interface is implicitly static so is never considered to be an inner class.


In practice programmers refer to a top level class that contains an inner class as the "outer class".  Also, there is a tendency to use "nested class" to refer to only to (explicitly or implicitly) static nested classes.

Note that there is a close relationship between anonymous inner classes and the lambdas, but lambdas are classes.

### Semantic differences

<li>
Top level classes are the "base case".  They are visible to other parts of a program subject to normal visibility rules based on access modifier semantics.  If non-abstract, they can be instantiated by any code that where the relevant constructors are visible based on the access modifiers.
</li>
<li>
Static nested classes follow the same access and instantiation rules as top level classes, with two exceptions:
<ul>
- A nested class may be declared as `private`, which makes it inaccessible outside of its enclosing top level class.
- A nested class has access to the `private` members of the enclosing top-level class and all of its tested class.

This makes static nested classes useful when you need to represent multiple "entity types" within a tight abstraction boundary; e.g. when the nested classes are used to hide "implementation details".

Inner classes add the ability to access non-static variables declared in enclosing scopes:

- A non-static member class can refer to instance variables.
- A local class (declared within a method) can also refer to the local variables of the method, provided that they are `final`.  (For Java 8 and later, they can be **effectively final**.)
- An anonymous inner class can be declared within either a class or a method, and can access variables according to the same rules.

The fact that an inner class instance can refer to variables in a enclosing class instance has implications for instantiation.  Specifically, an enclosing instance must be provided, either implicitly or explicitly, when an instance of an inner class is created.

