---
metaTitle: "C++ Comparison"
description: "Static Class Members, Classes Defined within Other Constructs, Pass-by-value & Pass-by-reference, Inheritance vs Composition, Outcast Downcasting, Abstract Methods & Classes"
---

# C++ Comparison


Java and C++ are similar languages.  This topic serves as a quick reference guide for Java and C++ Engineers.



## Static Class Members


Static members have class scope as opposed to object scope

### C++ [Example](http://www.bogotobogo.com/cplusplus/statics.php)

```java
// define in header
class Singleton {
   public:
      static Singleton *getInstance();

   private:
      Singleton() {}
      static Singleton *instance;
};

// initialize in .cpp
Singleton* Singleton::instance = 0;

```

### Java [Example](http://www.journaldev.com/1377/java-singleton-design-pattern-best-practices-examples)

```java
public class Singleton {
    private static Singleton instance;
    
    private Singleton() {}
    
    public static Singleton getInstance() {
        if(instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}

```



## Classes Defined within Other Constructs


### Defined within Another Class

### C++

Nested Class[[ref]](https://stackoverflow.com/questions/2687544/question-about-c-inner-class) (needs a reference to enclosing class)

```java
class Outer {
   class Inner {
      public:
         Inner(Outer* o) :outer(o) {}

      private:
         Outer*  outer;
   };
};

```

### Java

[non-static] Nested Class (aka Inner Class or Member Class)

```java
class OuterClass {
    ...
    class InnerClass {
        ...
    }
}

```

### Statically Defined within Another Class

### C++

Static Nested Class

```java
class Outer {
   class Inner {
      ...
   };
};

```

### Java

Static Nested Class (aka Static Member Class)[[ref]](https://www.javatpoint.com/static-nested-class)

```java
class OuterClass {
    ...
    static class StaticNestedClass {
        ...
    }
}

```

### Defined within a Method

(e.g. event handling)

### C++

Local Class[[ref]](http://www.geeksforgeeks.org/local-class-in-c/)

```java
void fun() {
   class Test {
      /* members of Test class */
   };
}

```

### Java

Local Class[[ref]](https://stackoverflow.com/questions/1183453/whats-the-use-of-a-method-local-inner-class)

```java
class Test {
    void f() {
        new Thread(new Runnable() {
            public void run() {
                doSomethingBackgroundish();
            }
        }).start();
    }
}

```



## Pass-by-value & Pass-by-reference


Many argue that Java is ONLY pass-by-value, but it's more nuanced than that.
Compare the following C++ and Java examples to see the many flavors of pass-by-value (aka copy) and pass-by-reference (aka alias).

### C++ Example [(complete code)](https://gitlab.com/johndifini/java-algos/blob/master/PassIt.cpp)

```

 // passes a COPY of the object
  static void passByCopy(PassIt obj) {
     obj.i = 22;  // only a "local" change
  }

  // passes a pointer
  static void passByPointer(PassIt* ptr) {
     ptr->i = 33;
     ptr = 0; // better to use nullptr instead if '0'
  }

  // passes an alias (aka reference)
  static void passByAlias(PassIt& ref) {
     ref.i = 44;
  }

  // This is an old-school way of doing it.
  // Check out std::swap for the best way to do this
  static void swap(PassIt** pptr1, PassIt** pptr2) {
     PassIt* tmp = *pptr1;
     *pptr1 = *pptr2;
     *pptr2 = tmp;
  }

```

### Java Example [(complete code)](https://gitlab.com/johndifini/java-algos/blob/master/PassIt.java)

```

  // passes a copy of the variable
   // NOTE: in java only primitives are pass-by-copy
   public static void passByCopy(int copy) {
      copy = 33;  // only a "local" change
   }

   // No such thing as pointers in Java
   /*
   public static void passByPointer(PassIt *ptr) {
      ptr->i = 33;
      ptr = 0; // better to use nullptr instead if '0'
   }
   */

   // passes an alias (aka reference)
   public static void passByAlias(PassIt ref) {
      ref.i = 44;
   }

   // passes aliases (aka references),
   // but need to do "manual", potentially expensive copies
   public static void swap(PassIt ref1, PassIt ref2) {
      PassIt tmp = new PassIt(ref1);
      ref1.copy(ref2);
      ref2.copy(tmp);
   }

```



## Inheritance vs Composition


C++ & Java are both object-oriented languages, thus the following diagram applies to both.

<img src="https://static1.squarespace.com/static/59051f5dbebafb1fcb3f32ec/t/597366fecd0f689adb91f4ee/1500735237131" alt="Inheritance vs Composition Example" />



## Outcast Downcasting


Beware of using "downcasting" - Downcasting is casting down the inheritance hierarchy from a base class to a subclass (i.e. opposite of polymorphism). In general, use polymorphism & overriding instead of instanceof & downcasting.

### C++ Example

```java
// explicit type case required
Child *pChild =  (Child *) &parent;

```

### Java Example

```java
if(mySubClass instanceof SubClass) {
   SubClass mySubClass = (SubClass)someBaseClass;
   mySubClass.nonInheritedMethod();
}

```



## Abstract Methods & Classes


### Abstract Method

declared without an implementation

### C++

pure virtual method

```java
virtual void eat(void) = 0;

```

### Java

abstract method

```java
abstract void draw();

```

### Abstract Class

cannot be instantiated

### C++

cannot be instantiated; has at least 1 pure virtual method

```java
class AB {public: virtual void f() = 0;};

```

### Java

cannot be instantiated; can have non-abstract methods

```java
abstract class GraphicObject {}

```

### Interface

no instance fields

### C++

**nothing comparable to Java**

### Java

very similar to abstract class, but 1) supports multiple inheritance; 2) no instance fields

```java
interface TestInterface {}

```



#### Remarks


### Classes Defined within Other Constructs#

### Defined within Another Class

### C++

Nested Class[[ref]](https://stackoverflow.com/questions/2687544/question-about-c-inner-class) (needs a reference to enclosing class)

```java
class Outer {
   class Inner {
      public:
         Inner(Outer* o) :outer(o) {}

      private:
         Outer*  outer;
   };
};

```

### Java

[non-static] Nested Class (aka Inner Class or Member Class)

```java
class OuterClass {
    ...
    class InnerClass {
        ...
    }
}

```

### Statically Defined within Another Class

### C++

Static Nested Class

```java
class Outer {
   class Inner {
      ...
   };
};

```

### Java

Static Nested Class (aka Static Member Class)[[ref]](https://www.javatpoint.com/static-nested-class)

```java
class OuterClass {
    ...
    static class StaticNestedClass {
        ...
    }
}

```

### Defined within a Method

(e.g. event handling)

### C++

Local Class[[ref]](http://www.geeksforgeeks.org/local-class-in-c/)

```java
void fun() {
   class Test {
      /* members of Test class */
   };
}

```

See also [Lambda expressions](http://en.cppreference.com/w/cpp/language/lambda)

### Java

Local Class[[ref]](https://stackoverflow.com/questions/1183453/whats-the-use-of-a-method-local-inner-class)

```java
class Test {
    void f() {
        new Thread(new Runnable() {
            public void run() {
                doSomethingBackgroundish();
            }
        }).start();
    }
}

```

### Overriding vs Overloading

The following Overriding vs Overloading points apply to both C++ and Java:

<li>An overridden method has the same name and arguments as its base
method.</li>
<li>An overloaded method has the same name but different
arguments and does not rely on inheritance.</li>
- Two methods with the same name and arguments but different return type are illegal. See related Stackoverflow questions about "overloading with different return type in Java" - [Question 1](https://stackoverflow.com/questions/2439782/overload-with-different-return-type-in-java); [Question 2](https://stackoverflow.com/questions/5561436/can-two-java-methods-have-same-name-with-different-return-types)

### Polymorphism

Polymorphism is the ability for objects of different classes related by inheritance to respond differently to the same method call.  Here's an example:

- base class Shape with area as an abstract method
- two derived classes, Square and Circle, implement area methods
- Shape reference points to Square and area is invoked

In C++, polymorphism is enabled by virtual methods.  In Java, methods are virtual by default.

### Order of Construction/Destruction

<img src="https://static1.squarespace.com/static/59051f5dbebafb1fcb3f32ec/t/59736a63be6594496c64018b/1500736103271" alt="Order of Construction/Destruction" />

### Object Cleanup

In C++, it's a good idea to declare a destructor as virtual to ensure that the subclass' destructor will be called if the base-class pointer is deleted.

In Java, a finalize method is similar a destructor in C++; however, finalizers are unpredictable (they rely on GC).  Best practice - use a "close" method to explicitly cleanup.

```java
protected void close() {
    try {
       // do subclass cleanup
    }
    finally {
       isClosed = true;
       super.close();
    }
}

protected void finalize() {
    try {
       if(!isClosed) close();
    }
    finally {
       super.finalize();
    }
}

```

### Abstract Methods & Classes

|Concept|C++|Java
|------
|**Abstract Method**<br>declared without an implementation|pure virtual method<br>`virtual void eat(void) = 0;`|abstract method<br>`abstract void draw();`
|**Abstract Class**<br>cannot be instantiated|cannot be instantiated; has at least 1 pure virtual method<br>`class AB {public: virtual void f() = 0;};`|cannot be instantiated; can have non-abstract methods<br>`abstract class GraphicObject {}`
|**Interface**<br>no instance fields|no "interface" keyword, but can mimic a Java interface with facilities of an abstract class|very similar to abstract class, but 1) supports multiple inheritance; 2) no instance fields<br>`interface TestInterface {}`

### Accessibility Modifiers

|Modifier|C++|Java
|------
|**Public** - accessible by all|**no special notes**|**no special notes**
|**Protected** - accessible by subclasses|also accessible by friends|also accessible within same package
|**Private** - accessible by members|also accessible by friends|**no special notes**
|**default**|class default is private; struct default is public|accessible by all classes within the same package
|**other**|Friend - a way to grant access to private & protected members without inheritance (see below)|

### C++ Friend Example

```java
class Node {
  private:
    int key;  Node *next;
    // LinkedList::search() can access "key" & "next"
    friend int LinkedList::search();
};

```

### The Dreaded Diamond Problem

> 
<p>The diamond problem is an ambiguity that arises when two classes B and
C inherit from A, and class D inherits from both B and C. If there is
a method in A that B and C have overridden, and D does not override
it, then which version of the method does D inherit: that of B, or
that of C? (from [Wikipedia](https://en.wikipedia.org/wiki/Multiple_inheritance))</p>


<img src="https://static1.squarespace.com/static/59051f5dbebafb1fcb3f32ec/t/59736cb8cd39c31649173052/1500736701011" alt="The Dreaded Diamond Problem" />

While C++ has always been susceptible to the diamond problem, Java was susceptible until Java 8.  Originally, Java didn't support multiple inheritance, but with the advent of default interface methods, Java classes can not inherit "implementation" from more than one class.

### java.lang.Object Class

In Java all classes inherit, either implicitly or explicitly, from the Object class.  Any Java reference can be cast to the Object type.

C++ doesn't have a comparable "Object" class.

### Java Collections & C++ Containers

Java Collections are symonymous with C++ Containers.

### Java Collections Flowchart

[<img src="https://static1.squarespace.com/static/59051f5dbebafb1fcb3f32ec/590521d703596ed6c176101f/59750b1a37c581dd4630cc6b/1500842780582/Java+Collections+Flowchart.png" width="400" height="220">](https://docs.google.com/drawings/d/12OlqJ9_4fB2BjvgrAefvYQ9vHap3i_1nRLMx2gMFa64/edit?usp=sharing)

### C++ Containers Flowchart

[<img src="https://static1.squarespace.com/static/59051f5dbebafb1fcb3f32ec/590521d703596ed6c176101f/597504ef3e00be4843ebd20e/1500841203471/C%2B%2B+Container+Flowchart.png" width="400" height="220">](https://docs.google.com/drawings/d/1c-qvy499kxaYXM70DM34rOnEBCzgQLopyNDEXdaK0eU/edit?usp=sharing)

### Integer Types

|Bits|Min|Max|C++ Type<br>(on LLP64 or LP64)|Java Type
|------
|8|-2(8-1) = -128|2(8-1)-1 = 127|char|byte
|8|0|2(8)-1 = 255|unsigned char|--
|16|-2(16-1) = -32,768|2(16-1)-1 = 32,767|short|short
|16|0 (\u0000)|2(16)-1 = 65,535 (\uFFFF)|unsigned short|char (unsigned)
|32|-2(32-1) = -2.147 billion|2(32-1)-1 = 2.147 billion|int|int
|32|0|2(32)-1 = 4.295 billion|unsigned int|--
|64|-2(64-1)|2(16-1)-1|long*|long long
|64|0|2(16)-1|unsigned long*<br>unsigned long long|--

`*` Win64 API is only 32 bit

[Lots more C++ types](http://en.cppreference.com/w/cpp/language/types)

