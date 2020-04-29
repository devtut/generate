---
metaTitle: "Non-Access Modifiers"
description: "final, static, abstract, strictfp, volatile, synchronized, transient"
---

# Non-Access Modifiers


Non-Access Modifiers ****do not change the accessibility of variables**** and methods, but they do provide them ****special properties****.



## final


`final` in Java can refer to variables, methods and classes. There are three simple rules:

- final variable cannot be reassigned
- final method cannot be overriden
- final class cannot be extended

**Usages**

**Good Programming Practice**

Some developer consider it good practice to mark a variable final when you can. If you have a variable that should not be changed, you should mark it final.

An important use of `final` keyword if for method parameters. If you want to emphasize that a method doesn't change its input parameters, mark the properties as final.

```java
public int sumup(final List<Integer> ints);

```

This emphasizes that the `sumup` method is not going to change the `ints`.

**Inner class Access**

If your anonymous inner class wants to access a variable, the variable should be marked `final`

```

 public IPrintName printName(){
    String name;
    return new IPrintName(){
        @Override
        public void printName(){
            System.out.println(name);
        }
    };
}

```

This class doesn't compile, as the variable `name`, is not final.

Effectively final variables are an exception. These are local variables that are written to only once and could therefore be made final. Effectively final variables can be accessed from anonymus classes too.

**`final static` variable**

Even though the code below is completely legal when `final` variable `foo` is not `static`, in case of `static` it will not compile:

```java
class TestFinal {
    private final static List foo;

    public Test() {
        foo = new ArrayList();
    }
}

```

The reason is, let's repeat again, **final variable cannot be reassigned**. Since `foo` is static, it is shared among all instances of class `TestFinal`. When a new instance of a class `TestFinal` is created, its constructor is invoked and therefore foo gets reassigned which compiler does not allow. A correct way to initialize variable `foo` in this case is either:

```java
class TestFinal {
    private static final List foo = new ArrayList();
    //..
}

```

or by using a static initializer:

```java
class TestFinal {
    private static final List foo;
    static {
        foo = new ArrayList();
    }
    //..
}

```

`final` methods are useful when base class implements some important functionality that derived class is not supposed to change it. They are also faster than non-final methods, because there is no concept of virtual table involved.

All wrapper classes in Java are final, such as `Integer`, `Long` etc. Creators of these classes didn't want that anyone can e.g. extend Integer into his own class and change the basic behavior of Integer class. One of the requirements to make a class immutable is that subclasses may not override methods. The simplest way to do this is to declare the class as `final`.



## static


The `static` keyword is used on a class, method, or field to make them work independently of any instance of the class.

- Static fields are common to all instances of a class. They do not need an instance to access them.
- Static methods can be run without an instance of the class they are in. However, they can only access static fields of that class.
- Static classes can be declared inside of other classes. They do not need an instance of the class they are in to be instantiated.

```java
public class TestStatic
{
    static int staticVariable;

    static {
        // This block of code is run when the class first loads
        staticVariable = 11;
    }

    int nonStaticVariable = 5;

    static void doSomething() {
        // We can access static variables from static methods
        staticVariable = 10;
    }

    void add() {
        // We can access both static and non-static variables from non-static methods
        nonStaticVariable += staticVariable;
    }

    static class StaticInnerClass {
        int number;
        public StaticInnerClass(int _number) {
            number = _number;
        }

        void doSomething() {
            // We can access number and staticVariable, but not nonStaticVariable
            number += staticVariable;
        }

        int getNumber() {
            return number;
        }
    }
}


// Static fields and methods
TestStatic object1 = new TestStatic();

System.out.println(object1.staticVariable); // 11
System.out.println(TestStatic.staticVariable); // 11

TestStatic.doSomething();

TestStatic object2 = new TestStatic();

System.out.println(object1.staticVariable); // 10
System.out.println(object2.staticVariable); // 10
System.out.println(TestStatic.staticVariable); // 10

object1.add();

System.out.println(object1.nonStaticVariable); // 15
System.out.println(object2.nonStaticVariable); // 10

// Static inner classes
StaticInnerClass object3 = new TestStatic.StaticInnerClass(100);
StaticInnerClass object4 = new TestStatic.StaticInnerClass(200);

System.out.println(object3.getNumber()); // 100
System.out.println(object4.getNumber()); // 200

object3.doSomething();

System.out.println(object3.getNumber()); // 110
System.out.println(object4.getNumber()); // 200

```



## abstract


Abstraction is a process of hiding the implementation details and showing only functionality to the user. An abstract class can never be instantiated. If a class is declared as abstract then the sole purpose is for the class to be extended.

```java
abstract class Car
{
    abstract void tagLine();
}
 
class Honda extends Car
{
    void tagLine()
    {
        System.out.println("Start Something Special");
    }
}
 
class Toyota extends Car
{
    void tagLine()
    {
        System.out.println("Drive Your Dreams");
    }
}

```



## strictfp


strictfp modifier is used for floating-point calculations. This modifier  makes floating point variable more consistent across multiple platforms and ensure all the floating point calculations are done according to IEEE 754 standards to avoid errors of calculation (round-off errors), overflows and underflows on both 32bit and 64bit architecture. This cannot be applied on abstract methods, variables or constructors.

```java
// strictfp keyword can be applied on methods, classes and interfaces.

strictfp class A{}

strictfp interface M{}

class A{  
    strictfp void m(){}
} 

```



## volatile


The `volatile` modifier is used in multi threaded programming. If you declare a field as `volatile` it is a signal to threads that they must read the most recent value, not a locally cached one. Furthermore, `volatile` reads and writes are guaranteed to be atomic (access to a non-`volatile` `long` or `double` is not atomic), thus avoiding certain read/write errors between multiple threads.

```java
public class MyRunnable implements Runnable
{
    private volatile boolean active;
 
    public void run(){ // run is called in one thread 
        active = true;
        while (active){
            // some code here
        }
    }
    
    public void stop(){ // stop() is called from another thread
        active = false;
    }
}

```



## synchronized


Synchronized modifier is used to control the access of a particular method or a block by multiple threads. Only one thread can enter into a method or a block which is declared as synchronized. synchronized keyword works on intrinsic lock of an object, in case of a synchronized method current objects lock and static method uses class object. Any thread trying to execute a synchronized block must acquire the object lock first.

```java
class Shared
{
    int i;
 
    synchronized void SharedMethod()
    {
        Thread t = Thread.currentThread();
 
        for(int i = 0; i <= 1000; i++)
        {
            System.out.println(t.getName()+" : "+i);
        }
    }

    void SharedMethod2()
    {
        synchronized (this)
        {
            System.out.println("Thais access to currect object is synchronize "+this);
        }
    }
}
 
public class ThreadsInJava
{
    public static void main(String[] args)
    {
        final Shared s1 = new Shared();
 
        Thread t1 = new Thread("Thread - 1")
        {
            @Override
            public void run()
            {
                s1.SharedMethod();
            }
        };
 
        Thread t2 = new Thread("Thread - 2")
        {
            @Override
            public void run()
            {
                s1.SharedMethod();
            }
        };
 
        t1.start();
 
        t2.start();
    }
}

```



## transient


A variable which is declared as transient will not be serialized during object serialization.

```java
public transient int limit = 55;   // will not persist
public int b; // will persist

```

