---
metaTitle: "Java - Object Class Methods and Constructor"
description: "hashCode() method, toString() method, equals() method, wait() and notify() methods, getClass() method, clone() method, finalize() method, Object constructor"
---

# Object Class Methods and Constructor


This documentation page is for showing details with example about java class [constructors](https://docs.oracle.com/javase/tutorial/java/javaOO/constructors.html) and about [Object Class Methods](https://docs.oracle.com/javase/tutorial/java/IandI/objectclass.html) which are automatically inherited from the superclass `Object` of any newly created class.



## hashCode() method


When a Java class overrides the `equals` method, it should override the `hashCode` method as well. As defined [in the method's contract](https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html):

> 
<ul>
- Whenever it is invoked on the same object more than once during an execution of a Java application, the `hashCode` method must consistently return the same integer, provided no information used in equals comparisons on the object is modified. This integer need not remain consistent from one execution of an application to another execution of the same application.
- If two objects are equal according to the `equals(Object)` method, then calling the `hashCode` method on each of the two objects must produce the same integer result.
- It is not required that if two objects are unequal according to the `equals(Object)` method, then calling the `hashCode` method on each of the two objects must produce distinct integer results. However, the programmer should be aware that producing distinct integer results for unequal objects may improve the performance of hash tables.
</ul>


Hash codes are used in hash implementations such as `HashMap`, `HashTable`, and `HashSet`. The result of the `hashCode` function determines the bucket in which an object will be put. These hash implementations are more efficient if the provided `hashCode` implementation is good. An important property of good `hashCode` implementation is that the distribution of the `hashCode` values is uniform. In other words, there is a small probability that numerous instances will be stored in the same bucket.

An algorithm for computing a hash code value may be similar to the following:

```java
public class Foo {
    private int field1, field2;
    private String field3;

    public Foo(int field1, int field2, String field3) {
        this.field1 = field1;
        this.field2 = field2;
        this.field3 = field3;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        Foo f = (Foo) obj;
        return field1 == f.field1 &&
               field2 == f.field2 &&
               (field3 == null ? f.field3 == null : field3.equals(f.field3);
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = 31 * hash + field1;
        hash = 31 * hash + field2;
        hash = 31 * hash + (field3 == null ? 0 : field3.hashCode());
        return hash;
    }
}

```

### Using Arrays.hashCode() as a short cut

In Java 1.2 and above, instead of developing an algorithm to compute a hash code, one can be generated using `java.util.Arrays#hashCode` by supplying an Object or primitives array containing the field values:

```java
@Override
public int hashCode() {
    return Arrays.hashCode(new Object[] {field1, field2, field3});
}

```

Java 1.7 introduced the `java.util.Objects` class which provides a convenience method, `hash(Object... objects)`, that computes a hash code based on the values of the objects supplied to it. This method works just like `java.util.Arrays#hashCode`.

```java
@Override
public int hashCode() {
    return Objects.hash(field1, field2, field3);
}

```

Note: this approach is inefficient, and produces garbage objects each time your custom `hashCode()` method is called:

- A temporary `Object[]` is created.  (In the `Objects.hash()` version, the array is created by the "varargs" mechanism.)
- If any of the fields are primitive types, they must be boxed and that may create more temporary objects.
- The array must be populated.
- The array must iterated by the `Arrays.hashCode` or `Objects.hash` method.
- The calls to `Object.hashCode()` that `Arrays.hashCode` or `Objects.hash`  has to make (probably) cannot be inlined.

### Internal caching of hash codes

Since the calculation of an object's hash code can be expensive, it can be attractive to cache the hash code value within the object the first time that it is calculated.  For example

```java
public final class ImmutableArray {
    private int[] array;
    private volatile int hash = 0;

    public ImmutableArray(int[] initial) {
        array = initial.clone();
    }

    // Other methods

    @Override
    public boolean equals(Object obj) {
         // ...
    }

    @Override
    public int hashCode() {
        int h = hash;
        if (h == 0) {
            h = Arrays.hashCode(array);
            hash = h;
        }
        return h;
    }
}

```

This approach trades off the cost of (repeatedly) calculating the hash code against the overhead of an extra field to cache the hash code.  Whether this pays off as a performance optimization will depend on how often a given object is hashed (looked up) and other factors.

You will also notice that if the true hashcode of an `ImmutableArray` happens to be zero (one chance in 2<sup>32</sup>), the cache is ineffective.

Finally, this approach is much harder to implement correctly if the object we are hashing is mutable.  However, there are bigger concerns if hash codes change; see the contract above.



## toString() method


The `toString()` method is used to create a `String` representation of an object by using the object´s content. This method should be overridden when writing your class. `toString()` is called implicitly when an object is concatenated to a string as in `"hello " + anObject`.

Consider the following:

```java
public class User {
    private String firstName;
    private String lastName;
    
    public User(String firstName, String lastName) {
        this.firstName = firstName;
        this.lastName = lastName;
    }
    
    @Override
    public String toString() {
        return firstName + " " + lastName;
    }
    
    public static void main(String[] args) {
        User user = new User("John", "Doe");
        System.out.println(user.toString()); // Prints "John Doe"
    }   
}

```

Here `toString()` from `Object` class is overridden in the `User` class to provide meaningful data regarding the object when printing it.

When using `println()`, the object's `toString()` method is implicitly called. Therefore, these statements do the same thing:

```java
System.out.println(user); // toString() is implicitly called on `user`
System.out.println(user.toString());

```

If the `toString()` is not overridden in the above mentioned `User` class, `System.out.println(user)` may return `User@659e0bfd` or a similar String with almost no useful information except the class name. This will be because the call will use the `toString()` implementation of the base Java `Object` class which does not know anything about the `User` class's structure or business rules. If you want to change this functionality in your class, simply override the method.



## equals() method


**TL;DR**

`==` tests for reference equality (whether they are the ****same object****)

`.equals()` tests for value equality (whether they are ****logically "equal"****)

`equals()` is a method used to compare two objects for equality. The default implementation of the `equals()` method in the `Object` class returns `true` if and only if both references are pointing to the same instance. It therefore behaves the same as comparison by `==`.

```java
public class Foo {
    int field1, field2;
    String field3;

    public Foo(int i, int j, String k) {
        field1 = i;
        field2 = j;
        field3 = k;
    }

    public static void main(String[] args) {
        Foo foo1 = new Foo(0, 0, "bar");
        Foo foo2 = new Foo(0, 0, "bar");

        System.out.println(foo1.equals(foo2)); // prints false
    }
}

```

Even though `foo1` and `foo2` are created with the same fields, they are pointing to two different objects in memory. The default `equals()` implementation therefore evaluates to `false`.

To compare the contents of an object for equality, `equals()` has to be overridden.

```java
public class Foo {
    int field1, field2;
    String field3;

    public Foo(int i, int j, String k) {
        field1 = i;
        field2 = j;
        field3 = k;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || getClass() != obj.getClass()) {
            return false;
        }

        Foo f = (Foo) obj;
        return field1 == f.field1 &&
               field2 == f.field2 &&
               (field3 == null ? f.field3 == null : field3.equals(f.field3));
    }

    @Override
    public int hashCode() {
        int hash = 1;
        hash = 31 * hash + this.field1;
        hash = 31 * hash + this.field2;
        hash = 31 * hash + (field3 == null ? 0 : field3.hashCode());
        return hash;
    }

    public static void main(String[] args) {
        Foo foo1 = new Foo(0, 0, "bar");
        Foo foo2 = new Foo(0, 0, "bar");

        System.out.println(foo1.equals(foo2)); // prints true
    }
}

```

Here the overridden `equals()` method decides that the objects are equal if their fields are the same.

Notice that the `hashCode()` method was also overwritten. The contract for that method states that when two objects are equal, their hash values must also be the same. That's why one must almost always override `hashCode()` and `equals()` together.

Pay special attention to the argument type of the `equals` method. It is `Object obj`, not `Foo obj`. If you put the latter in your method, that is not an override of the `equals` method.

When writing your own class, you will have to write similar logic when overriding `equals()` and `hashCode()`. Most IDEs can automatically generate this for you.

An example of an `equals()` implementation can be found in the `String` class, which is part of the core Java API. Rather than comparing pointers, the `String` class compares the content of the `String`.

Java 1.7 introduced the `java.util.Objects` class which provides a convenience method, `equals`, that compares two potentially `null` references, so it can be used to simplify implementations of the `equals` method.

```java
@Override
public boolean equals(Object obj) {
    if (this == obj) {
        return true;
    }
    if (obj == null || getClass() != obj.getClass()) {
        return false;
    }

    Foo f = (Foo) obj;
    return field1 == f.field1 && field2 == f.field2 && Objects.equals(field3, f.field3);
}

```

### Class Comparison

Since the equals method can run against any object, one of the first things the method often does (after checking for `null`) is to check if the class of the object being compared matches the current class.

```java
@Override
public boolean equals(Object obj) {
    //...check for null
    if (getClass() != obj.getClass()) {
        return false;
    }
    //...compare fields
}

```

This is typically done as above by comparing the class objects.  However, that can fail in a few special cases which may not be obvious.  For example, some frameworks generate dynamic proxies of classes and these dynamic proxies are actually a different class.  Here is an example using JPA.

```java
Foo detachedInstance = ...
Foo mergedInstance = entityManager.merge(detachedInstance);
if (mergedInstance.equals(detachedInstance)) {
    //Can never get here if equality is tested with getClass()
    //as mergedInstance is a proxy (subclass) of Foo
}

```

One mechanism to work around that limitation is to compare classes using `instanceof`

```java
@Override
public final boolean equals(Object obj) {
    if (!(obj instanceof Foo)) {
        return false;
    }
    //...compare fields
}

```

However, there are a few pitfalls that must be avoided when using `instanceof`.  Since Foo could potentially have other subclasses and those subclasses might override `equals()` you could get into a case where a `Foo` is equal to a `FooSubclass` but the `FooSubclass` is not equal to `Foo`.

```java
Foo foo = new Foo(7);
FooSubclass fooSubclass = new FooSubclass(7, false);
foo.equals(fooSubclass) //true
fooSubclass.equals(foo) //false

```

This violates the properties of symmetry and transitivity and thus is an invalid implementation of the `equals()` method.  As a result, when using `instanceof`, a good practice is to make the `equals()` method `final` (as in the above example).  This will ensure that no subclass overrides `equals()` and violates key assumptions.



## wait() and notify() methods


`wait()` and `notify()` work in tandem – when one thread calls `wait()` on an object, that thread will block until another thread calls `notify()` or `notifyAll()` on that same object.

(See Also: [wait()/notify()](http://stackoverflow.com/documentation/java/5409/wait-notify#t=20160811161648303307) )

```java
package com.example.examples.object;

import java.util.concurrent.atomic.AtomicBoolean;

public class WaitAndNotify {

    public static void main(String[] args) throws InterruptedException {
        final Object obj = new Object();
        AtomicBoolean aHasFinishedWaiting = new AtomicBoolean(false);
    
        Thread threadA = new Thread("Thread A") {
            public void run() {
                System.out.println("A1: Could print before or after B1");
                System.out.println("A2: Thread A is about to start waiting...");
                try {
                    synchronized (obj) { // wait() must be in a synchronized block
                        // execution of thread A stops until obj.notify() is called
                        obj.wait();
                    }
                    System.out.println("A3: Thread A has finished waiting. "
                            + "Guaranteed to happen after B3");
                } catch (InterruptedException e) {
                    System.out.println("Thread A was interrupted while waiting");
                } finally {
                    aHasFinishedWaiting.set(true);
                }
            }
        };
    
        Thread threadB = new Thread("Thread B") {
            public void run() {
                System.out.println("B1: Could print before or after A1");

                System.out.println("B2: Thread B is about to wait for 10 seconds");
                for (int i = 0; i < 10; i++) {
                    try {                        
                        Thread.sleep(1000); // sleep for 1 second 
                    } catch (InterruptedException e) {
                        System.err.println("Thread B was interrupted from waiting");
                    }
                }
            
                System.out.println("B3: Will ALWAYS print before A3 since "
                        + "A3 can only happen after obj.notify() is called.");
            
                while (!aHasFinishedWaiting.get()) {
                    synchronized (obj) {
                        // notify ONE thread which has called obj.wait()
                        obj.notify();
                    }
                }
            }
        };
    
        threadA.start();
        threadB.start();
    
        threadA.join();
        threadB.join();
    
        System.out.println("Finished!");
    }
}

```

Some example output:

```java
A1: Could print before or after B1
B1: Could print before or after A1
A2: Thread A is about to start waiting...
B2: Thread B is about to wait for 10 seconds
B3: Will ALWAYS print before A3 since A3 can only happen after obj.notify() is called.
A3: Thread A has finished waiting. Guaranteed to happen after B3
Finished!

B1: Could print before or after A1
B2: Thread B is about to wait for 10 seconds
A1: Could print before or after B1
A2: Thread A is about to start waiting...
B3: Will ALWAYS print before A3 since A3 can only happen after obj.notify() is called.
A3: Thread A has finished waiting. Guaranteed to happen after B3
Finished!

A1: Could print before or after B1
A2: Thread A is about to start waiting...
B1: Could print before or after A1
B2: Thread B is about to wait for 10 seconds
B3: Will ALWAYS print before A3 since A3 can only happen after obj.notify() is called.
A3: Thread A has finished waiting. Guaranteed to happen after B3
Finished!

```



## getClass() method


The `getClass()` method can be used to find the runtime class type of an object. See the example below:

```java
public class User {
   
    private long userID;
    private String name;

    public User(long userID, String name) {
        this.userID = userID;
        this.name = name;
    }
}

public class SpecificUser extends User {
    private String specificUserID;

    public SpecificUser(String specificUserID, long userID, String name) {
        super(userID, name);
        this.specificUserID = specificUserID;
    }
}

public static void main(String[] args){
    User user = new User(879745, "John");
    SpecificUser specificUser = new SpecificUser("1AAAA", 877777, "Jim");
    User anotherSpecificUser = new SpecificUser("1BBBB", 812345, "Jenny");

    System.out.println(user.getClass()); //Prints "class User"
    System.out.println(specificUser.getClass()); //Prints "class SpecificUser"
    System.out.println(anotherSpecificUser.getClass()); //Prints "class SpecificUser"
}

```

The `getClass()` method will return the most specific class type, which is why when `getClass()` is called on `anotherSpecificUser`, the return value is `class SpecificUser` because that is lower down the inheritance tree than `User`.

It is noteworthy that, while the `getClass` method is declared as:

```java
public final native Class<?> getClass();

```

The actual static type returned by a call to `getClass` is `Class<? extends T>` where `T` is the static type of the object on which `getClass` is called.

i.e. the following will compile:

```java
Class<? extends String> cls = "".getClass();

```



## clone() method


The `clone()` method is used to create and return a copy of an object. This method arguable should be avoided as it is problematic and a copy constructor or some other approach for copying should be used in favour of `clone()`.

For the method to be used all classes calling the method must implement the `Cloneable` interface.

The `Cloneable` interface itself is just a tag interface used to change the behaviour of the `native` `clone()` method which checks if the calling objects class implements `Cloneable`. If the caller does not implement this interface a `CloneNotSupportedException` will be thrown.

The `Object` class itself does not implement this interface so a `CloneNotSupportedException` will be thrown if the calling object is of class `Object`.

For a clone to be correct it should be independent of the object it is being cloned from, therefore it may be necessary to modify the object before it gets returned. This means to essentially create a "deep copy" by also copying any of the **mutable** objects that make up the internal structure of the object being cloned. If this is not implemented correctly the cloned object will not be independent and have the same references to the mutable objects as the object that it was cloned from. This would result in inconsistent behaviour as any changes to those in one would affect the other.

```java
class Foo implements Cloneable {
    int w;
    String x;
    float[] y;
    Date z;
    
    public Foo clone() {
        try {
            Foo result = new Foo();
            // copy primitives by value
            result.w = this.w;
            // immutable objects like String can be copied by reference
            result.x = this.x;
            
            // The fields y and z refer to a mutable objects; clone them recursively.
            if (this.y != null) {
              result.y = this.y.clone();
            }
            if (this.z != null) {
              result.z = this.z.clone();
            }
            
            // Done, return the new object
            return result;
            
        } catch (CloneNotSupportedException e) {
            // in case any of the cloned mutable fields do not implement Cloneable
            throw new AssertionError(e);
        }
    }
}

```



## finalize() method


This is a **protected** and **non-static** method of the `Object` class. This method is used to perform some final operations or clean up operations on an object before it gets removed from the memory.

> 
According to the doc, this method gets called by the garbage collector on an object when garbage collection determines that there are no more references to the object.


But there are no guarantees that `finalize()` method would gets called if the object is still reachable or no Garbage Collectors run when the object become eligible.
That's why it's better **not rely** on this method.

In Java core libraries some usage examples could be found, for instance in `FileInputStream.java`:

```java
protected void finalize() throws IOException {
    if ((fd != null) &&  (fd != FileDescriptor.in)) {
        /* if fd is shared, the references in FileDescriptor
         * will ensure that finalizer is only called when
         * safe to do so. All references using the fd have
         * become unreachable. We can call close()
         */
        close();
    }
}

```

In this case it's the last chance to close the resource if that resource has not been closed before.

Generally it's considered bad practice to use `finalize()` method in applications of any kind and should be avoided.

Finalizers are **not** meant for freeing resources (e.g., closing files).  The garbage collector gets called when (if!) the system runs low on heap space.  You can't rely on it to be called when the system is running low on file handles or, for any other reason.

The intended use-case for finalizers is for an object that is about to be reclaimed to notify some other object about its impending doom.  A better mechanism now exists for that purpose---the `java.lang.ref.WeakReference<T>` class.  If you think you need write a `finalize()` method, then you should look into whether you can solve the same problem using `WeakReference` instead.  If that won't solve your problem, then you may need to re-think your design on a deeper level.

For further reading [here](http://www.informit.com/articles/article.aspx?p=1216151&seqNum=7) is an Item about `finalize()` method from "Effective Java" book by Joshua Bloch.



## Object constructor


All constructors in Java must make a call to the `Object` constructor. This is done with the call `super()`. This has to be the first line in a constructor. The reason for this is so that the object can actually be created on the heap before any additional initialization is performed.

If you do not specify the call to `super()` in a constructor the compiler will put it in for you.

So all three of these examples are functionally identical

with explicit call to `super()` constructor

```java
public class MyClass {

    public MyClass() {
        super();
    }
}

```

with implicit call to `super()` constructor

```java
public class MyClass {

    public MyClass() {
        // empty
    }
}

```

with implicit constructor

```java
public class MyClass {

}

```

> 
What about Constructor-Chaining?


It is possible to call other constructors as the first instruction of a constructor. As both the explicit call to a super constructor and the call to another constructor have to be both first instructions, they are mutually exclusive.

```java
public class MyClass {

    public MyClass(int size) {

        doSomethingWith(size);

    }

    public MyClass(Collection<?> initialValues) {

        this(initialValues.size());
        addInitialValues(initialValues);
    }
}

```

Calling new `MyClass(Arrays.asList("a", "b", "c"))` will call the second constructor with the List-argument, which will in turn delegate to the first constructor (which will delegate implicitly to `super()`) and then call `addInitialValues(int size)` with the second size of the list. This is used to reduce code duplication where multiple constructors need to do the same work.

> 
How do I call a specific constructor?


Given the example above, one can either call `new MyClass("argument")` or `new MyClass("argument", 0)`. In other words, much like [method overloading](http://stackoverflow.com/documentation/java/980/polymorphism/3187/method-overloading#t=201608202249467776787), you just call the constructor with the parameters that are necessary for your chosen constructor.

> 
What will happen in the Object class constructor?


Nothing more than would happen in a sub-class that has a default empty constructor (minus the call to `super()`).

The default empty constructor can be explicitly defined but if not the compiler will put it in for you as long as no other constructors are already defined.

> 
How is an Object then created from the constructor in Object?


The actual creation of objects is down to the JVM. Every constructor in Java appears as a special method named `<init>` which is responsible for instance initializing. This `<init>` method is supplied by the compiler and because `<init>` is not a valid identifier in Java, it cannot be used directly in the language.

> 
How does the JVM invoke this `<init>` method?


The JVM will invoke the `<init>` method using the `invokespecial` instruction and can only be invoked on uninitialized class instances.

For more information take a look at the JVM specification and the Java Language Specification:

- Special Methods (JVM) - [JVMS - 2.9](https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html)
- Constructors - [JLS - 8.8](https://docs.oracle.com/javase/specs/jls/se8/html/jls-8.html)



#### Syntax


- public final native Class<?> getClass()
- public final native void notify()
- public final native void notifyAll()
- public final native void wait(long timeout) throws InterruptedException
- public final void wait() throws InterruptedException
- public final void wait(long timeout, int nanos)  throws InterruptedException
- public native int hashCode()
- public boolean equals(Object obj)
- public String toString()
- protected native Object clone() throws CloneNotSupportedException
- protected void finalize() throws Throwable

