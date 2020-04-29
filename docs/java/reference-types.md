---
metaTitle: "Reference Types"
description: "Different Reference Types"
---

# Reference Types



## Different Reference Types


[`java.lang.ref`](https://docs.oracle.com/javase/7/docs/api/java/lang/ref/package-summary.html) package provides reference-object classes, which support a limited degree of interaction with the garbage collector.

Java has four main different reference types. They are:

- Strong Reference
- Weak Reference
- Soft Reference
- Phantom Reference

**1. Strong Reference**

This is the usual form of creating objects.

```java
MyObject myObject = new MyObject();

```

The variable holder is holding a strong reference to the object created. As long as this variable is live and holds this value, the `MyObject` instance will not be collected by the garbage collector.

**2. Weak Reference**

When you do not want to keep an object longer, and you need to clear/free the memory allocated for an object as soon as possible, this is the way to do so.

```java
WeakReference myObjectRef = new WeakReference(MyObject);

```

Simply, a weak reference is a reference that isn't strong enough to force an object to remain in memory. Weak references allow you to leverage the garbage collector's ability to determine reachability for you, so you don't have to do it yourself.

When you need the object you created, just use `.get()` method:

```java
myObjectRef.get();

```

Following code will exemplify this:

```java
WeakReference myObjectRef = new WeakReference(MyObject);
System.out.println(myObjectRef.get()); // This will print the object reference address
System.gc();
System.out.println(myObjectRef.get()); // This will print 'null' if the GC cleaned up the object

```

**3. Soft Reference**

Soft references are slightly stronger than weak references. You can create a soft referenced object as following:

```java
SoftReference myObjectRef = new SoftReference(MyObject);

```

They can hold onto the memory more strongly than the weak reference. If you have enough memory supply/resources, garbage collector will not clean the soft references as enthusiastically as weak references.

Soft references are handy to use in caching. You can create soft referenced objects as a cache, where they kept until your memory runs out. When your memory can't supply enough resources, garbage collector will remove soft references.

```java
SoftReference myObjectRef = new SoftReference(MyObject);
System.out.println(myObjectRef.get()); // This will print the reference address of the Object
System.gc();
System.out.println(myObjectRef.get()); // This may or may not print the reference address of the Object

```

**4. Phantom Reference**

This is the weakest referencing type. If you created an object reference using Phantom Reference, the `get()` method will always return null!

The use of this referencing is that "Phantom reference objects, which are enqueued after the collector determines that their referents may otherwise be reclaimed. Phantom references are most often used for scheduling pre-mortem cleanup actions in a more flexible way than is possible with the Java finalization mechanism." - From [Phantom Reference Javadoc](https://docs.oracle.com/javase/8/docs/api/java/lang/ref/PhantomReference.html) from Oracle.

You can create an object of Phantom Reference as following:

```java
PhantomReference myObjectRef = new PhantomReference(MyObject);

```

