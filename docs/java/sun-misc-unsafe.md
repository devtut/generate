---
metaTitle: "sun.misc.Unsafe"
description: "Instantiating sun.misc.Unsafe via reflection, Instantiating sun.misc.Unsafe via bootclasspath, Getting Instance of Unsafe, Uses of Unsafe"
---

# sun.misc.Unsafe



## Instantiating sun.misc.Unsafe via reflection


```java
public static Unsafe getUnsafe() {
    try {
        Field unsafe = Unsafe.class.getDeclaredField("theUnsafe");
        unsafe.setAccessible(true);
        return (Unsafe) unsafe.get(null);
    } catch (IllegalAccessException e) {
        // Handle
    } catch (IllegalArgumentException e) {
        // Handle
    } catch (NoSuchFieldException e) {
        // Handle
    } catch (SecurityException e) {
        // Handle
    }
}

```

`sun.misc.Unsafe` has a Private constructor, and the static `getUnsafe()` method is guarded with a check of the classloader to ensure that the code was loaded with the primary classloader. Therefore, one method of loading the instance is to use reflection to get the static field.



## Instantiating sun.misc.Unsafe via bootclasspath


```java
public class UnsafeLoader {
    public static Unsafe loadUnsafe() {
        return Unsafe.getUnsafe();
    }
}

```

While this example will compile, it is likely to fail at runtime unless the Unsafe class was loaded with the primary classloader. To ensure that happens the JVM should be loaded with the appropriate arguments, like:

```java
java -Xbootclasspath:$JAVA_HOME/jre/lib/rt.jar:./UnsafeLoader.jar foo.bar.MyApp

```

The `foo.bar.MyApp` class can then use `UnsafeLoader.loadUnsafe()`.



## Getting Instance of Unsafe


Unsafe is stored as a private field that cannot be accessed directly. The constructor is private and the only method to access `public static Unsafe getUnsafe()` has privileged access. By use of reflection, there is a work-around to make private fields accessible:

```java
public static final Unsafe UNSAFE;

static {
    Unsafe unsafe = null;

    try {
        final PrivilegedExceptionAction<Unsafe> action = () -> {
            final Field f = Unsafe.class.getDeclaredField("theUnsafe");
            f.setAccessible(true);

            return (Unsafe) f.get(null);
        };

        unsafe = AccessController.doPrivileged(action);
    } catch (final Throwable t) {
        throw new RuntimeException("Exception accessing Unsafe", t);
    }

    UNSAFE = unsafe;
}

```



## Uses of Unsafe


Some uses of unsafe is s follows:

|Use|API
|------
|Off heap / direct memory allocation, reallocation and deallocation|`allocateMemory(bytes)`, `reallocateMemory(address, bytes)` and `freeMemory(address)`
|Memory fences|`loadFence()`, `storeFence()`, `fullFence()`
|Parking current thread|`park(isAbsolute, time)`, `unpark(thread)`
|Direct field and or memory access|`get*` and `put*` family of methods
|Throwing unchecked exceptions|`throwException(e)`
|CAS and Atomic Operations|`compareAndSwap*` family of methods
|Setting out memory|`setMemory`
|Volatile or concurrent operations|`get*Volatile`, `put*Volatile`, `putOrdered*`

The get and put family of methods are relative to a given object. If the object is null then it is treated as an absolute address.

```java
// Putting a value to a field
protected static long fieldOffset = UNSAFE.objectFieldOffset(getClass().getField("theField"));
UNSAFE.putLong(this, fieldOffset , newValue);

// Puting an absolute value
 UNSAFE.putLong(null, address, newValue);
 UNSAFE.putLong(address, newValue);

```

Some methods are only defined for int and longs. You can use these methods on floats and doubles using `floatToRawIntBits`, `intBitsToFloat,`doubleToRawLongBits`,`longBitsToDouble`



#### Remarks


The `Unsafe` class allows a program to do things that are not allowed by the Java compiler.  Normal programs should avoid using `Unsafe`.

**WARNINGS**

<li>
If you make a mistake using the `Unsafe` APIs, your applications are liable to cause the JVM to crash and/or exhibit symptoms that are hard to diagnose.
</li>
<li>
The `Unsafe` API is subject to change without notice.  If you use it in your code, you may need to rewrite the code when changing Java versions.
</li>

