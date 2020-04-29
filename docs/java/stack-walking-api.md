---
metaTitle: "Stack-Walking API"
description: "Print all stack frames of the current thread, Print current caller class, Showing reflection and other hidden frames"
---

# Stack-Walking API


Prior to Java 9, access to the thread stack frames was limited to an internal class `sun.reflect.Reflection`. Specifically the method `sun.reflect.Reflection::getCallerClass`. Some libraries relies on this method which is deprecated.

An alternative standard API is now provided in JDK 9 via the `java.lang.StackWalker﻿` class, and is designed to be efficient by allowing lazy access to the stack frames. Some applications may use this API to traverse the execution stack and filter on classes.



## Print all stack frames of the current thread


The following prints all stack frames of the current thread:

```java
1  package test;
2  
3  import java.lang.StackWalker.StackFrame;
4  import java.lang.reflect.InvocationTargetException;
5  import java.lang.reflect.Method;
6  import java.util.List;
7  import java.util.stream.Collectors;
8  
9  public class StackWalkerExample {
10 
11    public static void main(String[] args) throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
12        Method fooMethod = FooHelper.class.getDeclaredMethod("foo", (Class<?>[])null);
13        fooMethod.invoke(null, (Object[]) null);
14    }
15 }
16
17 class FooHelper {
18    protected static void foo() {
19        BarHelper.bar();
20    }
21 }
22 
23 class BarHelper {
24    protected static void bar() {
25        List<StackFrame> stack = StackWalker.getInstance()
26                .walk((s) -> s.collect(Collectors.toList()));
27        for(StackFrame frame : stack) {
28            System.out.println(frame.getClassName() + " " + frame.getLineNumber() + " " + frame.getMethodName());
29        }
30    }
31 }

```

**Output:**

```java
test.BarHelper 26 bar
test.FooHelper 19 foo
test.StackWalkerExample 13 main

```



## Print current caller class


The following prints the current caller class. Note that in this case, the [`StackWalker`](http://download.java.net/java/jdk9/docs/api/java/lang/StackWalker.html) needs to be created with the option [`RETAIN_CLASS_REFERENCE`](http://download.java.net/java/jdk9/docs/api/java/lang/StackWalker.Option.html#RETAIN_CLASS_REFERENCE), so that `Class` instances are retained in the [`StackFrame`](http://download.java.net/java/jdk9/docs/api/java/lang/StackWalker.StackFrame.html) objects. Otherwise an exception would occur.

```java
public class StackWalkerExample {

    public static void main(String[] args) {
        FooHelper.foo();
    }

}

class FooHelper {
    protected static void foo() {
        BarHelper.bar();
    }
}

class BarHelper {
    protected static void bar() {
        System.out.println(StackWalker.getInstance(Option.RETAIN_CLASS_REFERENCE).getCallerClass());
    }
}

```

**Output:**

```java
class test.FooHelper

```



## Showing reflection and other hidden frames


A couple of other options allow stack traces to include implementation and/or reflection frames. This may be useful for debugging purposes. For instance, we can add the [`SHOW_REFLECT_FRAMES`](http://download.java.net/java/jdk9/docs/api/java/lang/StackWalker.Option.html#SHOW_REFLECT_FRAMES) option to the [`StackWalker`](http://download.java.net/java/jdk9/docs/api/java/lang/StackWalker.html) instance upon creation, so that the frames for the reflective methods are printed as well:

```java
package test;

import java.lang.StackWalker.Option;
import java.lang.StackWalker.StackFrame;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Collectors;

public class StackWalkerExample {

    public static void main(String[] args) throws NoSuchMethodException, SecurityException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        Method fooMethod = FooHelper.class.getDeclaredMethod("foo", (Class<?>[])null);
        fooMethod.invoke(null, (Object[]) null);
    }
}

class FooHelper {
    protected static void foo() {
        BarHelper.bar();
    }
}

class BarHelper {
    protected static void bar() {
        // show reflection methods
        List<StackFrame> stack = StackWalker.getInstance(Option.SHOW_REFLECT_FRAMES)
                .walk((s) -> s.collect(Collectors.toList()));
        for(StackFrame frame : stack) {
            System.out.println(frame.getClassName() + " " + frame.getLineNumber() + " " + frame.getMethodName());
        }
    }
}

```

**Output:**

```java
test.BarHelper 27 bar
test.FooHelper 20 foo
jdk.internal.reflect.NativeMethodAccessorImpl -2 invoke0
jdk.internal.reflect.NativeMethodAccessorImpl 62 invoke
jdk.internal.reflect.DelegatingMethodAccessorImpl 43 invoke
java.lang.reflect.Method 563 invoke
test.StackWalkerExample 14 main

```

Note that line numbers for some reflection methods may not be available so `StackFrame.getLineNumber()` may return negative values.

