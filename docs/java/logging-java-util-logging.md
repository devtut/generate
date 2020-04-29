---
metaTitle: "Logging (java.util.logging)"
description: "Using the default logger, Logging complex messages (efficiently), Logging levels"
---

# Logging (java.util.logging)



## Using the default logger


This example shows how to use the default logging api.

```java
import java.util.logging.Level;
import java.util.logging.Logger;

public class MyClass {
    
    // retrieve the logger for the current class
    private static final Logger LOG = Logger.getLogger(MyClass.class.getName());
    
    public void foo() {
        LOG.info("A log message");
        LOG.log(Level.INFO, "Another log message");

        LOG.fine("A fine message");
    
        // logging an exception
        try {
            // code might throw an exception
        } catch (SomeException ex) {
            // log a warning printing "Something went wrong"
            // together with the exception message and stacktrace
            LOG.log(Level.WARNING, "Something went wrong", ex);
        }

        String s = "Hello World!";

        // logging an object
        LOG.log(Level.FINER, "String s: {0}", s);

        // logging several objects
        LOG.log(Level.FINEST, "String s: {0} has length {1}", new Object[]{s, s.length()});
    }

}

```



## Logging complex messages (efficiently)


Let's look at a sample of logging which you can see in many programs:

```java
public class LoggingComplex {
    
    private static final Logger logger = 
        Logger.getLogger(LoggingComplex.class.getName());

    private int total = 50, orders = 20;
    private String username = "Bob";

    public void takeOrder() {
        // (...) making some stuff
        logger.fine(String.format("User %s ordered %d things (%d in total)", 
                                  username, orders, total));
        // (...) some other stuff
    }

    // some other methods and calculations
}

```

The above example looks perfectly fine, but many programmers forgets that Java VM is stack machine. This means that all method's parameters are calculated **before** executing the method.

This fact is crucial for logging in Java, especially for logging something in low levels like `FINE`, `FINER`, `FINEST` which are disabled by default. Let's look at Java bytecode for the `takeOrder()` method.

The result for `javap -c LoggingComplex.class` is something like this:

```java
public void takeOrder();
    Code:
       0: getstatic     #27 // Field logger:Ljava/util/logging/Logger;
       3: ldc           #45 // String User %s ordered %d things (%d in total)
       5: iconst_3
       6: anewarray     #3  // class java/lang/Object
       9: dup
      10: iconst_0
      11: aload_0
      12: getfield      #40 // Field username:Ljava/lang/String;
      15: aastore
      16: dup
      17: iconst_1
      18: aload_0
      19: getfield      #36 // Field orders:I
      22: invokestatic  #47 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
      25: aastore
      26: dup
      27: iconst_2
      28: aload_0
      29: getfield      #34 // Field total:I
      32: invokestatic  #47 // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
      35: aastore
      36: invokestatic  #53 // Method java/lang/String.format:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/lang/String;
      39: invokevirtual #59 // Method java/util/logging/Logger.fine:(Ljava/lang/String;)V
      42: return

```

Line 39 runs the actual logging. All of the previous work (loading variables, creating new objects, concatenating Strings in `format` method) can be for nothing if logging level is set higher then `FINE` (and by default it is). Such logging can be very inefficient, consuming unnecessary memory and processor resources.

**That's why you should ask if the level you want to use is enabled.**

The right way should be:

```java
public void takeOrder() {
    // making some stuff
    if (logger.isLoggable(Level.FINE)) {
        // no action taken when there's no need for it
        logger.fine(String.format("User %s ordered %d things (%d in total)",
                                  username, orders, total));
    }
    // some other stuff
}

```

**Since Java 8:**

The Logger class has additional methods that take a `Supplier<String>` as parameter, which can simply be provided by a lambda:

```java
public void takeOrder() {
    // making some stuff
    logger.fine(() -> String.format("User %s ordered %d things (%d in total)",
            username, orders, total));
    // some other stuff
}

```

The Suppliers `get()`method - in this case the lambda - is only called when the corresponding level is enabled and so the `if`construction is not needed anymore.



## Logging levels


Java Logging Api has 7 [levels](https://docs.oracle.com/javase/8/docs/api/java/util/logging/Level.html). The levels in descending order are:

- `SEVERE` (highest value)
- `WARNING`
- `INFO`
- `CONFIG`
- `FINE`
- `FINER`
- `FINEST` (lowest value)

The default level is `INFO` (but this depends on the system and used a virtual machine).

**Note**:
There are also levels `OFF` (can be used to turn logging off) and `ALL` (the oposite of `OFF`).

Code example for this:

```java
import java.util.logging.Logger;

public class Levels {
    private static final Logger logger = Logger.getLogger(Levels.class.getName());

    public static void main(String[] args) {

        logger.severe("Message logged by SEVERE");
        logger.warning("Message logged by WARNING");
        logger.info("Message logged by INFO");
        logger.config("Message logged by CONFIG");
        logger.fine("Message logged by FINE");
        logger.finer("Message logged by FINER");
        logger.finest("Message logged by FINEST");

        // All of above methods are really just shortcut for
        // public void log(Level level, String msg):
        logger.log(Level.FINEST, "Message logged by FINEST");
    }
}

```

By default running this class will output only messages with level higher then `CONFIG`:

```java
Jul 23, 2016 9:16:11 PM LevelsExample main
SEVERE: Message logged by SEVERE
Jul 23, 2016 9:16:11 PM LevelsExample main
WARNING: Message logged by WARNING
Jul 23, 2016 9:16:11 PM LevelsExample main
INFO: Message logged by INFO

```

