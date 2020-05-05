---
metaTitle: "Java - Singletons"
description: "Enum Singleton, Singleton without use of Enum (eager initialization), Thread-safe lazy initialization using holder class | Bill Pugh Singleton implementation, Thread safe Singleton with double checked locking, Extending singleton (singleton inheritance)"
---

# Singletons


A singleton is a class that only ever has one single instance.  For more information on the Singleton **design pattern**, please refer to the [Singleton](http://stackoverflow.com/documentation/design-patterns/2179/singleton#t=201704180737023547863) topic in the [Design Patterns](http://stackoverflow.com/documentation/design-patterns/topics) tag.



## Enum Singleton


```java
public enum Singleton {
    INSTANCE;

    public void execute (String arg) {
        // Perform operation here 
    }
}

```

[Enums](http://stackoverflow.com/documentation/java/155/enums) have private constructors, are final and provide proper serialization machinery. They are also very concise and lazily initialized in a thread safe manner.

The JVM provides a guarantee that enum values will not be instantiated more than once each, which gives the enum singleton pattern a very strong defense against reflection attacks.

What the enum pattern **doesn't** protect against is other developers physically adding more elements to the source code. Consequently, if you choose this implementation style for your singletons it is imperative that you very clearly document that no new values should be added to those enums.

This is the recommended way of implementing the singleton pattern, as [explained](http://www.informit.com/articles/article.aspx?p=1216151&seqNum=3) by Joshua Bloch in Effective Java.



## Singleton without use of Enum (eager initialization)


```java
public class Singleton {    

    private static final Singleton INSTANCE = new Singleton();

    private Singleton() {}

    public static Singleton getInstance() {
        return INSTANCE;
    }
}

```

It can be argued that this example is **effectively** lazy initialization. [Section 12.4.1 of the Java Language Specification](https://docs.oracle.com/javase/specs/jls/se7/html/jls-12.html#jls-12.4.1) states:

> 
<p>A class or interface type T will be initialized immediately before the
first occurrence of any one of the following:</p>
<ul>
- T is a class and an instance of T is created
- T is a class and a static method declared by T is invoked
- A static field declared by T is assigned
- A static field declared by T is used and the field is not a constant variable
- T is a top level class, and an assert statement  lexically nested within T is executed.
</ul>


Therefore, as long as there are no other static fields or static methods in the class, the `Singleton` instance will not be initialized until the method `getInstance()` is invoked the first time.



## Thread-safe lazy initialization using holder class | Bill Pugh Singleton implementation


```java
public class Singleton {
    private static class InstanceHolder {
        static final Singleton INSTANCE = new Singleton();
    }

    public static Singleton getInstance() {
        return InstanceHolder.INSTANCE;
    }

    private Singleton() {}
}

```

This initializes the `INSTANCE` variable on the first call to `Singleton.getInstance()`, taking advantage of the language's thread safety guarantees for static initialization without requiring additional synchronization.

This implementation is also known as Bill Pugh singleton pattern.[ [Wiki]](https://en.wikipedia.org/wiki/Singleton_pattern#Initialization-on-demand_holder_idiom)



## Thread safe Singleton with double checked locking


This type of Singleton is thread safe, and prevents unnecessary locking after the Singleton instance has been created.

```java
public class MySingleton {

    // instance of class
    private static volatile MySingleton instance = null;

    // Private constructor
    private MySingleton() {
        // Some code for constructing object
    }

    public static MySingleton getInstance() {
        MySingleton result = instance;
        
        //If the instance already exists, no locking is necessary
        if(result == null) {
            //The singleton instance doesn't exist, lock and check again
            synchronized(MySingleton.class) {
                result = instance;
                if(result == null) {
                    instance = result = new MySingleton();
                }
            }
        }
        return result;
    }
}

```

It must be emphasized -- in versions prior to Java SE 5, the implementation above is [incorrect](http://www.cs.umd.edu/%7Epugh/java/memoryModel/DoubleCheckedLocking.html) and should be avoided.  It is not possible to implement double-checked locking correctly in Java prior to Java 5.



## Extending singleton (singleton inheritance)


In this example, base class `Singleton` provides `getMessage()` method that returns `"Hello world!"` message.

It's subclasses `UppercaseSingleton` and `LowercaseSingleton` override getMessage() method to provide appropriate representation of the message.

```java
//Yeah, we'll need reflection to pull this off.
import java.lang.reflect.*;

/*
Enumeration that represents possible classes of singleton instance.
If unknown, we'll go with base class - Singleton.
*/
enum SingletonKind {
    UNKNOWN,
    LOWERCASE,
    UPPERCASE
}

//Base class
class Singleton{

    /*
    Extended classes has to be private inner classes, to prevent extending them in 
    uncontrolled manner.
     */
    private class UppercaseSingleton extends Singleton {

        private UppercaseSingleton(){
            super();
        }

        @Override
        public String getMessage() {
            return super.getMessage().toUpperCase();
        }
    }

    //Another extended class.
    private class LowercaseSingleton extends Singleton
    {
        private LowercaseSingleton(){
            super();
        }

        @Override
        public String getMessage() {
            return super.getMessage().toLowerCase();
        }
    }

    //Applying Singleton pattern
    private static SingletonKind kind = SingletonKind.UNKNOWN;

    private static Singleton instance;

    /*
    By using this method prior to getInstance() method, you effectively change the
    type of singleton instance to be created.
     */
    public static void setKind(SingletonKind kind) {
        Singleton.kind = kind;
    }

    /*
    If needed, getInstance() creates instance appropriate class, based on value of
    singletonKind field.
     */
    public static Singleton getInstance() 
        throws  NoSuchMethodException, 
                IllegalAccessException, 
                InvocationTargetException, 
                InstantiationException {

        if(instance==null){
            synchronized (Singleton.class){
                if(instance==null){
                    Singleton singleton = new Singleton();
                    switch (kind){
                        case UNKNOWN:

                            instance = singleton;
                            break;

                        case LOWERCASE:

                            /*
                             I can't use simple

                             instance = new LowercaseSingleton();

                             because java compiler won't allow me to use
                             constructor of inner class in static context,
                             so I use reflection API instead.

                             To be able to access inner class by reflection API,
                             I have to create instance of outer class first.
                             Therefore, in this implementation, Singleton cannot be
                             abstract class.
                             */

                            //Get the constructor of inner class.
                            Constructor<LowercaseSingleton> lcConstructor =
                                    LowercaseSingleton.class.getDeclaredConstructor(Singleton.class);

                            //The constructor is private, so I have to make it accessible.
                            lcConstructor.setAccessible(true);

                            // Use the constructor to create instance.
                            instance = lcConstructor.newInstance(singleton);

                            break;

                        case UPPERCASE:

                            //Same goes here, just with different type
                            Constructor<UppercaseSingleton> ucConstructor =
                                    UppercaseSingleton.class.getDeclaredConstructor(Singleton.class);
                            ucConstructor.setAccessible(true);
                            instance = ucConstructor.newInstance(singleton);
                    }
                }
            }
        }
        return instance;
    }

    //Singletons state that is to be used by subclasses
    protected String message;

    //Private constructor prevents external instantiation.
    private Singleton()
    {
        message = "Hello world!";
    }

    //Singleton's API. Implementation can be overwritten by subclasses.
    public String getMessage() {
        return message;
    }
}

//Just a small test program
public class ExtendingSingletonExample {

    public static void main(String args[]){

        //just uncomment one of following lines to change singleton class

        //Singleton.setKind(SingletonKind.UPPERCASE);
        //Singleton.setKind(SingletonKind.LOWERCASE);

        Singleton singleton = null;
        try {
            singleton = Singleton.getInstance();
        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        }
        System.out.println(singleton.getMessage());
    }
}

```

