---
metaTitle: "Reflection API"
description: "Introduction, Dynamic Proxies, Getting and Setting fields, Misuse of Reflection API to change private and final variables, Evil Java hacks with Reflection, Call constructor, Invoking a method, Getting the Constants of an Enumeration, Get Class given its (fully qualified) name, Call overloaded constructors using reflection, Call constructor of nested class"
---

# Reflection API


Reflection is commonly used by programs which require the ability to examine or modify the runtime behavior of applications running in the JVM. [Java Reflection API](https://docs.oracle.com/javase/tutorial/reflect/) is used for that purpose where it makes it possible to inspect classes, interfaces, fields and methods at runtime, without knowing their names at compile time. And It also makes it possible to instantiate new objects, and to invoke methods using reflection.



## Introduction


**Basics**

The Reflection API allows one to check the class structure of the code at runtime and invoke code dynamically. This is very powerful, but it is also dangerous since the compiler is not able to statically determine whether dynamic invocations are valid.

A simple example would be to get the public constructors and methods of a given class:

```java
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

// This is a object representing the String class (not an instance of String!)
Class<String> clazz = String.class;

Constructor<?>[] constructors = clazz.getConstructors(); // returns all public constructors of String
Method[] methods = clazz.getMethods(); // returns all public methods from String and parents

```

With this information it is possible to instance the object and call different methods dynamically.

**Reflection and Generic Types**

Generic type information is available for:

- method parameters, using `getGenericParameterTypes()`.
- method return types, using `getGenericReturnType()`.
- **public** fields, using `getGenericType`.

The following example shows how to extract the generic type information in all three cases:

```java
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;

public class GenericTest {

    public static void main(final String[] args) throws Exception {
        final Method method = GenericTest.class.getMethod("testMethod", Map.class);
        final Field field = GenericTest.class.getField("testField");

        System.out.println("Method parameter:");
        final Type parameterType = method.getGenericParameterTypes()[0];
        displayGenericType(parameterType, "\t");

        System.out.println("Method return type:");
        final Type returnType = method.getGenericReturnType();
        displayGenericType(returnType, "\t");

        System.out.println("Field type:");
        final Type fieldType = field.getGenericType();
        displayGenericType(fieldType, "\t");

    }

    private static void displayGenericType(final Type type, final String prefix) {
        System.out.println(prefix + type.getTypeName());
        if (type instanceof ParameterizedType) {
            for (final Type subtype : ((ParameterizedType) type).getActualTypeArguments()) {
                displayGenericType(subtype, prefix + "\t");
            }
        }

    }

    public Map<String, Map<Integer, List<String>>> testField;

    public List<Number> testMethod(final Map<String, Double> arg) {
        return null;
    }

}

```

This results in the following output:

```java
Method parameter:
    java.util.Map<java.lang.String, java.lang.Double>
        java.lang.String
        java.lang.Double
Method return type:
    java.util.List<java.lang.Number>
        java.lang.Number
Field type:
    java.util.Map<java.lang.String, java.util.Map<java.lang.Integer, java.util.List<java.lang.String>>>
        java.lang.String
        java.util.Map<java.lang.Integer, java.util.List<java.lang.String>>
            java.lang.Integer
            java.util.List<java.lang.String>
                java.lang.String

```



## Dynamic Proxies


Dynamic Proxies do not really have much to do with Reflection but they are part of the API. It's basically a way to create a dynamic implementation of an interface. This could be helpful when creating mockup services.<br> A Dynamic Proxy is an instance of an interface that is created with a so-called invocation handler that intercepts all method calls and allows the handling of their invocation manually.

```java
public class DynamicProxyTest {

    public interface MyInterface1{
        public void someMethod1();
        public int someMethod2(String s);
    }

    public interface MyInterface2{
        public void anotherMethod();
    }
   
    public static void main(String args[]) throws Exception {
        // the dynamic proxy class 
        Class<?> proxyClass = Proxy.getProxyClass(
                ClassLoader.getSystemClassLoader(),
                new Class[] {MyInterface1.class, MyInterface2.class});
        // the dynamic proxy class constructor
        Constructor<?> proxyConstructor = 
            proxyClass.getConstructor(InvocationHandler.class);

        // the invocation handler
        InvocationHandler handler = new InvocationHandler(){
            // this method is invoked for every proxy method call
            // method is the invoked method, args holds the method parameters
            // it must return the method result
            @Override
            public Object invoke(Object proxy, Method method, Object[] args) throws Throwable { 
                String methodName = method.getName();

                if(methodName.equals("someMethod1")){
                    System.out.println("someMethod1 was invoked!");
                    return null;
                }
                if(methodName.equals("someMethod2")){
                    System.out.println("someMethod2 was invoked!");
                    System.out.println("Parameter: " + args[0]);
                    return 42;
                }
                if(methodName.equals("anotherMethod")){
                    System.out.println("anotherMethod was invoked!");
                    return null;
                }
                System.out.println("Unkown method!");
                return null;
            }
        };

        // create the dynamic proxy instances
        MyInterface1 i1 = (MyInterface1) proxyConstructor.newInstance(handler);
        MyInterface2 i2 = (MyInterface2) proxyConstructor.newInstance(handler);

        // and invoke some methods
        i1.someMethod1();
        i1.someMethod2("stackoverflow");
        i2.anotherMethod();
    }
}

```

The result of this code is this:

```java
someMethod1 was invoked!
someMethod2 was invoked!
Parameter: stackoverflow
anotherMethod was invoked!

```



## Getting and Setting fields


Using the Reflection API, it is possible to change or get the value of a field at runtime. For example, you could use it in an API to retrieve different fields based on a factor, like the OS. You can also remove modifiers like `final` to allow modifing fields that are final.

To do so, you will need to use the method [Class#getField()](https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getField-java.lang.String-) in a way such as the one shown below:

```java
// Get the field in class SomeClass "NAME".
Field nameField = SomeClass.class.getDeclaredField("NAME");

// Get the field in class Field "modifiers". Note that it does not 
// need to be static
Field modifiersField = Field.class.getDeclaredField("modifiers");

// Allow access from anyone even if it's declared private
modifiersField.setAccessible(true);

// Get the modifiers on the "NAME" field as an int.
int existingModifiersOnNameField = nameField.getModifiers();

// Bitwise AND NOT Modifier.FINAL (16) on the existing modifiers
// Readup here https://en.wikipedia.org/wiki/Bitwise_operations_in_C
// if you're unsure what bitwise operations are.
int newModifiersOnNameField = existingModifiersOnNameField & ~Modifier.FINAL;

// Set the value of the modifiers field under an object for non-static fields
modifiersField.setInt(nameField, newModifiersOnNameField);

// Set it to be accessible. This overrides normal Java 
// private/protected/package/etc access control checks.
nameField.setAccessible(true);

 // Set the value of "NAME" here. Note the null argument. 
 // Pass null when modifying static fields, as there is no instance object
nameField.set(null, "Hacked by reflection...");

// Here I can directly access it. If needed, use reflection to get it. (Below)
System.out.println(SomeClass.NAME);

```

Getting fields is much easier. We can use [Field#get()](https://docs.oracle.com/javase/8/docs/api/java/lang/reflect/Field.html#get-java.lang.Object-) and its variants to get its value:

```java
// Get the field in class SomeClass "NAME".
Field nameField = SomeClass.class.getDeclaredField("NAME");

// Set accessible for private fields
nameField.setAccessible(true);

// Pass null as there is no instance, remember?
String name = (String) nameField.get(null);

```

Do note this:

When using [Class#getDeclaredField](https://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getDeclaredField-java.lang.String-), use it to get a field in the class itself:

```java
class HackMe extends Hacked {
    public String iAmDeclared;
}

class Hacked {
    public String someState;
}

```

Here, `HackMe#iAmDeclared` is declared field.
However, `HackMe#someState` is not a declared field as it is inherited from its superclass, Hacked.



## Misuse of Reflection API to change private and final variables


Reflection is useful when it is properly used for right purpose. By using reflection, you can access private variables and re-initialize final variables.

Below is the code snippet, which is **not** recommended.

```java
import java.lang.reflect.*;

public class ReflectionDemo{
    public static void main(String args[]){
        try{
            Field[] fields = A.class.getDeclaredFields();
            A a = new A();
            for ( Field field:fields ) {
                if(field.getName().equalsIgnoreCase("name")){
                    field.setAccessible(true);
                    field.set(a, "StackOverFlow");
                    System.out.println("A.name="+field.get(a));
                }
                if(field.getName().equalsIgnoreCase("age")){
                    field.set(a, 20);
                    System.out.println("A.age="+field.get(a));
                }
                if(field.getName().equalsIgnoreCase("rep")){
                    field.setAccessible(true);
                    field.set(a,"New Reputation");
                    System.out.println("A.rep="+field.get(a));
                }
                if(field.getName().equalsIgnoreCase("count")){
                    field.set(a,25);
                    System.out.println("A.count="+field.get(a));
                }
            }                
        }catch(Exception err){
            err.printStackTrace();
        }
    }
}

class A {
    private String name;
    public int age;
    public final String rep;
    public static int count=0;
    
    public A(){
        name = "Unset";
        age = 0;
        rep = "Reputation";
        count++;
    }
}

```

Output:

```java
A.name=StackOverFlow
A.age=20
A.rep=New Reputation
A.count=25

```

Explanation:

In normal scenario, `private` variables can't be accessed outside of declared class ( without getter and setter methods). `final` variables can't be re-assigned after initialization.

`Reflection` breaks both barriers can be abused to change both private and final variables as explained above.

`field.setAccessible(true)` is the key to achieve desired functionality.



## Evil Java hacks with Reflection


The Reflection API could be used to change values of private and final fields even in the JDK default library. This could be used to manipulate the behaviour of some well known classes as we will see.

**What is not possible**

Lets start first with the only limitation means the only field we can't change with Reflection. That is the Java `SecurityManager`. It is declared in [java.lang.System](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/lang/System.java) as

```java
private static volatile SecurityManager security = null;

```

But it won't be listed in the System class if we run this code

```java
for(Field f : System.class.getDeclaredFields())
    System.out.println(f);

```

Thats because of the `fieldFilterMap` in [`sun.reflect.Reflection`](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/sun/reflect/Reflection.java) that holds the map itself and the security field
in the `System.class` and protects them against any access with Reflection.
So we could not deactivate the `SecurityManager`.

**Crazy Strings**

Each Java String is represented by the JVM as an instance of the `String` class.  However, in some situations the JVM saves heap space by using the same instance for Strings that are.  This happens for string literals, and also for strings that have been "interned" by calling `String.intern()`. So if you have `"hello"` in your code multiple times it is always the same object instance.

Strings are supposed to be immutable, but it is possible to use "evil" reflection to change them.  The example below show how we can change the characters in a String by replacing its `value` field.

```java
public class CrazyStrings {
    static {
        try {
            Field f = String.class.getDeclaredField("value");
            f.setAccessible(true);
            f.set("hello", "you stink!".toCharArray());
        } catch (Exception e) {
        }
    }
    public static void main(String args[])  {
        System.out.println("hello");
    }
}

```

So this code will print "you stink!"

**1 = 42**

The same idea could be used with the Integer Class

```java
public class CrazyMath {
    static {
        try {
            Field value = Integer.class.getDeclaredField("value");    
            value.setAccessible(true);          
            value.setInt(Integer.valueOf(1), 42);      
        } catch (Exception e) {
        }
    }
    public static void main(String args[])  {
        System.out.println(Integer.valueOf(1));
    }
}

```

**Everything is true**

And according to [this stackoverflow post](http://stackoverflow.com/questions/3301635/change-private-static-final-field-using-java-reflection) we can use reflection to do something really evil.

```java
public class Evil {    
    static {
        try {
            Field field = Boolean.class.getField("FALSE");
            field.setAccessible(true);
            Field modifiersField = Field.class.getDeclaredField("modifiers");
            modifiersField.setAccessible(true);
            modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
            field.set(null, true);
        } catch (Exception e) {
        }
    }
    public static void main(String args[]){
        System.out.format("Everything is %s", false);
    }
}

```

Note that what we are doing here is going to cause the JVM to behave in inexplicable ways.  This is very dangerous.



## Call constructor


### Getting the Constructor Object

You can obtain `Constructor` class from the `Class` object like this:

```java
Class myClass = ... // get a class object
Constructor[] constructors = myClass.getConstructors();

```

Where the `constructors` variable will have one `Constructor` instance for each public constructor declared in the class.

If you know the precise parameter types of the constructor you want to access, you can filter the specific constructor. The next example returns the public constructor of the given class which takes a `Integer` as parameter:

```java
Class myClass = ... // get a class object
Constructor constructor = myClass.getConstructor(new Class[]{Integer.class});

```

If no constructor matches the given constructor arguments a `NoSuchMethodException` is thrown.

### New Instance using Constructor Object

```java
Class myClass = MyObj.class // get a class object
Constructor constructor = myClass.getConstructor(Integer.class);
MyObj myObj = (MyObj) constructor.newInstance(Integer.valueOf(123));

```



## Invoking a method


Using reflection, a method of an object can be invoked during runtime.

The example shows how to invoke the methods of a `String` object.

```java
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

String s = "Hello World!";

// method without parameters
// invoke s.length()
Method method1 = String.class.getMethod("length");
int length = (int) method1.invoke(s); // variable length contains "12"

// method with parameters
// invoke s.substring(6)
Method method2 = String.class.getMethod("substring", int.class);
String substring = (String) method2.invoke(s, 6); // variable substring contains "World!"

```



## Getting the Constants of an Enumeration


Giving this enumeration as Example:

```java
enum Compass {
    NORTH(0),
    EAST(90),
    SOUTH(180),
    WEST(270);
    private int degree;
    Compass(int deg){
        degree = deg;
    }
    public int getDegree(){
        return degree;
    }
}

```

In Java an enum class is like any other class but has some definied constants for the enum values. Additionally it has a field that is an array that holds all the values and two static methods with name `values()` and `valueOf(String)`.<br>
We can see this if we use Reflection to print all fields in this class

```java
for(Field f : Compass.class.getDeclaredFields())
    System.out.println(f.getName());

```

the output will be:

> 
NORTH<br>EAST<br>SOUTH<br>WEST<br>degree<br>ENUM$VALUES


So we could examine enum classes with Reflection like any other class. But the Reflection API offers three enum-specific methods.

**enum check**

```java
Compass.class.isEnum();

```

Returns true for classes that represents an enum type.

**retrieving values**

```java
Object[] values = Compass.class.getEnumConstants();

```

Returns an array of all enum values like Compass.values() but without the need of an instance.

**enum constant check**

```java
for(Field f : Compass.class.getDeclaredFields()){
    if(f.isEnumConstant())
        System.out.println(f.getName());
}

```

Lists all the class fields that are enum values.



## Get Class given its (fully qualified) name


Given a `String` containing the name of a class, it's `Class` object can be accessed using `Class.forName`:

```java
Class clazz = null;
try {
    clazz = Class.forName("java.lang.Integer");
} catch (ClassNotFoundException ex) {
    throw new IllegalStateException(ex);
}

```

It can be specified, if the class should be initialized (second parameter of `forName`) and which `ClassLoader` should be used (third parameter):

```java
ClassLoader classLoader = ...
boolean initialize = ...
Class clazz = null;
try {
    clazz = Class.forName("java.lang.Integer", initialize, classLoader);
} catch (ClassNotFoundException ex) {
    throw new IllegalStateException(ex);
}

```



## Call overloaded constructors using reflection


**Example: Invoke different constructors by passing relevant parameters**

```java
import java.lang.reflect.*;

class NewInstanceWithReflection{
    public NewInstanceWithReflection(){
        System.out.println("Default constructor");
    }
    public NewInstanceWithReflection( String a){
        System.out.println("Constructor :String => "+a);
    }
    public static void main(String args[]) throws Exception {
        
        NewInstanceWithReflection object = (NewInstanceWithReflection)Class.forName("NewInstanceWithReflection").newInstance();
        Constructor constructor = NewInstanceWithReflection.class.getDeclaredConstructor( new Class[] {String.class});
        NewInstanceWithReflection object1 = (NewInstanceWithReflection)constructor.newInstance(new Object[]{"StackOverFlow"});
        
    }
}

```

output:

```java
Default constructor
Constructor :String => StackOverFlow

```

Explanation:

1. Create instance of class using `Class.forName` : It calls default constructor
1. Invoke `getDeclaredConstructor` of the class by passing type of parameters as `Class array`
1. After getting the constructor, create `newInstance` by passing parameter value as `Object array`



## Call constructor of nested class


If you want to create an instance of an inner nested class you need to provide a class object of the enclosing class as an extra parameter with [Class#getDeclaredConstructor](http://docs.oracle.com/javase/7/docs/api/java/lang/Class.html#getDeclaredConstructor%28java.lang.Class...%29).

```java
public class Enclosing{
    public class Nested{
    public Nested(String a){
            System.out.println("Constructor :String => "+a);
        }
    }       
    public static void main(String args[]) throws Exception {
        Class<?> clazzEnclosing = Class.forName("Enclosing");            
        Class<?> clazzNested = Class.forName("Enclosing$Nested");
        Enclosing objEnclosing = (Enclosing)clazzEnclosing.newInstance();
        Constructor<?> constructor = clazzNested.getDeclaredConstructor(new Class[]{Enclosing.class, String.class});
        Nested objInner = (Nested)constructor.newInstance(new Object[]{objEnclosing, "StackOverFlow"});
    }
}

```

If the nested class is static you will not need this enclosing instance.



#### Remarks


### Performance

Keep in mind that reflection might decrease performance, only use it when your task cannot be completed without reflection.

From the Java tutorial [The Reflection API](https://docs.oracle.com/javase/tutorial/reflect/) :

> 
<p>Because reflection involves types that are dynamically resolved, certain Java virtual machine optimizations can not be performed.
Consequently, reflective operations have slower performance than their
non-reflective counterparts, and should be avoided in sections of code
which are called frequently in performance-sensitive applications.</p>


