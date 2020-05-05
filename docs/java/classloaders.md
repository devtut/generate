---
metaTitle: "Java - Classloaders"
description: "Implementing a custom classLoader, Loading an external .class file, Instantiating and using a classloader"
---

# Classloaders



## Implementing a custom classLoader


Every custom loader must directly or indirectly extend the `java.lang.ClassLoader` class.  The main **extension points** are the following methods:

- `findClass(String)` - overload this method if your classloader follows the standard delegation model for class loading.
- `loadClass(String, boolean)` - overload this method to implement an alternative delegation model.
- `findResource` and `findResources` - overload these methods to customize resource loading.

The `defineClass` methods which are responsible for actually loading the class from a byte array are `final` to prevent overloading.  Any custom behavior needs to be performed prior to calling `defineClass`.

Here is a simple that loads a specific class from a byte array:

```java
public class ByteArrayClassLoader extends ClassLoader {
    private String classname;
    private byte[] classfile;

    public ByteArrayClassLoader(String classname, byte[] classfile) {
        this.classname = classname;
        this.classfile = classfile.clone();
    }

    @Override
    protected Class findClass(String classname) throws ClassNotFoundException {
        if (classname.equals(this.classname)) {
            return defineClass(classname, classfile, 0, classfile.length);
        } else {
            throw new ClassNotFoundException(classname);
        }
    }
}

```

Since we have only overridden the `findClass` method, this custom class loader is going to behave as follows when `loadClass` is called.

1. The classloader's `loadClass` method calls `findLoadedClass` to see if a class with this name has already been loaded by this classloader.  If that succeeds, the resulting `Class` object  is returned to the requestor.
1. The `loadClass` method then delegates to the parent classloader by calling its `loadClass` call.  If the parent can deal with the request, it will return a `Class` object which is then returned to the requestor.
1. If the parent classloader cannot load the class, `findClass` then calls our override `findClass` method, passing the name of the class to be loaded.
1. If the requested name matches `this.classname`, we call `defineClass` to load the actual class from the `this.classfile` byte array.  The resulting `Class` object is then returned.
1. If the name did not match, we throw `ClassNotFoundException`.



## Loading an external .class file


To load a class we first need to define it. The class is defined by the `ClassLoader`. There's just one problem, Oracle didn't write the `ClassLoader`'s code with this feature available. To define the class we will need to access a method named `defineClass()` which is a private method of the `ClassLoader`.

To access it, what we will do is create a new class, `ByteClassLoader`, and extend it to `ClassLoader`. Now that we have extended our class to `ClassLoader`, we can access the `ClassLoader`'s private methods. To make `defineClass()` available, we will create a new method that will act like a mirror for the private `defineClass()` method. To call the private method we will need the class name, `name`, the class bytes, `classBytes`, the first byte's offset, which will be `0` because `classBytes`' data starts at  `classBytes[0]`, and the last byte's offset, which will be `classBytes.lenght` because it represents the size of the data, which will be the last offset.

```java
public class ByteClassLoader extends ClassLoader {

    public Class<?> defineClass(String name, byte[] classBytes) {
        return defineClass(name, classBytes, 0, classBytes.length);
    }

}

```

Now, we have a public `defineClass()` method. It can be called by passing the name of the class and the class bytes as arguments.

Let's say we have class named `MyClass` in the package `stackoverflow`...

To call the method we need the class bytes so we create a `Path` object representing our class' path by using the `Paths.get()` method and passing the path of the binary class as an argument. Now, we can get the class bytes with `Files.readAllBytes(path)`. So we create a `ByteClassLoader` instance and use the method we created, `defineClass()`. We already have the class bytes but to call our method we also need the class name which is given by the package name (dot) the class canonical name, in this case `stackoverflow.MyClass`.

```java
Path path = Paths.get("MyClass.class");

ByteClassLoader loader = new ByteClassLoader();
loader.defineClass("stackoverflow.MyClass", Files.readAllBytes(path);

```

**Note**: The `defineClass()` method returns a `Class<?>` object. You can save it if you want.

To load the class, we just call `loadClass()` and pass the class name. This method can throw an `ClassNotFoundException` so we need to use a try cath block

```java
try{
    loader.loadClass("stackoverflow.MyClass");
} catch(ClassNotFoundException e){
    e.printStackTrace();
}

```



## Instantiating and using a classloader


This basic example shows how an application can instantiate a classloader and use it to dynamically load a class.

```java
URL[] urls = new URL[] {new URL("file:/home/me/extras.jar")};
Classloader loader = new URLClassLoader(urls);
Class<?> myObjectClass = loader.findClass("com.example.MyObject");

```

The classloader created in this example will have the default classloader as its parent, and will first try to find any class in the parent classloader before looking in "extra.jar".  If the requested class has already been loaded, the `findClass` call will return the reference to the previously loaded class.

The `findClass` call can fail in a variety of ways.  The most common are:

- If the named class cannot be found, the call with throw `ClassNotFoundException`.
- If the named class depends on some other class that cannot be found, the call will throw `NoClassDefFoundError`.



#### Remarks


A classloader is a class whose primary purpose is to mediate the location and loading of classes used by an application.  A class loader can also find and loaded **resources**.

The standard classloader classes can load classes and resources from directories in the file system and from JAR and ZIP files.  They can also download and cache JAR and ZIP files from a remote server.

Classloaders are normally chained, so that the JVM will try to load classes from the standard class libraries in preference to application-provided sources.  Custom classloaders allow the programmer to alter this.  The also can do such things as decrypting bytecode files and bytecode modification.

