---
metaTitle: "Resources (on classpath)"
description: "Loading default configuration, Loading an image from a resource, Finding and reading resources using a classloader, Loading same-name resource from multiple JARs"
---

# Resources (on classpath)


Java allows the retrieval of file-based resources stored inside of a JAR alongside compiled classes. This topic focuses on loading those resources and making them available to your code.



## Loading default configuration


To read default configuration properties:

```java
package com.example;

public class ExampleApplication {
    private Properties getDefaults() throws IOException {
        Properties defaults = new Properties();

        try (InputStream defaultsStream =
            ExampleApplication.class.getResourceAsStream("config.properties")) {

            defaults.load(defaultsStream);
        }

        return defaults;
    }
}

```



## Loading an image from a resource


To load a bundled image:

```java
package com.example;

public class ExampleApplication {
    private Image getIcon() throws IOException {
        URL imageURL = ExampleApplication.class.getResource("icon.png");
        return ImageIO.read(imageURL);
    }
}

```



## Finding and reading resources using a classloader


Resource loading in Java comprises the following steps:

1. Finding the `Class` or `ClassLoader` that will find the resource.
1. Finding the resource.
1. Obtaining the byte stream for the resource.
1. Reading and processing the byte stream.
1. Closing the byte stream.

The last three steps are typically accomplished by passing the URL to a library method or constructor to load the resource.  You will typically use a `getResource` method in this case.  It is also possible to read the resource data in application code.  You will typically use `getResourceAsStream` in this case.

### Absolute and relative resource paths

Resources that can be loaded from the classpath are denoted by a **path**.  The syntax of the path is similar to a UNIX / Linux file path.  It consists of simple names separated by forward slash (`/`) characters.  A **relative path** starts with a name, and an **absolute path** starts with a separator.

As the Classpath examples describe, a JVM's classpath defines a namespace by overlaying the namespaces of the directories and JAR or ZIP files in the classpath.  When an absolute path is resolved, it the classloaders interpret the initial `/` as meaning the root of the namespace.  By contrast, a relative path **may be** resolved relative to any "folder" in the namespace.  The folder used will depend on the object that you use to resolve the path.

### Obtaining a Class or Classloader

A resource can be located using either a `Class` object or a `ClassLoader` object.  A `Class` object can resolve relative paths, so you will typically use one of these if you have a (class) relative resource.  There are a variety of ways to obtain a `Class` object.  For example:

<li>
A **class literal** will give you the `Class` object for any class that you can name in Java source code; e.g. `String.class` gives you the `Class` object for the `String` type.
</li>
<li>
The `Object.getClass()` will give you the `Class` object for the type od any object; e.g. `"hello".getClass()` is another way to get `Class` of the `String` type.
</li>
<li>
The `Class.forName(String)` method will (if necessary) dynamically load a class and return its `Class` object; e.g. `Class.forName("java.lang.String")`.
</li>

A `ClassLoader` object is typically obtained by calling `getClassLoader()` on a `Class` object.  It is also possible to get hold of the JVM's default classloader using the static `ClassLoader.getSystemClassLoader()` method.

### The `get` methods

Once you have a `Class` or `ClassLoader` instance, you can find a resource, using one of the following methods:

|Methods|Description
|------
|`ClassLoader.getResource(path)`<br>`ClassLoader.getResources(path)`|Returns a URL which represents the location of the resource with the given path.
|`ClassLoader.getResources(path)`<br>`Class.getResources(path)`|Returns an `Enumeration<URL>` giving the URLs which can be used to locate the `foo.bar` resource; see below.
|`ClassLoader.getResourceAsStream(path)`<br>`Class.getResourceStream(path)`|Returns an `InputStream` from which you can read the contents of the `foo.bar` resource as a sequence of bytes.

Notes:

<li>
The main difference between the `ClassLoader` and `Class` versions of the methods is in the way that relative paths are interpreted.
<ul>
- The `Class` methods resolve a relative path in the "folder" that corresponds to the classes package.
- The `ClassLoader` methods treat relative paths as if they were absolute; i.e. the resolve them in the "root folder" of the classpath namespace.

If the requested resource (or resources) cannot be found, the `getResource` and getResourceAsStream`methods return`null`, and the`getResources`methods return an empty`Enumeration`.

The URLs returned will be resolvable using `URL.toStream()`.  They could be `file:` URLs or other conventional URLs, but if the resource resides in a JAR file, they will be `jar:` URLs that identify the JAR file and a specific resource within it.

If your code uses a `getResourceAsStream` method (or `URL.toStream()`) to obtain an `InputStream`, it is responsible for closing the stream object.  Failure to close the stream could lead to a resource leak.



## Loading same-name resource from multiple JARs


Resource with same path and name may exist in more than one JAR file on the classpath. Common cases are resources following a convention or that are part of a packaging specification. Examples for such resources are

- META-INF/MANIFEST.MF
- META-INF/beans.xml (CDI Spec)
- ServiceLoader properties containing implementation providers

To get access to **all** of these resources in different jars, one has to use a ClassLoader, which has a method for this. The returned `Enumeration` can be conveniently converted to a `List` using a Collections function.

```java
Enumeration<URL> resEnum = MyClass.class.getClassLoader().getResources("META-INF/MANIFEST.MF");
ArrayList<URL> resources = Collections.list(resEnum);

```



#### Remarks


A **resource** is file-like data with a path-like name, which resides in the classpath.  The most common use of resources is bundling application images, sounds, and read-only data (such as default configuration).

Resources can be accessed with the [ClassLoader.getResource](http://docs.oracle.com/javase/8/docs/api/java/lang/ClassLoader.html#getResource-java.lang.String-) and [ClassLoader.getResourceAsStream](http://docs.oracle.com/javase/8/docs/api/java/lang/ClassLoader.html#getResourceAsStream-java.lang.String-) methods.  The most common use case is to have resources placed in the same package as the class which reads them;  the [Class.getResource](http://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getResource-java.lang.String-) and [Class.getResourceAsStream](http://docs.oracle.com/javase/8/docs/api/java/lang/Class.html#getResourceAsStream-java.lang.String-) methods serve this common use case.

The only difference between a getResource method and getResourceAsStream method is that the former returns a URL, while the latter opens that URL and returns an InputStream.

The methods of ClassLoader accept a path-like resource name as an argument and search each location in the ClassLoader’s classpath for an entry matching that name.

- If a classpath location is a .jar file, a jar entry with the specified name is considered a match.
- If a classpath location is a directory, a relative file under that directory with the specified name is considered a match.

The resource name is similar to the path portion of a relative URL.  On **all platforms,** it uses forward slashes (`/`) as directory separators.  It must not start with a slash.

The corresponding methods of Class are similar, except:

- The resource name may start with a slash, in which case that initial slash is removed and the rest of the name is passed to the corresponding method of ClassLoader.
- If the resource name does not start with a slash, it is treated as relative to the class whose getResource or getResourceAsStream method is being called.  The actual resource name becomes **package**/**name**, where **package** is the name of the package to which the class belongs, with each period replaced by a slash, and **name** is the original argument given to the method.

For instance:

```java
package com.example;

public class ExampleApplication {
    public void readImage()
    throws IOException {

        URL imageURL = ExampleApplication.class.getResource("icon.png");

        // The above statement is identical to:
        // ClassLoader loader = ExampleApplication.class.getClassLoader();
        // URL imageURL = loader.getResource("com/example/icon.png");

        Image image = ImageIO.read(imageURL);
    }
}

```

Resources should be placed in named packages, rather than in the root of a .jar file, for the same reason classes are placed in packages:  To prevent collisions among multiple vendors.  For example, if multiple .jar files are in the classpath, and more than one of them contains a `config.properties` entry in its root, calls to the getResource or getResourceAsStream methods will return the config.properties from whichever .jar is listed first in the classpath.  This is not predictable behavior in environments where the classpath order is not under the direct control of the application, such as Java EE.

All getResource and getResourceAsStream methods return `null` if the specified resource does not exist.  Since resources must be added to the application at build time, their locations should be known when the code is being written;  a failure to find a resource at runtime is usually the result of programmer error.

Resources are read-only.  There is no way to write to a resource.  Novice developers often make the mistake of assuming that since the resource is a separate physical file when developing in an IDE (like Eclipse), it will be safe to treat it like a separate physical file in the general case.  However, this is not correct;  applications are almost always distributed as archives such as .jar or .war files, and in such cases, a resource will not be a separate file and will not be writable.  (The getFile method of the URL class is **not** a workaround for this;  despite its name, it merely returns the path portion of a URL, which is by no means guaranteed to be a valid filename.)

There is no safe way to list resources at runtime.  Again, since the developers are responsible for adding resource files to the application at build time, developers should already know their paths.  While there are workarounds, they are not reliable and will eventually fail.

