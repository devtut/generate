---
metaTitle: "Java - Multi-Release JAR Files"
description: "Example of a multi-release Jar file's contents, Creating a multi-release Jar using the jar tool, URL of a loaded class inside a multi-release Jar"
---

# Multi-Release JAR Files


One of the features introduced in Java 9 is the multi-release Jar (MRJAR) which allows bundling code targeting multiple Java releases within the same Jar file. The feature is specified in [JEP 238](http://openjdk.java.net/jeps/238).



## Example of a multi-release Jar file's contents


By setting [`Multi-Release: true`](http://download.java.net/java/jdk9/docs/api/java/util/jar/Attributes.Name.html#MULTI_RELEASE) in the MANIFEST.MF file, the Jar file becomes a multi-release Jar and the Java runtime (as long as it supports the MRJAR format) will pick the appropriate versions of classes depending on the current major version.

The structure of such a Jar is the following:

```java
jar root
  - A.class
  - B.class
  - C.class
  - D.class
  - META-INF
     - versions
        - 9
           - A.class
           - B.class
        - 10
           - A.class

```


- On JDKs < 9, only the classes in the root entry are visible to the Java runtime.
- On a JDK 9, the classes A and B will be loaded from the directory `root/META-INF/versions/9`, while C and D will be loaded from the base entry.
- On a JDK 10, class A would be loaded from the directory `root/META-INF/versions/10`.



## Creating a multi-release Jar using the jar tool


The `jar` command can be used to create a multi-release Jar containing two versions of the same class compiled for both Java 8 and Java 9, albeit with a warning telling that the classes are identical:

```java
C:\Users\manouti>jar --create --file MR.jar -C sampleproject-base demo --release 9 -C sampleproject-9 demo
Warning: entry META-INF/versions/9/demo/SampleClass.class contains a class that
is identical to an entry already in the jar

```

The `--release 9` option tells `jar` to include everything that follows (the `demo` package inside the `sampleproject-9` directory) inside a versioned entry in the MRJAR, namely under `root/META-INF/versions/9`. The result is the following contents:

```java
jar root
  - demo
     - SampleClass.class
  - META-INF
     - versions
        - 9
           - demo
              - SampleClass.class

```

Let us now create a class called Main that prints the URL of the `SampleClass`, and add it for the Java 9 version:

```java
package demo;

import java.net.URL;

public class Main {

    public static void main(String[] args) throws Exception {
        URL url = Main.class.getClassLoader().getResource("demo/SampleClass.class");
        System.out.println(url);
    }
}

```

If we compile this class and re-run the jar command, we get an error:

```java
C:\Users\manouti>jar --create --file MR.jar -C sampleproject-base demo --release 9 -C sampleproject-9 demoentry: META-INF/versions/9/demo/Main.class, contains a new public class not found in base entries
Warning: entry META-INF/versions/9/demo/Main.java, multiple resources with same name
Warning: entry META-INF/versions/9/demo/SampleClass.class contains a class that
is identical to an entry already in the jar
invalid multi-release jar file MR.jar deleted

```

The reason is that the `jar` tool prevents adding public classes to versioned entries if they are not added to the base entries as well. This is done so that the MRJAR exposes the same public API for the different Java versions. Note that at runtime, this rule is not required. It may be only applied by tools like `jar`. In this particular case, the purpose of `Main` is to run sample code, so we can simply add a copy in the base entry. If the class were part of a newer implementation that we only need for Java 9, it could be made non-public.

To add `Main` to the root entry, we first need to compile it to target a pre-Java 9 release. This can be done using the new `--release` option of `javac`:

```java
C:\Users\manouti\sampleproject-base\demo>javac --release 8 Main.java
C:\Users\manouti\sampleproject-base\demo>cd ../..
C:\Users\manouti>jar --create --file MR.jar -C sampleproject-base demo --release 9 -C sampleproject-9 demo

```

Running the Main class shows that the SampleClass gets loaded from the versioned directory:

```java
C:\Users\manouti>java --class-path MR.jar demo.Main
jar:file:/C:/Users/manouti/MR.jar!/META-INF/versions/9/demo/SampleClass.class

```



## URL of a loaded class inside a multi-release Jar


Given the following multi-release Jar:

```java
jar root
  - demo
     - SampleClass.class
  - META-INF
     - versions
        - 9
           - demo
              - SampleClass.class

```

The following class prints the URL of the `SampleClass`:

```java
package demo;

import java.net.URL;

public class Main {

    public static void main(String[] args) throws Exception {
        URL url = Main.class.getClassLoader().getResource("demo/SampleClass.class");
        System.out.println(url);
    }
}

```

If the class is compiled and added on the versioned entry for Java 9 in the MRJAR, running it would result in:

```java
C:\Users\manouti>java --class-path MR.jar demo.Main
jar:file:/C:/Users/manouti/MR.jar!/META-INF/versions/9/demo/SampleClass.class

```

