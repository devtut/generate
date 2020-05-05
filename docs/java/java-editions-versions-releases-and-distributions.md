---
metaTitle: "Java - Java Editions, Versions, Releases and Distributions"
description: "Differences between Java SE JRE or Java SE JDK distributions, Java SE Versions, Differences between Java EE, Java SE, Java ME and JavaFX, What is the difference between Oracle Hotspot and OpenJDK"
---

# Java Editions, Versions, Releases and Distributions




## Differences between Java SE JRE or Java SE JDK distributions


Sun / Oracle releases of Java SE come in two forms: JRE and JDK.  In simple terms, JREs support running Java applications, and JDKs also support Java development.

### Java Runtime Environment

Java Runtime Environment or JRE distributions consist of the set of libraries and tools needed to run and manage Java applications.  The tools in a typical modern JRE include:

- The `java` command for running a Java program in a JVM (Java Virtual Machine)
- The `jjs` command for running the Nashorn Javascript engine.
- The `keytool` command for manipulating Java keystores.
- The `policytool` command for editing security sandbox security policies.
- The `pack200` and `unpack200` tools for packing and unpacking "pack200" file for web deployment.
- The  `orbd`, `rmid`, `rmiregistry` and `tnameserv` commands that support Java CORBA and RMI applications.

"Desktop JRE" installers include a Java plugin suitable for some web browser.  This is deliberately left out of "Server JRE" installers.linux syscall read benchmarku

From Java 7 update 6 onwards, JRE installers have included JavaFX (version 2.2 or later).

### Java Development Kit

A Java Development Kit or JDK distribution includes the JRE tools, and additional tools for developing Java software.  The additional tools typically include:

- The `javac` command, which compiles Java source code (".java") to bytecode files (".class").
- The tools for creating JAR files such as `jar` and `jarsigner`
<li>Development tools such as:
<ul>
- `appletviewer` for running applets
- `idlj` the CORBA IDL to Java compiler
- `javah` the JNI stub generator
- `native2ascii` for character set conversion of Java source code
- `schemagen` the Java to XML schema generator (part of JAXB)
- `serialver` generate Java Object Serialization version string.
- the `wsgen` and `wsimport` support tools for JAX-WS

- `jdb` the basic Java debugger
- `jmap` and `jhat` for dumping and analysing a Java heap.
- `jstack` for getting a thread stack dump.
- `javap` for examining ".class" files.

- `jconsole` a management console,
- `jstat`, `jstatd`, `jinfo` and `jps` for application monitoring

A typical Sun / Oracle JDK installation also includes a ZIP file with the source code of the Java libraries.  Prior to Java 6, this was the only publicly available Java source code.

From Java 6 onwards, the complete source code for OpenJDK is available for download from the OpenJDK site.  It is typically not included in (Linux) JDK packages, but is available as a separate package.



## Java SE Versions


### Java SE Version History

The following table provides the timeline for the significant major versions of the Java SE platform.

|Java SE Version<sup>1</sup>|Code Name|End-of-life (free<sup>2</sup>)|Release Date
|---|---|---|---|---|---|---|---|---|---
|[Java SE 9 (Early Access)](http://download.java.net/java/jdk9/docs/api/)|**None**|future|2017-07-27 (estimated)
|[Java SE 8](http://docs.oracle.com/javase/8/docs/api/)|**None**|future|2014-03-18
|[Java SE 7](http://docs.oracle.com/javase/7/docs/api/)|Dolphin|2015-04-14|2011-07-28
|[Java SE 6](http://docs.oracle.com/javase/6/docs/api/)|Mustang|2013-04-16|2006-12-23
|[Java SE 5](http://docs.oracle.com/javase/1.5.0/docs/api/)|Tiger|2009-11-04|2004-10-04
|[Java SE 1.4.2](http://docs.oracle.com/javase/1.4.2/docs/api/)|Mantis|prior to 2009-11-04|2003-06-26
|Java SE 1.4.1|Hopper / Grasshopper|prior to 2009-11-04|2002-09-16
|Java SE 1.4|Merlin|prior to 2009-11-04|2002-02-06
|Java SE 1.3.1|Ladybird|prior to 2009-11-04|2001-05-17
|[Java SE 1.3](http://docs.oracle.com/javase/1.3/docs/api/)|Kestrel|prior to 2009-11-04|2000-05-08
|Java SE 1.2|Playground|prior to 2009-11-04|1998-12-08
|Java SE 1.1|Sparkler|prior to 2009-11-04|1997-02-19
|Java SE 1.0|Oak|prior to 2009-11-04|1996-01-21

Footnotes:

<li>
The links are to online copies of the respective releases documentation on Oracle's website.  The documentation for many older releases no longer online, though it typically can be downloaded from the Oracle Java Archives.
</li>
<li>
Most historical versions of Java SE have passed their official "end of life" dates.  When a Java version passes this milestone, Oracle stop providing free updates for it.  Updates are still available to customers with support contracts.
</li>

Source:

- [JDK release dates](http://mindprod.com/jgloss/jdkreleasedates.html) by Roedy Green of Canadian Mind Products

### Java SE Version Highlights

|Java SE Version|Highlights
|---|---|---|---|---|---|---|---|---|---
|Java SE 8|Lambda expressions and MapReduce-inspired Streams.  The Nashorn Javascript engine.  Annotations on types and repeating annotations.  Unsigned arithmetic extensions.  New Date and Time APIs.  Statically linked JNI libraries.  JavaFX launcher.  Removal of PermGen.
|Java SE 7|String switches, **try-with-resource**, the diamond (`<>`), numeric literal enhancements and exception handling / rethrowing improvements.  Concurrency library enhancements.  Enhanced support for native file systems.  Timsort.  ECC crypto algorithms.  Improved 2D graphics (GPU) support. Pluggable annotations.
|Java SE 6|Significant performance enhancements to JVM platform and Swing.  Scripting language API and Mozilla Rhino Javascript engine. JDBC 4.0.  Compiler API. JAXB 2.0. Web Services support (JAX-WS)
|Java SE 5|Generics, annotations, auto-boxing, `enum` classes, varargs, enhanced `for` loops and static imports.  Specification of the Java Memory Model.  Swing and RMI enhancements.  Addition of `java.util.concurrent.*` package and `Scanner`.
|Java SE 1.4|The `assert` keyword. Regular expression classes.  Exception chaining. NIO APIs - non-blocking I/O, `Buffer` and `Channel`.  `java.util.logging.*` API.  Image I/O API.  Integrated XML and XSLT (JAXP).  Integrated security and cryptography (JCE, JSSE, JAAS). Integrated Java Web Start.  Preferences API.
|Java SE 1.3|HotSpot JVM included.  CORBA / RMI integration.  Java Naming and Directory Interface (JNDI).  Debugger framework (JPDA).  JavaSound API.  Proxy API.
|Java SE 1.2|The `strictfp` keyword.  Swing APIs. The Java plugin (for web browsers).  CORBA interoperability.  Collections framework.
|Java SE 1.1|Inner classes.  Reflection.  JDBC.  RMI.  Unicode / character streams.  Internationalization support.  Overhaul of AWT event model.  JavaBeans.

Source:

- Wikipedia: [Java version history](https://en.wikipedia.org/wiki/Java_version_history)



## Differences between Java EE, Java SE, Java ME and JavaFX


Java technology is both a programming language and a platform. The Java programming language is a high-level object-oriented language that has a particular syntax and style. A Java platform is a particular environment in which Java programming language applications run.

There are several Java platforms. Many developers, even long-time Java programming language developers, do not understand how the different platforms relate to each other.

### The Java Programming Language Platforms

There are four platforms of the Java programming language:

<li>
Java Platform, Standard Edition (Java SE)
</li>
<li>
Java Platform, Enterprise Edition (Java EE)
</li>
<li>
Java Platform, Micro Edition (Java ME)
</li>
<li>
Java FX
</li>

All Java platforms consist of a Java Virtual Machine (VM) and an application programming interface (API). The Java Virtual Machine is a program, for a particular hardware and software platform, that runs Java technology applications. An API is a collection of software components that you can use to create other software components or applications. Each Java platform provides a virtual machine and an API, and this allows applications written for that platform to run on any compatible system with all the advantages of the Java programming language: platform-independence, power, stability, ease-of-development, and security.

### Java SE

When most people think of the Java programming language, they think of the Java SE API. Java SE's API provides the core functionality of the Java programming language. It defines everything from the basic types and objects of the Java programming language to high-level classes that are used for networking, security, database access, graphical user interface (GUI) development, and XML parsing.

In addition to the core API, the Java SE platform consists of a virtual machine, development tools, deployment technologies, and other class libraries and toolkits commonly used in Java technology applications.

### Java EE

The Java EE platform is built on top of the Java SE platform. The Java EE platform provides an API and runtime environment for developing and running large-scale, multi-tiered, scalable, reliable, and secure network applications.

### Java ME

The Java ME platform provides an API and a small-footprint virtual machine for running Java programming language applications on small devices, like mobile phones. The API is a subset of the Java SE API, along with special class libraries useful for small device application development. Java ME applications are often clients of Java EE platform services.

### Java FX

Java FX technology is a platform for creating rich internet applications written in Java FX ScriptTM. Java FX Script is a statically-typed declarative language that is compiled to Java technology bytecode, which can then be run on a Java VM. Applications written for the Java FX platform can include and link to Java programming language classes, and may be clients of Java EE platform services.

- Taken from the [Oracle documentation](http://docs.oracle.com/javaee/6/firstcup/doc/gkhoy.html)



## What is the difference between Oracle Hotspot and OpenJDK


Orthogonal to the JRE versus JDK dichotomy, there are two types of Java release that are widely available:

- The Oracle Hotspot releases are the ones that you download from the Oracle download sites.
- The OpenJDK releases are the ones that are built (typically by third-party providers) from the OpenJDK source repositories.

In functional terms, there is little difference between a Hotspot release and an OpenJDK release.  There are some extra "enterprise" features in Hotspot that Oracle (paying) Java customers can enable, but apart from that the same technology are present in both Hotspot and OpenJDK.

Another advantage of Hotspot over OpenJDK is that patch releases for Hotspot tend to be available a bit earlier.  This also depends on how agile your OpenJDK provider is; e.g. how long it takes a Linux distribution's build team to prepare and QA a new OpenJDK build, and get it into their public repositories.

The flipside is that the Hotspot releases are not available from the package repositories for most Linux distributions.  This means that keeping your Java software up-to-date on a Linux machine is usually more work if you use Hotspot.

