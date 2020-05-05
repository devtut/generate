---
metaTitle: "Java - Getting started with Java Language"
description: "Creating Your First Java Program"
---

# Getting started with Java Language



## Creating Your First Java Program


Create a new file in your [text editor](https://en.wikipedia.org/wiki/Text_editor) or [IDE](https://en.wikipedia.org/wiki/Integrated_development_environment) named `HelloWorld.java`. Then paste this code block into the file and save:

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}

```

[Run live on Ideone](https://ideone.com/rbWs4M)

****Note:**** For Java to recognize this as a `public class` (and not throw a [compile time error](http://stackoverflow.com/questions/1841847/can-i-compile-a-java-file-with-a-different-name-than-the-class)), the filename must be the same as the class name (`HelloWorld` in this example) with a `.java` extension. There should also be a `public` access modifier before it.

[Naming conventions](http://stackoverflow.com/documentation/java/2697/coding-standards/9031/naming-conventions#t=201607261255448465119) recommend that Java classes begin with an uppercase character, and be in [camel case](https://en.wikipedia.org/wiki/Camel_case?oldformat=true) format (in which the first letter of each word is capitalized). The conventions recommend against underscores (`_`) and dollar signs (`$`).

To compile, open a terminal window and navigate to the directory of `HelloWorld.java`:

```java
cd /path/to/containing/folder/

```

****Note:**** [cd](http://www.linfo.org/cd.html) is the terminal command to change directory.

Enter `javac` followed by the file name and extension as follows:

```java
$ javac HelloWorld.java

```

It's fairly common to get the error `'javac' is not recognized as an internal or external command, operable program or batch file.` even when you have installed the `JDK` and are able to run the program from `IDE` ex. `eclipse` etc. Since the path is not added to the environment by default.

In case you get this on windows, to resolve, first try browsing to your `javac.exe` path, it's most probably in your `C:\Program Files\Java\jdk(version number)\bin`. Then try running it with below.

```java
$ C:\Program Files\Java\jdk(version number)\bin\javac HelloWorld.java

```

Previously when we were calling `javac` it was same as above command. Only in that case your `OS` knew where `javac` resided. So let's tell it now, this way you don't have to type the whole path every-time. We would need to add this to our `PATH`

To edit the `PATH` environment variable in Windows XP/Vista/7/8/10:

- Control Panel ⇒ System ⇒ Advanced system settings
- Switch to "Advanced" tab ⇒ Environment Variables
- In "System Variables", scroll down to select "PATH" ⇒ Edit

**You cannot undo this** so be careful. First copy your existing path to notepad. Then to get the exact PATH to your `javac` browse manually to the folder where `javac` resides and click on the address bar and then copy it. It should look something like `c:\Program Files\Java\jdk1.8.0_xx\bin`

In "Variable value" field, paste this **IN FRONT** of all the existing directories, followed by a semi-colon (;). **DO NOT DELETE** any existing entries.

```java
Variable name  : PATH
Variable value : c:\Program Files\Java\jdk1.8.0_xx\bin;[Existing Entries...]

```

Now this should resolve.

For Linux Based systems [try here](https://docs.oracle.com/javase/tutorial/essential/environment/paths.html).

****Note:** The [`javac`](http://stackoverflow.com/documentation/java/4478/java-compiler-javac) command invokes the Java compiler.**

The compiler will then generate a [bytecode](https://en.wikipedia.org/wiki/Bytecode) file called `HelloWorld.class` which can be executed in the [Java Virtual Machine (JVM)](https://en.wikipedia.org/wiki/Java_virtual_machine). The Java programming language compiler, `javac`, reads source files written in the Java programming language and compiles them into `bytecode` class files. Optionally, the compiler can also process annotations found in source and class files using the `Pluggable` Annotation Processing API. The compiler is a command line tool but can also be invoked using the Java Compiler API.

To run your program, enter `java` followed by the name of the class which contains the `main` method (`HelloWorld` in our example). Note how the `.class` is omitted:

```java
$ java HelloWorld

```

****Note:** The [`java`](http://docs.oracle.com/javase/8/docs/technotes/tools/windows/java.html) command runs a Java application.**

This will output to your console:

> 
Hello, World!


You have successfully coded and built your very first Java program!

****Note:****  In order for Java commands (`java`, `javac`, etc) to be recognized, you will need to make sure:

- A JDK is installed (e.g. [Oracle](http://docs.oracle.com/javase/8/docs/technotes/guides/install/install_overview.html), [OpenJDK](http://openjdk.java.net/install/) and other sources)
- Your environment variables are properly [set up](https://docs.oracle.com/javase/tutorial/essential/environment/paths.html)

You will need to use a compiler (`javac`) and an executor (`java`) provided by your JVM. To find out which versions you have installed, enter `java -version` and `javac -version` on the command line. The version number of your program will be printed in the terminal (e.g. `1.8.0_73`).

### A closer look at the Hello World program

The "Hello World" program contains a single file, which consists of a `HelloWorld` class definition, a `main` method, and a statement inside the `main` method.

```java
public class HelloWorld {

```

The `class` keyword begins the class definition for a class named `HelloWorld`. Every Java application contains at least one class definition ([Further information about classes](https://stackoverflow.com/documentation/java/114/classes-and-objects)).

```

   public static void main(String[] args)  {

```

This is an entry point method (defined by its name and signature of `public static void main(String[])`) from which the `JVM` can run your program. Every Java program should have one. It is:

- `public`: meaning that the method can be called from anywhere mean from outside the program as well. See [Visibility](http://stackoverflow.com/documentation/java/134/visibility-controlling-access-to-members-of-a-class#t=201610281645589710879) for more information on this.
- `static`: meaning it exists and can be run by itself (at the class level without creating an object).
- `void`: meaning it returns no value. ****Note:** This is unlike C and C++ where a return code such as `int` is expected (Java's way is [`System.exit()`](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#exit-int-)).**

This main method accepts:

- An [array](http://stackoverflow.com/documentation/java/99/arrays/404/creating-and-initializing-arrays#t=201608030925337075654) (typically called `args`) of `String`s passed as arguments to main function (e.g. from [command line arguments](http://stackoverflow.com/documentation/java/84/java-overview/7980/command-line-arguments)).

Almost all of this is required for a Java entry point method.

Non-required parts:

- The name `args` is a variable name, so it can be called anything you want, although it is typically called `args`.
- Whether its parameter type is an array (`String[] args`) or [Varargs](http://stackoverflow.com/documentation/java/1948/varargs-variable-argument#t=201608020129571125749) (`String... args`) does not matter because arrays can be passed into varargs.

****Note:**** A single application may have multiple classes containing an entry point (`main`) method. The entry point of the application is determined by the class name passed as an argument to the `java` command.

Inside the main method, we see the following statement:

```

       System.out.println("Hello, World!");

```

Let's break down this statement element-by-element:

|Element|Purpose
|---|---|---|---|---|---|---|---|---|---
|`System`|this denotes that the subsequent expression will call upon the `System` class, from the `java.lang` package.
|`.`|this is a "dot operator". Dot operators provide you access to a classes members<sup>1</sup>; i.e. its fields (variables) and its methods. In this case, this dot operator allows you to reference the `out` static field within the `System` class.
|`out`|this is the name of the static field of `PrintStream` type within the `System` class containing the standard output functionality.
|`.`|this is another dot operator. This dot operator provides access to the `println` method within the `out` variable.
|`println`|this is the name of a method within the PrintStream class. This method in particular prints the contents of the parameters into the console and inserts a newline after.
|`(`|this parenthesis indicates that a method is being accessed (and not a field) and begins the parameters being passed into the `println` method.
|`"Hello, World!"`|this is the [String](https://stackoverflow.com/documentation/java/109/strings) literal that is passed as a parameter, into the `println` method. The double quotation marks on each end delimit the text as a String.
|`)`|this parenthesis signifies the closure of the parameters being passed into the `println` method.
|`;`|this semicolon marks the end of the statement.

****Note:** Each statement in Java must end with a semicolon (`;`).**

The method body and class body are then closed.

```

   }  // end of main function scope
}      // end of class HelloWorld scope 

```

Here's another example demonstrating the OO paradigm. Let's model a football team with one (yes, one!) member. There can be more, but we'll discuss that when we get to arrays.

First, let's define our `Team` class:

```java
public class Team {
    Member member;
    public Team(Member member) {  // who is in this Team?
        this.member = member;  //  one 'member' is in this Team!
    }
}

```

Now, let's define our `Member` class:

```java
class Member {
    private String name;
    private String type;
    private int level; // note the data type here
    private int rank; // note the data type here as well

    public Member(String name, String type, int level, int rank) {
        this.name = name; 
        this.type = type;
        this.level = level;
        this.rank = rank;
    }
}

```

Why do we use `private` here? Well, if someone wanted to know your name, they should ask you directly, instead of reaching into your pocket and pulling out your Social Security card. This `private` does something like that: it prevents outside entities from accessing your variables. You can only return `private` members through getter functions (shown below).

After putting it all together, and adding the getters and main method as discussed before, we have:

```java
public class Team {
    Member member;
    public Team(Member member) {  
        this.member = member;  
    }

    // here's our main method
    public static void main(String[] args) {
       Member myMember = new Member("Aurieel", "light", 10, 1); 
       Team myTeam = new Team(myMember); 
       System.out.println(myTeam.member.getName());
       System.out.println(myTeam.member.getType());
       System.out.println(myTeam.member.getLevel());
       System.out.println(myTeam.member.getRank());
    }
}

class Member {
    private String name;
    private String type;
    private int level;
    private int rank;

    public Member(String name, String type, int level, int rank) {
        this.name = name; 
        this.type = type;
        this.level = level;
        this.rank = rank;
    }
    
    /* let's define our getter functions here */
    public String getName() { // what is your name?
        return this.name; // my name is ...
    }
   
    public String getType() { // what is your type?
        return this.type; // my type is ...
    }
    
    public int getLevel() { // what is your level?
        return this.level; // my level is ...
    }
    
    public int getRank() { // what is your rank?
        return this.rank; // my rank is
    }
}

```

Output:

```java
Aurieel
light
10
1

```

[Run on ideone](https://ideone.com/hHWFdk)

Once again, the `main` method inside the `Test` class is the entry point to our program. Without the `main` method, we cannot tell the Java Virtual Machine (JVM) from where to begin execution of the program.

<sup>1 - Because the `HelloWorld` class has little relation to the `System` class, it can only access `public` data.</sup>



#### Remarks


The Java programming language is...

<li>
**General-purpose**: It is designed to be used for writing software in a wide variety of application domains, and lacks specialized features for any specific domain.
</li>
<li>
**Class-based**: Its object structure is defined in classes. Class instances always have those fields and methods specified in their class definitions (see [Classes and Objects](http://stackoverflow.com/documentation/java/114/classes-and-objects)). This is in contrast to non-class-based languages such as JavaScript.
</li>
<li>
**Statically-typed**: the compiler checks at compile time that variable types are respected. For example, if a method expects an argument of type `String`, that argument must in fact be a string when the method is called.
</li>
<li>
**Object-oriented**: most things in a Java program are class instances, i.e. bundles of state (fields) and behavior (methods which operate on data and form the object's **interface** to the outside world).
</li>
<li>
**Portable**: It can be compiled on any platform with `javac` and the resultant class files can run on any platform that has a JVM.
</li>

Java is intended to let application developers "write once, run anywhere" (WORA), meaning that compiled Java code can run on all platforms that support Java without the need for recompilation.

Java code is compiled to bytecode (the `.class` files) which in turn get interpreted by the Java Virtual Machine (JVM). In theory, bytecode created by one Java compiler should run the same way on any JVM, even on a different kind of computer. The JVM might (and in real-world programs will) choose to compile into native machine commands the parts of the bytecode that are executed often. This is called "Just-in-time (JIT) compilation".

### Java Editions and Versions

There are three "editions" of Java defined by Sun / Oracle:

- **Java Standard Edition (SE)** is the edition that is designed for general use.
- **Java Enterprise Edition (EE)** adds a range of facilities for building "enterprise grade" services in Java.  Java EE is covered [separately](http://stackoverflow.com/documentation/java-ee/topics).
- **Java Micro Edition (ME)** is based on a subset of **Java SE** and is intended for use on small devices with limited resources.

There is a separate topic on [Java SE / EE / ME editions](http://stackoverflow.com/documentation/java/8973/java-se-ee-me#t=201702040324213843444).

Each edition has multiple versions.  The Java SE versions are listed below.

### Installing Java

There is a separate topic on [Installing Java (Standard Edition)](http://stackoverflow.com/documentation/java/4754/installing-java-standard-edition#t=201608051432336281235).

### Compiling and running Java programs

There are separate topics on:

- [Compiling Java source code](http://stackoverflow.com/documentation/java/4478/java-compiler-javac)
- [Java deployment](http://stackoverflow.com/documentation/java/6840/java-deployment) including creating JAR files
- [Running Java applications](http://stackoverflow.com/documentation/java/5791/the-java-command-java-and-javaw)
- [The Classpath](http://stackoverflow.com/documentation/java/3720/the-classpath)

### What's next?

Here are links to subjects to continue learning and understanding the Java programming language. These subjects are the basics of the Java programming to get you started.

- [Primitive Data Types in Java](http://stackoverflow.com/documentation/java/148/primitive-data-types)
- [Operators in Java](http://stackoverflow.com/documentation/java/176/operators)
- [Strings in Java](http://stackoverflow.com/documentation/java/109/strings)
- [Basic Control Structures in Java](http://stackoverflow.com/documentation/java/118/basic-control-structures)
- [Classes and Objects in Java](http://stackoverflow.com/documentation/java/114/classes-and-objects)
- [Arrays in Java](http://stackoverflow.com/documentation/java/99/arrays)
- [Java code standards](http://stackoverflow.com/documentation/java/2697/oracle-official-code-standard#t=201612311321004449086)

### Testing

While Java does not have any support for testing in the standard library, there are 3rd-party libraries that are designed to support testing.  The two most popular unit testing libraries are:

- [JUnit](http://stackoverflow.com/documentation/junit/topics) ([Official Site](http://junit.org/junit4/))
- [TestNG](http://stackoverflow.com/documentation/testng) ([Official Site](http://testng.org/doc/index.html))

### Other

- Design patterns for Java are covered in [Design Patterns](http://stackoverflow.com/documentation/design-patterns/topics).
- Programming for Android is covered in [Android](http://stackoverflow.com/documentation/android/topics).
- Java Enterprise Edition technologies are covered in [Java EE](http://stackoverflow.com/documentation/java-ee/topics).
- The Oracle JavaFX technologies are covered in [JavaFX](http://stackoverflow.com/documentation/javafx/topics).

<sup>**1.** In **Versions** section the **end-of-life (free)** date is when Oracle will stop posting further updates of Java SE to its public download sites. Customers who need continued access to critical bug fixes and security fixes as well as general maintenance for Java SE can get long term support through [Oracle Java SE Support](http://www.oracle.com/us/technologies/java/java-se-support-393643.html?ssSourceSiteId=otnen). </sup>

