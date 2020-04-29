---
metaTitle: "JShell"
description: "Editting Snippets, Entering and Exiting JShell, Expressions, Methods and Classes, Variables"
---

# JShell


JShell is an interactive REPL for Java added in JDK 9. It allows developers to instantly evaluate expressions, test classes, and experiment with the Java language.
Early access for jdk 9 can be obtained from: [http://jdk.java.net/9/](http://jdk.java.net/9/)



## Editting Snippets


The basic unit of code used by JShell is the **snippet**, or **source entry**. Every time you declare a local variable or define a local method or class, you create a snippet whose name is the identifier of the variable/method/class. At any time, you can edit a snippet you have created with the `/edit` command. For example, let's say I have created the class `Foo` with a single, method, `bar`:

```java
jshell> class Foo {
   ...> void bar() {
   ...> }
   ...> }

```

Now, I want to fill in the body of my method. Rather than rewrite the entire class, I can edit it:

```java
jshell> /edit Foo

```

By default, a swing editor will pop up with the most basic features possible. However you can change the editor that JShell uses:

```java
jshell> /set editor emacs
jshell> /set editor vi
jshell> /set editor nano
jshell> /set editor -default

```

Note that if **the new version of the snippet contains any syntax errors, it may not be saved.** Likewise, a snippet is only created if the original declaration/definition is syntactically correct; the following does not work:

```java
jshell> String st = String 3
//error omitted
jshell> /edit st
|  No such snippet: st

```

However, snippets may be compiled and hence editable despite certain compile-time errors, such as mismatched types—the following works:

```java
jshell> int i = "hello"
//error omitted
jshell> /edit i

```

Finally, snippets may be deleted using the `/drop` command:

```java
jshell> int i = 13
jshell> /drop i
jshell> System.out.println(i)
|  Error:
|  cannot find symbol
|    symbol:   variable i
|  System.out.println(i)
|

```

To delete all snippets, thereby reseting the state of the JVM, use `\reset`:

```java
jshell> int i = 2

jshell> String s = "hi"

jshell> /reset
|  Resetting state.

jshell> i
|  Error:
|  cannot find symbol
|    symbol:   variable i
|  i
|  ^

jshell> s
|  Error:
|  cannot find symbol
|    symbol:   variable s
|  s
|  ^

```



## Entering and Exiting JShell


### Starting JShell

Before trying to start JShell, make sure your `JAVA_HOME` environment variable points to a JDK 9 installation. To start JShell, run the following command:

```java
$ jshell

```

If all goes well, you should see a `jshell>` prompt.

### Exiting JShell

To exit JShell, run the following command from the JShell prompt:

```java
jshell> /exit

```



## Expressions


Within JShell, you can evaluate Java expressions, with or without semicolons. These can range from basic expressions and statements to more complex ones:

```java
jshell> 4+2
jshell> System.out.printf("I am %d years old.\n", 421)

```

Loops and conditionals are fine, too:

```java
jshell> for (int i = 0; i<3; i++) {
   ...> System.out.println(i);
   ...> }

```

It is important to note that **expressions within blocks must have semicolons!**



## Methods and Classes


You can define methods and classes within JShell:

```java
jshell> void speak() {
   ...> System.out.println("hello");
   ...> }

jshell> class MyClass {
   ...> void doNothing() {}
   ...> }

```

No access modifiers are necessary. As with other blocks, semicolons are required inside of method bodies. Keep in mind that, as with variables, it is possible to redefine methods and classes. To see a list of methods or classes, enter `/methods` or `/types` at the JShell prompt, respectively.



## Variables


You can declare local variables within JShell:

```java
jshell> String s = "hi"
jshell> int i = s.length

```

Keep in mind that variables can be redeclared with different types; this is perfectly valid in JShell:

```java
jshell> String var = "hi"
jshell> int var = 3

```

To see a list of variables, enter `/vars` at the JShell prompt.



#### Syntax


- $ jshell — Start the JShell REPL
- jshell> /<command> — Run a given JShell command
- jshell> /exit — Exit JShell
- jshell> /help — See a list of JShell commands
- jshell> <java_expression> - Evaluate the given Java expression (semicolon optional)
- jshell> /vars OR /methods OR /types — See a list of variables, methods, or classes, respectively.
- jshell> /open <file> — read a file as input to the shell
- jshell> /edit <identifier> — edit a snippet in the set editor
- jshell> /set editor <command> — set the command to be used to edit snippets using /edit
- jshell> /drop <identifier> — delete a snippet
- jshell> /reset — Reset the JVM and delete all snippets



#### Remarks


JShell requires the Java 9 JDK, which can currently (March 2017) be downloaded as early access snapshots from [jdk9.java.net](http://jdk9.java.net). If, when you try to run the `jshell` command, you get an error beginning with `Unable to locate an executable`, make sure `JAVA_HOME` is set correctly.

### Default Imports

The following packages are imported automatically when JShell starts:

```java
import java.io.*
import java.math.*
import java.net.*
import java.nio.file.*
import java.util.*
import java.util.concurrent.*
import java.util.function.*
import java.util.prefs.*
import java.util.regex.*
import java.util.stream.*

```

