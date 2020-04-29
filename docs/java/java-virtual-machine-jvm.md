---
metaTitle: "Java Virtual Machine (JVM)"
description: "These are the basics."
---

# Java Virtual Machine (JVM)




## These are the basics.


JVM is an **abstract computing machine** or **Virtual machine** that resides in your RAM.
It has a platform-independent execution environment that interprets Java bytecode into native machine code. (Javac is Java Compiler which compiles your Java code into Bytecode)

Java program will be running inside the JVM which is then mapped onto the underlying physical machine. It is one of programming tool in JDK.

(**`Byte code`** is platform-independent code which is run on every platform and
**`Machine code`** is platform-specific code which is run in only specific platform such as windows or linux; it depend on execution.)

Some of the components:-

- Class Loder - load the .class file into RAM.
- Bytecode verifier - check whether there are any access restriction violations in your code.
- Execution engine - convert the byte code into executable machine code.
- JIT(just in time) - JIT is part of JVM which used to improves the performance of JVM.It will dynamically compile or translate java bytecode into native machine code during execution time.

(Edited)

