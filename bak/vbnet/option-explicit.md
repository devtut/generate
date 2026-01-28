---
metaTitle: "Visual Basic .NET - Option Explicit"
description: "What is it?, How to switch it on?"
---

# Option Explicit



## What is it?


It forces you to explicitly declare all variables.

**What is the difference between explicitly declaring and implicitly declaring a variable?**

Explicitly declaring a variable:

```vb
Dim anInteger As Integer = 1234

```

Implicitly declaring a variable:

```vb
'Did not declare aNumber using Dim
aNumber = 1234

```

**Conclusion**

Therefore, you should always have `Option Explicit On` as you could misspel a variable during assignment, which cause your program to behave unexpectedly.



## How to switch it on?


**Document level**

It is on by default, but you can have an extra layer of protection by placing `Option Explicit On` at the top of the code file. The option will apply to the whole document.

**Project level**

You can switch it on via the menu in Visual Studio:

> 
Project > [Project] Properties > Compile Tab > Option Explicit


Choose `On` in the drop-down menu. The option will apply to the whole document.

**All new projects**

You can switch it On by default for all new Projects by selecting:

> 
Tools > Options > Projects and Solutions > VB defaults > Option Explicit


Choose `On` in the drop-down menu.



#### Remarks


`Option Explicit On` is a recommended good practice with Visual Basic .Net. It helps you as the developer to produce cleaner, more stable, more bug-free, more maintainable code. In some cases it may also help you write programs with better performance too!

with ref to [https://support.microsoft.com/en-in/kb/311329#bookmark-3](https://support.microsoft.com/en-in/kb/311329#bookmark-3) option strict is also can be used instead of option explicit. Option strict inherits option explicit.

