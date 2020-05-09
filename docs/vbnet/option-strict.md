---
metaTitle: "Visual Basic .NET - Option Strict"
description: "Why Use It?, How to Switch It On"
---

# Option Strict




## Why Use It?


**`Option Strict On`** prevents three things from happening:

**1. Implicit Narrowing Conversion Errors**

It prevents you from assigning to a variable that has **less precision or smaller capacity** (a narrowing conversion) without an explicit cast.  Doing so would result in data loss.

```vb
Dim d As Double = 123.4
Dim s As Single = d 'This line does not compile with Option Strict On

```

**2. Late Binding Calls**

Late binding is not allowed. This is to prevent typos that would compile, but fail at runtime

```vb
Dim obj As New Object
obj.Foo 'This line does not compile with Option Strict On

```

**3. Implicit Object Type Errors**

This prevents variable being inferred as an Object when in fact they should have been declared as a type

```vb
Dim something = Nothing. 'This line does not compile with Option Strict On

```

**Conclusion**

Unless you need to do late binding, you should always have `Option Strict On` as it will cause the mentioned errors to generate compile time errors instead of runtime exceptions.

If you **have** to do late binding, you can **either**

- Wrap all your late binding calls into one class/module and use `Option Strict Off` at the top of the code file (this is the preferred method as it reduces the likelihood of a typos in other files), **or**
- Specify that Late Binding does not cause a compilation failure (`Project Properties > Compile Tab > Warning Configuration`)



## How to Switch It On


<li>
You can switch it On at the Module/Class Level by placing the directive at the top of the code file.

```vb
Option Strict On  

```


</li>
<li>
You can switch it on at the project level via the menu in Visual Studio
</li>

> 
Project > [Project] Properties > Compile Tab > Option Strict > On


[<img src="http://i.stack.imgur.com/H8Pxl.png" alt="Project Properties > Compile Tab, the highlighted word is the option strict that you want to change." />](http://i.stack.imgur.com/H8Pxl.png)

- You can switch it On by default for all new Projects by selecting:

> 
<p>Tools > Options > Projects and Solutions > VB defaults > Option Strict<br />
Set it to `On`.</p>




#### Syntax


- Option Strict { On | Off }



#### Remarks


`Option Strict On` is a recommended good practice with Visual Basic .Net. It helps you as the developer to produce cleaner, more stable, more bug-free, more maintainable code. In some cases it may also help you write programs with better performance, avoiding things such as Implicit Conversion.

`On` is **not** the default setting for a new installation of Visual Studio. It should be one of the first things changed before beginning programming if you are going to use VB.NET. The reason it is not the default setting comes from the first editions of Visual Studio when programmers were expected to be migrating projects from VB6.

