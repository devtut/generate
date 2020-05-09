---
metaTitle: "Visual Basic .NET - Declaring variables"
description: "Declaring and assigning a variable using a primitive type, Levels of declaration – Local and Member variables, Example of Access Modifiers"
---

# Declaring variables



## Declaring and assigning a variable using a primitive type


Variables in Visual Basic are declared using the `Dim` keyword. For example, this declares a new variable called `counter` with the data type `Integer`:

```vb
Dim counter As Integer

```

A variable declaration can also include an [access modifier](https://msdn.microsoft.com/en-us/library/76453kax.aspx), such as `Public`, `Protected`, `Friend`, or `Private`. This works in conjunction with the variable's [scope](https://msdn.microsoft.com/en-us/library/1t0wsc67.aspx) to determine its accessibility.

|Access Modifier|Meaning
|---|---|---|---|---|---|---|---|---|---
|[Public](https://msdn.microsoft.com/en-us/library/9dc6we3z.aspx)|All types which can access the enclosing type
|[Protected](https://msdn.microsoft.com/en-us/library/8050kawf.aspx)|Only the enclosing class and those that inherit from it
|[Friend](https://msdn.microsoft.com/en-us/library/08w05ey2.aspx)|All types in the same assembly that can access the enclosing type
|Protected Friend|The enclosing class and its inheritors, **or** the types in the same assembly that can access the enclosing class
|[Private](https://msdn.microsoft.com/en-us/library/wx059ey1.aspx)|Only the enclosing type
|[Static](https://msdn.microsoft.com/en-us/library/z2cty7t8.aspx)|Only on local variables and only initializes once.

As a shorthand, the `Dim` keyword can be replaced with the access modifier in the variable's declaration:

```vb
Public TotalItems As Integer
Private counter As Integer

```

The supported data types are outlined in the table below:

|Type|Alias|Memory allocation|Example
|---|---|---|---|---|---|---|---|---|---
|SByte|N/A|1 byte|`Dim example As SByte = 10`
|Int16|Short|2 bytes|`Dim example As Short = 10`
|Int32|Integer|4 bytes|`Dim example As Integer = 10`
|Int64|Long|8 bytes|`Dim example As Long = 10`
|Single|N/A|4 bytes|`Dim example As Single = 10.95`
|Double|N/A|8 bytes|`Dim example As Double = 10.95`
|Decimal|N/A|16 bytes|`Dim example As Decimal = 10.95`
|Boolean|N/A|Dictated by implementing platform|`Dim example As Boolean = True`
|Char|N/A|2 Bytes|`Dim example As Char = "A"C`
|String|N/A|<img src="https://chart.googleapis.com/chart?cht=tx&chl=20%2B%5Cleft%5Clfloor%5Cfrac%7Blength%7D%7B2%7D%5Cright%5Crfloor*4bytes" alt="formula" />[source](http://csharpindepth.com/Articles/General/Strings.aspx)|`Dim example As String = "Stack Overflow"`
|DateTime|Date|8 Bytes|`Dim example As Date = Date.Now`
|Byte|N/A|1 byte|`Dim example As Byte = 10`
|UInt16|UShort|2 bytes|`Dim example As UShort = 10`
|UInt32|UInteger|4 bytes|`Dim example As UInteger = 10`
|UInt64|ULong|8 bytes|`Dim example As ULong = 10`
|Object|N/A|4 bytes 32 bit architecture, 8 bytes 64 bit architecture|`Dim example As Object = Nothing`

There also exist data identifier and literal type characters usable in replacement for the textual type and or to force literal type:

|Type (or Alias)|Identifier type character|Literal type character
|---|---|---|---|---|---|---|---|---|---
|Short|N/A|`example = 10S`
|Integer|`Dim example%`|`example = 10%` or `example = 10I`
|Long|`Dim example&`|`example = 10&` or `example = 10L`
|Single|`Dim example!`|`example = 10!` or `example = 10F`
|Double|`Dim example#`|`example = 10#` or `example = 10R`
|Decimal|`Dim example@`|`example = 10@` or `example = 10D`
|Char|N/A|`example = "A"C`
|String|`Dim example$`|N/A
|UShort|N/A|`example = 10US`
|UInteger|N/A|`example = 10UI`
|ULong|N/A|`example = 10UL`

The integral suffixes are also usable with hexadecimal (&H) or octal (&O) prefixes:<br />
`example = &H8000S` or `example = &O77&`

Date(Time) objects can also be defined using literal syntax:<br />
`Dim example As Date = #7/26/2016 12:8 PM#`

Once a variable is declared it will exist within the [Scope](https://msdn.microsoft.com/en-us/library/1t0wsc67.aspx) of the containing type, `Sub` or `Function` declared, as an example:

```vb
Public Function IncrementCounter() As Integer
    Dim counter As Integer = 0
    counter += 1

    Return counter
End Function

```

The counter variable will only exist until the `End Function` and then will be out of scope.  If this counter variable is needed outside of the function you will have to define it at class/structure or module level.

```vb
Public Class ExampleClass

    Private _counter As Integer
   
    Public Function IncrementCounter() As Integer
       _counter += 1
       Return _counter
    End Function

End Class

```

Alternatively, you can use the `Static` (not to be confused with `Shared`) modifier to allow a local variable to retain it's value between calls of its enclosing method:

```vb
Function IncrementCounter() As Integer
    Static counter As Integer = 0
    counter += 1

    Return counter
End Function

```



## Levels of declaration – Local and Member variables


**Local variables** - Those declared within a procedure (subroutine or function) of a class (or other structure). In this example, `exampleLocalVariable` is a local variable  declared within `ExampleFunction()`:

```vb
Public Class ExampleClass1

    Public Function ExampleFunction() As Integer
        Dim exampleLocalVariable As Integer = 3
        Return exampleLocalVariable
    End Function

End Class

```

The `Static` keyword allows a local variable to be retained and keep its value after termination (where usually, local variables cease to exist when the containing procedure terminates).

In this example, the console is `024`. On each call to `ExampleSub()` from `Main()` the static variable retains the value it had at the end of the previous call:

```vb
Module Module1

    Sub Main()
        ExampleSub()
        ExampleSub()
        ExampleSub()
    End Sub

    Public Sub ExampleSub()
        Static exampleStaticLocalVariable As Integer = 0
        Console.Write(exampleStaticLocalVariable.ToString)
        exampleStaticLocalVariable += 2
    End Sub

End Module

```

**Member variables** - Declared outside of any procedure, at the class (or other structure) level. They may be **instance variables**, in which each instance of the containing class has its own distinct copy of that variable, or `Shared` **variables**, which exist as a single variable associated with the class itself, independent of any instance.

Here, `ExampleClass2` contains two member variables. Each instance of the `ExampleClass2` has an individual `ExampleInstanceVariable` which can be accessed via the class reference. The shared variable `ExampleSharedVariable` however is accessed using the class name:

```vb
Module Module1

    Sub Main()

        Dim instance1 As ExampleClass4 = New ExampleClass4
        instance1.ExampleInstanceVariable = "Foo"

        Dim instance2 As ExampleClass4 = New ExampleClass4
        instance2.ExampleInstanceVariable = "Bar"

        Console.WriteLine(instance1.ExampleInstanceVariable)
        Console.WriteLine(instance2.ExampleInstanceVariable)
        Console.WriteLine(ExampleClass4.ExampleSharedVariable)

    End Sub

    Public Class ExampleClass4

        Public ExampleInstanceVariable As String
        Public Shared ExampleSharedVariable As String = "FizzBuzz"

    End Class

End Module

```



## Example of Access Modifiers


In the following example consider you have a solution hosting two projects: **ConsoleApplication1** and **SampleClassLibrary**. The first project will have the classes **SampleClass1** and **SampleClass2**. The second one will have **SampleClass3** and **SampleClass4**. In other words we have two assemblies with two classes each. **ConsoleApplication1** has a reference to **SampleClassLibrary**.

See how **SampleClass1.MethodA** interacts with other classes and methods.

SampleClass1.vb:

SampleClass2.vb:

SampleClass3.vb:

SampleClass4.vb:



#### Syntax


- Public counter As Integer
- Private _counter As Integer
- Dim counter As Integer

