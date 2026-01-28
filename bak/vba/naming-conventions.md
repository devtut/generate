---
metaTitle: "VBA - Naming Conventions"
description: "Variable Names, Procedure Names"
---

# Naming Conventions




## Variable Names


Variables hold data. Name them after what they're used for, **not after their data type** or scope, using a **noun**. If you feel compelled to **number** your variables (e.g. `thing1, thing2, thing3`), then consider using an appropriate data structure instead (e.g. an array, a `Collection`, or a `Dictionary`).

Names of variables that represent an iteratable **set** of values - e.g. an array, a `Collection`, a `Dictionary`, or a `Range` of cells, should be plural.

Some common VBA naming conventions go thus:

**For procedure-level Variables**:

`camelCase`

```vb
Public Sub ExampleNaming(ByVal inputValue As Long, ByRef inputVariable As Long)

    Dim procedureVariable As Long
    Dim someOtherVariable As String

End Sub

```

**For module-level Variables:**

`PascalCase`

```vb
Public GlobalVariable As Long
Private ModuleVariable As String

```

**For Constants:**

`SHOUTY_SNAKE_CASE` is commonly used to differentiate constants from variables:

```vb
Public Const GLOBAL_CONSTANT As String = "Project Version #1.000.000.001"
Private Const MODULE_CONSTANT As String = "Something relevant to this Module"

Public Sub SomeProcedure()

    Const PROCEDURE_CONSTANT As Long = 10

End Sub

```

However `PascalCase` names make cleaner-looking code and are just as good, given IntelliSense uses different icons for variables and constants:

<img src="https://i.stack.imgur.com/2wPA4.png" alt="IntelliSense using different icons for constant Foo and variable Bar" />

### Hungarian Notation

> 
Name them after what they're used for, **not after their data type** or scope.


****"Hungarian Notation makes it easier to see what the type of a variable is"****

If you write your code such as procedures adhere to the **Single Responsibility Principle** (as it should), you should never be looking at a screenful of variable declarations at the top of any procedure; declare variables as close as possible to their first usage, and their data type will always be in plain sight if you declare them with an explicit type. The VBE's <kbd>Ctrl</kbd>+<kbd>i</kbd> shortcut can be used to display a variable's type in a tooltip, too.

What a variable is used for is much more useful information than its data type, **especially** in a language such as VBA which happily and implicitly converts a type into another as needed.

Consider `iFile` and `strFile` in this example:

```vb
Function bReadFile(ByVal strFile As String, ByRef strData As String) As Boolean
    Dim bRetVal As Boolean
    Dim iFile As Integer

    On Error GoTo CleanFail

    iFile = FreeFile
    Open strFile For Input As #iFile
    Input #iFile, strData

    bRetVal = True

CleanExit:
    Close #iFile
    bReadFile = bRetVal
    Exit Function
CleanFail:
    bRetVal = False
    Resume CleanExit
End Function

```

Compare to:

```vb
Function CanReadFile(ByVal path As String, ByRef outContent As String) As Boolean
    On Error GoTo CleanFail

    Dim handle As Integer
    handle = FreeFile

    Open path For Input As #handle
    Input #handle, outContent

    Dim result As Boolean
    result = True

CleanExit:
    Close #handle
    CanReadFile = result
    Exit Function
CleanFail:
    result = False
    Resume CleanExit
End Function

```

`strData` is passed `ByRef` in the top example, but beside the fact that we're lucky enough to see that it's **explicitly** passed as such, there's no indication that `strData` is actually **returned** by the function.

The bottom example names it `outContent`; this `out` prefix is what Hungarian Notation was invented for: to help clarify **what a variable is used for**, in this case to clearly identify it as an "out" parameter.

This is useful, because IntelliSense by itself doesn't display `ByRef`, even when the parameter is **explicitly** passed by reference:

<img src="https://i.stack.imgur.com/3FAHc.png" alt="IntelliSense showing "CanReadFile(ByVal path As String, outComment As String) As Boolean" when the actual signature specifies outComment as explicitly passed by reference ("ByRef")" />

Which leads to...

**Hungarian Done Right**

[Hungarian Notation originally didn't have anything to do with variable types](http://www.joelonsoftware.com/articles/Wrong.html). In fact, Hungarian Notation **done right** is actually useful. Consider this small example (`ByVal` and `As Integer` removed for brevety):

```vb
Public Sub Copy(iX1, iY1, iX2, iY2)
End Sub

```

Compare to:

```vb
Public Sub Copy(srcColumn, srcRow, dstColumn, dstRow)
End Sub

```

`src` and `dst` are **Hungarian Notation** prefixes here, and they convey **useful** information that cannot otherwise already be inferred from the parameter names or IntelliSense showing us the declared type.

Of course there's a better way to convey it all, using proper **abstraction** and real words that can be pronounced out loud and make sense - as a contrived example:

```vb
Type Coordinate
    RowIndex As Long
    ColumnIndex As Long
End Type

Sub Copy(source As Coordinate, destination As Coordinate)
End Sub

```



## Procedure Names


Procedures **do something**. Name them after what they're doing, using a **verb**. If accurately naming a procedure is not possible, likely the procedure is **doing too many things** and needs to be broken down into smaller, more specialized procedures.

Some common VBA naming conventions go thus:

**For all Procedures:**

`PascalCase`

```vb
Public Sub DoThing()

End Sub

Private Function ReturnSomeValue() As [DataType]

End Function

```

**For event handler procedures:**

`ObjectName_EventName`

```vb
Public Sub Workbook_Open()

End Sub

Public Sub Button1_Click()

End Sub

```

Event handlers are usually automatically named by the VBE; renaming them without renaming the object and/or the handled event will break the code - the code will run and compile, but the handler procedure will be orphaned and will never be executed.

**Boolean Members**

Consider a Boolean-returning function:

```vb
Function bReadFile(ByVal strFile As String, ByRef strData As String) As Boolean
End Function

```

Compare to:

```vb
Function CanReadFile(ByVal path As String, ByRef outContent As String) As Boolean
End Function

```

The `Can` prefix **does** serve the same purpose as the `b` prefix: it identifies the function's return value as a `Boolean`. But `Can` reads better than `b`:

```vb
If CanReadFile(path, content) Then

```

Compared to:

```vb
If bReadFile(strFile, strData) Then

```

Consider using prefixes such as `Can`, `Is` or `Has` in front of Boolean-returning members (functions and properties), but only when it adds value. This conforms with the [current Microsoft naming guidelines](https://msdn.microsoft.com/en-us/library/ms229012(v=vs.110).aspx).

