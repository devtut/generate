---
metaTitle: "Excel VBA - User Defined Functions (UDFs)"
description: "Allow full column references without penalty, UDF - Hello World, Count Unique values in Range"
---

# User Defined Functions (UDFs)



## Allow full column references without penalty


It's easier to implement some UDFs on the worksheet if full column references can be passed in as parameters. However, due to the explicit nature of coding, any loop involving these ranges may be processing hundreds of thousands of cells that are completely empty. This reduces your VBA project (and workbook) to a frozen mess while unnecessary non-values are processed.

Looping through a worksheet's cells is one of the slowest methods of accomplishing a task but sometimes it is unavoidable. Cutting the work performed down to what is actually required makes perfect sense.

The solution is to truncate the full column or full row references to the [Worksheet.UsedRange property](https://msdn.microsoft.com/en-us/library/office/ff840732.aspx) with the [Intersect method](https://msdn.microsoft.com/en-us/library/office/aa195772.aspx). The following sample will loosely replicate a worksheet's native SUMIF function so the **criteria_range** will also be resized to suit the **sum_range** since each value in the **sum_range** must be accompanied by a value in the **criteria_range**.

The [Application.Caller](https://msdn.microsoft.com/en-us/library/office/ff193687.aspx) for a UDF used on a worksheet is the cell in which it resides. The cell's [.Parent](https://msdn.microsoft.com/en-us/library/office/aa224980.aspx) property is the worksheet. This will be used to define the .UsedRange.

In a Module code sheet:

```vb
Option Explicit

Function udfMySumIf(rngA As Range, rngB As Range, _
                    Optional crit As Variant = "yes")
    Dim c As Long, ttl As Double
    
    With Application.Caller.Parent
        Set rngA = Intersect(rngA, .UsedRange)
        Set rngB = rngB.Resize(rngA.Rows.Count, rngA.Columns.Count)
    End With
    
    For c = 1 To rngA.Cells.Count
        If IsNumeric(rngA.Cells(c).Value2) Then
            If LCase(rngB(c).Value2) = LCase(crit) Then
                ttl = ttl + rngA.Cells(c).Value2
            End If
        End If
    Next c
    
    udfMySumIf = ttl

End Function

```

> 
<sup>Syntax:</sup><br/>        `=udfMySumIf(*sum_range*, *criteria_range*, [*criteria*])`


[<img src="http://i.stack.imgur.com/sgMr4.png" alt="udf_sumifs" />](http://i.stack.imgur.com/sgMr4.png)

While this is a fairly simplistic example, it adequately demonstrates passing in two full column references (1,048,576 rows each) but only processing 15 rows of data and criteria.

<sub>Linked official MSDN documentation of individual methods and properties courtesy of Microsoft™.</sub>



## UDF - Hello World


1. Open Excel
1. Open the Visual Basic Editor ( see  [Opening the Visual Basic Editor](http://stackoverflow.com/documentation/excel-vba/777/introduction-to-excel-vba/2801/opening-the-visual-basic-editor#t=201607252035449856939) )
1. Add a new module by clicking Insert --> Module :

[<img src="http://i.stack.imgur.com/0KhKM.png" alt="enter image description here" />](http://i.stack.imgur.com/0KhKM.png)

1. Copy and Paste the following code in the new module :

```vb
Public Function Hello() As String
'Note: the output of the function is simply the function's name
Hello = "Hello, World !"
End Function

```

To obtain :

[<img src="http://i.stack.imgur.com/1r1E7.png" alt="enter image description here" />](http://i.stack.imgur.com/1r1E7.png)

1. Go back to your workbook and type "=Hello()" into a cell to see the "Hello World".

[<img src="http://i.stack.imgur.com/PFQsX.png" alt="enter image description here" />](http://i.stack.imgur.com/PFQsX.png)



## Count Unique values in Range


```vb
Function countUnique(r As range) As Long
    'Application.Volatile False ' optional
    Set r = Intersect(r, r.Worksheet.UsedRange) ' optional if you pass entire rows or columns to the function
    Dim c As New Collection, v
    On Error Resume Next   ' to ignore the Run-time error 457: "This key is already associated with an element of this collection".
    For Each v In r.Value  ' remove .Value for ranges with more than one Areas
        c.Add 0, v & ""
    Next
    c.Remove "" ' optional to exclude blank values from the count
    countUnique = c.Count
End Function

```

[Collections](http://stackoverflow.com/documentation/vba/5838/collections)



#### Syntax


<li>
<p>**Function functionName(argumentVariable As dataType, argumentVariable2 As dataType, Optional argumentVariable3 As dataType) As functionReturnDataType**<br />
Basic declaration of a function. Every function needs a name, but it does not have to take any arguments. It may take 0 arguments, or it may take a given number of arguments. You may also declare an argument as optional (meaning it does not matter if you supply it when calling the function). It is best practice to supply the variable type for each argument, and likewise, to return what data type the function itself is going to return.</p>
</li>
<li>
<p>**functionName = theVariableOrValueBeingReturned**<br />
If you're coming from other programming languages, you may be used to the `Return` keyword. This is not used in VBA - instead, we use the function name. You can set it to the contents of a variable or to some directly supplied value. Note that if you did set a data type for the function's return, the variable or data you are supplying this time must be of that data type.</p>
</li>
<li>
<p>**End Function**<br />
Mandatory. Signifies the end of the `Function` codeblock and must thusly be at the end. The VBE usually supplies this automatically when you create a new function.</p>
</li>



#### Remarks


A User Defined Function (aka UDF) refers to a task specific function that has been created by the user. It can be called as a worksheet function (ex: `=SUM(...)`) or used to return a value to a running process in a Sub procedure. A UDF returns a value, typically from information passed into it as one or more parameters.

It can be created by :

1. using VBA .
1. using Excel C API - By creating an XLL that exports compiled functions to Excel.
1. using the COM interface.

