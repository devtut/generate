---
metaTitle: "Excel VBA - Common Mistakes"
description: "Qualifying References, Deleting rows or columns in a loop, ActiveWorkbook vs. ThisWorkbook, Single Document Interface Versus Multiple Document Interfaces"
---

# Common Mistakes



## Qualifying References


When referring to a `worksheet`, a `range` or individual `cells`, it is important to fully qualify the reference.

For example:

```vb
ThisWorkbook.Worksheets("Sheet1").Range(Cells(1, 2), Cells(2, 3)).Copy

```

Is not fully qualified: The `Cells` references do not have a workbook and worksheet associated with them. Without an explicit reference, Cells refers to the `ActiveSheet` by default.  So this code will fail (produce incorrect results) if a worksheet other than `Sheet1` is the current `ActiveSheet`.

The easiest way to correct this is to use a `With` statement as follows:

```vb
With ThisWorkbook.Worksheets("Sheet1")
    .Range(.Cells(1, 2), .Cells(2, 3)).Copy
End With

```

Alternatively, you can use a Worksheet variable.  (This will most likely be preferred method if your code needs to reference multiple Worksheets, like copying data from one sheet to another.)

```vb
Dim ws1 As Worksheet
Set ws1 = ThisWorkbook.Worksheets("Sheet1")
ws1.Range(ws1.Cells(1, 2), ws1.Cells(2, 3)).Copy

```

Another frequent problem is referencing the Worksheets collection without qualifying the Workbook. For example:

```vb
Worksheets("Sheet1").Copy

```

The worksheet `Sheet1` is not fully qualified, and lacks a workbook. This could fail if multiple workbooks are referenced in the code. Instead, use one of the following:

```vb
ThisWorkbook.Worksheets("Sheet1")       '<--ThisWorkbook refers to the workbook containing 
                                        'the running VBA code
Workbooks("Book1").Worksheets("Sheet1") '<--Where Book1 is the workbook containing Sheet1

```

However, avoid using the following:

```vb
ActiveWorkbook.Worksheets("Sheet1")     '<--Valid, but if another workbook is activated
                                        'the reference will be changed

```

Similarly for `range` objects, if not explicitly qualified, the `range` will refer to the currently active sheet:

```vb
Range("a1")

```

Is the same as:

```vb
ActiveSheet.Range("a1")

```



## Deleting rows or columns in a loop


If you want to delete rows (or columns) in a loop, you should always loop starting from the end of range and move back in every step. In case of using the code:

```vb
Dim i As Long
With Workbooks("Book1").Worksheets("Sheet1")
    For i = 1 To 4
        If IsEmpty(.Cells(i, 1)) Then .Rows(i).Delete
    Next i
End With

```

You will miss some rows. For example, if the code deletes row 3, then row 4 becomes row 3. However, variable `i` will change to 4. So, in this case the code will miss one row and check another, which wasn't in range previously.

The right code would be

```vb
Dim i As Long
With Workbooks("Book1").Worksheets("Sheet1")
    For i = 4 To 1 Step -1
        If IsEmpty(.Cells(i, 1)) Then .Rows(i).Delete
    Next i
End With

```



## ActiveWorkbook vs. ThisWorkbook


`ActiveWorkbook` and `ThisWorkbook` sometimes get used interchangeably by new users of VBA without fully understanding which each object relates to, this can cause undesired behaviour at run-time. Both of these objects belong to the [Application Object](http://stackoverflow.com/documentation/excel-vba/5645/application-object)

The `ActiveWorkbook` object refers to the workbook that is currently in the top-most view of the Excel application object at the time of execution. **(e.g. The workbook that you can see and interact with at the point when this object is referenced)**

```vb
Sub ActiveWorkbookExample()

'// Let's assume that 'Other Workbook.xlsx' has "Bar" written in A1.

    ActiveWorkbook.ActiveSheet.Range("A1").Value = "Foo"
    Debug.Print ActiveWorkbook.ActiveSheet.Range("A1").Value '// Prints "Foo"

    Workbooks.Open("C:\Users\BloggsJ\Other Workbook.xlsx")
    Debug.Print ActiveWorkbook.ActiveSheet.Range("A1").Value '// Prints "Bar"

    Workbooks.Add 1
    Debug.Print ActiveWorkbook.ActiveSheet.Range("A1").Value '// Prints nothing

End Sub

```

The `ThisWorkbook` object refers to the workbook in which the code belongs to at the time it is being executed.

```vb
Sub ThisWorkbookExample()

'// Let's assume to begin that this code is in the same workbook that is currently active

    ActiveWorkbook.Sheet1.Range("A1").Value = "Foo"
    Workbooks.Add 1
    ActiveWorkbook.ActiveSheet.Range("A1").Value = "Bar"

    Debug.Print ActiveWorkbook.ActiveSheet.Range("A1").Value '// Prints "Bar"
    Debug.Print ThisWorkbook.Sheet1.Range("A1").Value '// Prints "Foo"

End Sub

```



## Single Document Interface Versus Multiple Document Interfaces


> 
<p>Be aware that Microsoft Excel 2013 (and higher) uses Single Document
Interface (SDI) and that Excel 2010 (And below) uses Multiple Document
Interfaces (MDI).</p>


This implies that for Excel 2013 (SDI), each workbook in a single instance of Excel contains its **own** ribbon UI:

[<img src="http://i.stack.imgur.com/VY2TA.jpg" alt="enter image description here" />](http://i.stack.imgur.com/VY2TA.jpg)

Conversely for Excel 2010,  each workbook in a single instance of Excel utilized a **common** ribbon UI (MDI):
[<img src="http://i.stack.imgur.com/I4Dv1.jpg" alt="enter image description here" />](http://i.stack.imgur.com/I4Dv1.jpg)

This raise some important issues if you want to migrate a VBA code (2010 <->2013) that interact with the Ribbon.

> 
<p>A procedure has to be created to update ribbon UI controls in the same
state across all workbooks for Excel 2013 and Higher.</p>


Note that :

1. All Excel application-level window methods, events, and properties remain unaffected. (`Application.ActiveWindow`, `Application.Windows` ... )
1. In Excel 2013 and higher (SDI) all of the workbook-level window methods, events, and properties now operate on the top level window. It is possible to retrieve the handle of this top level window with `Application.Hwnd`

To get more details, see the source of this example:  [MSDN](https://msdn.microsoft.com/fr-fr/library/office/dn251093.aspx).

This also causes some trouble with modeless userforms. See [Here](http://www.jkp-ads.com/Articles/keepuserformontop.asp) for a solution.

