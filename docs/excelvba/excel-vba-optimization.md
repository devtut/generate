---
metaTitle: "Excel VBA - Excel-VBA Optimization"
description: "Disabling Worksheet Updating, Optimizing Error Search by Extended Debugging, Checking time of execution, Using With blocks, Row Deletion - Performance, Disabling All Excel Functionality Before executing large macros"
---

# Excel-VBA Optimization


Excel-VBA Optimization refers also to coding better error handling by documentation and additional details. This is shown here.



## Disabling Worksheet Updating


Disabling calculation of the worksheet can decrease running time of the macro significantly. Moreover, disabling events, screen updating and page breaks would be beneficial. Following `Sub` can be used in any macro for this purpose.

```vb
Sub OptimizeVBA(isOn As Boolean)
    Application.Calculation = IIf(isOn, xlCalculationManual, xlCalculationAutomatic)
    Application.EnableEvents = Not(isOn)
    Application.ScreenUpdating = Not(isOn)
    ActiveSheet.DisplayPageBreaks = Not(isOn)
End Sub

```

For optimization follow the below pseudo-code:

```vb
Sub MyCode()
    
    OptimizeVBA True

    'Your code goes here

    OptimizeVBA False

End Sub

```



## Optimizing Error Search by Extended Debugging


**Using Line Numbers ... and documenting them in case of error**
("The importance of seeing Erl")

Detecting which line raises an error is a substantial part of any debugging and narrows the search for the cause. To document identified error lines with a short description completes a successful error tracking, at best together with the names of module and procedure. The example below saves these data to a log file.

**Back ground**

The error object returns error number (Err.Number) and error description (Err.Description), but doesn't explicitly respond to the question where to locate the error. The **Erl** function, however, does, but on condition that you add ***line numbers <em>)**</em> to the code (BTW one of several other concessions to former Basic times).

If there are no error lines at all, then the Erl function returns 0, if numbering is incomplete you'll get the procedure's last preceding line number.

```vb
Option Explicit


Public Sub MyProc1()
Dim i As Integer
Dim j As Integer
On Error GoTo LogErr
10     j = 1 / 0    ' raises an error
okay:
Debug.Print "i=" & i
Exit Sub

LogErr:
MsgBox LogErrors("MyModule", "MyProc1", Err), vbExclamation, "Error " & Err.Number
Stop
Resume Next
End Sub

Public Function LogErrors( _
           ByVal sModule As String, _
           ByVal sProc As String, _
           Err As ErrObject) As String
' Purpose: write error number, description and Erl to log file and return error text
  Dim sLogFile As String: sLogFile = ThisWorkbook.Path & Application.PathSeparator & "LogErrors.txt"
  Dim sLogTxt  As String
  Dim lFile    As Long

' Create error text
  sLogTxt = sModule & "|" & sProc & "|Erl " & Erl & "|Err " & Err.Number & "|" & Err.Description

  On Error Resume Next
  lFile = FreeFile

  Open sLogFile For Append As lFile
  Print #lFile, Format$(Now(), "yy.mm.dd hh:mm:ss "); sLogTxt
      Print #lFile,
  Close lFile
' Return error text
  LogErrors = sLogTxt
 End Function

```

'**Additional Code to show log file**

```vb
Sub ShowLogFile()
Dim sLogFile As String: sLogFile = ThisWorkbook.Path & Application.PathSeparator & "LogErrors.txt"

On Error GoTo LogErr
Shell "notepad.exe " & sLogFile, vbNormalFocus

okay:
On Error Resume Next
Exit Sub

LogErr:
MsgBox LogErrors("MyModule", "ShowLogFile", Err), vbExclamation, "Error No " & Err.Number
Resume okay
End Sub

```



## Checking time of execution


Different procedures can give out the same result, but they would use different processing time. In order to check out which one is faster, a code like this can be used:

```vb
time1 = Timer

For Each iCell In MyRange
   iCell = "text"
Next iCell

time2 = Timer

For i = 1 To 30
   MyRange.Cells(i) = "text"
Next i

time3 = Timer

debug.print "Proc1 time: " & cStr(time2-time1)
debug.print "Proc2 time: " & cStr(time3-time2)

```

[MicroTimer](https://msdn.microsoft.com/en-us/library/office/ff700515(v=office.14).aspx#Anchor_5):

```vb
Private Declare PtrSafe Function getFrequency Lib "Kernel32" Alias "QueryPerformanceFrequency" (cyFrequency As Currency) As Long
Private Declare PtrSafe Function getTickCount Lib "Kernel32" Alias "QueryPerformanceCounter" (cyTickCount As Currency) As Long

Function MicroTimer() As Double
    Dim cyTicks1 As Currency
    Static cyFrequency As Currency

    MicroTimer = 0
    If cyFrequency = 0 Then getFrequency cyFrequency        'Get frequency
    getTickCount cyTicks1                                   'Get ticks
    If cyFrequency Then MicroTimer = cyTicks1 / cyFrequency 'Returns Seconds
End Function

```



## Using With blocks


Using with blocks can accelerate the process of running a macro. Instead writing a range, chart name, worksheet, etc. you can use with-blocks like below;

```vb
With ActiveChart
    .Parent.Width = 400
    .Parent.Height = 145
    .Parent.Top = 77.5 + 165 * step - replacer * 15
    .Parent.Left = 5
End With 

```

Which is faster than this:

```vb
ActiveChart.Parent.Width = 400
ActiveChart.Parent.Height = 145
ActiveChart.Parent.Top = 77.5 + 165 * step - replacer * 15
ActiveChart.Parent.Left = 5

```

Notes:

<li>
Once a With block is entered, object can't be changed. As a result, you can't use a single With statement to affect a number of different objects
</li>
<li>
**Don't jump into or out of With blocks**. If statements in a With block are executed, but either the With or End With statement is not executed, **a temporary variable containing a reference to the object remains in memory until you exit the procedure**
</li>
<li>
Don't Loop inside With statements, especially if the cached object is used as an iterator
</li>
<li>
You can nest With statements by placing one With block within another. However, because members of outer With blocks are masked within the inner With blocks, you must provide a fully qualified object reference in an inner With block to any member of an object in an outer With block.
</li>

Nesting Example:

This example uses the With statement to execute a series of statements on a single object.<br />
The object and its properties are generic names used for illustration purposes only.

```vb
With MyObject 
    .Height = 100               'Same as MyObject.Height = 100. 
    .Caption = "Hello World"    'Same as MyObject.Caption = "Hello World". 
    With .Font 
        .Color = Red            'Same as MyObject.Font.Color = Red. 
        .Bold = True            'Same as MyObject.Font.Bold = True. 
        MyObject.Height = 200   'Inner-most With refers to MyObject.Font (must be qualified
    End With
End With

```

More Info on [MSDN](https://msdn.microsoft.com/en-us/vba/language-reference-vba/articles/with-statement)



## Row Deletion - Performance


<li>
Deleting rows is slow, specially when looping through cells and deleting rows, one by one
</li>
<li>
A different approach is using an AutoFilter to hide the rows to be deleted
</li>
<li>
Copy the visible range and Paste it into a new WorkSheet
</li>
<li>
Remove the initial sheet entirely
</li>
<li>
With this method, the more rows to delete, the faster it will be
</li>

Example:

```vb
Option Explicit

'Deleted rows: 775,153, Total Rows: 1,000,009, Duration: 1.87 sec

Public Sub DeleteRows()
    Dim oldWs As Worksheet, newWs As Worksheet, wsName As String, ur As Range

    Set oldWs = ThisWorkbook.ActiveSheet
    wsName = oldWs.Name
    Set ur = oldWs.Range("F2", oldWs.Cells(oldWs.Rows.Count, "F").End(xlUp))

    Application.ScreenUpdating = False
    Set newWs = Sheets.Add(After:=oldWs)    'Create a new WorkSheet

    With ur    'Copy visible range after Autofilter (modify Criteria1 and 2 accordingly)
        .AutoFilter Field:=1, Criteria1:="<>0", Operator:=xlAnd, Criteria2:="<>"
        oldWs.UsedRange.Copy
    End With
    'Paste all visible data into the new WorkSheet (values and formats)
    With newWs.Range(oldWs.UsedRange.Cells(1).Address)
        .PasteSpecial xlPasteColumnWidths
        .PasteSpecial xlPasteAll
        newWs.Cells(1, 1).Select: newWs.Cells(1, 1).Copy
    End With

    With Application
        .CutCopyMode = False
        .DisplayAlerts = False
            oldWs.Delete
        .DisplayAlerts = True
        .ScreenUpdating = True
    End With
    newWs.Name = wsName
End Sub

```



## Disabling All Excel Functionality Before executing large macros


The procedures bellow will temporarily disable all Excel features at WorkBook and WorkSheet level

<li>
FastWB() is a toggle that accepts On or Off flags
</li>
<li>
FastWS() accepts an Optional WorkSheet object, or none
</li>
<li>
If the ws parameter is missing it will turn all features on and off for all WorkSheets in the collection
<ul>
- A custom type can be used to capture all settings before turning them off
- At the end of the process, the initial settings can be restored

```vb
Public Sub FastWB(Optional ByVal opt As Boolean = True)
    With Application
        .Calculation = IIf(opt, xlCalculationManual, xlCalculationAutomatic)
        If .DisplayAlerts <> Not opt Then .DisplayAlerts = Not opt
        If .DisplayStatusBar <> Not opt Then .DisplayStatusBar = Not opt
        If .EnableAnimations <> Not opt Then .EnableAnimations = Not opt
        If .EnableEvents <> Not opt Then .EnableEvents = Not opt
        If .ScreenUpdating <> Not opt Then .ScreenUpdating = Not opt
    End With
    FastWS , opt
End Sub

```

```vb
Public Sub FastWS(Optional ByVal ws As Worksheet, Optional ByVal opt As Boolean = True)
    If ws Is Nothing Then
        For Each ws In Application.ThisWorkbook.Sheets
            OptimiseWS ws, opt
        Next
    Else
        OptimiseWS ws, opt
    End If
End Sub
Private Sub OptimiseWS(ByVal ws As Worksheet, ByVal opt As Boolean)
    With ws
        .DisplayPageBreaks = False
        .EnableCalculation = Not opt
        .EnableFormatConditionsCalculation = Not opt
        .EnablePivotTable = Not opt
    End With
End Sub

```

Restore all Excel settings to default

```vb
Public Sub XlResetSettings()    'default Excel settings
    With Application
        .Calculation = xlCalculationAutomatic
        .DisplayAlerts = True
        .DisplayStatusBar = True
        .EnableAnimations = False
        .EnableEvents = True
        .ScreenUpdating = True
        Dim sh As Worksheet
        For Each sh In Application.ThisWorkbook.Sheets
            With sh
                .DisplayPageBreaks = False
                .EnableCalculation = True
                .EnableFormatConditionsCalculation = True
                .EnablePivotTable = True
            End With
        Next
    End With
End Sub

```



#### Remarks


*) Line numbers represent are integers, that is a signed 16 bit data type in the range of -32,768 to 32,767, otherwise you produce an overflow. Usually line numbers are inserted in steps of 10 over a part of the code or all procedures of a module as a whole.

