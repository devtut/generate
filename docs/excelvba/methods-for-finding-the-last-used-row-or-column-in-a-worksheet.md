---
metaTitle: "Excel VBA - Methods for Finding the Last Used Row or Column in a Worksheet"
description: "Find the Last Non-Empty Cell in a Column, Find the Last Non-Empty Row in Worksheet, Find the Last Non-Empty Column in Worksheet, Find the Last Non-Empty Cell in a Row, Find Last Row Using Named Range, Get the row of the last cell in a range, Last cell in Range.CurrentRegion, Find the Last Non-Empty Cell in Worksheet - Performance (Array)"
---

# Methods for Finding the Last Used Row or Column in a Worksheet




## Find the Last Non-Empty Cell in a Column


In this example, we will look at a method for returning the last non-empty row in a column for a data set.

This method will work regardless of empty regions within the data set.

However ****caution** should be used if **merged cells** are involved**, as the `End` method will be "stopped" against a merged region, returning the first cell of the merged region.

In addition non-empty cells in ****hidden rows**** will not be taken into account.

```vb
Sub FindingLastRow()
    Dim wS As Worksheet, LastRow As Long
    Set wS = ThisWorkbook.Worksheets("Sheet1")
    
    'Here we look in Column A
    LastRow = wS.Cells(wS.Rows.Count, "A").End(xlUp).Row
    Debug.Print LastRow
End Sub

```

To address the limitations indicated above, the line:
<br>`LastRow = wS.Cells(wS.Rows.Count, "A").End(xlUp).Row`

may be replaced with:

<li>
<p>for last used row of `"Sheet1"`:
<br>`LastRow = wS.UsedRange.Row - 1 + wS.UsedRange.Rows.Count`.</p>
</li>
<li>
for last non-empty cell of Column `"A"` in `"Sheet1"`:

```vb
 Dim i As Long
 For i = LastRow To 1 Step -1
     If Not (IsEmpty(Cells(i, 1))) Then Exit For
 Next i
 LastRow = i

```


</li>



## Find the Last Non-Empty Row in Worksheet


```vb
Private Sub Get_Last_Used_Row_Index()
    Dim wS As Worksheet
    
    Set wS = ThisWorkbook.Sheets("Sheet1")
    Debug.Print LastRow_1(wS)
    Debug.Print LastRow_0(wS)
End Sub

```

You can choose between 2 options,
regarding if you want to know if there is no data in the worksheet :

- NO : Use LastRow_1 : You can use it directly within `wS.Cells(LastRow_1(wS),...)`
- YES : Use LastRow_0 : You need to test if the result you get from the function is 0 or not before using it

```vb
Public Function LastRow_1(wS As Worksheet) As Double
    With wS
        If Application.WorksheetFunction.CountA(.Cells) <> 0 Then
            LastRow_1 = .Cells.Find(What:="*", _
                                After:=.Range("A1"), _
                                Lookat:=xlPart, _
                                LookIn:=xlFormulas, _
                                SearchOrder:=xlByRows, _
                                SearchDirection:=xlPrevious, _
                                MatchCase:=False).Row
        Else
            LastRow_1 = 1
        End If
    End With
End Function

Public Function LastRow_0(wS As Worksheet) As Double
    On Error Resume Next
    LastRow_0 = wS.Cells.Find(What:="*", _
                            After:=ws.Range("A1"), _
                            Lookat:=xlPart, _
                            LookIn:=xlFormulas, _
                            SearchOrder:=xlByRows, _
                            SearchDirection:=xlPrevious, _
                            MatchCase:=False).Row
End Function

```



## Find the Last Non-Empty Column in Worksheet


```vb
Private Sub Get_Last_Used_Row_Index()
    Dim wS As Worksheet
    
    Set wS = ThisWorkbook.Sheets("Sheet1")
    Debug.Print LastCol_1(wS)
    Debug.Print LastCol_0(wS)
End Sub

```

You can choose between 2 options,
regarding if you want to know if there is no data in the worksheet :

- NO : Use LastCol_1 : You can use it directly within `wS.Cells(...,LastCol_1(wS))`
- YES : Use LastCol_0 : You need to test if the result you get from the function is 0 or not before using it

```vb
Public Function LastCol_1(wS As Worksheet) As Double
    With wS
        If Application.WorksheetFunction.CountA(.Cells) <> 0 Then
            LastCol_1 = .Cells.Find(What:="*", _
                                After:=.Range("A1"), _
                                Lookat:=xlPart, _
                                LookIn:=xlFormulas, _
                                SearchOrder:=xlByColumns, _
                                SearchDirection:=xlPrevious, _
                                MatchCase:=False).Column
        Else
            LastCol_1 = 1
        End If
    End With
End Function

```

The Err object's properties are automatically reset to zero upon function exit.

```vb
Public Function LastCol_0(wS As Worksheet) As Double
    On Error Resume Next
    LastCol_0 = wS.Cells.Find(What:="*", _
                            After:=ws.Range("A1"), _
                            Lookat:=xlPart, _
                            LookIn:=xlFormulas, _
                            SearchOrder:=xlByColumns, _
                            SearchDirection:=xlPrevious, _
                            MatchCase:=False).Column
End Function

```



## Find the Last Non-Empty Cell in a Row


In this example, we will look at a method for returning the last non-empty column in a row.

This method will work regardless of empty regions within the data set.

However ****caution** should be used if **merged cells** are involved**, as the `End` method will be "stopped" against a merged region, returning the first cell of the merged region.

In addition non-empty cells in ****hidden columns**** will not be taken into account.

```vb
Sub FindingLastCol()
    Dim wS As Worksheet, LastCol As Long
    Set wS = ThisWorkbook.Worksheets("Sheet1")
    
    'Here we look in Row 1
    LastCol = wS.Cells(1, wS.Columns.Count).End(xlToLeft).Column
    Debug.Print LastCol
End Sub

```



## Find Last Row Using Named Range


In case you have a Named Range in your Sheet, and you want to dynamically get the last row of that Dynamic Named Range.
Also covers cases where the Named Range doesn't start from the first Row.

```vb
Sub FindingLastRow()
    
Dim sht As Worksheet
Dim LastRow As Long
Dim FirstRow As Long

Set sht = ThisWorkbook.Worksheets("form")

'Using Named Range "MyNameRange"
FirstRow = sht.Range("MyNameRange").Row

' in case "MyNameRange" doesn't start at Row 1
LastRow = sht.Range("MyNameRange").Rows.count + FirstRow - 1

End Sub

```

Update:
<br>A potential loophole was pointed out by @Jeeped for a a named range with non-contiguous rows as it generates unexpected result. To addresses that issue, the code is revised as below.
<br>Asumptions: targes sheet = `form`, named range = `MyNameRange`

```vb
Sub FindingLastRow()
    Dim rw As Range, rwMax As Long
    For Each rw In Sheets("form").Range("MyNameRange").Rows
        If rw.Row > rwMax Then rwMax = rw.Row
    Next
    MsgBox "Last row of 'MyNameRange' under Sheets 'form': " & rwMax
End Sub

```



## Get the row of the last cell in a range


```vb
'if only one area (not multiple areas):
With Range("A3:D20")
    Debug.Print .Cells(.Cells.CountLarge).Row
    Debug.Print .Item(.Cells.CountLarge).Row 'using .item is also possible
End With 'Debug prints: 20

'with multiple areas (also works if only one area):
Dim rngArea As Range, LastRow As Long
With Range("A3:D20, E5:I50, H20:R35")
    For Each rngArea In .Areas
        If rngArea(rngArea.Cells.CountLarge).Row > LastRow Then 
            LastRow = rngArea(rngArea.Cells.CountLarge).Row
        End If
    Next
    Debug.Print LastRow 'Debug prints: 50
End With

```



## Last cell in Range.CurrentRegion


[`Range.CurrentRegion`](https://msdn.microsoft.com/en-us/library/office/ff196678.aspx) is a rectangular range area surrounded by empty cells. Blank cells with formulas such as `=""` or `'` are not considered blank (even by the [`ISBLANK`](https://support.microsoft.com/en-us/kb/823838) Excel function).

```vb
Dim rng As Range, lastCell As Range
Set rng = Range("C3").CurrentRegion       ' or Set rng = Sheet1.UsedRange.CurrentRegion
Set lastCell = rng(rng.Rows.Count, rng.Columns.Count)

```



## Find the Last Non-Empty Cell in Worksheet - Performance (Array)


- The first function, using an array, is **much faster**
- If called without the optional parameter, will default to `.ThisWorkbook.ActiveSheet`
- If the range is empty will returns `Cell( 1, 1 )` as default, instead of `Nothing`

Speed:

> 
<p>`GetMaxCell (Array): Duration: 0.0000790063 seconds`<br />
`GetMaxCell (Find ): Duration: 0.0002903480 seconds`</p>
.Measured with [MicroTimer](https://msdn.microsoft.com/en-us/library/office/ff700515(v=office.14).aspx#Anchor_5)


```vb
Public Function GetLastCell(Optional ByVal ws As Worksheet = Nothing) As Range
    Dim uRng As Range, uArr As Variant, r As Long, c As Long
    Dim ubR As Long, ubC As Long, lRow As Long

    If ws Is Nothing Then Set ws = Application.ThisWorkbook.ActiveSheet
    Set uRng = ws.UsedRange
    uArr = uRng
    If IsEmpty(uArr) Then
        Set GetLastCell = ws.Cells(1, 1):   Exit Function
    End If
    If Not IsArray(uArr) Then
        Set GetLastCell = ws.Cells(uRng.Row, uRng.Column):  Exit Function
    End If
    ubR = UBound(uArr, 1):  ubC = UBound(uArr, 2)
    For r = ubR To 1 Step -1    '----------------------------------------------- last row
        For c = ubC To 1 Step -1
            If Not IsError(uArr(r, c)) Then
                If Len(Trim$(uArr(r, c))) > 0 Then
                    lRow = r:   Exit For
                End If
            End If
        Next
        If lRow > 0 Then Exit For
    Next
    If lRow = 0 Then lRow = ubR
    For c = ubC To 1 Step -1    '----------------------------------------------- last col
        For r = lRow To 1 Step -1
            If Not IsError(uArr(r, c)) Then
                If Len(Trim$(uArr(r, c))) > 0 Then
                    Set GetLastCell = ws.Cells(lRow + uRng.Row - 1, c + uRng.Column - 1)
                    Exit Function
                End If
            End If
        Next
    Next
End Function

```

```vb
'Returns last cell (max row & max col) using Find

Public Function GetMaxCell2(Optional ByRef rng As Range = Nothing) As Range 'Using Find

    Const NONEMPTY As String = "*"

    Dim lRow As Range, lCol As Range

    If rng Is Nothing Then Set rng = Application.ThisWorkbook.ActiveSheet.UsedRange

    If WorksheetFunction.CountA(rng) = 0 Then
        Set GetMaxCell2 = rng.Parent.Cells(1, 1)
    Else
        With rng
            Set lRow = .Cells.Find(What:=NONEMPTY, LookIn:=xlFormulas, _
                                        After:=.Cells(1, 1), _
                                        SearchDirection:=xlPrevious, _
                                        SearchOrder:=xlByRows)
            If Not lRow Is Nothing Then
                Set lCol = .Cells.Find(What:=NONEMPTY, LookIn:=xlFormulas, _
                                            After:=.Cells(1, 1), _
                                            SearchDirection:=xlPrevious, _
                                            SearchOrder:=xlByColumns)

                Set GetMaxCell2 = .Parent.Cells(lRow.Row, lCol.Column)
            End If
        End With
    End If
End Function

```

.

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



#### Remarks


You can find a good explanation on why other methods are discouraged/inaccurate here :
[http://stackoverflow.com/a/11169920/4628637](http://stackoverflow.com/a/11169920/4628637)

