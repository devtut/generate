---
metaTitle: "Excel VBA - Arrays"
description: "Populating arrays (adding values), Dynamic Arrays (Array Resizing and Dynamic Handling), Jagged Arrays (Arrays of Arrays), Check if Array is Initialized (If it contains elements or not)., Dynamic Arrays [Array Declaration, Resizing]"
---

# Arrays



## Populating arrays (adding values)


There are multiple ways to populate an array.

### Directly

```vb
'one-dimensional
Dim arrayDirect1D(2) As String
arrayDirect(0) = "A"
arrayDirect(1) = "B"
arrayDirect(2) = "C"

'multi-dimensional (in this case 3D)
Dim arrayDirectMulti(1, 1, 2)
arrayDirectMulti(0, 0, 0) = "A"
arrayDirectMulti(0, 0, 1) = "B"
arrayDirectMulti(0, 0, 2) = "C"
arrayDirectMulti(0, 1, 0) = "D"
'...


```

### Using Array() function

```vb
'one-dimensional only
Dim array1D As Variant 'has to be type variant
array1D = Array(1, 2, "A")
'-> array1D(0) = 1, array1D(1) = 2, array1D(2) = "A"

```

### From range

```vb
Dim arrayRange As Variant 'has to be type variant
    
'putting ranges in an array always creates a 2D array (even if only 1 row or column)
'starting at 1 and not 0, first dimension is the row and the second the column
arrayRange = Range("A1:C10").Value
'-> arrayRange(1,1) = value in A1
'-> arrayRange(1,2) = value in B1
'-> arrayRange(5,3) = value in C5
'...
    
'Yoo can get an one-dimensional array from a range (row or column)
'by using the worksheet functions index and transpose:

'one row from range into 1D-Array:
arrayRange = Application.WorksheetFunction.Index(Range("A1:C10").Value, 3, 0)
'-> row 3 of range into 1D-Array
'-> arrayRange(1) = value in A3, arrayRange(2) = value in B3, arrayRange(3) = value in C3

'one column into 1D-Array:
'limited to 65536 rows in the column, reason: limit of .Transpose
arrayRange = Application.WorksheetFunction.Index( _
Application.WorksheetFunction.Transpose(Range("A1:C10").Value), 2, 0)
'-> column 2 of range into 1D-Array
'-> arrayRange(1) = value in B1, arrayRange(2) = value in B2, arrayRange(3) = value in B3
'...

'By using Evaluate() - shorthand [] - you can transfer the
'range to an array and change the values at the same time.
'This is equivalent to an array formula in the sheet:
arrayRange = [(A1:C10*3)]
arrayRange = [(A1:C10&"_test")]
arrayRange = [(A1:B10*C1:C10)]
'...

```

### 2D with Evaluate()

```vb
Dim array2D As Variant
'[] ist a shorthand for evaluate()
'Arrays defined with evaluate start at 1 not 0
array2D = [{"1A","1B","1C";"2A","2B","3B"}]
'-> array2D(1,1) = "1A", array2D(1,2) = "1B", array2D(2,1) = "2A" ...

'if you want to use a string to fill the 2D-Array:
Dim strValues As String
strValues = "{""1A"",""1B"",""1C"";""2A"",""2B"",""2C""}"
array2D = Evaluate(strValues)

```

### Using Split() function

```vb
Dim arraySplit As Variant 'has to be type variant
arraySplit = Split("a,b,c", ",")
'-> arraySplit(0) = "a", arraySplit(1) = "b", arraySplit(2) = "c"

```



## Dynamic Arrays (Array Resizing and Dynamic Handling)


**Due to not being Excel-VBA exclusive contents this Example has been moved to VBA documentation.**

Link:
[Dynamic Arrays (Array Resizing and Dynamic Handling)](http://stackoverflow.com/documentation/vba/3064/arrays/16562/dynamic-arrays-array-resizing-and-dynamic-handling#t=201607301200366923224)



## Jagged Arrays (Arrays of Arrays)


**Due to not being Excel-VBA exclusive contents this Example has been moved to VBA documentation.**

Link:
[Jagged Arrays (Arrays of Arrays)](http://stackoverflow.com/documentation/vba/3064/arrays/16563/jagged-arrays-arrays-of-arrays#t=201607301204347129777)



## Check if Array is Initialized (If it contains elements or not).


A common problem might be trying to iterate over Array which has no values in it. For example:

```vb
Dim myArray() As Integer
For i = 0 To UBound(myArray) 'Will result in a "Subscript Out of Range" error

```

To avoid this issue, and to check if an Array contains elements, use this **oneliner**:

```vb
If Not Not myArray Then MsgBox UBound(myArray) Else MsgBox "myArray not initialised"

```



## Dynamic Arrays [Array Declaration, Resizing]


```vb
Sub Array_clarity()

Dim arr() As Variant  'creates an empty array
Dim x As Long
Dim y As Long

x = Range("A1", Range("A1").End(xlDown)).Cells.Count
y = Range("A1", Range("A1").End(xlToRight)).Cells.Count

ReDim arr(0 To x, 0 To y) 'fixing the size of the array

For x = LBound(arr, 1) To UBound(arr, 1)
    For y = LBound(arr, 2) To UBound(arr, 2)
        arr(x, y) = Range("A1").Offset(x, y) 'storing the value of Range("A1:E10") from activesheet in x and y variables
    Next
Next

'Put it on the same sheet according to the declaration:
Range("A14").Resize(UBound(arr, 1), UBound(arr, 2)).Value = arr

End Sub

```

