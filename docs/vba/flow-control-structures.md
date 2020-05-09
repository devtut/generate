---
metaTitle: "VBA - Flow control structures"
description: "For loop, Select Case, For Each loop, Do loop, While loop"
---

# Flow control structures



## For loop


The `For` loop is used to repeat the enclosed section of code a given number of times. The following simple example illustrates the basic syntax:

```vb
Dim i as Integer           'Declaration of i
For i = 1 to 10            'Declare how many times the loop shall be executed
    Debug.Print i          'The piece of code which is repeated
Next i                     'The end of the loop

```

The code above declares an Integer `i`. The `For` loop assigns every value between 1 and 10 to `i` and then executes `Debug.Print i` - i.e. the code prints the numbers 1 through 10 to the immediate window. Note that the loop variable is incremented by the `Next` statement, that is after the enclosed code executes as opposed to before it executes.

By default, the counter will be incremented by 1 each time the loop executes. However, a `Step` can be specified to change the amount of the increment as either a literal or the return value of a function. If the starting value, ending value, or `Step` value is a floating point number, it will be rounded to the nearest integer value. `Step` can be either a positive or negative value.

```vb
Dim i As Integer
For i = 1 To 10 Step 2
    Debug.Print i       'Prints 1, 3, 5, 7, and 9
Next

```

In general a `For` loop would be used in situations where it is known before the loop starts how many times to execute the enclosed code (otherwise a `Do` or `While` loop may be more appropriate). This is because the exit condition is fixed after the first entry into loop, as this code demonstrates:

```vb
Private Iterations As Long              'Module scope

Public Sub Example()
    Dim i As Long
    Iterations = 10
    For i = 1 To Iterations
        Debug.Print Iterations     'Prints 10 through 1, descending.
        Iterations = Iterations - 1
    Next
End Sub

```

A `For` loop can be exited early with the `Exit For` statement:

```vb
Dim i As Integer

For i = 1 To 10
    If i > 5 Then
        Exit For
    End If
    Debug.Print i       'Prints 1, 2, 3, 4, 5 before loop exits early.
Next

```



## Select Case


`Select Case` can be used when many different conditions are possible. The conditions are checked from top to bottom and only the first case that match will be executed.

```vb
Sub TestCase()
    Dim MyVar As String

    Select Case MyVar    'We Select the Variable MyVar to Work with
        Case "Hello"     'Now we simply check the cases we want to check
            MsgBox "This Case"
        Case "World"
            MsgBox "Important"
        Case "How"
            MsgBox "Stuff"
        Case "Are"
            MsgBox "I'm running out of ideas"
        Case "You?", "Today"  'You can separate several conditions with a comma
            MsgBox "Uuuhm..." 'if any is matched it will go into the case
        Case Else             'If none of the other cases is hit
            MsgBox "All of the other cases failed"
    End Select

    Dim i As Integer
    Select Case i
        Case Is > 2 '"Is" can be used instead of the variable in conditions.
            MsgBox "i is greater than 2"
        'Case 2 < Is '"Is" can only be used at the beginning of the condition.
        'Case Else is optional
    End Select
End Sub

```

The logic of the `Select Case` block can be inverted to support testing of different variables too, in this kind of scenario we can also use logical operators:

```vb
Dim x As Integer
Dim y As Integer

x = 2
y = 5

Select Case True
    Case x > 3
        MsgBox "x is greater than 3"
    Case y < 2
        MsgBox "y is less than 2"
    Case x = 1
        MsgBox "x is equal to 1"
    Case x = 2 Xor y = 3
        MsgBox "Go read about ""Xor"""
    Case Not y = 5
        MsgBox "y is not 5"
    Case x = 3 Or x = 10
        MsgBox "x = 3 or 10"
    Case y < 10 And x < 10
        MsgBox "x and y are less than 10"
    Case Else
        MsgBox "No match found"
End Select

```

Case statements can also use arithmetic operators. Where an arithmetic operator is being used against the `Select Case` value it should be preceded with the `Is` keyword:

```vb
Dim x As Integer

x = 5

Select Case x
    Case 1
        MsgBox "x equals 1"
    Case 2, 3, 4
        MsgBox "x is 2, 3 or 4"
    Case 7 To 10
        MsgBox "x is between 7 and 10 (inclusive)"
    Case Is < 2
        MsgBox "x is less than one"
    Case Is >= 7
        MsgBox "x is greater than or equal to 7"
    Case Else
        MsgBox "no match found"
End Select

```



## For Each loop


The `For Each` loop construct is ideal for iterating all elements of a collection.

```vb
Public Sub IterateCollection(ByVal items As Collection)

    'For Each iterator must always be variant
    Dim element As Variant

    For Each element In items
        'assumes element can be converted to a string
        Debug.Print element
    Next

End Sub

```

Use `For Each` when iterating object collections:

```vb
Dim sheet As Worksheet
For Each sheet In ActiveWorkbook.Worksheets
    Debug.Print sheet.Name
Next

```

Avoid `For Each` when iterating arrays; a `For` loop will offer significantly better performance with arrays. Conversely, a `For Each` loop will offer better performance when iterating a `Collection`.

### Syntax

```vb
For Each [item] In [collection]
    [statements]
Next [item]

```

The `Next` keyword may optionally be followed by the iterator variable; this can help clarify nested loops, although there are better ways to clarify nested code, such as extracting the inner loop into its own procedure.

```vb
Dim book As Workbook
For Each book In Application.Workbooks

    Debug.Print book.FullName

    Dim sheet As Worksheet
    For Each sheet In ActiveWorkbook.Worksheets
        Debug.Print sheet.Name
    Next sheet
Next book

```



## Do loop


```

Public Sub DoLoop()
    Dim entry As String
    entry = ""
    'Equivalent to a While loop will ask for strings until "Stop" in given
    'Prefer using a While loop instead of this form of Do loop
    Do While entry <> "Stop"
        entry = InputBox("Enter a string, Stop to end")
        Debug.Print entry
    Loop

    'Equivalent to the above loop, but the condition is only checked AFTER the
    'first iteration of the loop, so it will execute even at least once even 
    'if entry is equal to "Stop" before entering the loop (like in this case)
    Do
        entry = InputBox("Enter a string, Stop to end")
        Debug.Print entry
    Loop While entry <> "Stop"

    
    'Equivalent to writing Do While Not entry="Stop"
    '
    'Because the Until is at the top of the loop, it will
    'not execute because entry is still equal to "Stop"
    'when evaluating the condition
    Do Until entry = "Stop"
        entry = InputBox("Enter a string, Stop to end")
        Debug.Print entry
    Loop

    'Equivalent to writing Do ... Loop While Not i >= 100
    Do
        entry = InputBox("Enter a string, Stop to end")
        Debug.Print entry
    Loop Until entry = "Stop"
End Sub

```



## While loop


```vb
'Will return whether an element is present in the array
Public Function IsInArray(values() As String, ByVal whatToFind As String) As Boolean
    Dim i As Integer
    i = 0

    While i < UBound(values) And values(i) <> whatToFind
        i = i + 1
    Wend
    
    IsInArray = values(i) = whatToFind
End Function

```

