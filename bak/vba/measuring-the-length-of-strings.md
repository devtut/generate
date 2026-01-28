---
metaTitle: "VBA - Measuring the length of strings"
description: "Use the Len function to determine the number of characters in a string, Use the LenB function to determine the number of bytes in a string, Prefer `If Len(myString) = 0 Then` over `If myString =  Then`"
---

# Measuring the length of strings



## Use the Len function to determine the number of characters in a string


```vb
Const baseString As String = "Hello World"

Dim charLength As Long

charLength = Len(baseString)
'charlength = 11

```



## Use the LenB function to determine the number of bytes in a string


```vb
Const baseString As String = "Hello World"

Dim byteLength As Long

byteLength = LenB(baseString)
'byteLength = 22

```



## Prefer `If Len(myString) = 0 Then` over `If myString = "" Then`


When checking if a string is zero-length, it is better practice, and more efficient, to inspect the length of the string rather than comparing the string to an empty string.

```vb
Const myString As String = vbNullString

'Prefer this method when checking if myString is a zero-length string
If Len(myString) = 0 Then
    Debug.Print "myString is zero-length"
End If

'Avoid using this method when checking if myString is a zero-length string
If myString = vbNullString Then
    Debug.Print "myString is zero-length"
End If

```



#### Remarks


A string's length can be measured in two ways: The most frequently used measure of length is the number of characters using the `Len` functions, but VBA can also reveal the number of bytes using `LenB` functions. A double-byte or Unicode character is more than one byte long.

