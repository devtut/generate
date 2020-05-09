---
metaTitle: "VBA - Concatenating strings"
description: "Concatenate an array of strings using the Join function, Concatenate strings using the & operator"
---

# Concatenating strings



## Concatenate an array of strings using the Join function


```vb
'Declare and assign a string array
Dim widgetNames(2) As String
widgetNames(0) = "foo"
widgetNames(1) = "bar"
widgetNames(2) = "fizz"

'Concatenate with Join and separate each element with a 3-character string
concatenatedString = VBA.Strings.Join(widgetNames, " > ")
'concatenatedString = "foo > bar > fizz"

'Concatenate with Join and separate each element with a zero-width string
concatenatedString = VBA.Strings.Join(widgetNames, vbNullString)
'concatenatedString = "foobarfizz"

```



## Concatenate strings using the & operator


```vb
Const string1 As String = "foo"
Const string2 As String = "bar"
Const string3 As String = "fizz"
Dim concatenatedString As String

'Concatenate two strings
concatenatedString = string1 & string2
'concatenatedString = "foobar"

'Concatenate three strings
concatenatedString = string1 & string2 & string3
'concatenatedString = "foobarfizz"

```



#### Remarks


Strings can be concatenated, or joined together, using one or more concatenation operator `&`.

String arrays can also be concatenated using the `Join` function and providing a string (which can be zero-length) to be used between each array element.

