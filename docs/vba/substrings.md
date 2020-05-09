---
metaTitle: "VBA - Substrings"
description: "Use Left or Left$ to get the 3 left-most characters in a string, Use Right or Right$ to get the 3 right-most characters in a string, Use Mid or Mid$ to get specific characters from within a string, Use Trim to get a copy of the string without any leading or trailing spaces"
---

# Substrings



## Use Left or Left$ to get the 3 left-most characters in a string


```vb
Const baseString As String = "Foo Bar"

Dim leftText As String
leftText = Left$(baseString, 3)
'leftText = "Foo"

```



## Use Right or Right$ to get the 3 right-most characters in a string


```vb
Const baseString As String = "Foo Bar"
Dim rightText As String
rightText = Right$(baseString, 3)
'rightText = "Bar"

```



## Use Mid or Mid$ to get specific characters from within a string


```vb
Const baseString As String = "Foo Bar"

'Get the string starting at character 2 and ending at character 6
Dim midText As String
midText = Mid$(baseString, 2, 5)
'midText = "oo Ba"

```



## Use Trim to get a copy of the string without any leading or trailing spaces


```vb
'Trim the leading and trailing spaces in a string
Const paddedText As String = "    Foo Bar    "
Dim trimmedText As String
trimmedText = Trim$(paddedText)
'trimmedText = "Foo Bar"

```



#### Remarks


VBA has built-in functions for extracting specific parts of strings, including:

- `Left`/`Left$`
- `Right`/`Right$`
- `Mid`/`Mid$`
- `Trim`/`Trim$`

To avoid implicit type conversion onverhead (and therefore for better performance), use the $-suffixed version of the function when a string variable is passed to the function, and/or if the result of the function is assigned to a string variable.

Passing a `Null` parameter value to a $-suffixed function will raise a runtime error ("invalid use of null") - this is especially relevant for code involving a database.

