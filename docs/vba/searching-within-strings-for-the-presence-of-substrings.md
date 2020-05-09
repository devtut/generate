---
metaTitle: "VBA - Searching within strings for the presence of substrings"
description: "Use InStr to determine if a string contains a substring, Use InStrRev to find the position of the last instance of a substring, Use InStr to find the position of the first instance of a substring"
---

# Searching within strings for the presence of substrings



## Use InStr to determine if a string contains a substring


```vb
Const baseString As String = "Foo Bar"
Dim containsBar As Boolean

'Check if baseString contains "bar" (case insensitive)
containsBar = InStr(1, baseString, "bar", vbTextCompare) > 0
'containsBar = True

'Check if baseString contains bar (case insensitive)
containsBar = InStr(1, baseString, "bar", vbBinaryCompare) > 0
'containsBar = False

```



## Use InStrRev to find the position of the last instance of a substring


```vb
Const baseString As String = "Foo Bar"
Dim containsBar As Boolean

'Find the position of the last "B"
Dim posX As Long
'Note the different number and order of the paramters for InStrRev
posX = InStrRev(baseString, "X", -1, vbBinaryCompare)
'posX = 0

```



## Use InStr to find the position of the first instance of a substring


```vb
Const baseString As String = "Foo Bar"
Dim containsBar As Boolean

Dim posB As Long
posB = InStr(1, baseString, "B", vbBinaryCompare)
'posB = 5

```



#### Remarks


When you need to check for the presence or position of a substring within a string, VBA offers the `InStr` and `InStrRev` functions that return the character position of the substring in the string, if it is present.

