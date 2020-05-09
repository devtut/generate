---
metaTitle: "Excel VBA - Locating duplicate values in a range"
description: "Find duplicates in a range"
---

# Locating duplicate values in a range




## Find duplicates in a range


The following tests range A2 to A7 for duplicate values.
**Remark:** This example illustrates a possible solution as a first approach to a solution. It's faster to use an array than a range and one could use collections or dictionaries or xml methods to check for duplicates.

```

   Sub find_duplicates()
' Declare variables
  Dim ws     As Worksheet               ' worksheet
  Dim cell   As Range                   ' cell within worksheet range
  Dim n      As Integer                 ' highest row number
  Dim bFound As Boolean                 ' boolean flag, if duplicate is found
  Dim sFound As String: sFound = "|"    ' found duplicates
  Dim s      As String                  ' message string
  Dim s2     As String                  ' partial message string
' Set Sheet to memory
  Set ws = ThisWorkbook.Sheets("Duplicates")

' loop thru FULLY QUALIFIED REFERENCE
  For Each cell In ws.Range("A2:A7")
    bFound = False: s2 = ""             ' start each cell with empty values
 '  Check if first occurrence of this value as duplicate to avoid further searches
    If InStr(sFound, "|" & cell & "|") = 0 Then
    
      For n = cell.Row + 1 To 7           ' iterate starting point to avoid REDUNDANT SEARCH
        If cell = ws.Range("A" & n).Value Then
           If cell.Row <> n Then        ' only other cells, as same cell cannot be a duplicate
                 bFound = True             ' boolean flag
              '  found duplicates in cell A{n}
                 s2 = s2 & vbNewLine & " -> duplicate in A" & n
           End If
        End If
       Next
     End If
   ' notice all found duplicates
     If bFound Then
         ' add value to list of all found duplicate values
         ' (could be easily split to an array for further analyze)
           sFound = sFound & cell & "|"
           s = s & cell.Address & " (value=" & cell & ")" & s2 & vbNewLine & vbNewLine
     End If
   Next
' Messagebox with final result
  MsgBox "Duplicate values are " & sFound & vbNewLine & vbNewLine & s, vbInformation, "Found duplicates"
End Sub

```

Depending on your needs, the example can be modified - for instance, the upper limit of n can be the row value of last cell with data in the range, or the action in case of a True If condition can be edited to extract the duplicate value somewhere else. However, the mechanics of the routine would not change.

