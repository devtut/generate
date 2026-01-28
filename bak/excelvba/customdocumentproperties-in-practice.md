---
metaTitle: "Excel VBA - CustomDocumentProperties in practice"
description: "Organizing new invoice numbers"
---

# CustomDocumentProperties in practice


Using CustomDocumentProperties (CDPs) is a good method to store user defined values in a relatively safe way within the same work book, but avoiding to show related cell values simply in an unprotected work sheet *).

Note: CDPs represent a separate collection comparable to BuiltInDocumentProperties, but allow to create user defined property names of your own instead of a fixed collection.

*) Alternatively, you could enter values also in a hidden or "very hidden" workbook.



## Organizing new invoice numbers


Incrementing an invoice number and saving its value is a frequent task.
Using CustomDocumentProperties (CDPs) is a good method to store such numbers in a relatively safe way within the same work book, but avoiding to show related cell values simply in an unprotected work sheet.

**Additional hint:**

Alternatively, you could enter values also in a hidden worksheet or even a so called "very hidden" worksheet (see [Using xlVeryHidden Sheets](https://stackoverflow.com/documentation/excel-vba/2240/excel-vba-tips-and-tricks/7333/using-xlveryhidden-sheets#t=201707311517265257827)). Of course, it's possible to save data also to external files (e.g. ini file, csv or any other type) or the registry.

**Example content**:

The example below shows

- a function NextInvoiceNo that sets and returns the next invoice number,
- a procedure DeleteInvoiceNo, that deletes the invoice CDP completely, as well as
- a procedure showAllCDPs listing the complete CDPs collection with all names. Not using VBA, you can also list them via the workbook's information: Info | Properties [DropDown:] | Advanced Properties | Custom

You can get and set the next invoice number (last no plus one) simply by calling the above mentioned function, returning a string value in order to facilitate adding prefixes. "InvoiceNo" is implicitly used as CDP name in all procedures.

```vb
Dim sNumber As String
sNumber = NextInvoiceNo ()

```

**Example code:**

```vb
Option Explicit

Sub Test()
  Dim sNumber As String
  sNumber = NextInvoiceNo()
  MsgBox "New Invoice No: " & sNumber, vbInformation, "New Invoice Number"
End Sub

Function NextInvoiceNo() As String
' Purpose: a) Set Custom Document Property (CDP) "InvoiceNo" if not yet existing
'          b) Increment CDP value and return new value as string
' Declarations
  Dim prop As Object
  Dim ret  As String
  Dim wb   As Workbook
' Set workbook and CDPs
  Set wb = ThisWorkbook
  Set prop = wb.CustomDocumentProperties

  ' -------------------------------------------------------
  ' Generate new CDP "InvoiceNo" if not yet existing
  ' -------------------------------------------------------
    If Not CDPExists("InvoiceNo") Then
    '  set temporary starting value "0"
       prop.Add "InvoiceNo", False, msoPropertyTypeString, "0"
    End If

  ' --------------------------------------------------------
  ' Increment invoice no and return function value as string
  ' --------------------------------------------------------
       ret = Format(Val(prop("InvoiceNo")) + 1, "0")
  ' a) Set CDP "InvoiceNo" = ret
       prop("InvoiceNo").value = ret
  ' b) Return function value 
       NextInvoiceNo = ret
End Function

Private Function CDPExists(sCDPName As String) As Boolean
' Purpose: return True if custom document property (CDP) exists
' Method: loop thru CustomDocumentProperties collection and check if name parameter exists
' Site: cf. http://stackoverflow.com/questions/23917977/alternatives-to-public-variables-in-vba/23918236#23918236
' vgl.: https://answers.microsoft.com/en-us/msoffice/forum/msoffice_word-mso_other/using-customdocumentproperties-with-vba/91ef15eb-b089-4c9b-a8a7-1685d073fb9f
' Declarations
  Dim cdp As Variant      ' element of CustomDocumentProperties Collection
  Dim boo As Boolean      ' boolean value showing element exists
  For Each cdp In ThisWorkbook.CustomDocumentProperties
    If LCase(cdp.Name) = LCase(sCDPName) Then
       boo = True      ' heureka
       Exit For        ' exit loop
    End If
  Next
  CDPExists = boo          ' return value to function
End Function


Sub DeleteInvoiceNo()
' Declarations
  Dim wb     As Workbook
  Dim prop   As Object
' Set workbook and CDPs
  Set wb = ThisWorkbook
  Set prop = wb.CustomDocumentProperties

' ----------------------
' Delete CDP "InvoiceNo"
' ----------------------
 If CDPExists("InvoiceNo") Then
    prop("InvoiceNo").Delete
 End If

```

End Sub

```vb
Sub showAllCDPs()
' Purpose: Show all CustomDocumentProperties (CDP) and values (if set)
' Declarations
  Dim wb      As Workbook
  Dim cdp     As Object

  Dim i       As Integer
  Dim maxi   As Integer
  Dim s       As String
' Set workbook and CDPs
  Set wb = ThisWorkbook
  Set cdp = wb.CustomDocumentProperties
' Loop thru CDP getting name and value
  maxi = cdp.Count
  For i = 1 To maxi
    On Error Resume Next    ' necessary in case of unset value
    s = s & Chr(i + 96) & ") " & _
            cdp(i).Name & "=" & cdp(i).value & vbCr
  Next i
' Show result string
  Debug.Print s
End Sub

```

