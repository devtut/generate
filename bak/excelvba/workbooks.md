---
metaTitle: "Excel VBA - Workbooks"
description: "When To Use ActiveWorkbook and ThisWorkbook, Changing The Default Number of Worksheets In A New Workbook, Application Workbooks, Opening A (New) Workbook, Even If It's Already Open, Saving A Workbook Without Asking The User"
---

# Workbooks



## When To Use ActiveWorkbook and ThisWorkbook


It's a VBA Best Practice to always specify which workbook your VBA code refers. If this specification is omitted, then VBA assumes the code is directed at the currently active workbook (`ActiveWorkbook`).

```vb
'--- the currently active workbook (and worksheet) is implied
Range("A1").value = 3.1415
Cells(1, 1).value = 3.1415

```

However, when several workbooks are open at the same time -- particularly and especially when VBA code is running from an Excel Add-In -- references to the `ActiveWorkbook` may be confused or misdirected. For example, an add-in with a UDF that checks the time of day and compares it to a value stored on one of the add-in's worksheets (that are typically not readily visible to the user) will have to explicitly identify which workbook is being referenced. In our example, our open (and active) workbook has a formula in cell A1 `=EarlyOrLate()` and does NOT have any VBA written for that active workbook. In our add-in, we have the following User Defined Function (UDF):

```vb
Public Function EarlyOrLate() As String
    If Hour(Now) > ThisWorkbook.Sheets("WatchTime").Range("A1") Then
        EarlyOrLate = "It's Late!"
    Else
        EarlyOrLate = "It's Early!"
    End If
End Function

```

The code for the UDF is written and stored in the installed Excel add-in. It uses data stored on a worksheet in the add-in called "WatchTime". If the UDF had used `ActiveWorkbook` instead of `ThisWorkbook`, then it would never be able to guarantee which workbook was intended.



## Changing The Default Number of Worksheets In A New Workbook


The "factory default" number of worksheets created in a new Excel workbook is generally set to three. Your VBA code can explicitly set the number of worksheets in a new workbook.

```vb
'--- save the current Excel global setting
With Application
    Dim oldSheetsCount As Integer
    oldSheetsCount = .SheetsInNewWorkbook
    Dim myNewWB As Workbook
    .SheetsInNewWorkbook = 1
    Set myNewWB = .Workbooks.Add
    '--- restore the previous setting
    .SheetsInNewWorkbook = oldsheetcount
End With

```



## Application Workbooks


In many Excel applications, the VBA code takes actions directed at the workbook in which it's contained. You save that workbook with a ".xlsm" extension and the VBA macros only focus on the worksheets and data within. However, there are often times when you need to combine or merge data from other workbooks, or write some of your data to a separate workbook. Opening, closing, saving, creating, and deleting other workbooks is a common need for many VBA applications.

At any time in the VBA Editor, you can view and access any and all workbooks currently open by that instance of Excel by using the `Workbooks` property of the `Application` object. The [MSDN Documentation](https://msdn.microsoft.com/en-us/library/office/ff820765.aspx) explains it with references.



## Opening A (New) Workbook, Even If It's Already Open


If you want to access a workbook that's already open, then getting the assignment from the `Workbooks` collection is straightforward:

```vb
dim myWB as Workbook
Set myWB = Workbooks("UsuallyFullPathnameOfWorkbook.xlsx")

```

If you want to create a new workbook, then use the `Workbooks` collection object to `Add` a new entry.

```vb
Dim myNewWB as Workbook
Set myNewWB = Workbooks.Add

```

There are times when you may not or (or care) if the workbook you need is open already or not, or possible does not exist. The example function shows how to always return a valid workbook object.

```vb
Option Explicit
Function GetWorkbook(ByVal wbFilename As String) As Workbook
    '--- returns a workbook object for the given filename, including checks
    '    for when the workbook is already open, exists but not open, or
    '    does not yet exist (and must be created)
    '    ***  wbFilename must be a fully specified pathname
    Dim folderFile As String
    Dim returnedWB As Workbook
    
    '--- check if the file exists in the directory location
    folderFile = File(wbFilename)
    If folderFile = "" Then
        '--- the workbook doesn't exist, so create it
        Dim pos1 As Integer
        Dim fileExt As String
        Dim fileFormatNum As Long
        '--- in order to save the workbook correctly, we need to infer which workbook
        '    type the user intended from the file extension
        pos1 = InStrRev(sFullName, ".", , vbTextCompare)
        fileExt = Right(sFullName, Len(sFullName) - pos1)
        Select Case fileExt
            Case "xlsx"
                fileFormatNum = 51
            Case "xlsm"
                fileFormatNum = 52
            Case "xls"
                fileFormatNum = 56
            Case "xlsb"
                fileFormatNum = 50
            Case Else
                Err.Raise vbObjectError + 1000, "GetWorkbook function", _
                         "The file type you've requested (file extension) is not recognized. " & _
                         "Please use a known extension: xlsx, xlsm, xls, or xlsb."
        End Select
        Set returnedWB = Workbooks.Add
        Application.DisplayAlerts = False
        returnedWB.SaveAs filename:=wbFilename, FileFormat:=fileFormatNum
        Application.DisplayAlerts = True
        Set GetWorkbook = returnedWB
    Else
        '--- the workbook exists in the directory, so check to see if
        '    it's already open or not
        On Error Resume Next
        Set returnedWB = Workbooks(sFile)
        If returnedWB Is Nothing Then
            Set returnedWB = Workbooks.Open(sFullName)
        End If
    End If
End Function

```



## Saving A Workbook Without Asking The User


Often saving new data in an existing workbook using VBA will cause a pop-up question noting that the file already exists.

To prevent this pop-up question, you have to suppress these types of alerts.

```vb
Application.DisplayAlerts = False        'disable user prompt to overwrite file
myWB.SaveAs FileName:="NewOrExistingFilename.xlsx"
Application.DisplayAlerts = True         're-enable user prompt to overwrite file

```

