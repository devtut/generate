---
metaTitle: "Excel VBA - Application object"
description: "Simple Application Object example: Minimize the Excel window, Simple Application Object example: Display Excel and VBE Version"
---

# Application object



## Simple Application Object example: Minimize the Excel window


This code uses the top level **Application** object to minimize the main Excel window.

```vb
Sub MinimizeExcel()

    Application.WindowState = xlMinimized

End Sub

```



## Simple Application Object example: Display Excel and VBE Version


```vb
Sub DisplayExcelVersions()

    MsgBox "The version of Excel is " & Application.Version
    MsgBox "The version of the VBE is " & Application.VBE.Version

End Sub

```

The use of the Application.Version property is useful for ensuring code only operates on a compatible version of Excel.



#### Remarks


Excel VBA comes with a comprehensive **object model** which contains classes and objects that you can use to manipulate any part of the running Excel application. One of the most common objects you'll use is the **Application** object. This is a top-level catchall that represents the current running instance of Excel. Almost everything that is not connected to a particular Excel workbook is in the **Application** object.

The **Application** object, as a top-level object, has literally hundreds of properties, methods, and events which can be used to control every aspect of Excel.

