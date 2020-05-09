---
metaTitle: "VBA - CreateObject vs. GetObject"
description: "Demonstrating GetObject and CreateObject"
---

# CreateObject vs. GetObject




## Demonstrating GetObject and CreateObject


[MSDN-GetObject Function](https://msdn.microsoft.com/en-us/library/office/gg251785.aspx)

> 
Returns a reference to an object provided by an ActiveX component.


> 
Use the GetObject function when there is a current instance of the object or if you want to create the object with a file already loaded. If there is no current instance, and you don't want the object started with a file loaded, use the CreateObject function.


```vb
Sub CreateVSGet()
    Dim ThisXLApp As Excel.Application 'An example of early binding
    Dim AnotherXLApp As Object 'An example of late binding
    Dim ThisNewWB As Workbook
    Dim AnotherNewWB As Workbook
    Dim wb As Workbook
    
    'Get this instance of Excel
    Set ThisXLApp = GetObject(ThisWorkbook.Name).Application
    'Create another instance of Excel
    Set AnotherXLApp = CreateObject("Excel.Application")
    'Make the 2nd instance visible
    AnotherXLApp.Visible = True
    'Add a workbook to the 2nd instance
    Set AnotherNewWB = AnotherXLApp.Workbooks.Add
    'Add a sheet to the 2nd instance
    AnotherNewWB.Sheets.Add
    
    'You should now have 2 instances of Excel open
    'The 1st instance has 1 workbook: Book1
    'The 2nd instance has 1 workbook: Book2
    
    'Lets add another workbook to our 1st instance
    Set ThisNewWB = ThisXLApp.Workbooks.Add
    'Now loop through the workbooks and show their names
    For Each wb In ThisXLApp.Workbooks
        Debug.Print wb.Name
    Next
    'Now the 1st instance has 2 workbooks: Book1 and Book3
    'If you close the first instance of Excel,
    'Book1 and Book3 will close, but book2 will still be open
    
End Sub

```



#### Remarks


At its simplest, `CreateObject` creates an instance of an object whereas `GetObject` gets an existing instance of an object. Determining whether an object can be created or gotten will depend on it's [Instancing property](https://msdn.microsoft.com/en-us/library/aa242107%28v=vs.60%29.aspx?f=255&MSPPError=-2147217396). Some objects are SingleUse (eg, WMI) and cannot be created if they already exist. Other objects (eg, Excel) are MultiUse and allow multiple instances to run at once. If an instance of an object does not already exist and you attempt `GetObject`, you will receive the following trappable message: `Run-time error '429': ActiveX component can't create object`.

**GetObject** requires at least one of these two optional parameters to be present:

1. **Pathname** - Variant (String): The full path, including filename, of the file containing the object. This parameter is optional, but **Class** is required if **Pathname** is omitted.
1. **Class** - Variant (String): A string representing the formal definition (Application and ObjectType) of the object. **Class** is required if **Pathname** is omitted.

**CreateObject** has one required parameter and one optional parameter:

1. **Class**  - Variant (String): A string representing the formal definition (Application and ObjectType) of the object. **Class** is a required parameter.
1. **Servername** - Variant (String): The name of the remote computer on which the object will be created. If omitted, the object will be created on the local machine.

**Class** is always comprised of two parts in the form of `Application.ObjectType`:

1. **Application** - The name of the application which the object is part of. |
1. **Object Type** - The type of object being created. |

Some example classes are:

1. Word.Application
1. Excel.Sheet
1. Scripting.FileSystemObject

