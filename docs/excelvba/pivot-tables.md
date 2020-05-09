---
metaTitle: "Excel VBA - Pivot Tables"
description: "Adding Fields to a Pivot Table, Creating a Pivot Table, Pivot Table Ranges, Formatting the Pivot Table Data"
---

# Pivot Tables



## Adding Fields to a Pivot Table


Two important things to note when adding fields to a Pivot Table are Orientation and Position. Sometimes a developer may assume where a field is placed, so it's always clearer to explicitly define these parameters. These actions only affect the given Pivot Table, not the Pivot Cache.

```vb
Dim thisPivot As PivotTable
Dim ptSheet As Worksheet
Dim ptField As PivotField

Set ptSheet = ThisWorkbook.Sheets("SheetNameWithPivotTable")
Set thisPivot = ptSheet.PivotTables(1)

With thisPivot
    Set ptField = .PivotFields("Gender")
    ptField.Orientation = xlRowField
    ptField.Position = 1
    Set ptField = .PivotFields("LastName")
    ptField.Orientation = xlRowField
    ptField.Position = 2
    Set ptField = .PivotFields("ShirtSize")
    ptField.Orientation = xlColumnField
    ptField.Position = 1
    Set ptField = .AddDataField(.PivotFields("Cost"), "Sum of Cost", xlSum)
    .InGridDropZones = True
    .RowAxisLayout xlTabularRow
End With

```



## Creating a Pivot Table


One of the most powerful capabilities in Excel is the use of Pivot Tables to sort and analyze data. Using VBA to create and manipulate the Pivots is easier if you understand the relationship of Pivot Tables to Pivot Caches and how to reference and use the different parts of the Tables.

At its most basic, your source data is a `Range` area of data on a `Worksheet`. This data area **MUST** identify the data columns with a header row as the first row in the range. Once the Pivot Table is created, the user may view and change the source data at any time. However, changes may not be automatically or immediately reflected in the Pivot Table itself because there is an intermediate data storage structure called the Pivot Cache that is directly connected to the Pivot Table itself.

[<img src="http://i.stack.imgur.com/weSrg.png" alt="enter image description here" />](http://i.stack.imgur.com/weSrg.png)

If multiple Pivot Tables are needed, based on the same source data, the Pivot Cache may be re-used as the internal data store for each of the Pivot Tables. This is a good practice because it saves memory and reduces the size of the Excel file for storage.

[<img src="http://i.stack.imgur.com/Lxbln.png" alt="enter image description here" />](http://i.stack.imgur.com/Lxbln.png)

As an example, to create a Pivot Table based on the source data shown in the Figures above:

```vb
Sub test()
    Dim pt As PivotTable
    Set pt = CreatePivotTable(ThisWorkbook.Sheets("Sheet1").Range("A1:E15"))
End Sub

Function CreatePivotTable(ByRef srcData As Range) As PivotTable
    '--- creates a Pivot Table from the given source data and
    '    assumes that the first row contains valid header data
    '    for the columns
    Dim thisPivot As PivotTable
    Dim dataSheet As Worksheet
    Dim ptSheet As Worksheet
    Dim ptCache As PivotCache
    
    '--- the Pivot Cache must be created first...
    Set ptCache = ThisWorkbook.PivotCaches.Create(SourceType:=xlDatabase, _
                                                  SourceData:=srcData)
    '--- ... then use the Pivot Cache to create the Table
    Set ptSheet = ThisWorkbook.Sheets.Add
    Set thisPivot = ptCache.CreatePivotTable(TableDestination:=ptSheet.Range("A3"))
    Set CreatePivotTable = thisPivot
End Function

```

> 
<p>**References**
[MSDN Pivot Table Object](https://msdn.microsoft.com/en-us/library/office/ff837611.aspx)</p>




## Pivot Table Ranges


These excellent reference sources provide descriptions and illustrations of the various ranges in Pivot Tables.

> 
**References**
<ul>
- [Referencing Pivot Table Ranges in VBA](http://peltiertech.com/referencing-pivot-table-ranges-in-vba/) - from Jon Peltier's Tech Blog
- [Referencing an Excel Pivot Table Range using VBA](http://www.globaliconnect.com/excel/index.php?option=com_content&view=article&id=154:referencing-an-excel-pivot-table-range-using-vba&catid=79&Itemid=475) - from globaliconnect Excel VBA
</ul>




## Formatting the Pivot Table Data


This example changes/sets several formats in the data range area (`DataBodyRange`) of the given Pivot Table. All formattable parameters in a standard `Range` are available. Formatting the data only affects the Pivot Table itself, not the Pivot Cache.

NOTE: the property is named `TableStyle2` because the `TableStyle` property is not a member of the `PivotTable`'s object properties.

```vb
Dim thisPivot As PivotTable
Dim ptSheet As Worksheet
Dim ptField As PivotField

Set ptSheet = ThisWorkbook.Sheets("SheetNameWithPivotTable")
Set thisPivot = ptSheet.PivotTables(1)

With thisPivot
    .DataBodyRange.NumberFormat = "_($* #,##0.00_);_($* (#,##0.00);_($* "-"??_);_(@_)"
    .DataBodyRange.HorizontalAlignment = xlRight
    .ColumnRange.HorizontalAlignment = xlCenter
    .TableStyle2 = "PivotStyleMedium9"
End With

```



#### Remarks


There are many excellent reference and example sources on the Web. Some examples and explanations are created here as a collection point for quick answers. More detailed illustrations may be linked to external content (instead of copying existing original material).

