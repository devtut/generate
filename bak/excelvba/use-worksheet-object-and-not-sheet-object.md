---
metaTitle: "Excel VBA - Use Worksheet object and not Sheet object"
description: "Print the name of the first object"
---

# Use Worksheet object and not Sheet object


Plenty of VBA users consider Worksheets and Sheets objects synonyms.
They are not.

Sheets object consists of both Worksheets and Charts. Thus, if we have charts in our Excel Workbook, we should be careful, not to use `Sheets` and `Worksheets` as synonyms.



## Print the name of the first object


[<img src="https://i.stack.imgur.com/x3VBw.png" alt="enter image description here" />](https://i.stack.imgur.com/x3VBw.png)

```vb
Option Explicit

Sub CheckWorksheetsDiagram()

    Debug.Print Worksheets(1).Name
    Debug.Print Charts(1).Name
    Debug.Print Sheets(1).Name

End Sub

```

The result:

```vb
Sheet1
Chart1
Chart1

```

