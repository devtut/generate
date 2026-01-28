---
metaTitle: "Excel VBA - Creating a drop-down menu in the Active Worksheet with a Combo Box"
description: "Example 2: Options Not Included, Jimi Hendrix Menu"
---

# Creating a drop-down menu in the Active Worksheet with a Combo Box


This is a simple example demonstrating how to create a drop down menu in the Active Sheet of your workbook by inserting a Combo Box Activex object in the sheet. You'll be able to insert one of five Jimi Hendrix songs in any activated cell of the sheet and be able to clear it, accordingly.



## Example 2: Options Not Included


This example is used in specifying options that might not be included in a database of available housing and its attendant amenities.

It builds on the previous example, with some differences:

1. Two procedures are no longer necessary for a single combo box, done by combining the code into a single procedure.
1. The use of the LinkedCell property to allow for the correct input of the user selection every time
1. The inclusion of a backup feature for ensuring the active cell is in the correct column and an error prevention code, based on previous experience, where numeric values would formatted as strings when populated to the active cell.

```vb
Private Sub cboNotIncl_Change()

Dim n As Long
Dim notincl_array(1 To 9) As String

n = myTarget.Row
            
    If n >= 3 And n < 10000 Then
             
        If myTarget.Address = "$G$" & n Then
                
            'set up the array elements for the not included services
            notincl_array(1) = "Central Air"
            notincl_array(2) = "Hot Water"
            notincl_array(3) = "Heater Rental"
            notincl_array(4) = "Utilities"
            notincl_array(5) = "Parking"
            notincl_array(6) = "Internet"
            notincl_array(7) = "Hydro"
            notincl_array(8) = "Hydro/Hot Water/Heater Rental"
            notincl_array(9) = "Hydro and Utilities"
            
            cboNotIncl.List = notincl_array()
                    
        Else
                
            Exit Sub
                
        End If

        With cboNotIncl
                    
            'make sure the combo box moves to the target cell
            .Left = myTarget.Left
            .Top = myTarget.Top
                    
            'adjust the size of the cell to fit the combo box
            myTarget.ColumnWidth = .Width * 0.18
                    
            'make it look nice by editing some of the font attributes
            .Font.Size = 11
            .Font.Bold = False
                        
            'populate the cell with the user choice, with a backup guarantee that it's in column G
                
            If myTarget.Address = "$G$" & n Then
                    
                    .LinkedCell = myTarget.Address
                    
                    'prevent an error where a numerical value is formatted as text
                    myTarget.EntireColumn.TextToColumns
                    
            End If
                
        End With
                
    End If 'ensure that the active cell is only between rows 3 and 1000
            
End Sub

```

The above macro is initiated every time a cell is activated with the SelectionChange event in the worksheet module:

```vb
Public myTarget As Range

Private Sub Worksheet_SelectionChange(ByVal Target As Range)

    Set myTarget = Target

    'switch for Not Included
    If Target.Column = 7 And Target.Cells.Count = 1 Then

        Application.Run "Module1.cboNotIncl_Change"

    End If

End Sub

```



## Jimi Hendrix Menu


In general, the code is placed in the module of a sheet.

This is the Worksheet_SelectionChange event, which fires each time a different cell is selected in the active sheet. You can select "Worksheet" from the first drop-down menu above the code window, and "Selection_Change" from the drop down menu next to it. In this case, every time you activate a cell, the code is redirected to the Combo Box's code.

```vb
Private Sub Worksheet_SelectionChange(ByVal Target As Range)

   ComboBox1_Change
   
End Sub

```

Here, the routine dedicated to the ComboBox is coded to the Change event by default. In it, there is a fixed array, populated with all the options. Not the CLEAR option in the last position, which will be used to clear the contents of a cell. The array then is handed to to the Combo Box and passed to the routine that does the work.

```vb
Private Sub ComboBox1_Change()

Dim myarray(0 To 5)
    myarray(0) = "Hey Joe"
    myarray(1) = "Little Wing"
    myarray(2) = "Voodoo Child"
    myarray(3) = "Purple Haze"
    myarray(4) = "The Wind Cries Mary"
    myarray(5) = "CLEAR"
    
    With ComboBox1
        .List = myarray()
    End With

    FillACell myarray()

End Sub

```

The array is passed to the routine that fills the cells with the song name or null value to empty them. First, an integer variable is given the value of the position of the choice that the user makes. Then, the Combo Box is moved to the TOP LEFT corner of the cell the user activates and its dimensions adjusted to make the experience more fluid. The active cell is then assigned the value in the position in the integer variable, which tracks the user choice. In case the user selects CLEAR from the options, the cell is emptied.

The entire routine repeats for each selected cell.

```vb
Sub FillACell(MyArray As Variant)

Dim n As Integer

n = ComboBox1.ListIndex

ComboBox1.Left = ActiveCell.Left
ComboBox1.Top = ActiveCell.Top
Columns(ActiveCell.Column).ColumnWidth = ComboBox1.Width * 0.18

ActiveCell = MyArray(n)

If ComboBox1 = "CLEAR" Then
    Range(ActiveCell.Address) = ""
End If

End Sub

```

