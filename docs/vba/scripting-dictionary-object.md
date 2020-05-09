---
metaTitle: "VBA - Scripting.Dictionary object"
description: "Properties and Methods, Getting unique values with Scripting.Dictionary, Aggregating data with Scripting.Dictionary (Maximum, Count)"
---

# Scripting.Dictionary object



## Properties and Methods


A [Scripting Dictionary object](https://msdn.microsoft.com/en-us/library/x4k5wbx4(v=vs.84).aspx) stores information in Key/Item pairs. The Keys must be unique and not an array but the associated Items can be repeated (their uniqueness is held by the companion Key) and can be of any type of variant or object.

A dictionary can be thought of as a two field in-memory database with a primary unique index on the first 'field' (the **Key**). This unique index on the Keys property allows very fast 'lookups' to retrieve a Key's associated Item value.

**Properties**

|<sub>name</sub>|<sub>read/write</sub>|<sub>type</sub></th>|<sub>description</sub>
|---|---|---|---|---|---|---|---|---|---
|CompareMode|*read / write*|CompareMode constant</td>|Setting the CompareMode can only be performed on an empty dictionary. Accepted values are 0 (vbBinaryCompare), 1 (vbTextCompare), 2 (vbDatabaseCompare).
|Count|*read only*|unsigned long integer</td>|A one-based count of the key/item pairs in the scripting dictionary object.
|Key|*read / write*|non-array variant</td>|Each individual unique key in the dictionary.
|Item(**Key**)|*read / write*|any variant</td>|Default property.  Each individual item associated with a key in the dictionary. Note that attempting to retrieve an item with a key that does not exist in the dictionary will **implicitly add** the passed key.

**Methods**

|<sub>name</sub>|<sub>description</sub>
|---|---|---|---|---|---|---|---|---|---
|Add(*Key*,*Item*)|Adds a new Key and Item to the dictionary. The new key must not exist in the dictionary's current Keys collection but an item can be repeated among many unique keys.
|Exists(*Key*)|Boolean test to determine if a Key already exists in the dictionary.
|Keys|Returns the array or collection of unique keys.
|Items|Returns the array or collection of associated items.
|Remove(*Key*)|Removes an individual dictionary key and its associated item.
|RemoveAll|Clears all of a dictionary object's keys and items.

**Sample Code**

```vb
'Populate, enumerate, locate and remove entries in a dictionary that was created
'with late binding
Sub iterateDictionaryLate()
    Dim k As Variant, dict As Object
    
    Set dict = CreateObject("Scripting.Dictionary")
    dict.CompareMode = vbTextCompare          'non-case sensitive compare model
    
    'populate the dictionary
    dict.Add Key:="Red", Item:="Balloon"
    dict.Add Key:="Green", Item:="Balloon"
    dict.Add Key:="Blue", Item:="Balloon"
    
    'iterate through the keys
    For Each k In dict.Keys
        Debug.Print k & " - " & dict.Item(k)
    Next k

    'locate the Item for Green
    Debug.Print dict.Item("Green")
    
    'remove key/item pairs from the dictionary
    dict.Remove "blue"      'remove individual key/item pair by key
    dict.RemoveAll          'remove all remaining key/item pairs

End Sub

'Populate, enumerate, locate and remove entries in a dictionary that was created
'with early binding (see Remarks)
Sub iterateDictionaryEarly()
    Dim d As Long, k As Variant
    Dim dict As New Scripting.Dictionary
    
    dict.CompareMode = vbTextCompare          'non-case sensitive compare model
    
    'populate the dictionary
    dict.Add Key:="Red", Item:="Balloon"
    dict.Add Key:="Green", Item:="Balloon"
    dict.Add Key:="Blue", Item:="Balloon"
    dict.Add Key:="White", Item:="Balloon"
    
    'iterate through the keys
    For Each k In dict.Keys
        Debug.Print k & " - " & dict.Item(k)
    Next k

    'iterate through the keys by the count
    For d = 0 To dict.Count - 1
        Debug.Print dict.Keys(d) & " - " & dict.Items(d)
    Next d
    
    'iterate through the keys by the boundaries of the keys collection
    For d = LBound(dict.Keys) To UBound(dict.Keys)
        Debug.Print dict.Keys(d) & " - " & dict.Items(d)
    Next d
    
    'locate the Item for Green
    Debug.Print dict.Item("Green")
    'locate the Item for the first key
    Debug.Print dict.Item(dict.Keys(0))
    'locate the Item for the last key
    Debug.Print dict.Item(dict.Keys(UBound(dict.Keys)))
    
    'remove key/item pairs from the dictionary
    dict.Remove "blue"                         'remove individual key/item pair by key
    dict.Remove dict.Keys(0)                   'remove first key/item by index position
    dict.Remove dict.Keys(UBound(dict.Keys))   'remove last key/item by index position
    dict.RemoveAll                             'remove all remaining key/item pairs

End Sub

```



## Getting unique values with Scripting.Dictionary


The `Dictionary` allows getting a unique set of values very simply. Consider the following function:

```vb
Function Unique(values As Variant) As Variant()
    'Put all the values as keys into a dictionary
    Dim dict As New Scripting.Dictionary
    Dim val As Variant
    For Each val In values
        dict(val) = 1 'The value doesn't matter here
    Next
    Unique = dict.Keys
End Function

```

which you could then call like this:

```vb
Dim duplicates() As Variant
duplicates = Array(1, 2, 3, 1, 2, 3)
Dim uniqueVals() As Variant
uniqueVals = Unique(duplicates)

```

and `uniqueVals` would contain only `{1,2,3}`.

Note: This function can be used with any enumerable object.



## Aggregating data with Scripting.Dictionary (Maximum, Count)


Dictionaries are great for managing information where multiple entries occur, but you are only concerned with a single value for each set of entries — the first or last value, the mininmum or maximum value, an average, a sum etc.

Consider a workbook that holds a log of user activity, with a script that inserts the username and edit date every time someone edits the workbook:

> 
**`Log` worksheet**
<table><thead>|A|B
</thead><tbody>|bob|10/12/2016 9:00
|alice|10/13/2016 13:00
|bob|10/13/2016 13:30
|alice|10/13/2016 14:00
|alice|10/14/2016 13:00
</tbody></table>

Let's say you want to output the last edit time for each user, into a worksheet named `Summary`.

<sup>Notes:</sup><br />
<sup>1. The data is assumed to be in `ActiveWorkbook`.</sup><br />
<sup>2. We are using an array to pull the values from the worksheet; this is more efficient than iterating over each cell.</sup><br />
<sup>3. The `Dictionary` is created using early binding.</sup>

```vb
Sub LastEdit()
Dim vLog as Variant, vKey as Variant
Dim dict as New Scripting.Dictionary
Dim lastRow As Integer, lastColumn As Integer
Dim i as Long
Dim anchor As Range

With ActiveWorkbook
    With .Sheets("Log")
        'Pull entries in "log" into a variant array
        lastRow = .Range("a" & .Rows.Count).End(xlUp).Row
        vlog = .Range("a1", .Cells(lastRow, 2)).Value2

        'Loop through array
        For i = 1 to lastRow
            Dim username As String
            username = vlog(i, 1)
            Dim editDate As Date
            editDate = vlog(i, 2)

            'If the username is not yet in the dictionary:
            If Not dict.Exists(username) Then
                dict(username) = editDate
            ElseIf dict(username) < editDate Then
                dict(username) = editDate
            End If
        Next
    End With

    With .Sheets("Summary")
        'Loop through keys
        For Each vKey in dict.Keys
            'Add the key and value at the next available row
            Anchor = .Range("A" & .Rows.Count).End(xlUp).Offset(1,0)
            Anchor = vKey
            Anchor.Offset(0,1) = dict(vKey)
        Next vKey
    End With
End With
End Sub

```

and the output will look like this:

> 
**`Summary` worksheet**
<table><thead>|A|B
</thead><tbody>|bob|10/13/2016 13:30
|alice|10/14/2016 13:00
</tbody></table>

If on the other hand you want to output how many times each user edited the workbook, the body of the `For` loop should look like this:

```

       'Loop through array
        For i = 1 to lastRow
            Dim username As String
            username = vlog(i, 1)

            'If the username is not yet in the dictionary:
            If Not dict.Exists(username) Then
                dict(username) = 1
            Else
                dict(username) = dict(username) + 1
            End If
        Next

```

and the output will look like this:

> 
**`Summary` worksheet**
<table><thead>|A|B
</thead><tbody>|bob|2
|alice|3
</tbody></table>



#### Remarks


You must add Microsoft Scripting Runtime to the VBA project through the VBE's Tools → References command in order to implement early binding of the Scripting Dictionary object.
This library reference is carried with the project; it does not have to be re-referenced when the VBA project is distributed and run on another computer.

