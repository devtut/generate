---
metaTitle: "Excel VBA - Binding"
description: "Early Binding vs Late Binding"
---

# Binding



## Early Binding vs Late Binding


Binding is the process of assigning an object to an identifier or variable name. Early binding (also known as static binding) is when an object declared in Excel is of a specific object type, such as a Worksheet or Workbook. Late binding occurs when general object associations are made, such as the Object and Variant declaration types.

Early binding of references some advantages over late binding.

- Early binding is operationally faster than late binding during run-time. Creating the object with late binding in run-time takes time that early binding accomplishes when the VBA project is initially loaded.
- Early binding offers additional functionality through the identification of Key/Item pairs by their ordinal position.
- Depending on code structure, early binding may offer an additional level of type checking and reduce errors.
- The VBE's capitalization correction when typing a bound object's properties and methods is active with early binding but unavailable with late binding.

> 
<sup>**Note:** You must add the appropriate reference to the VBA project through the VBE's  Tools → References command in order to implement early binding.</sup><br/><sub>This library reference is then carried with the project; it does not have to be re-referenced when the VBA project is distributed and run on another computer.</sub>


```vb
'Looping through a dictionary that was created with late binding¹
Sub iterateDictionaryLate()
    Dim k As Variant, dict As Object
    
    Set dict = CreateObject("Scripting.Dictionary")
    dict.comparemode = vbTextCompare          'non-case sensitive compare model
    
    'populate the dictionary
    dict.Add Key:="Red", Item:="Balloon"
    dict.Add Key:="Green", Item:="Balloon"
    dict.Add Key:="Blue", Item:="Balloon"
    
    'iterate through the keys
    For Each k In dict.Keys
        Debug.Print k & " - " & dict.Item(k)
    Next k
    
    dict.Remove "blue"      'remove individual key/item pair by key
    dict.RemoveAll          'remove all remaining key/item pairs

End Sub

'Looping through a dictionary that was created with early binding¹
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
    
    dict.Remove "blue"                         'remove individual key/item pair by key
    dict.Remove dict.Keys(0)                   'remove first key/item by index position
    dict.Remove dict.Keys(UBound(dict.Keys))   'remove last key/item by index position
    dict.RemoveAll                             'remove all remaining key/item pairs

End Sub

```

However, if you are using early binding and the document is run on a system that lacks one of the libraries you have referenced, you will encounter problems. Not only will the routines that utilize the missing library not function properly, but the behavior of all code within the document will become erratic. It is likely that none of the document's code will function on that computer.

This is where late binding is advantageous. When using late binding you do not have to add the reference in the Tools>References menu. On machines that have the appropriate library, the code will still work. On machines without that library, the commands that reference the library will not work, but all the other code in your document will continue to function.

If you are not thoroughly familiar with the library you are referencing, it may be useful to use early binding while writing the code, then switch to late binding before deployment. That way you can take advantage of the VBE's IntelliSense and Object Browser during development.

