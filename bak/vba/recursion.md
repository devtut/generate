---
metaTitle: "VBA - Recursion"
description: "Factorials, Folder Recursion"
---

# Recursion


A function that calls itself is said to be **recursive**. Recursive logic can often be implemented as a loop, too. Recursion must be controlled with a parameter, so that the function knows when to stop recursing and deepening the call stack. **Infinite recursion** eventually causes a run-time error '28': "Out of stack space".

See [Recursion](http://stackoverflow.com/documentation/vba/3236/recursion).



## Factorials


```vb
Function Factorial(Value As Long) As Long
    If Value = 0 Or Value = 1 Then
         Factorial = 1
    Else
       Factorial = Factorial(Value - 1) * Value
    End If
End Function

```



## Folder Recursion


Early Bound (with a reference to `Microsoft Scripting Runtime`)

```

   Sub EnumerateFilesAndFolders( _
        FolderPath As String, _
        Optional MaxDepth As Long = -1, _
        Optional CurrentDepth As Long = 0, _
        Optional Indentation As Long = 2)
      
        Dim FSO As Scripting.FileSystemObject
        Set FSO = New Scripting.FileSystemObject
        
        'Check the folder exists
        If FSO.FolderExists(FolderPath) Then
            Dim fldr As Scripting.Folder
            Set fldr = FSO.GetFolder(FolderPath)
            
            'Output the starting directory path
            If CurrentDepth = 0 Then
              Debug.Print fldr.Path
            End If
            
            'Enumerate the subfolders
            Dim subFldr As Scripting.Folder
            For Each subFldr In fldr.SubFolders
                Debug.Print Space$((CurrentDepth + 1) * Indentation) & subFldr.Name
                If CurrentDepth < MaxDepth Or MaxDepth = -1 Then
                    'Recursively call EnumerateFilesAndFolders
                    EnumerateFilesAndFolders subFldr.Path, MaxDepth, CurrentDepth + 1, Indentation
                End If
            Next subFldr
            
            'Enumerate the files
            Dim fil As Scripting.File
            For Each fil In fldr.Files
                Debug.Print Space$((CurrentDepth + 1) * Indentation) & fil.Name
            Next fil
        End If
    End Sub

```



#### Remarks


Recursion allows for repeated, self-referencing calls of a procedure.

