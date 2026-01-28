---
metaTitle: "VBA - Scripting.FileSystemObject"
description: "Retrieve only the path from a file path, Retrieve just the extension from a file name, Recursively enumerate folders and files, Strip file extension from a file name, Creating a FileSystemObject, Reading a text file using a FileSystemObject, Creating a text file with FileSystemObject, Enumerate files in a directory using FileSystemObject, Writing to an existing file with FileSystemObject, Using FSO.BuildPath to build a Full Path from folder path and file name"
---

# Scripting.FileSystemObject




## Retrieve only the path from a file path


The GetParentFolderName method returns the parent folder for any path.  While this can also be used with folders, it is arguably more useful for extracting the path from an absolute file path:

```vb
Dim fso As New Scripting.FileSystemObject
Debug.Print fso.GetParentFolderName("C:\Users\Me\My Documents\SomeFile.txt")

```

Prints `C:\Users\Me\My Documents`

Note that the trailing path separator is not included in the returned string.



## Retrieve just the extension from a file name


```vb
Dim fso As New Scripting.FileSystemObject
Debug.Print fso.GetExtensionName("MyFile.something.txt")

```

Prints `txt`
Note that the `GetExtensionName()` method already handles multiple periods in a file name.



## Recursively enumerate folders and files


Early Bound (with a reference to `Microsoft Scripting Runtime`)

```vb
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

Output when called with arguments like: `EnumerateFilesAndFolders "C:\Test"`

```vb
C:\Test
  Documents
    Personal
      Budget.xls
      Recipes.doc
    Work
      Planning.doc
  Downloads
    FooBar.exe
  ReadMe.txt

```

Output when called with arguments like: `EnumerateFilesAndFolders "C:\Test", 0`

```vb
C:\Test
  Documents
  Downloads
  ReadMe.txt

```

Output when called with arguments like: `EnumerateFilesAndFolders "C:\Test", 1, 4`

```vb
C:\Test
    Documents
        Personal
        Work
    Downloads
        FooBar.exe
    ReadMe.txt

```



## Strip file extension from a file name


```vb
Dim fso As New Scripting.FileSystemObject
Debug.Print fso.GetBaseName("MyFile.something.txt")

```

Prints `MyFile.something`

Note that the `GetBaseName()` method already handles multiple periods in a file name.



## Creating a FileSystemObject


```vb
Const ForReading = 1
Const ForWriting = 2
Const ForAppending = 8

Sub FsoExample()
    Dim fso As Object ' declare variable
    Set fso = CreateObject("Scripting.FileSystemObject") ' Set it to be a File System Object

    ' now use it to check if a file exists
    Dim myFilePath As String
    myFilePath = "C:\mypath\to\myfile.txt"
    If fso.FileExists(myFilePath) Then
        ' do something
    Else
        ' file doesn't exist
        MsgBox "File doesn't exist"
    End If
End Sub

```



## Reading a text file using a FileSystemObject


```vb
Const ForReading = 1
Const ForWriting = 2
Const ForAppending = 8

Sub ReadTextFileExample()
    Dim fso As Object
    Set fso = CreateObject("Scripting.FileSystemObject")

    Dim sourceFile As Object
    Dim myFilePath As String
    Dim myFileText As String

    myFilePath = "C:\mypath\to\myfile.txt"
    Set sourceFile = fso.OpenTextFile(myFilePath, ForReading)
    myFileText = sourceFile.ReadAll ' myFileText now contains the content of the text file
    sourceFile.Close ' close the file
    ' do whatever you might need to do with the text

    ' You can also read it line by line
    Dim line As String
    Set sourceFile = fso.OpenTextFile(myFilePath, ForReading)
    While Not sourceFile.AtEndOfStream ' while we are not finished reading through the file
        line = sourceFile.ReadLine
        ' do something with the line...
    Wend
    sourceFile.Close
End Sub

```



## Creating a text file with FileSystemObject


```vb
Sub CreateTextFileExample()
    Dim fso As Object
    Set fso = CreateObject("Scripting.FileSystemObject")

    Dim targetFile As Object
    Dim myFilePath As String
    Dim myFileText As String

    myFilePath = "C:\mypath\to\myfile.txt"
    Set targetFile = fso.CreateTextFile(myFilePath, True)  ' this will overwrite any existing file
    targetFile.Write "This is some new text"
    targetFile.Write " And this text will appear right after the first bit of text."
    targetFile.WriteLine "This bit of text includes a newline character to ensure each write takes its own line."
    targetFile.Close ' close the file
End Sub

```



## Enumerate files in a directory using FileSystemObject


Early bound (requires a reference to Microsoft Scripting Runtime):

```vb
Public Sub EnumerateDirectory()
    Dim fso As Scripting.FileSystemObject
    Set fso = New Scripting.FileSystemObject

    Dim targetFolder As Folder
    Set targetFolder = fso.GetFolder("C:\")
    
    Dim foundFile As Variant
    For Each foundFile In targetFolder.Files
        Debug.Print foundFile.Name
    Next
End Sub

```

Late bound:

```vb
Public Sub EnumerateDirectory()
    Dim fso As Object
    Set fso = CreateObject("Scripting.FileSystemObject")

    Dim targetFolder As Object
    Set targetFolder = fso.GetFolder("C:\")
    
    Dim foundFile As Variant
    For Each foundFile In targetFolder.Files
        Debug.Print foundFile.Name
    Next
End Sub

```



## Writing to an existing file with FileSystemObject


```vb
Const ForReading = 1
Const ForWriting = 2
Const ForAppending = 8


Sub WriteTextFileExample()
    Dim oFso
    Set oFso = CreateObject("Scripting.FileSystemObject")

    Dim oFile as Object
    Dim myFilePath as String
    Dim myFileText as String

    myFilePath = "C:\mypath\to\myfile.txt"
    ' First check if the file exists
    If oFso.FileExists(myFilePath) Then
        ' this will overwrite any existing filecontent with whatever you send the file
        ' to append data to the end of an existing file, use ForAppending instead
        Set oFile = oFso.OpenTextFile(myFilePath, ForWriting)  
    Else
        ' create the file instead
        Set oFile = oFso.CreateTextFile(myFilePath) ' skipping the optional boolean for overwrite if exists as we already checked that the file doesn't exist.
    End If
    oFile.Write "This is some new text"
    oFile.Write " And this text will appear right after the first bit of text."
    oFile.WriteLine "This bit of text includes a newline character to ensure each write takes its own line."
    oFile.Close ' close the file
End Sub

```



## Using FSO.BuildPath to build a Full Path from folder path and file name


If you're accepting user input for folder paths, you might need to check for trailing backslashes (`\`) before building a file path. The `FSO.BuildPath` method makes this simpler:

```

 Const sourceFilePath As String = "C:\Temp"  '<-- Without trailing backslash
  Const targetFilePath As String = "C:\Temp\" '<-- With trailing backslash

  Const fileName As String = "Results.txt"
  
  Dim FSO As FileSystemObject
  Set FSO = New FileSystemObject
  
  Debug.Print FSO.BuildPath(sourceFilePath, fileName)
  Debug.Print FSO.BuildPath(targetFilePath, fileName)

```

Output:

```vb
C:\Temp\Results.txt
C:\Temp\Results.txt

```

