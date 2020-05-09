---
metaTitle: "Visual Basic .NET - File Handling"
description: "Write Data to a File, Read All Contents of a File, Write Lines Individually to a Text File using StreamWriter"
---

# File Handling



## Write Data to a File


**To write the contents of a string to a file:**

```vb
Dim toWrite As String = "This will be written to the file."
System.IO.File.WriteAllText("filename.txt", toWrite)

```

`WriteAllText` will open the specified file, write the data, and then close the file. If the target file exists, it is overwritten. If the target file does not exist, it is created.

**To write the contents of an array to a file:**

```vb
Dim toWrite As String() = {"This", "Is", "A", "Test"}
System.IO.File.WriteAllLines("filename.txt", toWrite)

```

`WriteAllLines` will open the specified file, write each value of the array on a new line, and then close the file. If the target file exists, it is overwritten. If the target file does not exist, it is created.



## Read All Contents of a File


**To read the contents to a file into a string variable:**

```vb
Dim fileContents As String = System.IO.File.ReadAllText("filename.txt")

```

`ReadAllText` will open the specified file, read data to the end, then close the file.

**To read a file, separating it into an array element for each line:**

```vb
Dim fileLines As String() = System.IO.File.ReadAllLines("filename.txt")

```

`ReadAllLines` will open the specified file, read each line of the file into a new index in an array until the end of the file, then close the file.



## Write Lines Individually to a Text File using StreamWriter


```vb
Using sw As New System.IO.StreamWriter("path\to\file.txt")
    sw.WriteLine("Hello world")
End Using

```

The use of a `Using` block is recommended good practice when using an object that Implements `IDisposable`



#### Syntax


- `System.IO.File.ReadAllLines(path As String)`
- `System.IO.File.ReadAllText(path As String)`
- `System.IO.File.WriteAllText(path As String, contents As String)`
- `System.IO.File.WriteAllLines(path As String, contents() As String)`

