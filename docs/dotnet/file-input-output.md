---
metaTitle: "File Input/Output"
description: "C# File.Exists(), VB WriteAllText, VB StreamWriter, C# StreamWriter, C# WriteAllText()"
---

# File Input/Output




## C# File.Exists()


```dotnet
using System;
using System.IO;
                
public class Program
{
    public static void Main()
    {
        string filePath = "somePath";
    
        if(File.Exists(filePath))
        {
            Console.WriteLine("Exists");
        }
        else
        {
            Console.WriteLine("Does not exist");    
        }
    }
}

```

Can also be used in a ternary operator.

```dotnet
Console.WriteLine(File.Exists(pathToFile) ? "Exists" : "Does not exist");

```



## VB WriteAllText


```dotnet
Imports System.IO

Dim filename As String = "c:\path\to\file.txt"
File.WriteAllText(filename, "Text to write" & vbCrLf)

```



## VB StreamWriter


```dotnet
Dim filename As String = "c:\path\to\file.txt"
If System.IO.File.Exists(filename) Then
    Dim writer As New System.IO.StreamWriter(filename)
    writer.Write("Text to write" & vbCrLf) 'Add a newline
    writer.close()
End If

```



## C# StreamWriter


```dotnet
using System.Text;
using System.IO;

string filename = "c:\path\to\file.txt";
//'using' structure allows for proper disposal of stream.
using (StreamWriter writer = new StreamWriter(filename"))
{
    writer.WriteLine("Text to Write\n");
}

```



## C# WriteAllText()


```dotnet
using System.IO;
using System.Text;

string filename = "c:\path\to\file.txt";
File.writeAllText(filename, "Text to write\n");

```



#### Parameters


|Parameter|Details
|------
|string path|Path of the file to check. (relative or fully qualified)



#### Remarks


Returns true if the file exists, false otherwise.

