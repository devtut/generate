---
metaTitle: "File and Stream I/O"
description: "Reading from a file using the System.IO.File class, Lazily reading a file line-by-line via an IEnumerable, Writing lines to a file using the System.IO.StreamWriter class, Writing to a file using the System.IO.File class, Copy File, Async write text to a file using StreamWriter, Create File, Move File, Delete File, Files and Directories"
---

# File and Stream I/O


Manages files.



## Reading from a file using the System.IO.File class


You can use the **[System.IO.File.ReadAllText](https://msdn.microsoft.com/en-us/library/system.io.file.readalltext(v=vs.110).aspx)** function to read the entire contents of a file into a string.

```cs
string text = System.IO.File.ReadAllText(@"C:\MyFolder\MyTextFile.txt");

```

You can also read a file as an array of lines using the **[System.IO.File.ReadAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.readlines(v=vs.110).aspx)** function:

```cs
string[] lines = System.IO.File.ReadAllLines(@"C:\MyFolder\MyTextFile.txt");

```



## Lazily reading a file line-by-line via an IEnumerable


When working with large files, you can use the `System.IO.File.ReadLines` method to read all lines from a file into an `IEnumerable<string>`. This is similar to `System.IO.File.ReadAllLines`, except that it doesn't load the whole file into memory at once, making it more efficient when working with large files.

```cs
IEnumerable<string> AllLines = File.ReadLines("file_name.txt", Encoding.Default);

```

**The second parameter of File.ReadLines is optional. You may use it when it is required to specify encoding.**

It is important to note that calling `ToArray`, `ToList` or another similar function will force all of the lines to be loaded at once, meaning that the benefit of using `ReadLines` is nullified. It is best to enumerate over the `IEnumerable` using a `foreach` loop or LINQ if using this method.



## Writing lines to a file using the System.IO.StreamWriter class


The **[System.IO.StreamWriter](https://msdn.microsoft.com/en-us/library/system.io.streamwriter(v=vs.110).aspx)** class:

> 
Implements a TextWriter for writing characters to a stream in a particular encoding.


Using the `WriteLine` method, you can write content line-by-line to a file.

Notice the use of the `using` keyword which makes sure the StreamWriter object is disposed as soon as it goes out of scope and thus the file is closed.

```cs
string[] lines = { "My first string", "My second string", "and even a third string" };
using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt"))
{
    foreach (string line in lines)
    {
        sw.WriteLine(line);
    }
}

```

Note that the StreamWriter can receive a second `bool` parameter in it's constructor, allowing to `Append` to a file instead of overwriting the file:

```cs
bool appendExistingFile = true;
using (System.IO.StreamWriter sw = new System.IO.StreamWriter(@"C:\MyFolder\OutputText.txt", appendExistingFile ))
{
    sw.WriteLine("This line will be appended to the existing file");
}

```



## Writing to a file using the System.IO.File class


You can use the **[System.IO.File.WriteAllText](https://msdn.microsoft.com/en-us/library/system.io.file.writealltext(v=vs.110).aspx)** function to write a string to a file.

```cs
string text = "String that will be stored in the file";
System.IO.File.WriteAllText(@"C:\MyFolder\OutputFile.txt", text);

```

You can also use the **[System.IO.File.WriteAllLines](https://msdn.microsoft.com/en-us/library/system.io.file.writealllines(v=vs.110).aspx)** function which receives an `IEnumerable<String>` as the second parameter (as opposed to a single string in the previous example). This lets you write content from an array of lines.

```cs
string[] lines = { "My first string", "My second string", "and even a third string" };
System.IO.File.WriteAllLines(@"C:\MyFolder\OutputFile.txt", lines);

```



## Copy File


**File static class**

`File` static class can be easily used for this purpose.

```cs
File.Copy(@"sourcePath\abc.txt", @"destinationPath\abc.txt");
File.Copy(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

```

**Remark:** By this method, file is copied, meaning that it will be read from the source and then written to the destination path. This is a resource consuming process, it would take relative time to the file size, and can cause your program to freeze if you don't utilize threads.



## Async write text to a file using StreamWriter


```cs
// filename is a string with the full path
// true is to append        
using (System.IO.StreamWriter file = new System.IO.StreamWriter(filename, true))
{
   // Can write either a string or char array
   await file.WriteAsync(text);
}

```



## Create File


**File static class**

By using `Create` method of the `File` static class we can create files. Method creates the file at the given path, at the same time it opens the file and gives us the `FileStream` of the file. Make sure you close the file after you are done with it.

ex1:

```cs
var fileStream1 = File.Create("samplePath");
/// you can write to the fileStream1
fileStream1.Close();

```

ex2:

```cs
using(var fileStream1 = File.Create("samplePath"))
{
    /// you can write to the fileStream1
}

```

ex3:

```cs
File.Create("samplePath").Close();

```

**FileStream class**

There are many overloads of this classes constructor which is actually well documented [here](https://msdn.microsoft.com/en-us/library/system.io.filestream(v=vs.110).aspx). Below example is for the one that covers most used functionalities of this class.

```cs
var fileStream2 = new FileStream("samplePath", FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None);

```

You can check the enums for [FileMode](https://msdn.microsoft.com/en-us/library/system.io.filemode(v=vs.110).aspx), [FileAccess](https://msdn.microsoft.com/en-us/library/4z36sx0f(v=vs.110).aspx), and [FileShare](https://msdn.microsoft.com/en-us/library/system.io.fileshare(v=vs.110).aspx) from those links. What they basically means are as follows:

**FileMode:** Answers "Should file be created? opened? create if not exist then open?" kinda questions.

**FileAccess:** Answers "Should I be able to read the file, write to the file or both?" kinda questions.

**FileShare:** Answers "Should other users be able to read, write etc. to the file while I am using it simultaneously?" kinda questions.



## Move File


**File static class**

File static class can easily be used for this purpose.

```cs
File.Move(@"sourcePath\abc.txt", @"destinationPath\xyz.txt");

```

**Remark1:** Only changes the index of the file (if the file is moved in the same volume). This operation does not take relative time to the file size.

**Remark2:** Cannot override an existing file on destination path.



## Delete File


```cs
string path = @"c:\path\to\file.txt";
File.Delete(path);

```

While `Delete` does not throw exception if file doesn't exist, it will throw exception e.g. if specified path is invalid or caller does not have the required permissions. You should always wrap calls to `Delete` inside [try-catch block](http://stackoverflow.com/documentation/c%23/26/keywords/148/try-catch-finally-throw#t=201608021340222162938) and handle all expected exceptions. In case of possible race conditions, wrap logic inside [lock statement](http://stackoverflow.com/documentation/c%23/1495/lock-statement/4865/simple-usage#t=201608021343504970522).



## Files and Directories


**Get all files in Directory**

```

var FileSearchRes = Directory.GetFiles(@Path, "*.*", SearchOption.AllDirectories);

```

Returns an array of `FileInfo`, representing all the files in the specified directory.

**Get Files with specific extension**

```

var FileSearchRes = Directory.GetFiles(@Path, "*.pdf", SearchOption.AllDirectories);

```

Returns an array of `FileInfo`, representing all the files in the specified directory with the specified extension.



#### Syntax


- `new System.IO.StreamWriter(string path)`
- `new System.IO.StreamWriter(string path, bool append)`
- `System.IO.StreamWriter.WriteLine(string text)`
- `System.IO.StreamWriter.WriteAsync(string text)`
- `System.IO.Stream.Close()`
- `System.IO.File.ReadAllText(string path)`
- `System.IO.File.ReadAllLines(string path)`
- `System.IO.File.ReadLines(string path)`
- `System.IO.File.WriteAllText(string path, string text)`
- `System.IO.File.WriteAllLines(string path, IEnumerable<string> contents)`
- `System.IO.File.Copy(string source, string dest)`
- `System.IO.File.Create(string path)`
- `System.IO.File.Delete(string path)`
- `System.IO.File.Move(string source, string dest)`
- `System.IO.Directory.GetFiles(string path)`



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|path|The location of the file.
|append|If the file exist, true will add data to the end of the file (append), false will overwrite the file.
|text|Text to be written or stored.
|contents|A collection of strings to be written.
|source|The location of the file you want to use.
|dest|The location you want a file to go to.



#### Remarks


- Always make sure to close `Stream` objects. This can be done with a `using` block as shown above or by manually calling `myStream.Close()`.
- Make sure the current user has necessary permissions on the path you are trying to create the file.
- [Verbatim strings](http://stackoverflow.com/documentation/c%23/16/verbatim-strings) should be used when declaring a path string that includes backslashes, like so: `@"C:\MyFolder\MyFile.txt"`

