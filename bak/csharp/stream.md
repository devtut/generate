---
metaTitle: "C# | Stream"
description: "Using Streams"
---

# Stream



## Using Streams


A stream is an object that provides a low-level means to transfer data. They themselves do not act as data containers.

The data that we deal with is in form of byte array(`byte []`). The functions for reading and writing are all byte orientated, e.g. `WriteByte()`.

There are no functions for dealing with integers, strings etc. This makes the stream very general-purpose, but less simple to work with if, say, you just want to transfer text. Streams can be particularly very helpful when you are dealing with large amount of data.

We will need to use different type of Stream based where it needs to be written/read from (i.e. the backing store). For example, if the source is a file, we need to use `FileStream`:

```cs
string filePath = @"c:\Users\exampleuser\Documents\userinputlog.txt";
using (FileStream fs = new FileStream(filePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
{
    // do stuff here...

    fs.Close();
}

```

Similarly, `MemoryStream` is used if the backing store is memory:

```cs
// Read all bytes in from a file on the disk.
byte[] file = File.ReadAllBytes(“C:\\file.txt”);

// Create a memory stream from those bytes.
using (MemoryStream memory = new MemoryStream(file))
{
   // do stuff here...
}

```

Similarly, `System.Net.Sockets.NetworkStream` is used for network access.

All Streams are derived from the generic class `System.IO.Stream`. Data cannot be directly read or written from streams. The .NET Framework provides helper classes such as `StreamReader`, `StreamWriter`, `BinaryReader` and `BinaryWriter` that convert between native types and the low-level stream interface, and transfer the data to or from the stream for you.

Reading and writing to streams can be done via `StreamReader` and `StreamWriter`. One should be careful when closing these. By default, closing will also close contained stream as well and make it unusable for further uses. This default behaviour can be change by using a [constructor](https://msdn.microsoft.com/en-us/library/gg712952(v=vs.110).aspx) which has `bool leaveOpen` parameter and setting its value as `true`.

`StreamWriter`:

```cs
FileStream fs = new FileStream("sample.txt", FileMode.Create);
StreamWriter sw = new StreamWriter(fs);
string NextLine = "This is the appended line.";
sw.Write(NextLine);
sw.Close();
//fs.Close(); There is no need to close fs. Closing sw will also close the stream it contains.

```

`StreamReader`:

```cs
using (var ms = new MemoryStream())
{
    StreamWriter sw = new StreamWriter(ms);
    sw.Write(123);
    //sw.Close();     This will close ms and when we try to use ms later it will cause an exception
    sw.Flush();     //You can send the remaining data to stream. Closing will do this automatically
    // We need to set the position to 0 in order to read 
    // from the beginning.
    ms.Position = 0;
    StreamReader sr = new StreamReader(ms);
    var myStr = sr.ReadToEnd();
    sr.Close();
    ms.Close();
}

```

Since Classes `Stream`, `StreamReader`, `StreamWriter`, etc. implement the `IDisposable` interface, we can call the `Dispose()` method on objects of these classes.

