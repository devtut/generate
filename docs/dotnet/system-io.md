---
metaTitle: ".NET Framework - System.IO"
description: "Reading a text file using StreamReader, Serial Ports using System.IO.SerialPorts, Reading/Writing Data Using System.IO.File"
---

# System.IO



## Reading a text file using StreamReader


```dotnet
string fullOrRelativePath = "testfile.txt";

string fileData;

using (var reader = new StreamReader(fullOrRelativePath))
{
    fileData = reader.ReadToEnd();
}

```

Note that this `StreamReader` constructor overload does some auto [encoding](//#file-encodings) detection, which may or may not conform to the actual encoding used in the file.

Please note that there are some convenience methods that read all text from file available on the `System.IO.File` class, namely `File.ReadAllText(path)` and `File.ReadAllLines(path)`.



## Serial Ports using System.IO.SerialPorts


### Iterating over connected serial ports

```dotnet
using System.IO.Ports;
string[] ports = SerialPort.GetPortNames();
for (int i = 0; i < ports.Length; i++)
{
    Console.WriteLine(ports[i]);
}

```

### Instantiating a System.IO.SerialPort object

```dotnet
using System.IO.Ports;
SerialPort port = new SerialPort();
SerialPort port = new SerialPort("COM 1"); ;
SerialPort port = new SerialPort("COM 1", 9600);

```

**NOTE**: Those are just three of the seven overloads of the constructor for the SerialPort type.

### Reading/Writing data over the SerialPort

The simplest way is to use the `SerialPort.Read` and `SerialPort.Write` methods.
However you can also retrieve a `System.IO.Stream` object which you can use to stream data over the SerialPort. To do this, use `SerialPort.BaseStream`.

**Reading**

```dotnet
int length = port.BytesToRead;
//Note that you can swap out a byte-array for a char-array if you prefer.
byte[] buffer = new byte[length];
port.Read(buffer, 0, length);

```

You can also read all data available:

```dotnet
string curData = port.ReadExisting();

```

Or simply read to the first newline encountered in the incoming data:

```dotnet
string line = port.ReadLine();

```

**Writing**

The easiest way to write data over the SerialPort is:

```dotnet
port.Write("here is some text to be sent over the serial port.");

```

However you can also send data over like this when needed:

```dotnet
//Note that you can swap out the byte-array with a char-array if you so choose.
byte[] data = new byte[1] { 255 };
port.Write(data, 0, data.Length);

```



## Reading/Writing Data Using System.IO.File


First, let's see three different ways of extracting data from a file.

```dotnet
string fileText = File.ReadAllText(file);
string[] fileLines = File.ReadAllLines(file);
byte[] fileBytes = File.ReadAllBytes(file);

```


- On the first line, we read all the data in the file as a string.
<li>On the second line, we read the data in the file into a string-array.
Each line in the file becomes an element in the array.</li>
- On the third we read the bytes from the file.

Next, let's see three different methods of **appending** data to a file.
If the file you specify doesn't exist, each method will automatically create the file before attempting to append the data to it.

```

File.AppendAllText(file, "Here is some data that is\nappended to the file.");
 File.AppendAllLines(file, new string[2] { "Here is some data that is", "appended to the file." });
 using (StreamWriter stream = File.AppendText(file))
 {
     stream.WriteLine("Here is some data that is");
     stream.Write("appended to the file.");
 }

```


- On the first line we simply add a string to the end of the specified file.
- On the second line we add each element of the array onto a new line in the file.
- Finally on the third line we use `File.AppendText` to open up a streamwriter which will append whatever data is written to it.

And lastly, let's see three different methods of **writing** data to a file.
The difference between **appending** and **writing** being that writing **over-writes** the data in the file while appending **adds** to the data in the file.
If the file you specify doesn't exist, each method will automatically create the file before attempting to write the data to it.

```dotnet
File.WriteAllText(file, "here is some data\nin this file.");
File.WriteAllLines(file, new string[2] { "here is some data", "in this file" });
File.WriteAllBytes(file, new byte[2] { 0, 255 });

```


- The first line writes a string to the file.
- The second line writes each string in the array on it's own line in the file.
- And the third line allows you to write a byte array to the file.

