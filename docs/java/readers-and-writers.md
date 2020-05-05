---
metaTitle: "Java - Readers and Writers"
description: "BufferedReader, StringWriter Example"
---

# Readers and Writers


Readers and Writers and their respective subclasses provide simple I/O for text / character-based data.



## BufferedReader


### Introduction

The `BufferedReader` class is a wrapper for other `Reader` classes that serves two main purposes:

<li>
A `BufferedReader` provides buffering for the wrapped `Reader`.  This allows an application to read characters one at a time without undue I/O overheads.
</li>
<li>
A `BufferedReader` provides functionality for reading text a line at a time.
</li>

### Basics of using a BufferedReader

The normal pattern  for using a `BufferedReader` is as follows.  First, you obtain the `Reader` that you want to read characters from.  Next you instantiate a `BufferedReader` that wraps the `Reader`.  Then you read character data.  Finally you close the `BufferedReader` which close the wrapped `Reader.    For example:

```java
File someFile = new File(...);
int aCount = 0;
try (FileReader fr = new FileReader(someFile);
     BufferedReader br = new BufferedReader(fr)) {
    // Count the number of 'a' characters.
    int ch;
    while ((ch = br.read()) != -1) {
        if (ch == 'a') {
            aCount++;
        }
    }
    System.out.println("There are " + aCount + " 'a' characters in " + someFile);
}

```

You can apply this pattern to any `Reader`

Notes:

<li>
We have used Java 7 (or later) **try-with-resources** to ensure that the underlying reader is always closed.  This avoids a potential resource leak.  In earlier versions of Java, you would explicitly close the `BufferedReader` in a `finally` block.
</li>
<li>
The code inside the `try` block is virtually identical to what we would use if we read directly from the `FileReader`.  In fact, a `BufferedReader` functions exactly like the `Reader` that it wraps would behave.  The difference is that **this** version is a lot more efficient.
</li>

### The BufferedReader buffer size

### The BufferedReader.readLine() method

### Example: reading all lines of a File into a List

This is done by getting each line in a file, and adding it into a `List<String>`. The list is then returned:

```java
public List<String> getAllLines(String filename) throws IOException {
    List<String> lines = new ArrayList<String>();
    try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
        String line = null;
        while ((line = reader.readLine) != null) {
            lines.add(line);
        }
    }
    return lines;
}

```

Java 8 provides a more concise way to do this using the `lines()` method:

```java
public List<String> getAllLines(String filename) throws IOException {
    try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
        return br.lines().collect(Collectors.toList());
    }
    return Collections.empty();
}

```



## StringWriter Example


Java StringWriter class is a character stream that collects output from string buffer, which can be used to construct a string.

The StringWriter class extends the Writer class.

In StringWriter class, system resources like network sockets and files are not used, therefore closing the StringWriter is not necessary.

```java
import java.io.*;  
public class StringWriterDemo {  
    public static void main(String[] args) throws IOException {  
        char[] ary = new char[1024];  
        StringWriter writer = new StringWriter();  
        FileInputStream input = null;  
        BufferedReader buffer = null;  
        input = new FileInputStream("c://stringwriter.txt");  
        buffer = new BufferedReader(new InputStreamReader(input, "UTF-8"));  
        int x;  
        while ((x = buffer.read(ary)) != -1) {  
                   writer.write(ary, 0, x);  
        }  
        System.out.println(writer.toString());        
        writer.close();  
        buffer.close();  
    }  
}

```

The above example helps us to know simple example of StringWriter using BufferedReader to read file data from the stream.

