---
metaTitle: "InputStreams and OutputStreams"
description: "Closing Streams, Reading InputStream into a String, Wrapping Input/Output Streams, Writing bytes to an OutputStream, Copying Input Stream to Output Stream, DataInputStream Example"
---

# InputStreams and OutputStreams



## Closing Streams


Most streams must be closed when you are done with them, otherwise you could introduce a memory leak or leave a file open. It is important that streams are closed even if an exception is thrown.

```java
try(FileWriter fw = new FileWriter("outfilename");
    BufferedWriter bw = new BufferedWriter(fw);
    PrintWriter out = new PrintWriter(bw))
{
    out.println("the text");
    //more code
    out.println("more text");
    //more code
} catch (IOException e) {
    //handle this however you 
}

```

Remember: try-with-resources guarantees, that the resources have been closed when the block is exited,
whether that happens with the usual control flow or because of an exception.

Sometimes, try-with-resources is not an option, or maybe you're supporting older version of Java 6 or earlier. In this case, proper handling is to use a `finally` block:

```java
FileWriter fw = null;
BufferedWriter bw = null;
PrintWriter out = null;
try {
    fw = new FileWriter("myfile.txt");
    bw = new BufferedWriter(fw);
    out = new PrintWriter(bw);
    out.println("the text");
    out.close();
} catch (IOException e) {
    //handle this however you want
}
finally {
    try {
        if(out != null)
            out.close();
    } catch (IOException e) {
        //typically not much you can do here...
    }
}

```

Note that closing a wrapper stream will also close its underlying stream. This means you cannot wrap a stream, close the wrapper and then continue using the original stream.



## Reading InputStream into a String


Sometimes you may wish to read byte-input into a String. To do this you will need to find something that converts between `byte` and the "native Java" UTF-16 Codepoints used as `char`. That is done with a [`InputStreamReader`](https://docs.oracle.com/javase/8/docs/api/java/io/InputStreamReader.html).

To speed the process up a bit, it's "usual" to allocate a buffer, so that we don't have too much overhead when reading from Input.

```java
public String inputStreamToString(InputStream inputStream) throws Exception {
     StringWriter writer = new StringWriter();

     char[] buffer = new char[1024];
     try (Reader reader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))) {
           int n;
           while ((n = reader.read(buffer)) != -1) {
                // all this code does is redirect the output of `reader` to `writer` in
                // 1024 byte chunks
                writer.write(buffer, 0, n);
           }
     }
     return writer.toString();
}

```

Transforming this example to Java SE 6 (and lower)-compatible code is left out as an exercise for the reader.



## Wrapping Input/Output Streams


`OutputStream` and `InputStream` have many different classes, each of them with a unique functionality. By wrapping a stream around another, you gain the functionality of both streams.

You can wrap a stream any number of times, just take note of the ordering.

### Useful combinations

Writing characters to a file while using a buffer

```java
File myFile = new File("targetFile.txt");
PrintWriter writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(myFile)));

```

Compressing and encrypting data before writing to a file while using a buffer

```java
Cipher cipher = ... // Initialize cipher
File myFile = new File("targetFile.enc");
BufferedOutputStream outputStream = new BufferedOutputStream(new DeflaterOutputStream(new CipherOutputStream(new FileOutputStream(myFile), cipher)));

```

### List of Input/Output Stream wrappers

|Wrapper|Description
|---|---|---|---|---|---|---|---|---|---
|BufferedOutputStream/ BufferedInputStream|While `OutputStream` writes data one byte at a time, `BufferedOutputStream` writes data in chunks. This reduces the number of system calls, thus improving performance.
|DeflaterOutputStream/ DeflaterInputStream|Performs data compression.
|InflaterOutputStream/ InflaterInputStream|Performs data decompression.
|CipherOutputStream/ CipherInputStream|Encrypts/Decrypts data.
|DigestOutputStream/ DigestInputStream|Generates Message Digest to verify data integrity.
|CheckedOutputStream/ CheckedInputStream|Generates a CheckSum. CheckSum is a more trivial version of Message Digest.
|DataOutputStream/ DataInputStream|Allows writing of primitive data types and Strings. Meant for writing bytes. Platform independent.
|PrintStream|Allows writing of primitive data types and Strings. Meant for writing bytes. Platform dependent.
|OutputStreamWriter|Converts a OutputStream into a Writer. An OutputStream deals with bytes while Writers deals with characters
|PrintWriter|Automatically calls OutputStreamWriter. Allows writing of primitive data types and Strings. Strictly for writing characters and best for writing characters



## Writing bytes to an OutputStream


Writing bytes to an `OutputStream` one byte at a time

```java
OutputStream stream = object.getOutputStream();

byte b = 0x00;
stream.write( b );

```

Writing a byte array

```java
byte[] bytes = new byte[] { 0x00, 0x00 };

stream.write( bytes );

```

Writing a section of a byte array

```java
int offset = 1;
int length = 2;
byte[] bytes = new byte[] { 0xFF, 0x00, 0x00, 0xFF };

stream.write( bytes, offset, length );

```



## Copying Input Stream to Output Stream


This function copies data between two streams -

```java
void copy(InputStream in, OutputStream out) throws IOException {
    byte[] buffer = new byte[8192];
    while ((bytesRead = in.read(buffer)) > 0) {
        out.write(buffer, 0, bytesRead);
    }
}

```

Example -

```

// reading from System.in and writing to System.out
copy(System.in, System.out);

```



## DataInputStream Example


```java
package com.streams;  
import java.io.*;    
public class DataStreamDemo {  
  public static void main(String[] args) throws IOException {  
    InputStream input = new FileInputStream("D:\\datastreamdemo.txt");  
    DataInputStream inst = new DataInputStream(input);  
    int count = input.available();  
    byte[] arr = new byte[count];  
    inst.read(arr);  
    for (byte byt : arr) {  
      char ki = (char) byt;  
      System.out.print(ki+"-");  
    }  
  }  
}

```



#### Syntax


- int read(byte[] b) throws IOException



#### Remarks


Note that most of the time you do NOT use `InputStream`s directly but use `BufferedStream`s, or similar. This is because `InputStream` reads from the source every time the read method is called. This can cause significant CPU usage in context switches into and out of the kernel.

