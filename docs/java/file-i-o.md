---
metaTitle: "File I/O"
description: "Migrating from java.io.File to Java 7 NIO (java.nio.file.Path), Reading an image from a file, File Read/Write Using  FileInputStream/FileOutputStream, Reading all bytes to a byte[], Copying a file using Channel, Writing a byte[] to a file, Stream vs Writer/Reader API, Reading a file with a Scanner, Reading a whole file at once, Iterating over a directory and filter by file extension, Reading from a binary file, Locking, Copying a file using InputStream and OutputStream, Reading a file using Channel and Buffer, Reading  a file using BufferedInputStream, Writing a file using Channel and Buffer, Writing a file using PrintStream, Iterate over a directory printing subdirectories in it, Adding Directories, Blocking or redirecting standard output / error, Accessing the contents of a ZIP file"
---

# File I/O


[Java I/O](https://docs.oracle.com/javase/7/docs/api/java/io/package-summary.html) (Input and Output) is used to process the input and produce the output. Java uses the concept of stream to make I/O operation fast. The java.io package contains all the classes required for input and output operations. [Handling files](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) is also done in java by Java I/O API.



## Migrating from java.io.File to Java 7 NIO (java.nio.file.Path)


These examples assume that you already know what Java 7's NIO is in general, and you are used to writing code using `java.io.File`. Use these examples as a means to quickly find more NIO-centric documentation for migrating.

There is much more to Java 7's NIO such as [memory-mapped files](https://docs.oracle.com/javase/7/docs/api/java/nio/channels/FileChannel.html) or [opening a ZIP or JAR file using FileSystem](http://docs.oracle.com/javase/7/docs/technotes/guides/io/fsp/zipfilesystemprovider.html). These examples will only cover a limited number of basic use cases.

As a basic rule, if you are used to perform a file system read/write operation using a [`java.io.File`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) instance method, you will find it as a static method within [`java.nio.file.Files`](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html).

### Point to a path

```java
// -> IO
File file = new File("io.txt");

// -> NIO
Path path = Paths.get("nio.txt");

```

### Paths relative to another path

```java
// Forward slashes can be used in place of backslashes even on a Windows operating system
// -> IO
File folder = new File("C:/");
File fileInFolder = new File(folder, "io.txt");

// -> NIO
Path directory = Paths.get("C:/");
Path pathInDirectory = directory.resolve("nio.txt");

```

### Converting File from/to Path for use with libraries

```java
// -> IO to NIO
Path pathFromFile = new File("io.txt").toPath();

// -> NIO to IO
File fileFromPath = Paths.get("nio.txt").toFile();

```

### Check if the file exists and delete it if it does

```java
// -> IO
if (file.exists()) {
    boolean deleted = file.delete();
    if (!deleted) {
        throw new IOException("Unable to delete file");
    }
}

// -> NIO
Files.deleteIfExists(path);

```

### Write to a file via an OutputStream

There are several ways to write and read from a file using NIO for different performance and memory constraints, readability and use cases, such as [`FileChannel`](http://stackoverflow.com/questions/7366266/best-way-to-write-string-to-file-using-java-nio), [`Files.write(Path path, byte\[\] bytes, OpenOption... options)`](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#write(java.nio.file.Path,%20byte%5B%5D,%20java.nio.file.OpenOption...))... In this example, only `OutputStream` is covered, but you are strongly encouraged to learn about memory-mapped files and the various static methods available in `java.nio.file.Files`.

```java
List<String> lines = Arrays.asList(
        String.valueOf(Calendar.getInstance().getTimeInMillis()),
        "line one",
        "line two");

// -> IO
if (file.exists()) {
    // Note: Not atomic
    throw new IOException("File already exists");
}
try (FileOutputStream outputStream = new FileOutputStream(file)) {
    for (String line : lines) {
        outputStream.write((line + System.lineSeparator()).getBytes(StandardCharsets.UTF_8));
    }
}

// -> NIO
try (OutputStream outputStream = Files.newOutputStream(path, StandardOpenOption.CREATE_NEW)) {
    for (String line : lines) {
        outputStream.write((line + System.lineSeparator()).getBytes(StandardCharsets.UTF_8));
    }
}

```

### Iterating on each file within a folder

```java
// -> IO
for (File selectedFile : folder.listFiles()) {
    // Note: Depending on the number of files in the directory folder.listFiles() may take a long time to return
    System.out.println((selectedFile.isDirectory() ? "d" : "f") + " " + selectedFile.getAbsolutePath());
}

// -> NIO
Files.walkFileTree(directory, EnumSet.noneOf(FileVisitOption.class), 1, new SimpleFileVisitor<Path>() {
    @Override
    public FileVisitResult preVisitDirectory(Path selectedPath, BasicFileAttributes attrs) throws IOException {
        System.out.println("d " + selectedPath.toAbsolutePath());
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path selectedPath, BasicFileAttributes attrs) throws IOException {
        System.out.println("f " + selectedPath.toAbsolutePath());
        return FileVisitResult.CONTINUE;
    }
});

```

### Recursive folder iteration

```java
// -> IO
recurseFolder(folder);

// -> NIO
// Note: Symbolic links are NOT followed unless explicitly passed as an argument to Files.walkFileTree
Files.walkFileTree(directory, new SimpleFileVisitor<Path>() {
    @Override
    public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException {
        System.out.println("d " + selectedPath.toAbsolutePath());
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFile(Path selectedPath, BasicFileAttributes attrs) throws IOException {
        System.out.println("f " + selectedPath.toAbsolutePath());
        return FileVisitResult.CONTINUE;
    }
});


private static void recurseFolder(File folder) {
    for (File selectedFile : folder.listFiles()) {
        System.out.println((selectedFile.isDirectory() ? "d" : "f") + " " + selectedFile.getAbsolutePath());
        if (selectedFile.isDirectory()) {
            // Note: Symbolic links are followed
            recurseFolder(selectedFile);
        }
    }
}

```



## Reading an image from a file


```java
import java.awt.Image;
import javax.imageio.ImageIO;

...

try {
    Image img = ImageIO.read(new File("~/Desktop/cat.png"));
} catch (IOException e) {
    e.printStackTrace();
}

```



## File Read/Write Using  FileInputStream/FileOutputStream


Write to a file test.txt:

```java
String filepath ="C:\\test.txt";
FileOutputStream fos = null;
try {
      fos = new FileOutputStream(filepath);
      byte[] buffer = "This will be written in test.txt".getBytes();
      fos.write(buffer, 0, buffer.length);
      fos.close();
} catch (FileNotFoundException e) {
      e.printStackTrace();
} catch (IOException e) {
      e.printStackTrace();
} finally{
      if(fos != null)
        fos.close();
}

```

Read from file test.txt:

```java
String filepath ="C:\\test.txt";        
FileInputStream fis = null;
try {
   fis = new FileInputStream(filepath);
   int length = (int) new File(filepath).length();
   byte[] buffer = new byte[length];
   fis.read(buffer, 0, length);
} catch (FileNotFoundException e) {
     e.printStackTrace();
} catch (IOException e) {
     e.printStackTrace();
} finally{
   if(fis != null)
     fis.close();
}

```

Note, that since Java 1.7 the [try-with-resources](http://stackoverflow.com/documentation/java/89/exceptions/1581/using-try-with-resources) statement was introduced what made implementation of reading\writing operation much simpler:

Write to a file test.txt:

```java
String filepath ="C:\\test.txt";
try (FileOutputStream fos = new FileOutputStream(filepath)){
    byte[] buffer = "This will be written in test.txt".getBytes();
    fos.write(buffer, 0, buffer.length);
} catch (FileNotFoundException e) {
    e.printStackTrace();
} catch (IOException e) {
    e.printStackTrace();
}

```

Read from file test.txt:

```java
String filepath ="C:\\test.txt";
try (FileInputStream fis = new FileInputStream(filepath)){
    int length = (int) new File(filepath).length();
    byte[] buffer = new byte[length];
    fis.read(buffer, 0, length);
} catch (FileNotFoundException e) {
    e.printStackTrace();
} catch (IOException e) {
    e.printStackTrace();
}

```



## Reading all bytes to a byte[]


Java 7 introduced the very useful [Files](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html) class

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.Path;

Path path = Paths.get("path/to/file");

try {
    byte[] data = Files.readAllBytes(path);
} catch(IOException e) {
    e.printStackTrace();
}

```



## Copying a file using Channel


We can use `Channel` to copy file content faster. To do so, we can use `transferTo()` method of `FileChannel` .

```java
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;

public class FileCopier {
    
    public static void main(String[] args) {
        File sourceFile = new File("hello.txt");
        File sinkFile = new File("hello2.txt");
        copy(sourceFile, sinkFile);
    }

    public static void copy(File sourceFile, File destFile) {
        if (!sourceFile.exists() || !destFile.exists()) {
            System.out.println("Source or destination file doesn't exist");
            return;
        }


        try (FileChannel srcChannel = new FileInputStream(sourceFile).getChannel();
             FileChannel sinkChanel = new FileOutputStream(destFile).getChannel()) {

            srcChannel.transferTo(0, srcChannel.size(), sinkChanel);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```



## Writing a byte[] to a file


```java
byte[] bytes = { 0x48, 0x65, 0x6c, 0x6c, 0x6f };

try(FileOutputStream stream = new FileOutputStream("Hello world.txt")) {
    stream.write(bytes);
} catch (IOException ioe) {
    // Handle I/O Exception
    ioe.printStackTrace();
}

```

```java
byte[] bytes = { 0x48, 0x65, 0x6c, 0x6c, 0x6f };

FileOutputStream stream = null;
try {
    stream = new FileOutputStream("Hello world.txt");
    stream.write(bytes);
} catch (IOException ioe) {
    // Handle I/O Exception
    ioe.printStackTrace();
} finally {
    if (stream != null) {
        try {
            stream.close();
        } catch (IOException ignored) {}
    }
}

```

Most java.io file APIs accept both `String`s and `File`s as arguments, so you could as well use

```java
File file = new File("Hello world.txt");
FileOutputStream stream = new FileOutputStream(file);

```



## Stream vs Writer/Reader API


Streams provide the most direct access to the binary content, so any [`InputStream`](https://docs.oracle.com/javase/7/docs/api/java/io/InputStream.html) / [`OutputStream`](https://docs.oracle.com/javase/7/docs/api/java/io/OutputStream.html) implementations always operate on `int`s and `byte`s.

```java
// Read a single byte from the stream
int b = inputStream.read();
if (b >= 0) { // A negative value represents the end of the stream, normal values are in the range 0 - 255
    // Write the byte to another stream
    outputStream.write(b);
}

// Read a chunk
byte[] data = new byte[1024];
int nBytesRead = inputStream.read(data);
if (nBytesRead >= 0) { // A negative value represents end of stream
    // Write the chunk to another stream
    outputStream.write(data, 0, nBytesRead);
}

```

There are some exceptions, probably most notably the [`PrintStream`](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html) which adds the "ability to print representations of various data values conveniently". This allows to use [`System.out`](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#out) both as a binary `InputStream` and as a textual output using methods such as `System.out.println()`.

Also, some stream implementations work as an interface to higher-level contents such as Java objects (see Serialization) or native types, e.g. [`DataOutputStream`](https://docs.oracle.com/javase/7/docs/api/java/io/DataOutputStream.html) / [`DataInputStream`](https://docs.oracle.com/javase/7/docs/api/java/io/DataInputStream.html).

With the [`Writer`](https://docs.oracle.com/javase/7/docs/api/java/io/Writer.html) and [`Reader`](https://docs.oracle.com/javase/7/docs/api/java/io/Reader.html) classes, Java also provides an API for explicit character streams. Although most applications will base these implementations on streams, the character stream API does not expose any methods for binary content.

```java
// This example uses the platform's default charset, see below
// for a better implementation.

Writer writer = new OutputStreamWriter(System.out);
writer.write("Hello world!");

Reader reader = new InputStreamReader(System.in);
char singleCharacter = reader.read();

```

Whenever it is necessary to encode characters into binary data (e.g. when using the `InputStreamWriter` / `OutputStreamWriter` classes), you should specify a charset if you do not want to depend on the platform's default charset. When in doubt, use a Unicode-compatible encoding, e.g. UTF-8 which is supported on all Java platforms. Therefore, you should probably stay away from classes like `FileWriter` and `FileReader` as those always use the default platform charset. A better way to access files using character streams is this:

```java
Charset myCharset = StandardCharsets.UTF_8;

Writer writer = new OutputStreamWriter( new FileOutputStream("test.txt"), myCharset );
writer.write('Ä');
writer.flush();
writer.close();

Reader reader = new InputStreamReader( new FileInputStream("test.txt"), myCharset );
char someUnicodeCharacter = reader.read();
reader.close();

```

One of the most commonly used `Reader`s is `BufferedReader` which provides a method to read whole lines of text from another reader and is presumably the simplest way to read a character stream line by line:

```java
// Read from baseReader, one line at a time
BufferedReader reader = new BufferedReader( baseReader );
String line;
while((line = reader.readLine()) != null) {
  // Remember: System.out is a stream, not a writer!
  System.out.println(line);
}

```



## Reading a file with a Scanner


Reading a file line by line

```java
public class Main {

    public static void main(String[] args) {
        try {
            Scanner scanner = new Scanner(new File("example.txt"));
            while(scanner.hasNextLine())
            {
                String line = scanner.nextLine();
                //do stuff
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}

```

word by word

```java
public class Main {

    public static void main(String[] args) {
        try {
            Scanner scanner = new Scanner(new File("example.txt"));
            while(scanner.hasNext())
            {
                String line = scanner.next();
                //do stuff
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}

```

and you can also change the delimeter by using scanner.useDelimeter() method



## Reading a whole file at once


```java
File f = new File(path);
String content = new Scanner(f).useDelimiter("\\Z").next();

```

\Z is the EOF (End of File) Symbol. When set as delimiter the Scanner will read the fill until the EOF Flag is reached.



## Iterating over a directory and filter by file extension


```

   public void iterateAndFilter() throws IOException {
        Path dir = Paths.get("C:/foo/bar");
        PathMatcher imageFileMatcher =
            FileSystems.getDefault().getPathMatcher(
                "regex:.*(?i:jpg|jpeg|png|gif|bmp|jpe|jfif)");

        try (DirectoryStream<Path> stream = Files.newDirectoryStream(dir,
                entry -> imageFileMatcher.matches(entry.getFileName()))) {

            for (Path path : stream) {
                System.out.println(path.getFileName());
            }
        }
    }

```



## Reading from a binary file


You can read an a binary file using this piece of code in all recent versions of Java:

```java
File file = new File("path_to_the_file");
byte[] data = new byte[(int) file.length()];
DataInputStream stream = new DataInputStream(new FileInputStream(file));
stream.readFully(data);
stream.close();

```

If you are using Java 7 or later, there is a simpler way using the `nio API`:

```java
Path path = Paths.get("path_to_the_file");
byte [] data = Files.readAllBytes(path);

```



## Locking


A file can be locked using the `FileChannel` API that can be acquired from Input Output `streams` and `readers`

Example with `streams`

// Open a file stream
FileInputStream ios = new FileInputStream(filename);

```

   // get underlying channel
    FileChannel channel = ios.getChannel();

    /*
     * try to lock the file. true means whether the lock is shared or not i.e. multiple processes can acquire a
     * shared lock (for reading only) Using false with readable channel only will generate an exception. You should
     * use a writable channel (taken from FileOutputStream) when using false. tryLock will always return immediately
     */
    FileLock lock = channel.tryLock(0, Long.MAX_VALUE, true);

    if (lock == null) {
        System.out.println("Unable to acquire lock");
    } else {
        System.out.println("Lock acquired successfully");
    }

    // you can also use blocking call which will block until a lock is acquired.
    channel.lock();

    // Once you have completed desired operations of file. release the lock
    if (lock != null) {
        lock.release();
    }

    // close the file stream afterwards
    // Example with reader
    RandomAccessFile randomAccessFile = new RandomAccessFile(filename,  "rw");
    FileChannel channel = randomAccessFile.getChannel();
    //repeat the same steps as above but now you can use shared as true or false as the channel is in read write mode

```



## Copying a file using InputStream and OutputStream


We can directly copy data from a source to a data sink using a loop. In this example, we are reading data from an InputStream and at the same time, writing to an OutputStream. Once we are done reading and writing, we have to close the resource.

```java
public void copy(InputStream source, OutputStream destination) throws IOException {
    try {
        int c;
        while ((c = source.read()) != -1) {
            destination.write(c);
        }
    } finally {
        if (source != null) {
            source.close();
        }
        if (destination != null) {
            destination.close();
        }
    }
}

```



## Reading a file using Channel and Buffer


`Channel` uses a `Buffer` to read/write data. A buffer is a fixed sized container where we can write a block of data at once. `Channel` is a quite faster than stream-based I/O.

To read data from a file using `Channel` we need to have the following steps-

1. We need an instance of `FileInputStream`. `FileInputStream`has a method named `getChannel()` which returns a Channel.
1. Call the `getChannel()` method of FileInputStream and acquire Channel.
1. Create a ByteBuffer. ByteBuffer is a fixed size container of bytes.
1. Channel has a read method and we have to provide a ByteBuffer as an argument to this read method.   ByteBuffer has two modes - read-only mood and write-only mood. We can change the mode using `flip()` method call. Buffer has a position, limit, and capacity. Once a buffer is created with a fixed size, its limit and capacity are the same as the size and the position starts from zero. While a buffer is written with data, its position gradually increases. Changing mode means, changing the position. To read data from the beginning of a buffer, we have to set the position to zero. flip() method change the position
1. When we call the read method of the `Channel`, it fills up the buffer using data.
1. If we need to read the data from the `ByteBuffer`, we need to flip the buffer to change its mode to write-only to read-only mode and then keep reading data from the buffer.
1. When there is no longer data to read, the `read()` method of channel returns 0 or -1.

```java
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class FileChannelRead {
 
public static void main(String[] args) {
  
   File inputFile = new File("hello.txt");
  
   if (!inputFile.exists()) {
    System.out.println("The input file doesn't exit.");
    return;
   }

  try {
   FileInputStream fis = new FileInputStream(inputFile);
   FileChannel fileChannel = fis.getChannel();
   ByteBuffer buffer = ByteBuffer.allocate(1024);

   while (fileChannel.read(buffer) > 0) {
    buffer.flip();
    while (buffer.hasRemaining()) {
     byte b = buffer.get();
     System.out.print((char) b);
    }
    buffer.clear();
   }

   fileChannel.close();
  } catch (IOException e) {
   e.printStackTrace();
  }
 }
}

```



## Reading  a file using BufferedInputStream


Reading file using a `BufferedInputStream` generally faster than `FileInputStream` because it maintains an internal buffer to store bytes read from the underlying input stream.

```java
import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;


public class FileReadingDemo {

    public static void main(String[] args) {
        String source = "hello.txt";
        
        try (BufferedInputStream bis = new BufferedInputStream(new FileInputStream(source))) {
            byte data;
            while ((data = (byte) bis.read()) != -1) {
                System.out.println((char) data);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}

```



## Writing a file using Channel and Buffer


To write data to a file using `Channel` we need to have the following steps:

1. First, we need to get an object of `FileOutputStream`
1. Acquire `FileChannel` calling the `getChannel()` method from the `FileOutputStream`
1. Create a `ByteBuffer` and then fill it with data
1. Then we have to call the `flip()` method of the `ByteBuffer` and pass it as an argument of the `write()` method of the `FileChannel`
1. Once we are done writing, we have to close the resource

```java
import java.io.*;
import java.nio.*;
public class FileChannelWrite {

 public static void main(String[] args) {

  File outputFile = new File("hello.txt");
  String text = "I love Bangladesh.";

  try {
   FileOutputStream fos = new FileOutputStream(outputFile);
   FileChannel fileChannel = fos.getChannel();
   byte[] bytes = text.getBytes();
   ByteBuffer buffer = ByteBuffer.wrap(bytes);
   fileChannel.write(buffer);
   fileChannel.close();
  } catch (java.io.IOException e) {
   e.printStackTrace();
  }
 }
}  

```



## Writing a file using PrintStream


We can use `PrintStream` class to write a file. It has several methods that let you print any data type values. `println()` method appends a new line.
Once we are done printing, we have to flush the `PrintStream`.

```java
import java.io.FileNotFoundException;
import java.io.PrintStream;
import java.time.LocalDate;

public class FileWritingDemo {
    public static void main(String[] args) {
        String destination = "file1.txt";

        try(PrintStream ps = new PrintStream(destination)){
            ps.println("Stackoverflow documentation seems fun.");
            ps.println();
            ps.println("I love Java!");
            ps.printf("Today is: %1$tm/%1$td/%1$tY", LocalDate.now());

            ps.flush();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }

    }
}

```



## Iterate over a directory printing subdirectories in it


```

 public void iterate(final String dirPath) throws IOException {
    final DirectoryStream<Path> paths = Files.newDirectoryStream(Paths.get(dirPath));
    for (final Path path : paths) {
      if (Files.isDirectory(path)) {
        System.out.println(path.getFileName());
      }
    }
  }

```



## Adding Directories


To make a new directory from a `File` instance you would need to use one of two methods: `mkdirs()` or `mkdir()`.

- `mkdir()` - Creates the directory named by this abstract pathname. ([source](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#mkdir()))
- `mkdirs()` - Creates the directory named by this abstract pathname, including any necessary but nonexistent parent directories. Note that if this operation fails it may have succeeded in creating some of the necessary parent directories. ([source](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#mkdirs()))

**Note:** `createNewFile()` will not create a new directory only a file.

```java
File singleDir = new File("C:/Users/SomeUser/Desktop/A New Folder/");
    
File multiDir = new File("C:/Users/SomeUser/Desktop/A New Folder 2/Another Folder/");

// assume that neither "A New Folder" or "A New Folder 2" exist

singleDir.createNewFile(); // will make a new file called "A New Folder.file"
singleDir.mkdir(); // will make the directory
singleDir.mkdirs(); // will make the directory

multiDir.createNewFile(); // will throw a IOException
multiDir.mkdir(); // will not work
multiDir.mkdirs(); // will make the directory

```



## Blocking or redirecting standard output / error


Sometimes a poorly designed 3rd-party library will write unwanted diagnostics to  `System.out` or `System.err` streams.  The recommended solutions to this would be to either find a better library or (in the case of open source) fix the problem and contribute a patch to the developers.

If the above solutions are not feasible, then you should consider redirecting the streams.

**Redirection on the command line**

On a UNIX, Linux or MacOSX system can be done from the shell using `>` redirection.  For example:

```java
$ java -jar app.jar arg1 arg2 > /dev/null 2>&1
$ java -jar app.jar arg1 arg2 > out.log 2> error.log

```

The first one redirects standard output and standard error to "/dev/null", which throws away anything written to those streams.  The second of redirects standard output to "out.log" and standard error to "error.log".

(For more information on redirection, refer to the documentation of the command shell you are using.  Similar advice applies to Windows.)

Alternatively, you could implement the redirection in a wrapper script or batch file that launches the Java application.

**Redirection within a Java application**

It is also possible to redired the streams **within** a Java application using `System.setOut()` and `System.setErr()`.  For example, the following snippet redirects standard output and standard error to 2 log files:

```java
System.setOut(new PrintStream(new FileOutputStream(new File("out.log"))));
System.setErr(new PrintStream(new FileOutputStream(new File("err.log"))));

```

If you want to throw away the output entirely, you can create an output stream that "writes" to an invalid file descriptor.  This is functionally equivalent to writing to "/dev/null" on UNIX.

```java
System.setOut(new PrintStream(new FileOutputStream(new FileDescriptor())));
System.setErr(new PrintStream(new FileOutputStream(new FileDescriptor())));

```

Caution: be careful how you use `setOut` and `setErr`:

1. The redirection will affect the entire JVM.
1. By doing this, you are taking away the user's ability to redirect the streams from the command line.



## Accessing the contents of a ZIP file


The FileSystem API of Java 7 allows to read and add entries from or to a Zip file using the Java NIO file API in the same way as operating on any other filesystem.

The FileSystem is a resource that should be properly closed after use, therefore the try-with-resources block should be used.

### Reading from an existing file

```java
Path pathToZip = Paths.get("path/to/file.zip");
try(FileSystem zipFs = FileSystems.newFileSystem(pathToZip, null)) {
  Path root = zipFs.getPath("/");
  ... //access the content of the zip file same as ordinary files
} catch(IOException ex) {
  ex.printStackTrace();
}

```

### Creating a new file

```java
Map<String, String> env = new HashMap<>();  
env.put("create", "true"); //required for creating a new zip file
env.put("encoding", "UTF-8"); //optional: default is UTF-8
URI uri = URI.create("jar:file:/path/to/file.zip");
try (FileSystem zipfs = FileSystems.newFileSystem(uri, env)) {
  Path newFile = zipFs.getPath("/newFile.txt");
  //writing to file
  Files.write(newFile, "Hello world".getBytes());
} catch(IOException ex) {
  ex.printStackTrace();
}

```

