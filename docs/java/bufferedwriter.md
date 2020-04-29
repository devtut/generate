---
metaTitle: "BufferedWriter"
description: "Write a line of text to File"
---

# BufferedWriter



## Write a line of text to File


This code writes the string to a file. It is important to close the writer, so this is done in a `finally` block.

```

 public void writeLineToFile(String str) throws IOException {
    File file = new File("file.txt");
    BufferedWriter bw = null;
    try {
      bw = new BufferedWriter(new FileWriter(file));
      bw.write(str);
    } finally {
      if (bw != null) {
        bw.close();
      }
    }
  }

```

Also note that `write(String s)` does not place newline character after string has been written. To put it use `newLine()` method.

Java 7 adds the [`java.nio.file`](https://docs.oracle.com/javase/7/docs/api/java/nio/file/package-summary.html) package, and [try-with-resources](http://stackoverflow.com/documentation/java/89/exceptions/1581/using-try-with-resources#t=201607231444506229575):

```java
public void writeLineToFile(String str) throws IOException {
    Path path = Paths.get("file.txt");
    try (BufferedWriter bw = Files.newBufferedWriter(path)) {
        bw.write(str);
    }
}

```



#### Syntax


- new BufferedWriter(Writer);  //The default constructor
- BufferedWriter.write(int c);  //Writes a single character
- BufferedWriter.write(String str);  //Writes a string
- BufferedWriter.newLine();  //Writes a line separator
- BufferedWriter.close();  //Closes the BufferedWriter



#### Remarks


- If you try to write from a `BufferedWriter` (using `BufferedWriter.write()`) after closing the `BufferedWriter` (using `BufferedWriter.close()`), it will throw an `IOException`.
- The `BufferedWriter(Writer)` constructor does NOT throw an `IOException`. However, the `FileWriter(File)` constructor throws a `FileNotFoundException`, which extends `IOException`. So catching `IOException` will also catch `FileNotFoundException`, there is never a need for a second catch statement unless you plan on doing something different with the `FileNotFoundException`.

