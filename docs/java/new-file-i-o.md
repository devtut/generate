---
metaTitle: "Java - New File I/O"
description: "Manipulating paths, Creating paths, Retrieving information about a path, Retrieving information using the filesystem, Reading files, Writing files"
---

# New File I/O




## Manipulating paths


### Joining Two Paths

Paths can be joined using the `resolve()` method. The path passed has to be  a partial path, which is a path that doesn't include the root element.

```java
Path p5 = Paths.get("/home/");
Path p6 = Paths.get("arthur/files");
Path joined = p5.resolve(p6);
Path otherJoined = p5.resolve("ford/files");

```

```java
joined.toString() == "/home/arthur/files"
otherJoined.toString() == "/home/ford/files"

```

### Normalizing a path

Paths may contain the elements `.` (which points to the directory you're currently in) and `..`(which points to the parent directory).

When used in a path, `.` can be removed at any time without changing the path's destination, and `..` can be removed together with the preceding element.

With the Paths API, this is done using the `.normalize()` method:

```java
Path p7 = Paths.get("/home/./arthur/../ford/files");
Path p8 = Paths.get("C:\\Users\\.\\..\\Program Files");

```

```java
p7.normalize().toString() == "/home/ford/files"
p8.normalize().toString() == "C:\\Program Files"

```



## Creating paths


The `Path` class is used to programmaticaly represent a path in the file system (and can therefore point to files as well as directories, even to non-existent ones)

A path can be obtained using the helper class `Paths`:

```java
Path p1 = Paths.get("/var/www");
Path p2 = Paths.get(URI.create("file:///home/testuser/File.txt"));
Path p3 = Paths.get("C:\\Users\\DentAr\\Documents\\HHGTDG.odt");
Path p4 = Paths.get("/home", "arthur", "files", "diary.tex");

```



## Retrieving information about a path


Information about a path can be get using the methods of a `Path` object:

<li>
`toString()` returns the string representation of the path

```java
Path p1 = Paths.get("/var/www"); // p1.toString() returns "/var/www"

```


</li>
<li>
`getFileName()` returns the file name (or, more specifically, the last element of the path

```java
Path p1 = Paths.get("/var/www"); // p1.getFileName() returns "www"
Path p3 = Paths.get("C:\\Users\\DentAr\\Documents\\HHGTDG.odt"); // p3.getFileName() returns "HHGTDG.odt"

```


</li>
<li>
`getNameCount()` returns the number of elements that form the path

```java
Path p1 = Paths.get("/var/www"); // p1.getNameCount() returns 2

```


</li>
<li>
`getName(int index)` returns the element at the given index

```java
Path p1 = Paths.get("/var/www"); // p1.getName(0) returns "var", p1.getName(1) returns "www"

```


</li>
<li>
`getParent()` returns the path of the parent directory

```java
Path p1 = Paths.get("/var/www"); // p1.getParent().toString() returns "/var"

```


</li>
<li>
`getRoot()` returns the root of the path

```java
Path p1 = Paths.get("/var/www"); // p1.getRoot().toString() returns "/"
Path p3 = Paths.get("C:\\Users\\DentAr\\Documents\\HHGTDG.odt"); // p3.getRoot().toString() returns "C:\\"

```


</li>



## Retrieving information using the filesystem


To interact with the filesystem you use the methods of the class `Files`.

### Checking existence

To check the existence of the file or directory a path points to, you use the following methods:

```java
Files.exists(Path path)

```

and

```java
Files.notExists(Path path)

```

`!Files.exists(path)` does not neccesarily have to be equal to `Files.notExists(path)`, because there are three possible scenarios:

- A file's or directory's existence is verified (`exists` returns `true` and `notExists` returns `false` in this case)
- A file's or directory's nonexistence is verfied (`exists` returns `false` and `notExists` returns `true`)
- Neither the existence nor the nonexistence of a file or a directory can be verified (for example due to access restrictions): Both `exists` and `nonExists` return false.

### Checking whether a path points to a file or a directory

This is done using `Files.isDirectory(Path path)` and `Files.isRegularFile(Path path)`

```java
Path p1 = Paths.get("/var/www");
Path p2 = Paths.get("/home/testuser/File.txt");

```

```java
Files.isDirectory(p1) == true
Files.isRegularFile(p1) == false

Files.isDirectory(p2) == false
Files.isRegularFile(p2) == true

```

### Getting properties

This can be done using the following methods:

```java
Files.isReadable(Path path)
Files.isWritable(Path path)
Files.isExecutable(Path path)

Files.isHidden(Path path)
Files.isSymbolicLink(Path path)

```

### Getting MIME type

```java
Files.probeContentType(Path path)

```

This tries to get the MIME type of a file. It returns a MIME type String, like this:

- `text/plain` for text files
- `text/html` for HTML pages
- `application/pdf` for PDF files
- `image/png` for PNG files



## Reading files


Files can be read byte- and line-wise using the `Files` class.

```java
Path p2 = Paths.get(URI.create("file:///home/testuser/File.txt"));
byte[] content = Files.readAllBytes(p2);
List<String> linesOfContent = Files.readAllLines(p2);

```

`Files.readAllLines()` optionally takes a charset as parameter (default is `StandardCharsets.UTF_8`):

```java
List<String> linesOfContent = Files.readAllLines(p2, StandardCharsets.ISO_8859_1);

```



## Writing files


Files can be written bite- and line-wise using the `Files` class

```java
Path p2 = Paths.get("/home/testuser/File.txt");
List<String> lines = Arrays.asList(
    new String[]{"First line", "Second line", "Third line"});

Files.write(p2, lines);

```

```java
Files.write(Path path, byte[] bytes)

```

Existing files wile be overridden, non-existing files will be created.



#### Syntax


- Paths.get(String first, String... more) // Creates a Path instance by its String elements
- Paths.get(URI uri) // Creates a Path instance by a URI

