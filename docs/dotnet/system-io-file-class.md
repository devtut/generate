---
metaTitle: "System.IO.File class"
description: "Delete a file, Strip unwanted lines from a text file, Convert text file encoding, Enumerate files older than a specified amount, Move a File from one location to another, Touch a large amount of files (to update last write time)"
---

# System.IO.File class



## Delete a file


To delete a file (if you have required permissions) is as simple as:

```dotnet
File.Delete(path);

```

However many things may go wrong:

- You do not have required permissions (`UnauthorizedAccessException` is thrown).
- File may be in use by someone else (`IOException` is thrown).
- File cannot be deleted because of low level error or media is read-only (`IOException` is thrown).
- File does not exist anymore (`IOException` is thrown).

Note that last point (file does not exist) is usually **circumvented** with a code snippet like this:

```dotnet
if (File.Exists(path))
    File.Delete(path);

```

However it's not an atomic operation and file may be delete by someone else between the call to `File.Exists()` and before `File.Delete()`. Right approach to handle I/O operation requires exception handling (assuming an alternative course of actions may be taken when operation fails):

```dotnet
if (File.Exists(path))
{
    try
    {
        File.Delete(path);
    }
    catch (IOException exception)
    {
        if (!File.Exists(path))
            return; // Someone else deleted this file

        // Something went wrong...
    }
    catch (UnauthorizedAccessException exception)
    {
        // I do not have required permissions
    }
}

```

Note that this I/O errors sometimes are transitory (file in use, for example) and if a network connection is involved then it may automatically recover without any action from our side. It's then common to **retry** an I/O operation few times with a small delay between each attempt:

```dotnet
public static void Delete(string path)
{
    if (!File.Exists(path))
        return;

    for (int i=1; ; ++i)
    {
        try
        {
            File.Delete(path);
            return;
        }
        catch (IOException e)
        {
            if (!File.Exists(path))
                return;

            if (i == NumberOfAttempts)
                throw;

            Thread.Sleep(DelayBetweenEachAttempt);
        }

        // You may handle UnauthorizedAccessException but this issue
        // will probably won't be fixed in few seconds...
    }
}

private const int NumberOfAttempts = 3;
private const int DelayBetweenEachAttempt = 1000; // ms

```

Note: in Windows environment file will not be really deleted when you call this function, if someone else open the file using `FileShare.Delete` then file can be deleted but it will effectively happen only when owner will close the file.



## Strip unwanted lines from a text file


To change a text file is not easy because its content must be moved around. For **small** files easiest method is to read its content in memory and then write back modified text.

In this example we read all lines from a file and drop all blank lines then we write back to original path:

```dotnet
File.WriteAllLines(path,
    File.ReadAllLines(path).Where(x => !String.IsNullOrWhiteSpace(x)));

```

If file is too big to load it in memory and output path is different from input path:

```dotnet
File.WriteAllLines(outputPath,
    File.ReadLines(inputPath).Where(x => !String.IsNullOrWhiteSpace(x)));

```



## Convert text file encoding


Text is saved encoded (see also [Strings](http://stackoverflow.com/documentation/.net/2227/strings#t=201607221053414331058) topic) then sometimes you may need to change its encoding, this example assumes (for simplicity) that file is not too big and it can be entirely read in memory:

```dotnet
public static void ConvertEncoding(string path, Encoding from, Encoding to)
{
    File.WriteAllText(path, File.ReadAllText(path, from), to);
}

```

When performing conversions do not forget that file may contain BOM (Byte Order Mark), to better understand how it's managed refer to [Encoding.UTF8.GetString doesn't take into account the Preamble/BOM](http://stackoverflow.com/q/11701341/1207195).



## Enumerate files older than a specified amount


This snippet is an helper function to enumerate all files older than a specified age, it's useful - for example - when you have to delete old log files or old cached data.

```dotnet
static IEnumerable<string> EnumerateAllFilesOlderThan(
                               TimeSpan maximumAge,
                               string path,
                               string searchPattern = "*.*",
                               SearchOption options = SearchOption.TopDirectoryOnly)
{
    DateTime oldestWriteTime = DateTime.Now - maximumAge;

    return Directory.EnumerateFiles(path, searchPattern, options)
        .Where(x => Directory.GetLastWriteTime(x) < oldestWriteTime);
}

```

Used like this:

```dotnet
var oldFiles = EnumerateAllFilesOlderThan(TimeSpan.FromDays(7), @"c:\log", "*.log");

```

Few things to note:

- Search is performed using `Directory.EnumerateFiles()` instead of `Directory.GetFiles()`. Enumeration is **alive** then you won't need to wait until all file system entries have been fetched.
- We're checking for last write time but you may use creation time or last access time (for example to delete **unused** cached files, note that access time may be disabled).
- Granularity isn't uniform for all those properties (write time, access time, creation time), check MSDN for details about this.



## Move a File from one location to another


### File.Move

`File.Move(@"C:\TemporaryFile.txt", @"C:\TemporaryFiles\TemporaryFile.txt");`

However, there are many things that could go wrong with this simple operation. For instance, what if the user running your program does not have a Drive that is labelled 'C'? What if they did - but they decided to rename it to 'B', or 'M'?

What if the Source file (the file in which you would like to move) has been moved without your knowing - or what if it simply doesn't exist.

This can be circumvented by first checking to see whether the source file does exist:

```dotnet
string source = @"C:\TemporaryFile.txt", destination = @"C:\TemporaryFiles\TemporaryFile.txt";
if(File.Exists("C:\TemporaryFile.txt"))
{
    File.Move(source, destination);
}

```

This will ensure that at that very moment, the file does exist, and can be moved to another location. There may be times where a simple call to `File.Exists` won't be enough. If it isn't, check again, convey to the user that the operation failed - or handle the exception.

A `FileNotFoundException` is not the only exception you are likely to encounter.

See below for possible exceptions:

|Exception Type|Description
|------
|`IOException`|The file already exists or the source file could not be found.
|`ArgumentNullException`|The value of the Source and/or Destination parameters is null.
|`ArgumentException`|The value of the Source and/or Destination parameters are empty, or contain invalid characters.
|`UnauthorizedAccessException`|You do not have the required permissions in order to perform this action.
|`PathTooLongException`|The Source, Destination or specified path(s) exceed the maximum length. On Windows, a Path's length must be less than 248 characters, while File names must be less than 260 characters.
|`DirectoryNotFoundException`|The specified directory could not be found.
|`NotSupportedException`|The Source or Destination paths or file names are in an invalid format.



## "Touch" a large amount of files (to update last write time)


This example updates last write time of a huge number of files (using `System.IO.Directory.EnumerateFiles` instead of `System.IO.Directory.GetFiles()`). Optionally you can specify a search pattern (default is `"*.*"` and eventually search through a directory tree (not only the specified directory):

```dotnet
public static void Touch(string path,
                         string searchPattern = "*.*",
                         SearchOptions options = SearchOptions.None)
{
    var now = DateTime.Now;

    foreach (var filePath in Directory.EnumerateFiles(path, searchPattern, options))
    {
        File.SetLastWriteTime(filePath, now);
    }
}

```



#### Syntax


- string source;
- string destination;



#### Parameters


|Parameter|Details
|------
|`source`|The file that is to be moved to another location.
|`destination`|The directory in which you would like to move `source` to (this variable should also contain the name (and file extension) of the file.

