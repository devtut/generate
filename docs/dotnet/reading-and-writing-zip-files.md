---
metaTitle: ".NET Framework - Reading and writing Zip files"
description: "Listing ZIP contents, Extracting files from ZIP files, Updating a ZIP file"
---

# Reading and writing Zip files

The **ZipFile** class lives in the **System.IO.Compression** namespace. It can be used to read from, and write to Zip files.

## Listing ZIP contents

This snippet will list all the filenames of a zip archive. The filenames are relative to the zip root.

```dotnet
using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
{
    for (int i = 0; i < archive.Entries.Count; i++)
    {
        Console.WriteLine($"{i}: {archive.Entries[i]}");
    }
}

```

## Extracting files from ZIP files

Extracting all the files into a directory is very easy:

```dotnet
using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
{
    archive.ExtractToDirectory(AppDomain.CurrentDomain.BaseDirectory);
}

```

When the file already exists, a **System.IO.IOException** will be thrown.

Extracting specific files:

```dotnet
using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Read))
{
    // Get a root entry file
    archive.GetEntry("test.txt").ExtractToFile("test_extracted_getentries.txt", true);

    // Enter a path if you want to extract files from a subdirectory
    archive.GetEntry("sub/subtest.txt").ExtractToFile("test_sub.txt", true);

    // You can also use the Entries property to find files
    archive.Entries.FirstOrDefault(f => f.Name == "test.txt")?.ExtractToFile("test_extracted_linq.txt", true);

    // This will throw a System.ArgumentNullException because the file cannot be found
    archive.GetEntry("nonexistingfile.txt").ExtractToFile("fail.txt", true);
}

```

Any of these methods will produce the same result.

## Updating a ZIP file

To update a ZIP file, the file has to be opened with ZipArchiveMode.Update instead.

```dotnet
using (FileStream fs = new FileStream("archive.zip", FileMode.Open))
using (ZipArchive archive = new ZipArchive(fs, ZipArchiveMode.Update))
{
    // Add file to root
    archive.CreateEntryFromFile("test.txt", "test.txt");

    // Add file to subfolder
    archive.CreateEntryFromFile("test.txt", "symbols/test.txt");
}

```

There is also the option to write directly to a file within the archive:

```dotnet
var entry = archive.CreateEntry("createentry.txt");
using(var writer = new StreamWriter(entry.Open()))
{
    writer.WriteLine("Test line");
}

```

#### Remarks

<li>
You can also use a MemoryStream instead of a FileStream.
</li>
<li>
Exceptions
</li>

| Exception                   | Condition                                                                                                                                 |
| --------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------- |
| ArgumentException           | The stream has already been closed, or the capabilities of the stream does not match the mode (eg: trying to write to a read only stream) |
| ArgumentNullException       | input **stream** is null                                                                                                                  |
| ArgumentOutOfRangeException | **mode** has an invalid value                                                                                                             |
| InvalidDataException        | See list below                                                                                                                            |

When a **InvalidDataException** is thrown, it can have 3 causes:

- The contents of the stream could not be interpreted as a zip archive
- **mode** is Update and an entry is missing from the archive or is corrupt and cannot be read
- **mode** is Update and an entry is too large to fit into memory

All information has been taken from [this MSDN page](<https://msdn.microsoft.com/en-us/library/system.io.compression.ziparchive(v=vs.110).aspx>)
