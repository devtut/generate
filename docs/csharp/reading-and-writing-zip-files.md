---
metaTitle: "Reading and writing .zip files"
description: "Writing to a zip file, Writing Zip Files in-memory, Get files from a Zip file, The following example shows how to open a zip archive and extract all .txt files to a folder"
---

# Reading and writing .zip files




## Writing to a zip file


To write a new .zip file:

```cs
System.IO.Compression
System.IO.Compression.FileSystem

using (FileStream zipToOpen = new FileStream(@"C:\temp", FileMode.Open)) 
{
    using (ZipArchive archive = new ZipArchive(zipToOpen, ZipArchiveMode.Update)) 
    {
        ZipArchiveEntry readmeEntry = archive.CreateEntry("Readme.txt");
        using (StreamWriter writer = new StreamWriter(readmeEntry.Open())) 
        {
            writer.WriteLine("Information about this package.");
            writer.WriteLine("========================");
        }
    }
}

```



## Writing Zip Files in-memory


The following example will return the `byte[]` data of a zipped file containing the files provided to it, without needing access to the file system.

```cs
public static byte[] ZipFiles(Dictionary<string, byte[]> files)
{
    using (MemoryStream ms = new MemoryStream())
    {
        using (ZipArchive archive = new ZipArchive(ms, ZipArchiveMode.Update))
        {
            foreach (var file in files)
            {
                ZipArchiveEntry orderEntry = archive.CreateEntry(file.Key); //create a file with this name
                using (BinaryWriter writer = new BinaryWriter(orderEntry.Open()))
                {
                    writer.Write(file.Value); //write the binary data
                }
            }
        }
        //ZipArchive must be disposed before the MemoryStream has data
        return ms.ToArray();
    }
}

```



## Get files from a Zip file


This example gets a listing of files from the provided zip archive binary data:

```cs
public static Dictionary<string, byte[]> GetFiles(byte[] zippedFile) 
{
    using (MemoryStream ms = new MemoryStream(zippedFile))
    using (ZipArchive archive = new ZipArchive(ms, ZipArchiveMode.Read)) 
    {
        return archive.Entries.ToDictionary(x => x.FullName, x => ReadStream(x.Open()));
    }
}

private static byte[] ReadStream(Stream stream) 
{
    using (var ms = new MemoryStream()) 
    {
        stream.CopyTo(ms);
        return ms.ToArray();
    }
}

```



## The following example shows how to open a zip archive and extract all .txt files to a folder


```cs
using System;
using System.IO;
using System.IO.Compression;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            string zipPath = @"c:\example\start.zip";
            string extractPath = @"c:\example\extract";

            using (ZipArchive archive = ZipFile.OpenRead(zipPath))
            {
                foreach (ZipArchiveEntry entry in archive.Entries)
                {
                    if (entry.FullName.EndsWith(".txt", StringComparison.OrdinalIgnoreCase))
                    {
                        entry.ExtractToFile(Path.Combine(extractPath, entry.FullName));
                    }
                }
            } 
        }
    }
}

```



#### Syntax


1. public static ZipArchive OpenRead(string archiveFileName)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|archiveFileName|The path to the archive to open, specified as a relative or absolute path. A relative path is interpreted as relative to the current working directory.

