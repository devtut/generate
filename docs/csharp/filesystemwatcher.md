---
metaTitle: "C# | FileSystemWatcher"
description: "IsFileReady, Basic FileWatcher "
---

# FileSystemWatcher



## IsFileReady


A common mistake a lot of people starting out with FileSystemWatcher does is not taking into account That the FileWatcher event is raised as soon as the file is created.
However, it may take some time for the file to be finished .

**Example**:

Take a file size of 1 GB for example . The file apr ask created by another program (Explorer.exe copying it from somewhere) but it will take minutes to finish that process. The event is raised that creation time and you need to wait for the file to be ready to be copied.

This is a method for checking if the file is ready.

```cs

public static bool IsFileReady(String sFilename)
{
    // If the file can be opened for exclusive access it means that the file
    // is no longer locked by another process.
    try
    {
        using (FileStream inputStream = File.Open(sFilename, FileMode.Open, FileAccess.Read, FileShare.None))
        {
            if (inputStream.Length > 0)
            {
                return true;
            }
            else
            {
                return false;
            }

        }
    }
    catch (Exception)
    {
        return false;
    }
}

```



## Basic FileWatcher 


The following example creates a `FileSystemWatcher` to watch the directory specified at run time. The component is set to watch for changes in **LastWrite** and **LastAccess** time, the creation, deletion, or renaming of text files in the directory. If a file is changed, created, or deleted, the path to the file prints to the console. When a file is renamed, the old and new paths print to the console.

Use the System.Diagnostics and System.IO namespaces for this example.

```cs
FileSystemWatcher watcher;

private void watch()
{
  // Create a new FileSystemWatcher and set its properties.
  watcher = new FileSystemWatcher();
  watcher.Path = path;

 /* Watch for changes in LastAccess and LastWrite times, and
       the renaming of files or directories. */
  watcher.NotifyFilter = NotifyFilters.LastAccess | NotifyFilters.LastWrite
                         | NotifyFilters.FileName | NotifyFilters.DirectoryName;

  // Only watch text files.      
  watcher.Filter = "*.txt*";

  // Add event handler.
  watcher.Changed += new FileSystemEventHandler(OnChanged);
  // Begin watching.      
  watcher.EnableRaisingEvents = true;
}

// Define the event handler.
private void OnChanged(object source, FileSystemEventArgs e)
{
  //Copies file to another directory or another action.
  Console.WriteLine("File: " +  e.FullPath + " " + e.ChangeType);
}

```



#### Syntax


- public FileSystemWatcher()
- public FileSystemWatcher(string path)
- public FileSystemWatcher(string path, string filter)



#### Parameters


|path|filter
|---|---|---|---|---|---|---|---|---|---
|The directory to monitor, in standard or Universal Naming Convention (UNC) notation.|The type of files to watch. For example, "*.txt" watches for changes to all text files.

