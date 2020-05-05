---
metaTitle: ".NET Framework - System.Diagnostics"
description: "Stopwatch, Run shell commands, Send Command to CMD and Receive Output"
---

# System.Diagnostics



## Stopwatch


This example shows how `Stopwatch` can be used to benchmark a block of code.

```dotnet
using System;            
using System.Diagnostics;
        
public class Benchmark : IDisposable
{
    private Stopwatch sw;

    public Benchmark()
    {
        sw = Stopwatch.StartNew();
    }

    public void Dispose()
    {
        sw.Stop();
        Console.WriteLine(sw.Elapsed);
    }
}

public class Program
{
    public static void Main()
    {
        using (var bench = new Benchmark())
        {
            Console.WriteLine("Hello World");
        }
    }
}

```



## Run shell commands


```dotnet
string strCmdText = "/C copy /b Image1.jpg + Archive.rar Image2.jpg";
System.Diagnostics.Process.Start("CMD.exe",strCmdText);

```

This is to hide the cmd window.

```dotnet
System.Diagnostics.Process process = new System.Diagnostics.Process();
System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
startInfo.FileName = "cmd.exe";
startInfo.Arguments = "/C copy /b Image1.jpg + Archive.rar Image2.jpg";
process.StartInfo = startInfo;
process.Start();

```



## Send Command to CMD and Receive Output


This method allows a `command` to be sent to `Cmd.exe`, and returns the standard output (including standard error) as a string:

```dotnet
private static string SendCommand(string command)
{
    var cmdOut = string.Empty;
    
    var startInfo = new ProcessStartInfo("cmd", command)
    {
        WorkingDirectory = @"C:\Windows\System32", // Directory to make the call from
        WindowStyle = ProcessWindowStyle.Hidden,   // Hide the window
        UseShellExecute = false,                   // Do not use the OS shell to start the process
        CreateNoWindow = true,                     // Start the process in a new window 
        RedirectStandardOutput = true,             // This is required to get STDOUT
        RedirectStandardError = true               // This is required to get STDERR
    };

    var p = new Process {StartInfo = startInfo};

    p.Start();

    p.OutputDataReceived += (x, y) => cmdOut += y.Data;
    p.ErrorDataReceived += (x, y) => cmdOut += y.Data;
    p.BeginOutputReadLine();
    p.BeginErrorReadLine();
    p.WaitForExit();
    return cmdOut;
}

```

**Usage**

```dotnet
var servername = "SVR-01.domain.co.za";
var currentUsers = SendCommand($"/C QUERY USER /SERVER:{servername}")

```

**Output**

> 
string currentUsers = "USERNAME              SESSIONNAME        ID  STATE   IDLE TIME  LOGON TIME Joe.Bloggs           ica-cgp#0           2  Active  24692+13:29  25/07/2016 07:50 Jim.McFlannegan             ica-cgp#1           3  Active          .  25/07/2016 08:33 Andy.McAnderson              ica-cgp#2           4  Active          .  25/07/2016 08:54 John.Smith                 ica-cgp#4           5  Active         14  25/07/2016 08:57 Bob.Bobbington                ica-cgp#5           6  Active  24692+13:29  25/07/2016 09:05 Tim.Tom           ica-cgp#6           7  Active          .  25/07/2016 09:08 Bob.Joges       ica-cgp#7           8  Active  24692+13:29  25/07/2016 09:13"


On some occasions, access to the server in question may be restricted to certain users. If you have the login credentials for this user, then it is possible to send queries with this method:

```dotnet
private static string SendCommand(string command)
{
    var cmdOut = string.Empty;
    
    var startInfo = new ProcessStartInfo("cmd", command)
    {
        WorkingDirectory = @"C:\Windows\System32",
        WindowStyle = ProcessWindowStyle.Hidden,    // This does not actually work in conjunction with "runas" - the console window will still appear!
        UseShellExecute = false,
        CreateNoWindow = true,
        RedirectStandardOutput = true, 
        RedirectStandardError = true,

        Verb = "runas",
        Domain = "doman1.co.za",
        UserName = "administrator",
        Password = GetPassword()
    };

    var p = new Process {StartInfo = startInfo};

    p.Start();

    p.OutputDataReceived += (x, y) => cmdOut += y.Data;
    p.ErrorDataReceived += (x, y) => cmdOut += y.Data;
    p.BeginOutputReadLine();
    p.BeginErrorReadLine();
    p.WaitForExit();
    return cmdOut;
}

```

Getting the password:

```dotnet
static SecureString GetPassword()
{
    var plainText = "password123";
    var ss = new SecureString();
    foreach (char c in plainText)
    {
        ss.AppendChar(c);
    }

    return ss;
}

```

**Notes**

Both of the above methods will return a concatenation of STDOUT and STDERR, as `OutputDataReceived` and `ErrorDataReceived` are both appending to the same variable - `cmdOut`.

