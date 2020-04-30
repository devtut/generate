---
metaTitle: "System.Management.Automation"
description: "Invoke simple synchronous pipeline"
---

# System.Management.Automation



## Invoke simple synchronous pipeline


Get the current date and time.

```cs
public class Program
{
    static void Main()
    {
        // create empty pipeline
        PowerShell ps = PowerShell.Create();

        // add command
        ps.AddCommand("Get-Date");

        // run command(s)
        Console.WriteLine("Date: {0}", ps.Invoke().First());

        Console.ReadLine();
    }
}

```

[<img src="http://i.stack.imgur.com/x2IIE.png" alt="enter image description here" />](http://i.stack.imgur.com/x2IIE.png)



#### Remarks


> 
<p>The **System.Management.Automation** namespace is the root namespace for
Windows PowerShell.</p>


[System.Management.Automation](https://www.nuget.org/packages/System.Management.Automation) is an extension library from Microsoft and it can be added to Visual Studio projects via NuGet package manager or package manager console.

[<img src="http://i.stack.imgur.com/QJlb8.png" alt="nuget-ui" />](http://i.stack.imgur.com/QJlb8.png)

```cs
PM> Install-Package System.Management.Automation

```

