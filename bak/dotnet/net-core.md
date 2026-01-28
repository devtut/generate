---
metaTitle: ".NET Framework - .NET Core"
description: "Basic Console App"
---

# .NET Core


.NET Core is a general purpose development platform maintained by Microsoft and the .NET community on GitHub. It is cross-platform, supporting Windows, macOS and Linux, and can be used in device, cloud, and embedded/IoT scenarios.

When you think of .NET Core the following should come to mind (flexible deployment, cross-platform, command-line tools, open source).

Another great thing is that even if it's open source Microsoft is actively supporting it.



## Basic Console App


```dotnet
public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("\nWhat is your name? ");
        var name = Console.ReadLine();
        var date = DateTime.Now;
        Console.WriteLine("\nHello, {0}, on {1:d} at {1:t}", name, date);
        Console.Write("\nPress any key to exit...");
        Console.ReadKey(true);
    }
}

```



#### Remarks


By itself, .NET Core includes a single application model -- console apps -- which is useful for tools, local services and text-based games. Additional application models have been built on top of .NET Core to extend its functionality, such as:

- ASP.NET Core
- Windows 10 Universal Windows Platform (UWP)
- Xamarin.Forms

Also, .NET Core implements the .NET Standard Library, and therefore supports .NET Standard Libraries.

The .NET Standard Library is an API spec that describes the consistent set of .NET APIs that developers can expect in each .NET implementation. .NET implementations need to implement this spec in order to be considered .NET Standard Library compliant and to support libraries that target the .NET Standard Library.

