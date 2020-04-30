---
metaTitle: "Getting started with C# Language"
description: "Creating a new console application (Visual Studio), Creating a new project in Visual Studio (console application) and Running it in Debug mode, Creating a new program using Mono, Creating a new program using .NET Core, Creating a new query using LinqPad, Creating a new project using Xamarin Studio"
---

# Getting started with C# Language



## Creating a new console application (Visual Studio)


1. Open Visual Studio
1. In the toolbar, go to **File** → **New Project**
1. Select the **Console Application** project type
1. Open the file `Program.cs` in the Solution Explorer
1. Add the following code to `Main()`:

```cs
public class Program
{
    public static void Main()
    {
        // Prints a message to the console.
        System.Console.WriteLine("Hello, World!");

        /* Wait for the user to press a key. This is a common
           way to prevent the console window from terminating
           and disappearing before the programmer can see the contents
           of the window, when the application is run via Start from within VS. */
        System.Console.ReadKey();
    }
}

```


1. In the toolbar, click **Debug** -> **Start Debugging** or hit **F5** or **ctrl + F5** (running without debugger) to run the program.

[Live Demo on ideone](https://ideone.com/3OhmnG)

### Explanation

<li>
`class Program` is a class declaration. The class `Program` contains the data and method definitions that your program uses. Classes generally contain multiple methods. Methods define the behavior of the class. However, the `Program` class has only one method: `Main`.
</li>
<li>
`static void Main()` defines the `Main` method, which is the entry point for all C# programs. The `Main` method states what the class does when executed. Only one `Main` method is allowed per class.
</li>
<li>
`System.Console.WriteLine("Hello, world!");` method prints a given data (in this example, `Hello, world!`) as an output in the console window.
</li>
<li>
`System.Console.ReadKey()`, ensures that the program won't close immediately after displaying the message. It does this by waiting for the user to press a key on the keyboard. Any key press from the user will terminate the program. The program terminates when it has finished the last line of code in the `main()` method.
</li>

### Using the command line

To compile via command line use either `MSBuild` or `csc.exe` **(the C# compiler)**, both part of the [Microsoft Build Tools](https://www.visualstudio.com/downloads/download-visual-studio-vs#d-build-tools) package.

To compile this example, run the following command in the same directory where `HelloWorld.cs` is located:

```cs
%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs

```

It can also be possible that you have two main methods inside one application. In this case, you have to tell the compiler which main method to execute by typing the following command in the **console**.(suppose Class `ClassA` also has a main method in the same `HelloWorld.cs` file in HelloWorld namespace)

```cs
%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\csc.exe HelloWorld.cs /main:HelloWorld.ClassA 

```

where HelloWorld is namespace

****Note**: This is the path where **.NET framework v4.0** is located in general. Change the path according to your .NET version. In addition, the directory might be **framework** instead of **framework64** if you're using the 32-bit .NET Framework.  From the Windows Command Prompt, you can list all the csc.exe Framework paths by running the following commands (the first for 32-bit Frameworks):**

```cs
dir %WINDIR%\Microsoft.NET\Framework\csc.exe /s/b
dir %WINDIR%\Microsoft.NET\Framework64\csc.exe /s/b

```

<img src="https://i.stack.imgur.com/xT8kk.png" alt="Compiling the .cs file" />

There should now be an executable file named `HelloWorld.exe` in the same directory. To execute the program from the command prompt, simply type the executable's name and hit <kbd>Enter</kbd> as follows:

```cs
HelloWorld.exe

```

This will produce:

> 
Hello, world!


<img src="https://i.stack.imgur.com/x0Fek.png" alt="Executing the exe file in the console" />

You may also double click the executable and launch a new console window with the message "**Hello, world!**"

<img src="https://i.stack.imgur.com/qstu1.png" alt="Running the executable and using double click" />



## Creating a new project in Visual Studio (console application) and Running it in Debug mode


<li>
**Download and install [Visual Studio](https://www.visualstudio.com/products/vs-2015-product-editions)**. Visual Studio can be downloaded from [VisualStudio.com](http://www.visualstudio.com). The Community edition is suggested, first because it is free, and second because it involves all the general features and can be extended further.
</li>
<li>
**Open Visual Studio.**
</li>
<li>
<p>**Welcome.** Go to **File  → <strong>New**  → Project</strong>.
[<img src="https://i.stack.imgur.com/fpvTX.png" alt="Microsoft Visual Studio - File Menu" />](https://i.stack.imgur.com/fpvTX.png)</p>
</li>
<li>
Click **Templates** → **Visual C#** → **Console Application**
[<img src="https://i.stack.imgur.com/kKGls.png" alt="Microsoft Visual Studio - New Project window" />](https://i.stack.imgur.com/kKGls.png)
</li>
<li>
**After selecting Console Application,** Enter a name for your project, and a location to save and press <kbd>OK</kbd>. Don't worry about the Solution name.
</li>
<li>
**Project created**. The newly created project will look similar to:
[<img src="https://i.stack.imgur.com/WVkeF.png" alt="Microsoft Visual Studio - c# Default Project" />](https://i.stack.imgur.com/WVkeF.png)
**(Always use descriptive names for projects so that they can easily be distinguished from other projects.  It is recommended not to use spaces in project or class name.)**
</li>
<li>
**Write code.** You can now update your `Program.cs` to present "Hello world!" to the user.

```cs
using System;

namespace ConsoleApplication1
{
    public class Program
    {
        public static void Main(string[] args)
        {
        }
    }
}

```


Add the following two lines to the `public static void Main(string[] args)` object in `Program.cs`: (make sure it's inside the braces)

```cs
Console.WriteLine("Hello world!");
Console.Read();

```


**Why** `Console.Read()`**?** The first line prints out the text "Hello world!" to the console, and the second line waits for a single character to be entered; in effect, this causes the program to pause execution so that you're able to see the output while debugging.  Without `Console.Read();`, when you start debugging the application it will just print "Hello world!" to the console and then immediately close.  Your code window should now look like the following:

```cs
using System;

namespace ConsoleApplication1
{
    public class Program
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("Hello world!");
            Console.Read();
        }
    }
}

```


</li>
<li>
**Debug your program.** Press the Start Button on the toolbar near the top of the window [<img src="https://i.stack.imgur.com/odDu6.png" alt="Start Debugging Button" />](https://i.stack.imgur.com/odDu6.png) or press <kbd>F5</kbd> on your keyboard to run your application. If the button is not present, you can run the program from the top menu: **Debug → Start Debugging**. The program will compile and then open a console window. It should look similar to the following screenshot:
</li>

[<img src="https://i.stack.imgur.com/ZD5MF.png" alt="Console running the Hello World application" />](https://i.stack.imgur.com/ZD5MF.png)

1. **Stop the program.** To close the program, just press any key on your keyboard. The `Console.Read()` we added was for this same purpose. Another way to close the program is by going to the menu where the <kbd>Start</kbd> button was, and clicking on the <kbd>Stop</kbd> button.



## Creating a new program using Mono


First install [Mono](http://www.mono-project.com/) by going through the install instructions for the platform of your choice as described in their [installation section](http://www.mono-project.com/docs/getting-started/install/).

Mono is available for Mac OS X, Windows and Linux.

After installation is done, create a text file, name it `HelloWorld.cs` and copy the following content into it:

```cs
public class Program
{
    public static void Main()
    {
        System.Console.WriteLine("Hello, world!");
        System.Console.WriteLine("Press any key to exit..");
        System.Console.Read();
    }
}

```

If you are using Windows, run the Mono Command Prompt which is included in the Mono installation and ensures that the necessary environment variables are set. If on Mac or Linux, open a new terminal.

To compile the newly created file, run the following command in the directory containing `HelloWorld.cs`:

```cs
mcs -out:HelloWorld.exe HelloWorld.cs

```

The resulting `HelloWorld.exe` can then be executed with:

```cs
mono HelloWorld.exe

```

which will produce the output:

```cs
Hello, world!   
Press any key to exit..

```



## Creating a new program using .NET Core


First install the [**.NET Core SDK**](https://docs.microsoft.com/en-us/dotnet/articles/core/) by going through the installation instructions for the platform of your choice:

- [Windows](https://www.microsoft.com/net/core#windows)
- [OSX](https://www.microsoft.com/net/core#macos)
- [Linux](https://www.microsoft.com/net/core#linuxubuntu)
- [Docker](https://www.microsoft.com/net/core#dockercmd)

After the installation has completed, open a command prompt, or terminal window.

<li>
Create a new directory with `mkdir hello_world` and change into the newly created directory with `cd hello_world`.
</li>
<li>
<p>Create a new console application with `dotnet new console`.<br />
This will produce two files:</p>
<ul>
<li>
**hello_world.csproj**

```cs
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>netcoreapp1.1</TargetFramework>
  </PropertyGroup>

</Project>

```


</li>
<li>
**Program.cs**

```cs
using System;

namespace hello_world
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }
}

```


</li>
</ul>
</li>
<li>
Restore the needed packages with `dotnet restore`.
</li>
<li>
**Optional** Build the application with `dotnet build` for Debug or `dotnet build -c Release` for Release. `dotnet run` will also run the compiler and throw build errors, if any are found.
</li>
<li>
Run the application with `dotnet run` for Debug or `dotnet run .\bin\Release\netcoreapp1.1\hello_world.dll` for Release.
</li>

### Command Prompt output

[<img src="https://i.stack.imgur.com/arqCl.png" alt="enter image description here" />](https://i.stack.imgur.com/arqCl.png)



## Creating a new query using LinqPad


LinqPad is a great tool that allows you to learn and test features of .Net languages (C#, F# and VB.Net.)

<li>
Install [LinqPad](http://www.linqpad.net/)
</li>
<li>
<p>Create a new Query (<kbd>Ctrl</kbd> + <kbd>N</kbd>)
[<img src="http://i.stack.imgur.com/D0tSi.png" alt="enter image description here" />](http://i.stack.imgur.com/D0tSi.png)</p>
</li>
<li>
<p>Under language, select "C# statements"
[<img src="http://i.stack.imgur.com/kC5Ur.jpg" alt="enter image description here" />](http://i.stack.imgur.com/kC5Ur.jpg)</p>
</li>
<li>
Type the following code and hit run (<kbd>F5</kbd>)

```cs
string hw = "Hello World";

hw.Dump(); //or Console.WriteLine(hw);

```


</li>

[<img src="http://i.stack.imgur.com/LO4kD.jpg" alt="enter image description here" />](http://i.stack.imgur.com/LO4kD.jpg)

<li>You should see "Hello World" printed out in the results screen.
[<img src="http://i.stack.imgur.com/GzsrS.jpg" alt="enter image description here" />](http://i.stack.imgur.com/GzsrS.jpg)</li>
<li>Now that you have created your first .Net program, go and check out the samples included in LinqPad via the "Samples" browser. There are many great examples that will show you many different features of the .Net languages.
[<img src="http://i.stack.imgur.com/yucuf.jpg" alt="enter image description here" />](http://i.stack.imgur.com/yucuf.jpg)</li>

**Notes:**

<li>If you click on "IL", you can inspect the IL code that your .net code generates. This is a great learning tool.
[<img src="http://i.stack.imgur.com/XPumO.jpg" alt="enter image description here" />](http://i.stack.imgur.com/XPumO.jpg)</li>
1. When using `LINQ to SQL` or `Linq to Entities` you can inspect the SQL that's being generated which is another great way to learn about LINQ.



## Creating a new project using Xamarin Studio


1. Download and install [Xamarin Studio Community](https://store.xamarin.com/).
1. Open Xamarin Studio.
1. Click **File** → **New** → **Solution**.

[<img src="http://i.stack.imgur.com/hHjMM.png" alt="Creating New Project in Xamarin Studio" />](http://i.stack.imgur.com/hHjMM.png)

1. Click **.NET** → **Console Project** and choose **C#**.
1. Click <kbd>Next</kbd> to proceed.

[<img src="http://i.stack.imgur.com/s58Ju.png" alt="Choosing Template for new project" />](http://i.stack.imgur.com/s58Ju.png)

1. Enter the **Project Name** and <kbd>Browse...</kbd> for a **Location** to Save and then click <kbd>Create</kbd>.

[<img src="http://i.stack.imgur.com/lrK8L.png" alt="Project name and location" />](http://i.stack.imgur.com/lrK8L.png)

1. The newly created project will look similar to:

[<img src="http://i.stack.imgur.com/vva82.png" alt="enter image description here" />](http://i.stack.imgur.com/vva82.png)

1. This is the code in the Text Editor:

```cs
using System;

namespace FirstCsharp
{
    public class MainClass
    {
        public static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
            Console.ReadLine();
        }
    }
}

```


1. To run the code, press <kbd>F5</kbd> or click the **Play Button** as shown below:

[<img src="http://i.stack.imgur.com/6q4ZN.png" alt="Run the code" />](http://i.stack.imgur.com/6q4ZN.png)

1. Following is the Output:

[<img src="http://i.stack.imgur.com/cqBsK.png" alt="output" />](http://i.stack.imgur.com/cqBsK.png)



#### Remarks


C# is a multi-paradigm, C-descendant programming language from Microsoft. C# is a managed language that compiles to [CIL](https://en.wikipedia.org/wiki/Common_Intermediate_Language), intermediate bytecode which can be executed on Windows, Mac OS X and Linux.

Versions 1.0, 2.0 and 5.0 were standardized by ECMA (as [ECMA-334](http://www.ecma-international.org/publications/standards/Ecma-334.htm)), and standardization efforts for modern C# are underway.

