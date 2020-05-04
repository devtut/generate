---
metaTitle: "Getting started with .NET Framework"
description: "Hello World in C#, Hello World in F#, Hello World in Visual Basic .NET, Hello World in C++/CLI, Hello World in IL, Hello World in PowerShell, Hello World in Nemerle, Hello World in Oxygene, Hello World in Boo, Hello World in Python (IronPython)"
---

# Getting started with .NET Framework



## Hello World in C#


```dotnet
using System;

class Program
{
    // The Main() function is the first function to be executed in a program
    static void Main()
    {
        // Write the string "Hello World to the standard out
        Console.WriteLine("Hello World");
    }
}

```

`Console.WriteLine` has several overloads. In this case, the string "Hello World" is the parameter, and it will output the "Hello World" to the standard out stream during execution. Other overloads may call the `.ToString` of the argument before writing to the stream.   See the [.NET Framework Documentation](https://msdn.microsoft.com/en-us/library/system.console.writeline) for more information.

[Live Demo in Action at .NET Fiddle](https://dotnetfiddle.net/S7hjxp)

[Introduction to C#](https://stackoverflow.com/documentation/c%23/15/compile-and-run-your-first-c-sharp-program)



## Hello World in F#


```dotnet
open System

[<EntryPoint>]
let main argv = 
    printfn "Hello World" 
    0 

```

[Live Demo in Action at .NET Fiddle](https://dotnetfiddle.net/hDvqwC)

[Introduction to F#](http://stackoverflow.com/documentation/f%23/817/introduction-to-f)



## Hello World in Visual Basic .NET


```dotnet
Imports System

Module Program
    Public Sub Main()
        Console.WriteLine("Hello World")
    End Sub
End Module

```

[Live Demo in Action at .NET Fiddle](https://dotnetfiddle.net/dRDZVe)

[Introduction to Visual Basic .NET](https://stackoverflow.com/documentation/vb.net/352/hello-world)



## Hello World in C++/CLI


```dotnet
using namespace System;

int main(array<String^>^ args)
{
    Console::WriteLine("Hello World");
}

```



## Hello World in IL


```dotnet
.class public auto ansi beforefieldinit Program
       extends [mscorlib]System.Object
{
  .method public hidebysig static void  Main() cil managed
  { 
    .maxstack  8
    IL_0000:  nop
    IL_0001:  ldstr      "Hello World"
    IL_0006:  call       void [mscorlib]System.Console::WriteLine(string)
    IL_000b:  nop
    IL_000c:  ret
  }

  .method public hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    .maxstack  8
    IL_0000:  ldarg.0
    IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
    IL_0006:  ret
  }

}

```



## Hello World in PowerShell


```dotnet
Write-Host "Hello World"

```

[Introduction to PowerShell](https://stackoverflow.com/documentation/powershell/822/introduction-to-powershell)



## Hello World in Nemerle


```dotnet
System.Console.WriteLine("Hello World");

```



## Hello World in Oxygene


```dotnet
namespace HelloWorld;

interface

type
  App = class
  public
    class method Main(args: array of String);
  end;

implementation

class method App.Main(args: array of String);
begin
  Console.WriteLine('Hello World');
end;

end.

```



## Hello World in Boo


```dotnet
print "Hello World"

```



## Hello World in Python (IronPython)


```dotnet
print "Hello World"

```

```dotnet
import clr
from System import Console
Console.WriteLine("Hello World")

```



#### Remarks


The .NET Framework is a set of libraries and a runtime, originally designed by Microsoft. All .NET programs compile to a bytecode called Microsoft Intermediate Language (MSIL). The MSIL is run by the Common Language Runtime (CLR).

Below you can find several examples of "Hello World" in various languages that support the .NET Framework. "Hello World" is a program that displays "Hello World" on the display device. It's used for illustrating the basic syntax for constructing a working program. It can also be used as a sanity test to make sure that a language's compiler, development environment, and runtime environment are all working correctly.

[List of languages supported by .NET](https://en.wikipedia.org/wiki/List_of_CLI_languages)

