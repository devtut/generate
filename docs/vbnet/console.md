---
metaTitle: "Visual Basic .NET - Console"
description: "Console.ReadLine(), Console.WriteLine(), Console.Read(), Console.ReadKey(), Prototype of command line prompt, Console.Write()"
---

# Console



## Console.ReadLine()


```vb
Dim input as String = Console.ReadLine()

```

`Console.ReadLine()` will read the console input from the user, up until the next newline is detected (usually upon pressing the Enter or Return key). Code execution is paused in the current thread until a newline is provided. Afterwards, the next line of code will be executed.



## Console.WriteLine()


```vb
Dim x As Int32 = 128
Console.WriteLine(x) ' Variable '
Console.WriteLine(3) ' Integer '
Console.WriteLine(3.14159) ' Floating-point number '
Console.WriteLine("Hello, world") ' String '
Console.WriteLine(myObject) ' Outputs the value from calling myObject.ToString()

```

The `Console.WriteLine()` method will print the given argument(s) **with** a newline attached at the end. This will print any object supplied, including, but not limited to, strings, integers, variables, floating-point numbers.

When writing objects that are not explicitly called out by the various `WriteLine` overloads (that is, you are using the overload that expects a value of type `Object`, WriteLine will use the `.ToString()` method to generate a `String` to actually write. Your custom objects should OverRide the `.ToString` method and produce something more meaningful than the default implementation (which typically just writes the fully qualified type name).



## Console.Read()


```vb
Dim inputCode As Integer = Console.Read()

```

`Console.Read()` awaits input from the user and, upon receipt, returns an integer value corresponding with the character code of the entered character. If the input stream is ended in some way before input can be obtained, -1 is returned instead.



## Console.ReadKey()


```vb
Dim inputChar As ConsoleKeyInfo = Console.ReadKey()

```

`Console.ReadKey()` awaits input from the user and, upon receipt, returns an object of class `ConsoleKeyInfo`, which holds information relevant to the character which the user provided as input. For detail regarding the information provided, visit the [MSDN documentation](https://msdn.microsoft.com/en-us/library/system.consolekeyinfo.aspx).



## Prototype of command line prompt


```vb
Module MainPrompt
Public Const PromptSymbol As String = "TLA > "
Public Const ApplicationTitle As String = GetType(Project.BaseClass).Assembly.FullName
REM Or you can use a custom string
REM Public Const ApplicationTitle As String = "Short name of the application"

Sub Main()
    Dim Statement As String
    Dim BrokenDownStatement As String()
    Dim Command As String
    Dim Args As String()
    Dim Result As String

    Console.ForegroundColor = ConsoleColor.Cyan
    Console.Title = ApplicationTitle & " command line console"

    Console.WriteLine("Welcome to " & ApplicationTitle & "console frontend")
    Console.WriteLine("This package is version " & GetType(Project.BaseClass).Assembly.GetName().Version.ToString)
    Console.WriteLine()
    Console.Write(PromptSymbol)

    Do While True
        Statement = Console.ReadLine()
        BrokenDownStatement = Statement.Split(" ")
        ReDim Args(BrokenDownStatement.Length - 1)
        Command = BrokenDownStatement(0)

        For i = 1 To BrokenDownStatement.Length - 1
            Args(i - 1) = BrokenDownStatement(i)
        Next

        Select Case Command.ToLower
            Case "example"
                Result = DoSomething(Example)
            Case "exit", "quit"
                Exit Do
            Case "ver"
                Result = "This package is version " & GetType(Project.BaseClass).Assembly.GetName().Version.ToString
            Case Else
                Result = "Command not acknowldged: -" & Command & "-"
        End Select
        Console.WriteLine(" " & Result)
        Console.Write(PromptSymbol)
    Loop

    Console.WriteLine("I am exiting, time is " & DateTime.Now.ToString("u"))
    Console.WriteLine("Goodbye")
    Environment.Exit(0)
End Sub
End Module

```

This prototype generate a basic command line interpreter.

It automatically get the application name and version to communicate to the user.
For each input line, it recognize the command and an arbitrary list of arguments, all separated by space.

As a basic example, this code understand **ver**, **quit** and **exit** commands.

The parameter **Project.BaseClass** is a class of your project where the Assembly details are set.



## Console.Write()


```vb
Dim x As Int32 = 128
Console.Write(x) ' Variable '
Console.Write(3) ' Integer '
Console.Write(3.14159) ' Floating-point number '
Console.Write("Hello, world") ' String '

```

The `Console.Write()` method is identical to the `Console.WriteLine()` method except that it prints the given argument(s) **without** a newline attached at the end. This method can be made functionally identical to `WriteLine` by adding a newline string to the end of any arguments provided:

```vb
Console.Write("this is the value" & Environment.NewLine)

```

