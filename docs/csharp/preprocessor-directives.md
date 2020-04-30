---
metaTitle: "Preprocessor directives"
description: "Conditional Expressions, Other Compiler Instructions, Defining and Undefining Symbols, Region Blocks, Disabling and Restoring Compiler Warnings, Generating Compiler Warnings and Errors, Using the Conditional attribute, Custom Preprocessors at project level"
---

# Preprocessor directives



## Conditional Expressions


When the following is compiled, it will return a different value depending on which directives are defined.

```cs
// Compile with /d:A or /d:B to see the difference
string SomeFunction() 
{
#if A
    return "A";
#elif B
    return "B";
#else
    return "C";
#endif
}

```

Conditional expressions are typically used to log additional information for debug builds.

```cs
void SomeFunc()
{
    try
    {
        SomeRiskyMethod();
    }
    catch (ArgumentException ex)
    {
        #if DEBUG
        log.Error("SomeFunc", ex);
        #endif

        HandleException(ex);
    }
}

```



## Other Compiler Instructions


### Line

`#line` controls the line number and filename reported by the compiler when outputting warnings and errors.

```cs
void Test()
{
    #line 42 "Answer"
    #line filename "SomeFile.cs"
    int life; // compiler warning CS0168 in "SomeFile.cs" at Line 42
    #line default
    // compiler warnings reset to default
}

```

### Pragma Checksum

`#pragma checksum` allows the specification of a specific checksum for a generated program database (PDB) for debugging.

```cs
#pragma checksum "MyCode.cs" "{00000000-0000-0000-0000-000000000000}" "{0123456789A}"

```



## Defining and Undefining Symbols


A compiler symbol is a keyword that is defined at compile-time that can be checked for to conditionally execute specific sections of code.

There are three ways to define a compiler symbol. They can be defined via code:

```cs
#define MYSYMBOL

```

They can be defined in Visual Studio, under Project Properties > Build > Conditional Compilation Symbols:

<img src="http://i.imgur.com/PHG04dI.png" alt="VS Compiler Symbols" />

**(Note that `DEBUG` and `TRACE` have their own checkboxes and do not need to be specified explicitly.)**

Or they can be defined at compile-time using the `/define:[name]` switch on the C# compiler, `csc.exe`.

You can also undefined symbols using the `#undefine` directive.

The most prevalent example of this is the `DEBUG` symbol, which gets defined by Visual Studio when an application is compiled in Debug mode (versus Release mode).

```cs
public void DoBusinessLogic()
{
    try
    {
        AuthenticateUser();
        LoadAccount();
        ProcessAccount();
        FinalizeTransaction();
    }
    catch (Exception ex)
    {
#if DEBUG
        System.Diagnostics.Trace.WriteLine("Unhandled exception!");
        System.Diagnostics.Trace.WriteLine(ex);
        throw;
#else
        LoggingFramework.LogError(ex);
        DisplayFriendlyErrorMessage();
#endif
    }
}

```

In the example above, when an error occurs in the business logic of the application, if the application is compiled in Debug mode (and the `DEBUG` symbol is set), the error will be written to the trace log, and the exception will be re-thrown for debugging. However, if the application is compiled in Release mode (and no `DEBUG` symbol is set), a logging framework is used to quietly log the error, and a friendly error message is displayed to the end user.



## Region Blocks


Use `#region` and `#endregion` to define a collapsible code region.

```cs
#region Event Handlers

public void Button_Click(object s, EventArgs e)
{
    // ...
}

public void DropDown_SelectedIndexChanged(object s, EventArgs e)
{
    // ...
}

#endregion

```

These directives are only beneficial when an IDE that supports collapsible regions (such as [Visual Studio](https://www.visualstudio.com/en-us/visual-studio-homepage-vs.aspx)) is used to edit the code.



## Disabling and Restoring Compiler Warnings


You can disable compiler warnings using `#pragma warning disable` and restore them using `#pragma warning restore`:

```cs
#pragma warning disable CS0168

// Will not generate the "unused variable" compiler warning since it was disabled
var x = 5;

#pragma warning restore CS0168

// Will generate a compiler warning since the warning was just restored
var y = 8;

```

Comma-separated warning numbers are allowed:

```cs
#pragma warning disable CS0168, CS0219

```

The `CS` prefix is optional, and can even be intermixed (though this is not a best practice):

```cs
#pragma warning disable 0168, 0219, CS0414

```



## Generating Compiler Warnings and Errors


Compiler warnings can be generated using the `#warning` directive, and errors can likewise be generated using the `#error` directive.

```cs
#if SOME_SYMBOL
#error This is a compiler Error.
#elif SOME_OTHER_SYMBOL
#warning This is a compiler Warning.
#endif

```



## Using the Conditional attribute


Adding a `Conditional` attribute from `System.Diagnostics` namespace to a method is a clean way to control which methods are called in your builds and which are not.

```cs
#define EXAMPLE_A

using System.Diagnostics;
class Program
{
    static void Main()
    {
        ExampleA(); // This method will be called
        ExampleB(); // This method will not be called
    }

    [Conditional("EXAMPLE_A")]
    static void ExampleA() {...}

    [Conditional("EXAMPLE_B")]
    static void ExampleB() {...}
}

```



## Custom Preprocessors at project level


It is convenient to set custom conditional preprocessing at project level when some actions need to be skipped lets say for tests.

Go to `Solution Explorer` -> Click <kbd>Right Mouse</kbd> on project you want to set variable to -> `Properties` -> `Build` -> In General find field `Conditional compilation symbols` and enter your conditional variable here

[<img src="http://i.stack.imgur.com/B2pi1.png" alt="enter image description here" />](http://i.stack.imgur.com/B2pi1.png)

Code example that will skip some code:

```cs
public void Init()
{
    #if !IGNOREREFRESHDB
    // will skip code here
     db.Initialize();
    #endif
}

```



#### Syntax


- #define **[symbol]** // Defines a compiler symbol.
- #undef **[symbol]** // Undefines a compiler symbol.
- #warning **[warning message]** // Generates a compiler warning. Useful with #if.
- #error **[error message]** // Generates a compiler error. Useful with #if.
- #line **[line number] (file name)** // Overrides the compiler line number (and optionally source file name). Used with [T4 text templates](https://msdn.microsoft.com/en-us/library/bb126445.aspx).
- #pragma warning [disable|restore] **[warning numbers]** // Disables/restores compiler warnings.
- #pragma checksum "**[filename]**" "**[guid]**" "**[checksum]**" // Validates a source file's contents.
- #region **[region name]** // Defines a collapsible code region.
- #endregion // Ends a code region block.
- #if **[condition]** // Executes the code below if the condition is true.
- #else // Used after an #if.
- #elif **[condition]** // Used after an #if.
- #endif // Ends a conditional block started with #if.



#### Remarks


Preprocessor directives are typically used to make source programs easy to change and easy to compile in different execution environments. Directives in the source file tell the preprocessor to perform specific actions. For example, the preprocessor can replace tokens in the text, insert the contents of other files into the source file, or suppress compilation of part of the file by removing sections of text. Preprocessor lines are recognized and carried out before macro expansion. Therefore, if a macro expands into something that looks like a preprocessor command, that command is not recognized by the preprocessor.

Preprocessor statements use the same character set as source file statements, with the exception that escape sequences are not supported. The character set used in preprocessor statements is the same as the execution character set. The preprocessor also recognizes negative character values.

### Conditional Expressions

Conditional expressions (`#if`, `#elif`, etc) do support a limited subset of boolean operators. They are:

- `==` and `!=`. These can only be used for testing whether the symbol is true (defined) or false (not defined)
- `&&`, `||`, `!`
- `()`

For example:

```cs
#if !DEBUG && (SOME_SYMBOL || SOME_OTHER_SYMBOL) && RELEASE == true
Console.WriteLine("OK!");
#endif

```

would compile code that prints "OK!" to the console if `DEBUG` is not defined, either `SOME_SYMBOL` or `SOME_OTHER_SYMBOL` is defined, and `RELEASE` is defined.

Note: These substitutions are done **at compile time** and are therefore not available for inspection at run time. Code eliminated through use of `#if` is not part of the compiler's output.

See Also: [C# Preprocessor Directives](https://msdn.microsoft.com/en-us/library/ed8yd1ha.aspx) at MSDN.

