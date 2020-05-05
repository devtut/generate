---
metaTitle: "C# | Comments and regions"
description: "Comments, Regions, Documentation comments"
---

# Comments and regions



## Comments


Using comments in your projects is a handy way of leaving explanations of your design choices, and should aim to make your (or someone else's) life easier when maintaining or adding to the code.

There are a two ways of adding a comment to your code.

### Single line comments

Any text placed after `//` will be treated as a comment.

```cs
public class Program
{
    // This is the entry point of my program.
    public static void Main()
    {
        // Prints a message to the console. - This is a comment!
        System.Console.WriteLine("Hello, World!"); 

        // System.Console.WriteLine("Hello, World again!"); // You can even comment out code.
        System.Console.ReadLine();
    }
}

```

### Multi line or delimited comments

Any text between `/*` and `*/` will be treated as a comment.

```cs
public class Program
{
    public static void Main()
    {
        /*
            This is a multi line comment
            it will be ignored by the compiler.
        */
        System.Console.WriteLine("Hello, World!");

        // It's also possible to make an inline comment with /* */
        // although it's rarely used in practice
        System.Console.WriteLine(/* Inline comment */ "Hello, World!");
  
        System.Console.ReadLine();
    }
}

```



## Regions


A region is a collapsible block of code, that can help with the readability and organisation of your code.

**NOTE:** StyleCop's rule SA1124 DoNotUseRegions discourages use of regions. They are usually a sign of badly organized code, as C# includes partial classes and other features which make regions obsolete.

You can use regions in the following way:

```cs
class Program
{
    #region Application entry point
    static void Main(string[] args)
    {
        PrintHelloWorld();
        System.Console.ReadLine();
    }
    #endregion

    #region My method
    private static void PrintHelloWorld()
    {
        System.Console.WriteLine("Hello, World!");
    }
    #endregion
}

```

When the above code is view in an IDE, you will be able to collapse and expand the code using the + and - symbols.

**Expanded**

[<img src="http://i.stack.imgur.com/zYxwK.png" alt="The above code in Visual Studio" />](http://i.stack.imgur.com/zYxwK.png)

**Collapsed**

[<img src="http://i.stack.imgur.com/T4rl5.png" alt="The above code in Visual Studio Collapsed using regions" />](http://i.stack.imgur.com/T4rl5.png)



## Documentation comments


XML documentation comments can be used to provide API documentation that can be easily processed by tools:

```cs
/// <summary>
/// A helper class for validating method arguments.
/// </summary>
public static class Precondition
{
    /// <summary>
    ///     Throws an <see cref="ArgumentOutOfRangeException"/> with the parameter
    ///     name set to <c>paramName</c> if <c>value</c> does not satisfy the 
    ///     <c>predicate</c> specified.
    /// </summary>
    /// <typeparam name="T">
    ///     The type of the argument checked
    /// </typeparam>
    /// <param name="value">
    ///     The argument to be checked
    /// </param>
    /// <param name="predicate">
    ///     The predicate the value is required to satisfy
    /// </param>
    /// <param name="paramName">
    ///     The parameter name to be passed to the
    ///     <see cref="ArgumentOutOfRangeException"/>.
    /// </param>
    /// <returns>The value specified</returns>
    public static T Satisfies<T>(T value, Func<T, bool> predicate, string paramName)
    {
        if (!predicate(value))
            throw new ArgumentOutOfRangeException(paramName);

        return value;
    }
}

```

Documentation is instantly picked up by IntelliSense:

[<img src="https://i.stack.imgur.com/cfvnh.png" alt="IntelliSense displaying method documentation" />](https://i.stack.imgur.com/cfvnh.png)

