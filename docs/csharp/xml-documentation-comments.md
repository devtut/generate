---
metaTitle: "XML Documentation Comments"
description: "Simple method annotation, Generating XML from documentation comments, Method documentation comment with param and returns elements, Interface and class documentation comments, Referencing another class in documentation"
---

# XML Documentation Comments



## Simple method annotation


Documentation comments are placed directly above the method or class they describe. They begin with three forward slashes `///`, and allow meta information to be stored via XML.

```cs
/// <summary>
/// Bar method description
/// </summary>
public void Bar()
{ 
        
}

```

Information inside the tags can be used by Visual Studio and other tools to provide services such as IntelliSense:

[<img src="https://i.stack.imgur.com/NDAnP.png" alt="Method xml annotation example" />](https://i.stack.imgur.com/NDAnP.png)

See also [Microsoft's list of common documentation tags](https://msdn.microsoft.com/en-us/library/5ast78ax.aspx).



## Generating XML from documentation comments


To generate an XML documentation file from documentation comments in the code, use the `/doc` option with the `csc.exe` C# compiler.

In Visual Studio 2013/2015, In **Project** -> **Properties** -> **Build** -> **Output**, check the `XML documentation file` checkbox:

[<img src="https://i.stack.imgur.com/tXXQy.png" alt="XML documentation file" />](https://i.stack.imgur.com/tXXQy.png)

When you build the project, an XML file will be produced by the compiler with a name corresponding to the project name (e.g. `XMLDocumentation.dll` -> `XMLDocumentation.xml`).

When you use the assembly in another project, make sure that the XML file is in the same directory as the DLL being referenced.

This example:

```cs
/// <summary>
/// Data class description
/// </summary>
public class DataClass
{
    /// <summary>
    /// Name property description
    /// </summary>
    public string Name { get; set; }
}


/// <summary>
/// Foo function
/// </summary>
public class Foo
{
    /// <summary>
    /// This method returning some data
    /// </summary>
    /// <param name="id">Id parameter</param>
    /// <param name="time">Time parameter</param>
    /// <returns>Data will be returned</returns>
    public DataClass GetData(int id, DateTime time)
    {
        return new DataClass();
    }
}

```

Produces this xml on build:

```cs
<?xml version="1.0"?>
<doc>
    <assembly>
        <name>XMLDocumentation</name>
    </assembly>
    <members>
        <member name="T:XMLDocumentation.DataClass">
            <summary>
            Data class description
            </summary>
        </member>
        <member name="P:XMLDocumentation.DataClass.Name">
            <summary>
            Name property description
            </summary>
        </member>
        <member name="T:XMLDocumentation.Foo">
            <summary>
            Foo function
            </summary>
        </member>
        <member name="M:XMLDocumentation.Foo.GetData(System.Int32,System.DateTime)">
            <summary>
            This method returning some data
            </summary>
            <param name="id">Id parameter</param>
            <param name="time">Time parameter</param>
            <returns>Data will be returned</returns>
        </member>
    </members>
</doc>

```



## Method documentation comment with param and returns elements


```cs
/// <summary>
/// Returns the data for the specified ID and timestamp.
/// </summary>
/// <param name="id">The ID for which to get data. </param>
/// <param name="time">The DateTime for which to get data. </param>
/// <returns>A DataClass instance with the result. </returns>
public DataClass GetData(int id, DateTime time)
{
   // ...
}

```

**IntelliSense** shows you the description for each parameter:

[<img src="https://i.stack.imgur.com/cH3OQ.png" alt="parameter comment" />](https://i.stack.imgur.com/cH3OQ.png)

Tip: If Intellisense doesn't display in Visual Studio, delete the first bracket or comma and then type it again.



## Interface and class documentation comments


```cs
/// <summary>
/// This interface can do Foo
/// </summary>
public interface ICanDoFoo
{
    // ... 
}

/// <summary>
/// This Bar class implements ICanDoFoo interface
/// </summary>
public class Bar : ICanDoFoo
{
    // ...
}

```

**Result**

Interface summary

[<img src="https://i.stack.imgur.com/ExpwI.png" alt="interface summary" />](https://i.stack.imgur.com/ExpwI.png)

Class summary

[<img src="https://i.stack.imgur.com/730eY.png" alt="class summary" />](https://i.stack.imgur.com/730eY.png)



## Referencing another class in documentation


The `<see>` tag can be used to link to another class. It contains the `cref` member which should contain the name of the class that is to be referenced. Visual Studio will provide Intellsense when writing this tag and such references will be processed when renaming the referenced class, too.

```cs
/// <summary>
/// You might also want to check out <see cref="SomeOtherClass"/>.
/// </summary>
public class SomeClass
{
}

```

In Visual Studio Intellisense popups such references will also be displayed colored in the text.

To reference a generic class, use something similar to the following:

```cs
/// <summary>
/// An enhanced version of <see cref="List{T}"/>.
/// </summary>
public class SomeGenericClass<T>
{
}

```



#### Remarks


Some times you need to **create extended text documentation** from you xml comments. Unfortunatly ****there is no standard way for it****.

But there are some separate projects that you can use for this case:

- [Sandcastle](http://sandcastle.codeplex.com/)
- [Docu](http://docu.jagregory.com/)
- [NDoc](http://sandcastle.codeplex.com/)
- [DocFX](https://dotnet.github.io/docfx/)

