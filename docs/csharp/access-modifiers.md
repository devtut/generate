---
metaTitle: "Access Modifiers"
description: "Access Modifiers Diagrams, public, private, internal, protected, protected internal"
---

# Access Modifiers



## Access Modifiers Diagrams


Here are all access modifiers in venn diagrams, from more limiting to more accessible:

<th align="right">Access Modifier</th>|Diagram
|---|---|---|---|---|---|---|---|---|---
<td align="right">private</td>|<img src="https://i.stack.imgur.com/SdeM9.png" alt="private" />
<td align="right">internal</td>|<img src="https://i.stack.imgur.com/8o7Dm.png" alt="internal" />
<td align="right">protected</td>|<img src="https://i.stack.imgur.com/uniOu.png" alt="protected" />
<td align="right">protected internal</td>|<img src="https://i.stack.imgur.com/VaQQ9.png" alt="protected internal" />
<td align="right">public</td>|<img src="https://i.stack.imgur.com/VGgjh.png" alt="public" />

Below you could find more information.



## public


The `public` keyword makes a class (including nested classes), property, method or field available to every consumer:

```cs
public class Foo()
{
    public string SomeProperty { get; set; }

    public class Baz
    {
        public int Value { get; set; }
    }
}

public class Bar()
{
    public Bar()
    {
        var myInstance = new Foo();
        var someValue = foo.SomeProperty;
        var myNestedInstance = new Foo.Baz();
        var otherValue = myNestedInstance.Value;
    }        
}

```



## private


The `private` keyword marks properties, methods, fields and nested classes for use inside the class only:

```cs
public class Foo()
{
    private string someProperty { get; set; }

    private class Baz
    {
        public string Value { get; set; }
    }

    public void Do()
    {
        var baz = new Baz { Value = 42 };
    }
}

public class Bar()
{
    public Bar()
    {
        var myInstance = new Foo();

        // Compile Error - not accessible due to private modifier
        var someValue = foo.someProperty;
        // Compile Error - not accessible due to private modifier
        var baz = new Foo.Baz();
    }
}

```



## internal


The internal keyword makes a class (including nested classes), property, method or field available to every consumer in the same assembly:

```cs
internal class Foo
{
    internal string SomeProperty {get; set;}
}

internal class Bar
{
    var myInstance = new Foo();
    internal string SomeField = foo.SomeProperty;

    internal class Baz
    {
        private string blah;
        public int N { get; set; }
    }
}

```

This can be broken to allow a testing assembly to access the code via adding code to AssemblyInfo.cs file:

```cs
using System.Runtime.CompilerServices;

[assembly:InternalsVisibleTo("MyTests")]

```



## protected


The `protected` keyword marks field, methods properties and nested classes for use inside the same class and derived classes only:

```cs
public class Foo()
{
    protected void SomeFooMethod()
    {
        //do something
    }

    protected class Thing
    {
        private string blah;
        public int N { get; set; }
    }
}

public class Bar() : Foo
{
    private void someBarMethod()
    {
        SomeFooMethod(); // inside derived class
        var thing = new Thing(); // can use nested class
    }
}

public class Baz()
{
    private void someBazMethod()
    {
        var foo = new Foo();
        foo.SomeFooMethod(); //not accessible due to protected modifier
    }
}

```



## protected internal


The `protected internal` keyword marks field, methods, properties and nested classes for use inside the same assembly or derived classes in another assembly:

> 
Assembly 1


```cs
public class Foo
{
    public string MyPublicProperty { get; set; }
    protected internal string MyProtectedInternalProperty  { get; set; }

    protected internal class MyProtectedInternalNestedClass
    {
        private string blah;
        public int N { get; set; }
    }
}

public class Bar
{
    void MyMethod1()
    {
        Foo foo = new Foo();
        var myPublicProperty = foo.MyPublicProperty;
        var myProtectedInternalProperty = foo.MyProtectedInternalProperty;
        var myProtectedInternalNestedInstance =
            new Foo.MyProtectedInternalNestedClass();
    }
}

```

> 
Assembly 2


```cs
public class Baz : Foo
{
    void MyMethod1()
    {
        var myPublicProperty = MyPublicProperty;
        var myProtectedInternalProperty = MyProtectedInternalProperty;
        var thing = new MyProtectedInternalNestedClass();
    }

    void MyMethod2()
    {
        Foo foo = new Foo();
        var myPublicProperty = foo.MyPublicProperty;

        // Compile Error
        var myProtectedInternalProperty = foo.MyProtectedInternalProperty;
        // Compile Error
        var myProtectedInternalNestedInstance =
            new Foo.MyProtectedInternalNestedClass();
    }

}

public class Qux
{
    void MyMethod1()
    {
        Baz baz = new Baz();
        var myPublicProperty = baz.MyPublicProperty;

        // Compile Error
        var myProtectedInternalProperty = baz.MyProtectedInternalProperty;
        // Compile Error
        var myProtectedInternalNestedInstance =
            new Baz.MyProtectedInternalNestedClass();
    }

    void MyMethod2()
    {
        Foo foo = new Foo();
        var myPublicProperty = foo.MyPublicProperty;

        //Compile Error
        var myProtectedInternalProperty = foo.MyProtectedInternalProperty;
        // Compile Error
        var myProtectedInternalNestedInstance =
            new Foo.MyProtectedInternalNestedClass();
    }
}

```



#### Remarks


If the access modifier is omitted,

- classes are by default `internal`
- methods are by deault `private`
- getters and setters inherit the modifier of the property, by default this is `private`

Access modifiers on setters or getters of properties can only restrict access, not widen it:
`public string someProperty {get; private set;}`

