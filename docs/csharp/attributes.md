---
metaTitle: "Attributes"
description: "Creating a custom attribute, Reading an attribute, Using an attribute, DebuggerDisplay Attribute, Caller info attributes, Reading an attribute from interface, Obsolete Attribute"
---

# Attributes



## Creating a custom attribute


```cs
//1) All attributes should be inherited from System.Attribute
//2) You can customize your attribute usage (e.g. place restrictions) by using System.AttributeUsage Attribute
//3) You can use this attribute only via reflection in the way it is supposed to be used
//4) MethodMetadataAttribute is just a name. You can use it without "Attribute" postfix - e.g. [MethodMetadata("This text could be retrieved via reflection")].
//5) You can overload an attribute constructors
[System.AttributeUsage(System.AttributeTargets.Method | System.AttributeTargets.Class)]
public class MethodMetadataAttribute : System.Attribute
{
    //this is custom field given just for an example
    //you can create attribute without any fields
    //even an empty attribute can be used - as marker
    public string Text { get; set; }

    //this constructor could be used as [MethodMetadata]
    public MethodMetadataAttribute ()
    {
    }

    //This constructor could be used as [MethodMetadata("String")]
    public MethodMetadataAttribute (string text)
    {
        Text = text;
    }
}

```



## Reading an attribute


Method `GetCustomAttributes` returns an array of custom attributes applied to the member. After retrieving this array you can search for one or more specific attributes.

```cs
var attribute = typeof(MyClass).GetCustomAttributes().OfType<MyCustomAttribute>().Single();

```

Or iterate through them

```cs
foreach(var attribute in typeof(MyClass).GetCustomAttributes()) {
    Console.WriteLine(attribute.GetType());
}

```

`GetCustomAttribute` extension method from `System.Reflection.CustomAttributeExtensions` retrieves a custom attribute of a specified type, it can be applied to any `MemberInfo`.

```cs
var attribute = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute));

```

`GetCustomAttribute` also has generic signature to specify type of attribute to search for.

```cs
var attribute = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>();

```

Boolean argument `inherit` can be passed to both of those methods. If this value set to `true` the ancestors of element would be also to inspected.



## Using an attribute


```cs
[StackDemo(Text = "Hello, World!")]
public class MyClass
{
    [StackDemo("Hello, World!")]
    static void MyMethod()
    {
    }
}

```



## DebuggerDisplay Attribute


Adding the `DebuggerDisplay` Attribute will change the way the debugger displays the class when it is hovered over.

Expressions that are wrapped in `{}` will be evaluated by the debugger. This can be a simple property like in the following sample or more complex logic.

```cs
[DebuggerDisplay("{StringProperty} - {IntProperty}")]
public class AnObject
{
   public int ObjectId { get; set; }
   public string StringProperty { get; set; }
   public int IntProperty { get; set; }
}

```

[<img src="https://i.stack.imgur.com/6JjJs.png" alt="DebuggerDisplay Example" />](https://i.stack.imgur.com/6JjJs.png)

Adding `,nq` before the closing bracket removes the quotes when outputting a string.

```cs
[DebuggerDisplay("{StringProperty,nq} - {IntProperty}")]

```

Even though general expressions are allowed in the `{}` they are not recommended. The `DebuggerDisplay` attribute will be written into the assembly metadata as a string. Expressions in `{}` are not checked for validity. So a `DebuggerDisplay` attribute containing more complex logic than i.e. some simple arithmetic might work fine in C#, but the same expression evaluated in VB.NET will probably not be syntactically valid and produce an error while debugging.

A way to make `DebuggerDisplay` more language agnostic is to write the expression in a method or property and call it instead.

```cs
[DebuggerDisplay("{DebuggerDisplay(),nq}")]
public class AnObject
{
   public int ObjectId { get; set; }
   public string StringProperty { get; set; }
   public int IntProperty { get; set; }

   private string DebuggerDisplay()
    {
        return $"{StringProperty} - {IntProperty}"";
    }
}

```

One might want `DebuggerDisplay`to output all or just some of the properties and when debugging and inspecting also the type of the object.<br />
The example below also surrounds the helper method with `#if DEBUG` as `DebuggerDisplay` is used in debugging environments.

```cs
[DebuggerDisplay("{DebuggerDisplay(),nq}")]
public class AnObject
{
   public int ObjectId { get; set; }
   public string StringProperty { get; set; }
   public int IntProperty { get; set; }

#if DEBUG
   private string DebuggerDisplay()
    {
        return
            $"ObjectId:{this.ObjectId}, StringProperty:{this.StringProperty}, Type:{this.GetType()}";
    }
    #endif
}

```



## Caller info attributes


Caller info attributes can be used to pass down information about the invoker to the invoked method. The declaration looks like this:

```cs
using System.Runtime.CompilerServices;

public void LogException(Exception ex,
                         [CallerMemberName]string callerMemberName = "",
                         [CallerLineNumber]int callerLineNumber = 0,
                         [CallerFilePath]string callerFilePath = "")
{
    //perform logging
}

```

And the invocation looks like this:

```cs
public void Save(DBContext context)
{
    try
    {
        context.SaveChanges();
    }
    catch (Exception ex)
    {
        LogException(ex);
    }
}

```

Notice that only the first parameter is passed explicitly to the `LogException` method whereas the rest of them will be provided at compile time with the relevant values.

The `callerMemberName` parameter will receive the value `"Save"` - the name of the calling method.

The `callerLineNumber` parameter will receive the number of whichever line the `LogException` method call is written on.

And the 'callerFilePath' parameter will receive the full path of the file `Save` method is declared in.



## Reading an attribute from interface


There is no simple way to obtain attributes from an interface, since classes does not inherit attributes from an interface. Whenever implementing an interface or overriding members in a derived class, you need to re-declare the attributes.
So in the example below output would be `True` in all three cases.

```cs
using System;
using System.Linq;
using System.Reflection;

namespace InterfaceAttributesDemo {
    
    [AttributeUsage(AttributeTargets.Interface, Inherited = true)]
    class MyCustomAttribute : Attribute {
        public string Text { get; set; }
    }
    
    [MyCustomAttribute(Text = "Hello from interface attribute")]
    interface IMyClass {
        void MyMethod();
    }
    
    class MyClass : IMyClass {
        public void MyMethod() { }
    }
    
    public class Program {
        public static void Main(string[] args) {
            GetInterfaceAttributeDemo();
        }
        
        private static void GetInterfaceAttributeDemo() {
            var attribute1 = (MyCustomAttribute) typeof(MyClass).GetCustomAttribute(typeof(MyCustomAttribute), true);
            Console.WriteLine(attribute1 == null); // True
            
            var attribute2 = typeof(MyClass).GetCustomAttributes(true).OfType<MyCustomAttribute>().SingleOrDefault();
            Console.WriteLine(attribute2 == null); // True
            
            var attribute3 = typeof(MyClass).GetCustomAttribute<MyCustomAttribute>(true);
            Console.WriteLine(attribute3 == null); // True
        }
    }
}

```

One way to retrieve interface attributes is to search for them through all the interfaces implemented by a class.

```cs
var attribute = typeof(MyClass).GetInterfaces().SelectMany(x => x.GetCustomAttributes().OfType<MyCustomAttribute>()).SingleOrDefault();
Console.WriteLine(attribute == null); // False
Console.WriteLine(attribute.Text); // Hello from interface attribute

```



## Obsolete Attribute


System.Obsolete is an attribute that is used to mark a type or a member that has a better version, and thus should not be used.

```cs
[Obsolete("This class is obsolete. Use SomeOtherClass instead.")]
class SomeClass
{
    //
}

```

In case the class above is used, the compiler will give the warning "This class is obsolete. Use SomeOtherClass instead."

