---
metaTitle: "Xamarin - Bindings"
description: "Removing Types, Implementing Java interfaces, Bindings libraries may rename methods and interfaces"
---

# Bindings



## Removing Types


It is possible to instruct the Xamarin.Android Bindings Generator to ignore a Java type and not bind it. This is done by adding a `remove-node` XML element to the metadata.xml file:

```cs
<remove-node path="/api/package[@name='{package_name}']/class[@name='{name}']" />

```



## Implementing Java interfaces


If a java library contains interfaces that should be implemented by the user (e.g. click listeners like `View.IOnClickListener` or callbacks), the implementing class has to inherit -- directly or indirectly -- from `Java.Lang.Object` or `Java.Lang.Throwable`. This is a common error, because the packaging steps just print a warning that is overlooked easily:

> 
Type 'MyListener ' implements Android.Runtime.IJavaObject but does not inherit from Java.Lang.Object. It is not supported.


**Wrong**

The usage of this implementation will result in unexpected behavior.

```cs
class MyListener : View.IOnClickListener
{
    public IntPtr Handle { get; }

    public void Dispose()
    {            
    }

    
    public void OnClick(View v)
    {
        // ...
    }
}

```

**Correct**

```cs
class MyListener : 
    Java.Lang.Object, // this is the important part
    View.IOnClickListener
{       
    public void OnClick(View v)
    {
        // ...
    }
}

```



## Bindings libraries may rename methods and interfaces


Not everything in a bindings library will have the same name in C# as it does in Java.

In C#, interface names start with "I", but Java has no such convention. When you import a Java library, an interface named `SomeInterface` will become `ISomeInterface`.

Similarly, Java doesn't have properties like C# does. When a library is bound, Java getter and setter methods might be refactored as properties. For example, the following Java code

```cs
public int getX() { return someInt; }

public int setX(int someInt) { this.someInt = someInt; }

```

may be refactored as

```cs
public int X { get; set; }

```

when it's bound.

