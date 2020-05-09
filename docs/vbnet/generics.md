---
metaTitle: "Visual Basic .NET - Generics"
description: "Create a generic class, Instance of a Generic Class, Define a 'generic' class, Use a generic class, Limit the possible types given, Create a new instance of the given type"
---

# Generics



## Create a generic class


A generic type is created to adapt so that the same functionallity can be accessible for different data types.

```vb
Public Class SomeClass(Of T)
    Public Sub doSomething(newItem As T)
        Dim tempItem As T
        ' Insert code that processes an item of data type t.
    End Sub
End Class

```



## Instance of a Generic Class


By creating an instance of the same class with a different type given, the interface of the class changes depending on the given type.

```vb
Dim theStringClass As New SomeClass(Of String)
Dim theIntegerClass As New SomeClass(Of Integer)

```

[<img src="http://i.stack.imgur.com/9trTP.png" alt="enter image description here" />](http://i.stack.imgur.com/9trTP.png)



## Define a 'generic' class


A generic class is a class who adapts to a later-given type so that the same functionality can be offered to different types.

In this basic example a generic class is created. It has a sub who uses the generic type T.  While programming this class, we don't  know the type of T.  In this case T has all the characteristics of Object.

```vb
Public Class SomeClass(Of T)
    Public Sub doSomething(newItem As T)
        Dim tempItem As T
        ' Insert code that processes an item of data type t.
    End Sub
End Class

```



## Use a generic class


In this example there are 2 instances created of the SomeClass Class. Depending on the type given the 2 instances have a different interface:

```vb
Dim theStringClass As New SomeClass(Of String)
Dim theIntegerClass As New SomeClass(Of Integer)

```

[<img src="http://i.stack.imgur.com/8qt7U.png" alt="enter image description here" />](http://i.stack.imgur.com/8qt7U.png)
[<img src="http://i.stack.imgur.com/cJyvz.png" alt="enter image description here" />](http://i.stack.imgur.com/cJyvz.png)

The most famous generic class is List(of )



## Limit the possible types given


The possible types passed to a new instance of SomeClass must inherit SomeBaseClass. This can also be an interface.  The characteristics of SomeBaseClass are accessible within this class definition.

```vb
Public Class SomeClass(Of T As SomeBaseClass)
    Public Sub DoSomething(newItem As T)
        newItem.DoSomethingElse()
        ' Insert code that processes an item of data type t.
    End Sub
End Class

Public Class SomeBaseClass
    Public Sub DoSomethingElse()
    End Sub
End Class

```



## Create a new instance of the given type


Creating a new intance of a generic type can be done/checed at compile time.

```vb
Public Class SomeClass(Of T As {New})
    Public Function GetInstance() As T
        Return New T
    End Function
End Class

```

Or with limited types:

```vb
Public Class SomeClass(Of T As {New, SomeBaseClass})
    Public Function GetInstance() As T
        Return New T
    End Function
End Class

Public Class SomeBaseClass
End Class

```

The baseClass (if none given it is Object) must have a parameter less constructor.

**This can also be done at runtime through [reflection](http://stackoverflow.com/documentation/vb.net/1598/reflection/14527/create-an-instance-of-a-generic-type)**

