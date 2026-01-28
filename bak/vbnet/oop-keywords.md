---
metaTitle: "Visual Basic .NET - OOP Keywords"
description: "Defining a class, Inheritance Modifiers (on classes), Inheritance Modifiers (on properties and methods), MyBase, Me vs MyClass, Overloading, Shadows, Interfaces"
---

# OOP Keywords




## Defining a class


**Classes** are vital aspects of OOP. A class is like the "blueprint" of an object. An object has the properties of a class, but the characteristics are not defined within the class itself. As each object can be different, they define their own characteristics.

```vb
Public Class Person
End Class
 
Public Class Customer
End Class

```

A class can also contain **subclasses**. A subclass inherits the same properties and behaviors as its parent class, but can have its own unique properties and classes.



## Inheritance Modifiers (on classes)


### Inherits

Specifies the base (or parent) class

```vb
Public Class Person
End Class

Public Class Customer
    Inherits Person

End Class

'One line notation
Public Class Student : Inherits Person
End Class

```

Possible objects:

```vb
Dim p As New Person
Dim c As New Customer
Dim s As New Student

```

### NotInheritable

Prevents programmers from using the class as a base class.

```vb
Public NotInheritable Class Person
End Class

```

Possible objects:

```vb
Dim p As New Person

```

### MustInherit

Specifies that the class is intended for use as a base class only. (Abstract class)

```vb
Public MustInherit Class Person
End Class

Public Class Customer
    Inherits Person
End Class

```

Possible objects:

```vb
Dim c As New Customer

```



## Inheritance Modifiers (on properties and methods)


### Overridable

Allows a property or method in a class to be overridden in a derived class.

```vb
Public Class Person
    Public Overridable Sub DoSomething()
        Console.WriteLine("Person")
    End Sub
End Class

```

### Overrides

Overrides an Overridable property or method defined in the base class.

```vb
Public Class Customer
    Inherits Person

    'Base Class must be Overridable
    Public Overrides Sub DoSomething()
        Console.WriteLine("Customer")
    End Sub
End Class

```

### NotOverridable

Prevents a property or method from being overridden in an inheriting class. Default behaviour. Can only be declared on **overrides methods**

```vb
Public Class Person

    Public Overridable Sub DoSomething()
        Console.WriteLine("Person")
    End Sub

End Class

Public Class Customer
    Inherits Person

    Public NotOverridable Overrides Sub DoSomething()
        Console.WriteLine("Customer")
    End Sub

End Class

Public Class DetailedCustomer
    Inherits Customer

    'DoSomething can't be overridden
End Class

```

Example Usage:

```vb
Dim p As New Person
p.DoSomething()

Dim c As New Customer
c.DoSomething()

Dim d As New DetailedCustomer
d.DoSomething()

```

Output:

```vb
Person
Customer
Customer

```

### MustOverride

Requires that a derived class override the property or method.

MustOverride methods must be declared in **MustInherit classes.**

```vb
Public MustInherit Class Person

    Public MustOverride Sub DoSomething()
    'No method definition here

End Class

Public Class Customer
    Inherits Person

    'DoSomething must be overridden
    Public Overrides Sub DoSomething()
        Console.WriteLine("Customer")
    End Sub

End Class

```

Example Usage:

```vb
Dim c As New Customer
c.DoSomething()

```

Output:

```vb
Customer

```



## MyBase


The MyBase keyword behaves like an object variable that refers to the base class of the current instance of a class.

```vb
Public Class Person
    Public Sub DoSomething()
        Console.WriteLine("Person")
    End Sub
End Class

Public Class Customer
    Inherits Person

    Public Sub DoSomethingElse()
        MyBase.DoSomething()
    End Sub

End Class

```

Usage example:

```vb
Dim p As New Person
p.DoSomething()

Console.WriteLine("----")

Dim c As New Customer
c.DoSomething()
c.DoSomethingElse()

```

Output:

```vb
Person
----
Person
Person

```



## Me vs MyClass


**Me** uses the current object instance.

**MyClass** uses the memberdefinition in the class where the member is called

```vb
Class Person
    Public Overridable Sub DoSomething()
        Console.WriteLine("Person")
    End Sub

    Public Sub useMe()
        Me.DoSomething()
    End Sub

    Public Sub useMyClass()
        MyClass.DoSomething()
    End Sub
End Class

Class Customer
    Inherits Person

    Public Overrides Sub DoSomething()
        Console.WriteLine("Customer")
    End Sub
End Class

```

Example Usage:

```vb
Dim c As New Customer
c.useMe()
c.useMyClass()

```

Output:

```vb
Customer
Person

```



## Overloading


Overloading is the creation of more than one procedure, instance constructor, or property in a class with the same name but different argument types.

```vb
Class Person
    Overloads Sub Display(ByVal theChar As Char)
        ' Add code that displays Char data.
    End Sub

    Overloads Sub Display(ByVal theInteger As Integer)
        ' Add code that displays Integer data.
    End Sub

    Overloads Sub Display(ByVal theDouble As Double)
        ' Add code that displays Double data.
    End Sub
End Class

```



## Shadows


It redeclares a member that is not overridable. Only calls to the instance will be affected. Code inside the base classes will not be affected by this.

```vb
Public Class Person
    Public Sub DoSomething()
        Console.WriteLine("Person")
    End Sub


    Public Sub UseMe()
        Me.DoSomething()
    End Sub
End Class
Public Class Customer
    Inherits Person
    Public Shadows Sub DoSomething()
        Console.WriteLine("Customer")
    End Sub

End Class

```

Example usage:

```vb
Dim p As New Person
Dim c As New Customer
p.UseMe()
c.UseMe()
Console.WriteLine("----")
p.DoSomething()
c.DoSomething()

```

Output:

```vb
Person
Person
----
Person
Customer

```

**Pitfalls**:

Example1, Creating a new object through a generic.   Which function will be used??

```vb
Public Sub CreateAndDoSomething(Of T As {Person, New})()
    Dim obj As New T
    obj.DoSomething()
End Sub

```

example usage:

```vb
Dim p As New Person
p.DoSomething()
Dim s As New Student
s.DoSomething()
Console.WriteLine("----")
CreateAndDoSomething(Of Person)()
CreateAndDoSomething(Of Student)()

```

Output: By intuition the result should be the same. Yet that is not true.

```vb
Person
Student
----
Person
Person

```

Example 2:

```vb
Dim p As Person
Dim s As New Student
p = s
p.DoSomething()
s.DoSomething()

```

Output: By intuition you could think that p and s are equal and will behave equal.  Yet that is not true.

```vb
Person
Student

```

In this simple examples it is easy to learn the strange behaviour of Shadows.  But in real-life it brings a lot of surprises.  It is advisably to prevent the usage of shadows. One should use other alternatives as much as possible (overrides etc..)



## Interfaces


```vb
Public Interface IPerson
    Sub DoSomething()
End Interface

Public Class Customer
    Implements IPerson
    Public Sub DoSomething() Implements IPerson.DoSomething
        Console.WriteLine("Customer")
    End Sub

End Class

```

