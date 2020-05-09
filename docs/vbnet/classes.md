---
metaTitle: "Visual Basic .NET - Classes"
description: "Abstract Classes , Creating classes"
---

# Classes


A class groups different functions, methods, variables, and properties, which are called its members. A class encapsulates the members, which can be accessed by an instance of the class, called an object. Classes are extremely useful for the programmer, as they make the task convenient and fast, with characteristics such as modularity, re-usability, maintainability, and readability of the code.

Classes are the building blocks of object-oriented programming languages.



## Abstract Classes 


If classes share common functionality you can group this in a base or abstract class.  Abstract classes can contain partial or no implementation at all and allow the derived type to override the base implementation.

Abstract classes within VisualBasic.NET must be declared as `MustInherit` and cannot be instantiated.

```vb
Public MustInherit Class Vehicle
     Private Property _numberOfWheels As Integer
     Private Property _engineSize As Integer

     Public Sub New(engineSize As Integer, wheels As Integer)
         _numberOfWheels = wheels
         _engineSize = engineSize
     End Sub

     Public Function DisplayWheelCount() As Integer
         Return _numberOfWheels
     End Function
End Class

```

A sub type can then `inherit` this abstract class as shown below:

```vb
Public Class Car
    Inherits Vehicle
End Class

```

Car will inherit all of the declared types within vehicle, but can only access them based upon the underlying access modifier.

```vb
Dim car As New Car()
car.DisplayWheelCount()

```

In the above example a new Car instance is created.  The `DisplayWheelCount()` method is then invoked which will call the base class `Vehicles` implementation.



## Creating classes


Classes provide a way of creating your own types within the .NET framework.  Within a class definition you may include the following:

- Fields
- Properties
- Methods
- Constructors
- Events

To declare a class you use the following syntax:

```vb
Public Class Vehicle     
End Class

```

Other .NET types can be encapsulated within the class and exposed accordingly, as shown below:

```vb
Public Class Vehicle
     Private Property _numberOfWheels As Integer
     Private Property _engineSize As Integer

     Public Sub New(engineSize As Integer, wheels As Integer)
         _numberOfWheels = wheels
         _engineSize = engineSize
     End Sub

     Public Function DisplayWheelCount() As Integer
         Return _numberOfWheels
     End Function
End Class

```

