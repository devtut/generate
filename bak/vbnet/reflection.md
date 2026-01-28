---
metaTitle: "Visual Basic .NET - Reflection"
description: "Retrieve Properties for an Instance of a Class, Get a method and invoke it, Create an instance of a generic type, Get the members of a type"
---

# Reflection



## Retrieve Properties for an Instance of a Class


```vb
Imports System.Reflection

Public Class PropertyExample

    Public Function GetMyProperties() As PropertyInfo()
        Dim objProperties As PropertyInfo()
        objProperties = Me.GetType.GetProperties(BindingFlags.Public Or BindingFlags.Instance)
        Return objProperties
    End Function

    Public Property ThisWillBeRetrieved As String = "ThisWillBeRetrieved"

    Private Property ThisWillNot As String = "ThisWillNot"

    Public Shared Property NeitherWillThis As String = "NeitherWillThis"

    Public Overrides Function ToString() As String
        Return String.Join(",", GetMyProperties.Select(Function(pi) pi.Name).ToArray)
    End Function
End Class   

```

The Parameter of GetProperties defines which kinds of Properties will be returned by the function.
Since we pass Public and Instance, the method will return only properties that are both public and non-shared. See [The Flags attribute](http://stackoverflow.com/documentation/vb.net/1809/enum/5910/the-flags-attribute) for and explanation on how Flag-enums can be combined.



## Get a method and invoke it


Static method:

```vb
Dim parseMethod = GetType(Integer).GetMethod("Parse",{GetType(String)})
Dim result = DirectCast(parseMethod.Invoke(Nothing,{"123"}), Integer)

```

Instance method:

```

Dim instance = "hello".ToUpper
 Dim method = Gettype(String).GetMethod("ToUpper",{})
 Dim result = method.Invoke(instance,{}) 
 Console.WriteLine(result) 'HELLO

```



## Create an instance of a generic type


```

   Dim openListType = GetType(List(Of ))
    Dim typeParameters = {GetType(String)}
    Dim stringListType = openListType.MakeGenericType(typeParameters)
    Dim instance = DirectCast(Activator.CreateInstance(stringListType), List(Of String))
    instance.Add("Hello")

```



## Get the members of a type


```vb
Dim flags = BindingFlags.Static Or BindingFlags.Public Or BindingFlags.Instance
Dim members = GetType(String).GetMembers(flags)
For Each member In members
    Console.WriteLine($"{member.Name}, ({member.MemberType})")
Next          

```

