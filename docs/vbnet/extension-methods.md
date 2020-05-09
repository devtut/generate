---
metaTitle: "Visual Basic .NET - Extension methods"
description: "Creating an extension method, Making the language more functional with extension methods, Padding Numerics, Getting Assembly Version From Strong Name"
---

# Extension methods



## Creating an extension method


Extension methods are useful to extend the behaviour of libraries we don't own.

They are used similar to instance methods thanks to the compiler's syntactic sugar:

```vb
Sub Main()
    Dim stringBuilder = new StringBuilder()

    'Extension called directly on the object.
    stringBuilder.AppendIf(true, "Condition was true")

    'Extension called as a regular method. This defeats the purpose
    'of an extension method but should be noted that it is possible.
    AppendIf(stringBuilder, true, "Condition was true")

End Sub

<Extension>
Public Function AppendIf(stringBuilder As StringBuilder, condition As Boolean, text As String) As StringBuilder
    If(condition) Then stringBuilder.Append(text)

    Return stringBuilder
End Function

```

To have a usable extension method, the method needs the `Extension` attribute and needs to be declared in a `Module`.



## Making the language more functional with extension methods


A good use of extension method is to make the language more functional

```vb
Sub Main()
    Dim strings = { "One", "Two", "Three" }

    strings.Join(Environment.NewLine).Print()
End Sub

<Extension>
Public Function Join(strings As IEnumerable(Of String), separator As String) As String
    Return String.Join(separator, strings)
End Function

<Extension>
Public Sub Print(text As String)
    Console.WriteLine(text)
End Sub

```



## Padding Numerics


```vb
Public Module Usage
  Public Sub LikeThis()
    Dim iCount As Integer
    Dim sCount As String

    iCount = 245
    sCount = iCount.PadLeft(4, "0")

    Console.WriteLine(sCount)
    Console.ReadKey()
  End Sub
End Module



Public Module Padding
  <Extension>
  Public Function PadLeft(Value As Integer, Length As Integer) As String
    Return Value.PadLeft(Length, Space(Length))
  End Function



  <Extension>
  Public Function PadRight(Value As Integer, Length As Integer) As String
    Return Value.PadRight(Length, Space(Length))
  End Function



  <Extension>
  Public Function PadLeft(Value As Integer, Length As Integer, Character As Char) As String
    Return CStr(Value).PadLeft(Length, Character)
  End Function



  <Extension>
  Public Function PadRight(Value As Integer, Length As Integer, Character As Char) As String
    Return CStr(Value).PadRight(Length, Character)
  End Function
End Module

```



## Getting Assembly Version From Strong Name


Example of calling an extension method as an extension and as a regular method.

```vb
public Class MyClass  
  Sub Main()
        
        'Extension called directly on the object.
        Dim Version = Assembly.GetExecutingAssembly.GetVersionFromAssembly()

        'Called as a regular method.
        Dim Ver = GetVersionFromAssembly(SomeOtherAssembly)

    End Sub
End Class

```

The Extension Method in a Module. Make the Module Public if extensions are compiled to a dll and will be referenced in another assembly.

```vb
Public Module Extensions
    ''' <summary>
    ''' Returns the version number from the specified assembly using the assembly's strong name.
    ''' </summary>
    ''' <param name="Assy">[Assembly] Assembly to get the version info from.</param>
    ''' <returns>[String]</returns>
    <Extension>
    Friend Function GetVersionFromAssembly(ByVal Assy As Assembly) As String
        Return Split(Split(Assy.FullName,",")(1),"=")(1)
    End Function
End Module

```



#### Remarks


Extension methods are methods (`Sub` or `Function`) that add functionality to a Type (which may be a Reference Type or a Value Type).  These Types may or may not be owned by you.

They may or may not be in the same assembly as the Type they are intended to modify.  You can allow an opt-in to your extension methods by isolating them in their own namespace.  Or if you prefer you can make them always available by including them in the same namespace as the Type they modify (assuming all the assembly references are in place and correct).  See the Entity Framework Core 1.0 project on GitHub for a good example of the opt-in style of extension methods.

Extension methods in VB have a few requirements:

- Extension methods may only be declared in modules.
- Extension methods must be decorated with the `Extension()` attribute.
<li>The ExtensionAttribute namespace must be available within your module.<br />
`Imports System.Runtime.CompilerServices`</li>
- The first parameter to the method must be of a type that this method will be attached to.
- The first parameter of the method will represent the instance that this method operates on.  (Equivalent to `Me` if this were a real instance method).
- An extension method can be called as a regular method by supplying all parameters if not called on the instantiated object.

