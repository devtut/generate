---
metaTitle: "Visual Basic .NET - NullReferenceException"
description: "Empty Return, Uninitialized variable"
---

# NullReferenceException



## Empty Return


```vb
Function TestFunction() As TestClass
    Return Nothing
End Function

```

**BAD CODE**

```vb
TestFunction().TestMethod()

```

**GOOD CODE**

```vb
Dim x = TestFunction()
If x IsNot Nothing Then x.TestMethod()

```

[**Null Conditional Operator**](https://msdn.microsoft.com/en-us/library/dn986595.aspx?cs-save-lang=1&cs-lang=vb#code-snippet-1)

```vb
TestFunction()?.TestMethod()

```



## Uninitialized variable


**BAD CODE**

```vb
Dim f As System.Windows.Forms.Form
f.ShowModal()

```

**GOOD CODE**

```vb
Dim f As System.Windows.Forms.Form = New System.Windows.Forms.Form
' Dim f As New System.Windows.Forms.Form ' alternative syntax
f.ShowModal()

```

**EVEN BETTER CODE**
(Ensure proper disposal of IDisposable object [more info](https://msdn.microsoft.com/en-us/library/htd05whh.aspx))

```vb
Using f As System.Windows.Forms.Form = New System.Windows.Forms.Form
' Using f As New System.Windows.Forms.Form ' alternative syntax
    f.ShowModal()
End Using

```



#### Remarks


NullReferenceException is thrown whenever a variable is empty and one of its method/properties are referenced. To avoid this, be sure all variables are initialized correctly (`new` operator), and all methods returns a non-null value.

