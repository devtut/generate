---
metaTitle: "Visual Basic .NET - Option Infer"
description: "How to enable/disable it, What is it?, When to use type inference"
---

# Option Infer



## How to enable/disable it


**Document level**

It is on by default, but you can set it by placing `Option Infer On|Off` at the top of the code file. The option will apply to the whole document.

**Project level**

You can switch it on/off via the menu in Visual Studio:

> 
Project > [Project] Properties > Compile Tab > Option infer


Choose `On|Off` in the drop-down menu. The option will apply to the whole document.

**All new projects**

You can switch it On by default for all new Projects by selecting:

> 
Tools > Options > Projects and Solutions > VB defaults > Option Infer


Choose `On|Off` in the drop-down menu.



## What is it?


Enables the use of local type inference in declaring variables.

**What is type inference?**

You can declare local variables without explicitly stating a data type. The compiler infers the data type of a variable from the type of its initialization expression.

**Option Infer On**:

```vb
Dim aString  = "1234" '--> Will be treated as String by the compiler
Dim aNumber  = 4711   '--> Will be treated as Integer by the compiler

```

vs. explicit type declaration:

```vb
'State a type explicitly
Dim aString as String = "1234"
Dim aNumber as Integer = 4711

```

**Option Infer Off:**<br />
The compiler behavior with `Option Infer Off` depends on the `Option Strict` setting
which is already [documented here](http://stackoverflow.com/documentation/vb.net/4022/option-strict#t=201608031033503223787).

<li>
<p>**Option Infer Off - Option Strict Off**<br />
All variables without explicit type declarations are declared as `Object`.</p>

```vb
Dim aString  = "1234" '--> Will be treated as Object by the compiler

```


</li>
<li>
<p>**Option Infer Off - Option Strict On**<br />
The compiler won´t let you declare a variable without an explicit type.</p>

```vb
'Dim aString  = "1234" '--> Will not compile due to missing type in declaration

```


</li>



## When to use type inference


Basically you can use type inference whenever it is possible.<br />
However, be careful when combining `Option Infer Off` and `Option Strict Off`, as this  can lead to undesired run-time behavior:

```vb
Dim someVar = 5
someVar.GetType.ToString() '--> System.Int32
someVar = "abc"
someVar.GetType.ToString() '--> System.String

```

**Anonymous Type**<br />
Anonymous types can **only** be declared with `Option Infer On`.<br />
They are often used when dealing with [LINQ](http://stackoverflow.com/documentation/vb.net/3111/linq#t=201608031014104425303):

```vb
Dim countryCodes = New List(Of String)
countryCodes.Add("en-US")
countryCodes.Add("en-GB")
countryCodes.Add("de-DE")
countryCodes.Add("de-AT")

Dim q = From code In countryCodes
        Let split = code.Split("-"c)
        Select New With {.Language = split(0), .Country = split(1)}

```


<li>
<p>**Option Infer On**<br />
The compiler will recognize the anonymous type:
[<img src="http://i.stack.imgur.com/TtgR0.png" alt="Anonymous Type - Option Infer On" />](http://i.stack.imgur.com/TtgR0.png)</p>
</li>
<li>
<p>**Option Infer Off**<br />
The compiler will either throw an error (with `Option Strict On`)<br />
or will consider `q` as type `object` (with `Option Strict Off`).<br />
Both cases will produce the outcome that you cannot use the anonymous type.</p>
</li>

**Doubles/Decimals**<br />
Numeric variables with decimal places will be infered as `Double` by default:

```vb
Dim aNumber = 44.11 '--> Will be treated as type `Double` by the compiler

```

If another type like `Decimal` is desired the value which initialized the variable needs to be marked:

```vb
Dim mDecimal = 47.11D '--> Will be treated as type `Decimal` by the compiler

```

