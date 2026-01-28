---
metaTitle: "PowerShell - Using existing static classes"
description: "Using the .Net Math Class, Adding types, Creating new GUID instantly"
---

# Using existing static classes


These classes are reference libraries of methods and properties that do not change state, in one word, immutable. You don't need to create them, you simply use them. Classes and methods such as these are called static classes because they are not created, destroyed, or changed.You can refer to a static class by surrounding the class name with square brackets.



## Using the .Net Math Class


You can use the .Net Math class to do calculations ([System.Math])

If you want to know which methods are available you can use:

```powershell
[System.Math] | Get-Member -Static -MemberType Methods

```

Here are some examples how to use the Math class:

```powershell
PS C:\> [System.Math]::Floor(9.42)
9
PS C:\> [System.Math]::Ceiling(9.42)
10
PS C:\> [System.Math]::Pow(4,3)
64
PS C:\> [System.Math]::Sqrt(49)
7

```



## Adding types


By Assembly Name, add library

```powershell
Add-Type -AssemblyName "System.Math"

```

or by file path:

```powershell
Add-Type -Path "D:\Libs\CustomMath.dll"

```

To Use added type:

```powershell
[CustomMath.NameSpace]::Method(param1, $variableParam, [int]castMeAsIntParam)

```



## Creating new GUID instantly


Use existing .NET classes instantly with PowerShell by using [class]::Method(args):

```powershell
PS C:\> [guid]::NewGuid()

Guid
----
8874a185-64be-43ed-a64c-d2fe4b6e31bc

```

Similarly, in PowerShell 5+ you may use the `New-Guid` cmdlet:

```powershell
PS C:\> New-Guid

Guid
----
8874a185-64be-43ed-a64c-d2fe4b6e31bc

```

To get the GUID as a `[String]` only, referenced the `.Guid` property:

```powershell
[guid]::NewGuid().Guid

```

