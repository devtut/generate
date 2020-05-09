---
metaTitle: "PowerShell - Special Operators"
description: "Array Expression Operator, Call Operation, Dot sourcing operator"
---

# Special Operators




## Array Expression Operator


Returns the expression as an array.

```powershell
@(Get-ChildItem $env:windir\System32\ntdll.dll)

```

Will return an array with one item

```powershell
@(Get-ChildItem $env:windir\System32)

```

Will return an array with all the items in the folder (which is not a change of behavior from the inner expression.



## Call Operation


```powershell
$command = 'Get-ChildItem'
& $Command

```

Will execute `Get-ChildItem`



## Dot sourcing operator


. .\myScript.ps1

runs `.\myScript.ps1` in the current scope making any functions, and variable available in the current scope.

