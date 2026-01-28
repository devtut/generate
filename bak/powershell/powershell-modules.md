---
metaTitle: "PowerShell - Powershell Modules"
description: "Create a Module Manifest, Simple Module Example, Exporting a Variable from a Module, Structuring PowerShell Modules, Location of Modules, Module Member Visibility"
---

# Powershell Modules


Starting with PowerShell version 2.0, developers can create PowerShell modules. PowerShell modules encapsulate a set of common functionality. For example, there are vendor-specific PowerShell modules that manage various cloud services. There are also generic PowerShell modules that interact with social media services, and perform common programming tasks, such as Base64 encoding, working with Named Pipes, and more.

Modules can expose command aliases, functions, variables, classes, and more.



## Create a Module Manifest


```powershell
@{
  RootModule = 'MyCoolModule.psm1'
  ModuleVersion = '1.0'
  CompatiblePSEditions = @('Core')
  GUID = '6b42c995-67da-4139-be79-597a328056cc'
  Author = 'Bob Schmob'
  CompanyName = 'My Company'
  Copyright = '(c) 2017 Administrator. All rights reserved.'
  Description = 'It does cool stuff.'
  FunctionsToExport = @()
  CmdletsToExport = @()
  VariablesToExport = @()
  AliasesToExport = @()
  DscResourcesToExport = @()
}

```

Every good PowerShell module has a module manifest. The module manifest simply contains metadata about a PowerShell module, and doesn't define the actual contents of the module.

The manifest file is a PowerShell script file, with a `.psd1` file extension, that contains a HashTable. The HashTable in the manifest must contain specific keys, in order for PowerShell to correctly interpret it as a PowerShell module file.

The example above provides a list of the core HashTable keys that make up a module manifest, but there are many others. The `New-ModuleManifest` command helps you create a new module manifest skeleton.



## Simple Module Example


```powershell
function Add {
  [CmdletBinding()]
  param (
    [int] $x
  , [int] $y
  )

  return $x + $y
}

Export-ModuleMember -Function Add

```

This is a simple example of what a PowerShell script module file might look like. This file would be called `MyCoolModule.psm1`, and is referenced from the module manifest (.psd1) file. You'll notice that the `Export-ModuleMember` command enables us to specify which functions in the module we want to "export," or expose, to the user of the module. Some functions will be internal-only, and shouldn't be exposed, so those would be omitted from the call to `Export-ModuleMember`.



## Exporting a Variable from a Module


```powershell
$FirstName = 'Bob'
Export-ModuleMember -Variable FirstName

```

To export a variable from a module, you use the `Export-ModuleMember` command, with the `-Variable` parameter. Remember, however, that if the variable is also not explicitly exported in the module manifest (.psd1) file, then the variable will not be visible to the module consumer. Think of the module manifest like a "gatekeeper." If a function or variable isn't allowed in the module manifest, it won't be visible to the module consumer.

**Note:** Exporting a variable is similar to making a field in a class public.  It is not advisable.  It would be better to expose a function to get the field and a function to set the field.



## Structuring PowerShell Modules


Rather than defining all of your functions in a single `.psm1` PowerShell script module file, you might want to break apart your function into individual files. You can then dot-source these files from your script module file, which in essence, treats them as if they were part of the `.psm1` file itself.

Consider this module directory structure:

```powershell
\MyCoolModule
  \Functions
    Function1.ps1
    Function2.ps1
    Function3.ps1
MyCoolModule.psd1
MyCoolModule.psm1

```

Inside your `MyCoolModule.psm1` file, you could insert the following code:

```powershell
Get-ChildItem -Path $PSScriptRoot\Functions | 
  ForEach-Object -Process { . $PSItem.FullName }

```

This would dot-source the individual function files into the `.psm1` module file.



## Location of Modules


PowerShell looks for modules in the directories listed in the $Env:PSModulepath.

A module called **foo**, in a folder called **foo** will be found with `Import-Module foo`

In that folder, PowerShell will look for a module manifest (foo.psd1), a module file (foo.psm1), a DLL (foo.dll).



## Module Member Visibility


By default, only functions defined in a module are visible outside of the module. In other words, if you define variables and aliases in a module, they won't be available except in the module's code.

To override this behavior, you can use the `Export-ModuleMember` cmdlet.  It has parameters called `-Function`, `-Variable`, and `-Alias` which allow you to specify exactly which members are exported.

It is important to note that if you use `Export-ModuleMember`, **only** the items you specify will be visible.

