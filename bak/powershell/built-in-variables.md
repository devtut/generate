---
metaTitle: "PowerShell - Built-in variables"
description: "$PSScriptRoot, $Args, $PSItem, $?, $error"
---

# Built-in variables


PowerShell offers a variety of useful "automatic" (built-in) variables. Certain automatic variables are only populated in special circumstances, while others are available globally.



## $PSScriptRoot


```powershell
Get-ChildItem -Path $PSScriptRoot

```

This example retrieves the list of child items (directories and files) from the folder where the script file resides.

The `$PSScriptRoot` automatic variable is `$null` if used from outside a PowerShell code file. If used **inside** a PowerShell script, it automatically defined the fully-qualified filesystem path to the directory that contains the script file.

In Windows PowerShell 2.0, this variable is valid only in script modules
(.psm1). Beginning in Windows PowerShell 3.0, it is valid in all scripts.



## $Args


```powershell
$Args

```

Contains an array of the undeclared parameters and/or parameter
values that are passed to a function, script, or script block.
When you create a function, you can declare the parameters by using the
param keyword or by adding a comma-separated list of parameters in
parentheses after the function name.

In an event action, the $Args variable contains objects that represent
the event arguments of the event that is being processed. This variable
is populated only within the Action block of an event registration
command.  The value of this variable can also be found in the SourceArgs
property of the PSEventArgs object (System.Management.Automation.PSEventArgs)
that Get-Event returns.



## $PSItem


```powershell
Get-Process | ForEach-Object -Process { 
  $PSItem.Name
}

```

Same as `$_`. Contains the current object in the pipeline object.
You can use this variable in commands that perform an action on every
object or on selected objects in a pipeline.



## $?


```powershell
Get-Process -Name doesnotexist
Write-Host -Object "Was the last operation successful? $?"

```

Contains the execution status of the last operation. It contains
TRUE if the last operation succeeded and FALSE if it failed.



## $error


```powershell
Get-Process -Name doesnotexist
Write-Host -Object ('The last error that occurred was: {0}' -f $error[0].Exception.Message)

```

Contains an array of error objects that represent the most
recent errors. The most recent error is the first error object in the
array ($Error[0]).

To prevent an error from being added to the $Error array, use the
ErrorAction common parameter with a value of Ignore. For more
information, see about_CommonParameters
([http://go.microsoft.com/fwlink/?LinkID=113216)](http://go.microsoft.com/fwlink/?LinkID=113216)).

