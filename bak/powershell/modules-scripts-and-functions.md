---
metaTitle: "PowerShell - Modules, Scripts and Functions"
description: "Function, Script, Module, Advanced Functions"
---

# Modules, Scripts and Functions


**PowerShell modules** bring extendibility to the systems administrator, DBA, and developer. Whether it’s simply as a method to share functions and scripts.

**Powershell Functions** are to avoid repitetive codes. Refer [PS Functions][1]
[1]: [PowerShell Functions](http://stackoverflow.com/documentation/powershell/1673/powershell-functions#t=201612281053398988991)

**PowerShell Scripts** are used for automating administrative tasks which consists of command-line shell and associated cmdlets built on top of .NET Framework.



## Function


A function is a named block of code which is used to define reusable code that should be easy to use. It is usually included inside a script to help reuse code (to avoid duplicate code) or distributed as part of a module to make it useful for others in multiple scripts.

Scenarios where a function might be useful:

- Calculate the average of a group of numbers
- Generate a report for running processes
- Write a function that tests is a computer is "healthy" by pinging the computer and accessing the `c$`-share

Functions are created using the `function` keyword, followed by a single-word name and a script block containing the code to executed when the function name is called.

```powershell
function NameOfFunction {
    Your code
}

```

### Demo

```powershell
function HelloWorld {
    Write-Host "Greetings from PowerShell!"
}

```

Usage:

```powershell
> HelloWorld
Greetings from PowerShell!

```



## Script


A script is a text file with the file extension `.ps1` that contains PowerShell commands that will be executed when the script is called. Because scripts are saved files, they are easy to transfer between computers.

Scripts are often written to solve a specific problem, ex.:

- Run a weekly maintenance task
- To install and configure a solution/application on a computer

### Demo

MyFirstScript.ps1:

```powershell
Write-Host "Hello World!"
2+2

```

You can run a script by entering the path to the file using an:

- Absolute path, ex. `c:\MyFirstScript.ps1`
- Relative path, ex `.\MyFirstScript.ps1` if the current directory of your PowerShell console was `C:\`

Usage:

```powershell
> .\MyFirstScript.ps1
Hello World!
4

```

A script can also import modules, define it's own functions etc.

MySecondScript.ps1:

```powershell
function HelloWorld {
    Write-Host "Greetings from PowerShell!"
}

HelloWorld
Write-Host "Let's get started!"
2+2
HelloWorld

```

Usage:

```powershell
> .\MySecondScript.ps1
Greetings from PowerShell!
Let's get started!
4
Greetings from PowerShell!

```



## Module


A module is a collection of related reusable functions (or cmdlets) that can easily be distributed to other PowerShell users and used in multiple scripts or directly in the console. A module is usually saved in it's own directory and consists of:

- One or more code files with the `.psm1` file extension containing functions or binary assemblies (`.dll`) containing cmdlets
- A module manifest `.psd1` describing the modules name, version, author, description, which functions/cmdlets it provides etc.
- Other requirements for it to work incl. dependencies, scripts etc.

Examples of modules:

- A module containing functions/cmdlets that perform statistics on a dataset
- A module for querying and configuring databases

To make it easy for PowerShell to find and import a module, it is often placed in one of the known PowerShell module-locations defined in `$env:PSModulePath`.

### Demo

List modules that are installed to one of the known module-locations:

```powershell
Get-Module -ListAvailable

```

Import a module, ex. `Hyper-V` module:

```powershell
Import-Module Hyper-V

```

List available commands in a module, ex. the `Microsoft.PowerShell.Archive`-module

```powershell
> Import-Module Microsoft.PowerShell.Archive
> Get-Command -Module Microsoft.PowerShell.Archive

CommandType Name             Version Source                      
----------- ----             ------- ------                      
Function    Compress-Archive 1.0.1.0 Microsoft.PowerShell.Archive
Function    Expand-Archive   1.0.1.0 Microsoft.PowerShell.Archive

```



## Advanced Functions


Advanced functions behave the in the same way as cmdlets.  The PowerShell ISE includes two skeletons of advanced functions.  Access these via the menu, edit, code snippets, or by Ctrl+J.  (As of PS 3.0, later versions may differ)

Key things that advanced functions include are,

- built-in, customized help for the function, accessible via `Get-Help`
- can use [CmdletBinding()] which makes the function act like a cmdlet
- extensive parameter options

Simple version:

```powershell
<#
.Synopsis
   Short description
.DESCRIPTION
   Long description
.EXAMPLE
   Example of how to use this cmdlet
.EXAMPLE
   Another example of how to use this cmdlet
#>
function Verb-Noun
{
    [CmdletBinding()]
    [OutputType([int])]
    Param
    (
        # Param1 help description
        [Parameter(Mandatory=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        $Param1,

        # Param2 help description
        [int]
        $Param2
    )

    Begin
    {
    }
    Process
    {
    }
    End
    {
    }
}

```

Complete version:

```powershell
<#
.Synopsis
   Short description
.DESCRIPTION
   Long description
.EXAMPLE
   Example of how to use this cmdlet
.EXAMPLE
   Another example of how to use this cmdlet
.INPUTS
   Inputs to this cmdlet (if any)
.OUTPUTS
   Output from this cmdlet (if any)
.NOTES
   General notes
.COMPONENT
   The component this cmdlet belongs to
.ROLE
   The role this cmdlet belongs to
.FUNCTIONALITY
   The functionality that best describes this cmdlet
#>
function Verb-Noun
{
    [CmdletBinding(DefaultParameterSetName='Parameter Set 1', 
                  SupportsShouldProcess=$true, 
                  PositionalBinding=$false,
                  HelpUri = 'http://www.microsoft.com/',
                  ConfirmImpact='Medium')]
    [OutputType([String])]
    Param
    (
        # Param1 help description
        [Parameter(Mandatory=$true, 
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true, 
                   ValueFromRemainingArguments=$false, 
                   Position=0,
                   ParameterSetName='Parameter Set 1')]
        [ValidateNotNull()]
        [ValidateNotNullOrEmpty()]
        [ValidateCount(0,5)]
        [ValidateSet("sun", "moon", "earth")]
        [Alias("p1")] 
        $Param1,

        # Param2 help description
        [Parameter(ParameterSetName='Parameter Set 1')]
        [AllowNull()]
        [AllowEmptyCollection()]
        [AllowEmptyString()]
        [ValidateScript({$true})]
        [ValidateRange(0,5)]
        [int]
        $Param2,

        # Param3 help description
        [Parameter(ParameterSetName='Another Parameter Set')]
        [ValidatePattern("[a-z]*")]
        [ValidateLength(0,15)]
        [String]
        $Param3
    )

    Begin
    {
    }
    Process
    {
        if ($pscmdlet.ShouldProcess("Target", "Operation"))
        {
        }
    }
    End
    {
    }
}

```

