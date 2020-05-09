---
metaTitle: "PowerShell - PowerShell Functions"
description: "Basic Parameters, Advanced Function, Mandatory Parameters, Parameter Validation, Simple Function with No Parameters"
---

# PowerShell Functions


A function is basically a named block of code. When you call the function name, the script block within that function runs. It is a list of PowerShell statements that has a name that you assign. When you run a function, you type the function name.It is a method of saving time when tackling repetitive tasks. PowerShell formats in three parts: the keyword 'Function', followed by a Name, finally, the payload containing the script block, which is enclosed by curly/parenthesis style bracket.



## Basic Parameters


A function can be defined with parameters using the param block:

```powershell
function Write-Greeting {
    param(
        [Parameter(Mandatory,Position=0)]
        [String]$name,
        [Parameter(Mandatory,Position=1)]
        [Int]$age
    )
    "Hello $name, you are $age years old."
}

```

Or using the simple function syntax:

```powershell
function Write-Greeting ($name, $age) {
    "Hello $name, you are $age years old."
}

```

**Note:** Casting parameters is not required in either type of parameter definition.

Simple function syntax (SFS) has very limited capabilities in comparison to the param block.<br />
Though you can define parameters to be exposed within the function, you cannot specify [Parameter Attributes](https://msdn.microsoft.com/en-us/library/ms714348(v=vs.85).aspx), utilize [Parameter Validation](https://msdn.microsoft.com/en-gb/library/ms714432(v=vs.85).aspx), include `[CmdletBinding()]`, with SFS (and this is a non-exhaustive list).

Functions can be invoked with ordered or named parameters.

The order of the parameters on the invocation is matched to the order of the declaration in the function header (by default), or can be specified using the `Position` Parameter Attribute (as shown in the advanced function example, above).

```powershell
$greeting = Write-Greeting "Jim" 82

```

Alternatively, this function can be invoked with named parameters

```powershell
$greeting = Write-Greeting -name "Bob" -age 82

```



## Advanced Function


This is a copy of the advanced function snippet from the Powershell ISE.  Basically this is a template for many of the things you can use with advanced functions in Powershell.  Key points to note:

- get-help integration - the beginning of the function contains a comment block that is set up to be read by the get-help cmdlet.  The function block may be located at the end, if desired.
- cmdletbinding - function will behave like a cmdlet
- parameters
- parameter sets

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
    [Alias()]
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



## Mandatory Parameters


Parameters to a function can be marked as mandatory

```powershell
function Get-Greeting{
    param
    (
        [Parameter(Mandatory=$true)]$name
    )
    "Hello World $name"
}

```

If the function is invoked without a value, the command line will prompt for the value:

```powershell
$greeting = Get-Greeting 

cmdlet Get-Greeting at command pipeline position 1
Supply values for the following parameters:
name: 

```



## Parameter Validation


There are a variety of ways to validate parameter entry, in PowerShell.

Instead of writing code within functions or scripts to validate parameter values, these ParameterAttributes will throw if invalid values are passed.

### ValidateSet

Sometimes we need to restrict the possible values that a parameter can accept. Say we want to allow only red, green and blue for the `$Color` parameter in a script or function.

We can use the `ValidateSet` parameter attribute to restrict this. It has the additional benefit of allowing tab completion when setting this argument (in some environments).

```powershell
param(
    [ValidateSet('red','green','blue',IgnoreCase)]
    [string]$Color
)

```

You can also specify `IgnoreCase` to disable case sensitivity.

### ValidateRange

This method of parameter validation takes a min and max Int32 value, and requires the parameter to be within that range.

```powershell
param(
    [ValidateRange(0,120)]
    [Int]$Age
)

```

### ValidatePattern

This method of parameter validation accepts parameters that match the regex pattern specified.

```powershell
param(
    [ValidatePattern("\w{4-6}\d{2}")]
    [string]$UserName
)

```

### ValidateLength

This method of parameter validation tests the length of the passed string.

```powershell
param(
    [ValidateLength(0,15)]
    [String]$PhoneNumber
)

```

### ValidateCount

This method of parameter validation tests the amount of arguments passed in, for example, an array of strings.

```powershell
param(
    [ValidateCount(1,5)]
    [String[]]$ComputerName
)

```

### ValidateScript

Finally, the ValidateScript method is extraordinarily flexible, taking a scriptblock and evaluating it using $_ to represent the passed argument. It then passes the argument if the result is $true (including any output as valid).

This can be used to test that a file exists:

```powershell
param(
    [ValidateScript({Test-Path $_})]
    [IO.FileInfo]$Path 
)

```

To check that a user exists in AD:

```powershell
param(
    [ValidateScript({Get-ADUser $_})]
    [String]$UserName
)

```

And pretty much anything else you can write (as it's not restricted to oneliners):

```powershell
param(
    [ValidateScript({
        $AnHourAgo = (Get-Date).AddHours(-1)
        if ($_ -lt $AnHourAgo.AddMinutes(5) -and $_ -gt $AnHourAgo.AddMinutes(-5)) {
            $true
        } else {
            throw "That's not within five minutes. Try again."
        }
    })]
    [String]$TimeAboutAnHourAgo
)

```



## Simple Function with No Parameters


This is an example of a function which returns a string.  In the example, the function is called in a statement assigning a value to a variable.  The value in this case is the return value of the function.

```powershell
function Get-Greeting{
    "Hello World"
}

# Invoking the function
$greeting = Get-Greeting

# demonstrate output
$greeting
Get-Greeting

```

`function` declares the following code to be a function.

`Get-Greeting` is the name of the function.  Any time that function needs to be used in the script, the function can be called by means of invoking it by name.

`{ ... }` is the script block that is executed by the function.

If the above code is executed in the ISE, the results would be something like:

```powershell
Hello World
Hello World

```

