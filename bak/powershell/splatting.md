---
metaTitle: "PowerShell - Splatting"
description: "Piping and Splatting, Passing a Switch parameter using Splatting, Splatting From Top Level Function to a Series of Inner Function, Splatting parameters"
---

# Splatting


Splatting is a method of passing multiple parameters to a command as a single unit. This is done by storing the parameters and their values as key-value pairs in a [hashtable](http://stackoverflow.com/documentation/powershell/8083/hashtables) and splatting it to a cmdlet using the splatting operator `@`.

Splatting can make a command more readable and allows you to reuse parameters in mulitple command calls.



## Piping and Splatting


Declaring the splat is useful for reusing sets of parameters multiple times or with slight variations:

```powershell
$splat = @{
   Class = "Win32_SystemEnclosure"
   Property = "Manufacturer"
   ErrorAction = "Stop"
}

Get-WmiObject -ComputerName $env:COMPUTERNAME @splat
Get-WmiObject -ComputerName "Computer2" @splat
Get-WmiObject -ComputerName "Computer3" @splat

```

However, if the splat is not indented for reuse, you may not wish to declare it. It can be piped instead:

```powershell
@{
   ComputerName = $env:COMPUTERNAME
   Class = "Win32_SystemEnclosure"
   Property = "Manufacturer"
   ErrorAction = "Stop"
} | % { Get-WmiObject @_ }

```



## Passing a Switch parameter using Splatting


To use Splatting to call `Get-Process` with the `-FileVersionInfo` switch similar to this:

```powershell
Get-Process -FileVersionInfo

```

This is the call using splatting:

```powershell
$MyParameters = @{
    FileVersionInfo = $true
}

Get-Process @MyParameters

```

**Note:** This is useful because you can create a default set of paramaters and make the call many times  like this

```powershell
$MyParameters = @{
    FileVersionInfo = $true
}

Get-Process @MyParameters -Name WmiPrvSE
Get-Process @MyParameters -Name explorer

```



## Splatting From Top Level Function to a Series of Inner Function


Without splatting it is very cumbersome to try and pass values down through the call stack. But if you combine splatting with the power of the **@PSBoundParameters** then you can pass the top level parameter collection down through the layers.

```powershell
Function Outer-Method
{
    Param
    (
        [string]
        $First,
        
        [string]
        $Second
    )
    
    Write-Host ($First) -NoNewline
    
    Inner-Method @PSBoundParameters
}

Function Inner-Method
{
    Param
    (
        [string]
        $Second
    )
    
    Write-Host (" {0}!" -f $Second)
}

$parameters = @{
    First = "Hello"
    Second = "World"
}


Outer-Method @parameters

```



## Splatting parameters


Splatting is done by replacing the dollar-sign `$` with the splatting operator `@` when using a variable containing a [HashTable](http://stackoverflow.com/documentation/powershell/8083/hashtables) of parameters and values in a command call.

```powershell
$MyParameters = @{
    Name = "iexplore"
    FileVersionInfo = $true
}

Get-Process @MyParameters

```

Without splatting:

```powershell
Get-Process -Name "iexplore" -FileVersionInfo

```

You can combine normal parameters with splatted parameters to easily add common parameters to your calls.

```powershell
$MyParameters = @{
    ComputerName = "StackOverflow-PC"
}

Get-Process -Name "iexplore" @MyParameters

Invoke-Command -ScriptBlock { "Something to excute remotely" } @MyParameters

```



#### Remarks


**Note:** The [Array expression operator or `@()`](http://stackoverflow.com/documentation/powershell/8981/special-operators/27942/array-expression-operator) have very different behavior than the Splatting operator `@`.

Read more at [about_Splatting @ TechNet](https://technet.microsoft.com/en-us/library/jj672955.aspx)

