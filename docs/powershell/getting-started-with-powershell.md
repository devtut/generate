---
metaTitle: "PowerShell - Getting started with PowerShell"
description: "Allow scripts stored on your machine to run un-signed, Aliases & Similar Functions, The Pipeline - Using Output from a PowerShell cmdlet, Calling .Net Library Methods, Installation or Setup, Commenting, Creating Objects"
---

# Getting started with PowerShell



## Allow scripts stored on your machine to run un-signed


For security reasons, PowerShell is set up by default to only allow signed scripts to execute. Executing the following command will allow you to run unsigned scripts (you must run PowerShell as Administrator to do this).

```powershell
Set-ExecutionPolicy RemoteSigned

```

Another way to run PowerShell scripts is to use `Bypass` as `ExecutionPolicy`:

```powershell
powershell.exe -ExecutionPolicy Bypass -File "c:\MyScript.ps1"

```

Or from within your existing PowerShell console or ISE session by running:

```

Set-ExecutionPolicy Bypass Process

```

A temporary workaround for execution policy can also be achieved by running the Powershell executable and passing any valid policy as `-ExecutionPolicy` parameter. The policy is in effect only during process' lifetime, so no administrative access to the registry is needed.

```powershell
C:\>powershell -ExecutionPolicy RemoteSigned

```

There are multiple other policies available, and sites online often encourage you to use `Set-ExecutionPolicy Unrestricted`.  This policy stays in place until changed, and lowers the system security stance.  This is not advisable.  Use of `RemoteSigned` is recommended because it allows locally stored and written code, and requires remotely acquired code be signed with a certificate from a trusted root.

Also, beware that the Execution Policy may be enforced by Group Policy, so that even if the policy is changed to `Unrestricted` system-wide, Group Policy may revert that setting at its next enforcement interval (typically 15 minutes).  You can see the execution policy set at the various scopes using `Get-ExecutionPolicy -List`

TechNet Documentation:<br />
[Set-ExecutionPolicy](https://technet.microsoft.com/en-us/library/hh849812.aspx)<br />
[about_Execution_Policies](https://technet.microsoft.com/en-us/library/hh847748.aspx)



## Aliases & Similar Functions


In PowerShell, there are many ways to achieve the same result.  This can be illustrated nicely with the simple & familiar `Hello World` example:

Using `Write-Host`:

```powershell
Write-Host "Hello World"  

```

Using `Write-Output`:

```powershell
Write-Output 'Hello world'

```

It's worth noting that although `Write-Output` & `Write-Host` both write to the screen there is a subtle difference.  `Write-Host` writes **only** to stdout (i.e. the console screen), whereas `Write-Output` writes to both stdout **AND** to the output [success] stream allowing for [redirection](https://blogs.technet.microsoft.com/heyscriptingguy/2014/03/30/understanding-streams-redirection-and-write-host-in-powershell/).  Redirection (and streams in general) allow for the output of one command to be directed as input to another including assignment to a variable.

```powershell
> $message = Write-Output "Hello World"
> $message
"Hello World"

```

These similar functions are not aliases, but can produce the same results if one wants to avoid "polluting" the success stream.

`Write-Output` is aliased to `Echo` or `Write`

```powershell
Echo 'Hello world'
Write 'Hello world'

```

Or, by simply typing 'Hello world'!

```powershell
'Hello world'

```

All of which will result with the expected console output

```powershell
Hello world

```

Another example of aliases in PowerShell is the common mapping of both older command prompt commands and BASH commands to PowerShell cmdlets.  All of the following produce a directory listing of the current directory.

```powershell
C:\Windows> dir
C:\Windows> ls
C:\Windows> Get-ChildItem

```

Finally, you can create your own alias with the Set-Alias cmdlet!  As an example let's alisas `Test-NetConnection`, which is essentially the PowerShell equivalent to the command prompt's ping command, to "ping".

```powershell
Set-Alias -Name ping -Value Test-NetConnection

```

Now you can use `ping` instead of `Test-NetConnection`!  Be aware that if the alias is already in use, you'll overwrite the association.

The Alias will be alive, till the session is active. Once you close the session and try to run the alias which you have created in your last session, it will not work. To overcome this issue, you can import all your aliases from an excel into your session once, before starting your work.



## The Pipeline - Using Output from a PowerShell cmdlet


One of the first questions people have when they begin to use PowerShell for scripting is how to manipulate the output from a cmdlet to perform another action.

The pipeline symbol `|` is used at the end of a cmdlet to take the data it exports and feed it to the next cmdlet.  A simple example is using Select-Object to only show the Name property of a file shown from Get-ChildItem:

```powershell
Get-ChildItem | Select-Object Name
#This may be shortened to:
gci | Select Name

```

More advanced usage of the pipeline allows us to pipe the output of a cmdlet into a foreach loop:

```powershell
Get-ChildItem | ForEach-Object {
    Copy-Item -Path $_.FullName -destination C:\NewDirectory\ 
}

#This may be shortened to:
gci | % { Copy $_.FullName C:\NewDirectory\ }

```

Note that the example above uses the $_ automatic variable.  $_ is the short alias of $PSItem which is an automatic variable which contains the current item in the pipeline.



## Calling .Net Library Methods


Static .Net library methods can be called from PowerShell by encapsulating the full class name in third bracket and then calling the method using `::`

```powershell
#calling Path.GetFileName()
C:\> [System.IO.Path]::GetFileName('C:\Windows\explorer.exe')
explorer.exe

```

Static methods can be called from the class itself, but calling non-static methods requires an instance of the .Net class (an object).

For example, the AddHours method cannot be called from the System.DateTime class itself. It requires an instance of the class :

```powershell
C:\> [System.DateTime]::AddHours(15)
Method invocation failed because [System.DateTime] does not contain a method named 'AddHours'.
At line:1 char:1
+ [System.DateTime]::AddHours(15)
+ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : InvalidOperation: (:) [], RuntimeException
    + FullyQualifiedErrorId : MethodNotFound

```

In this case, we first [create an object](http://stackoverflow.com/documentation/powershell/822/getting-started-with-powershell/17893/creating-objects), for example :

```powershell
C:\> $Object = [System.DateTime]::Now

```

Then, we can use methods of that object, even methods which cannot be called directly from the System.DateTime class, like the AddHours method :

```powershell
C:\> $Object.AddHours(15)

Monday 12 September 2016 01:51:19

```



## Installation or Setup


### Windows

PowerShell is included with the Windows Management Framework.  Installation and Setup are not required on modern versions of Windows.

Updates to PowerShell can be accomplished by installing a newer version of the Windows Management Framework.

### Other Platforms

"Beta" version of PowerShell 6 can be installed on other platforms. The installation packages are available [here](https://github.com/PowerShell/PowerShell#get-powershell).

For example, PowerShell 6, for Ubuntu 16.04, is published to package repositories for easy installation (and updates).

To install run the following:

After registering the Microsoft repository once as superuser,
from then on, you just need to use `sudo apt-get upgrade powershell` to update it.
Then just run `powershell`



## Commenting


To comment on power scripts by prepending the line using the `#` (hash) symbol

```powershell
# This is a comment in powershell
Get-ChildItem

```

You can also have multi-line comments using `<#` and `#>` at the beginning and end of the comment respectively.

```powershell
<#
This is a 
multi-line
comment
#>
Get-ChildItem

```



## Creating Objects


The `New-Object` cmdlet is used to create an object.

```powershell
# Create a DateTime object and stores the object in variable "$var"
$var = New-Object System.DateTime

# calling constructor with parameters
$sr = New-Object System.IO.StreamReader -ArgumentList "file path"

```

In many instances, a new object will be created in order to export data or pass it to another commandlet.  This can be done like so:

```powershell
$newObject = New-Object -TypeName PSObject -Property @{
    ComputerName = "SERVER1"
    Role = "Interface"
    Environment = "Production"
}

```

There are many ways of creating an object. The following method is probably the shortest and fastest way to create a `PSCustomObject`:

```powershell
$newObject = [PSCustomObject]@{
    ComputerName = 'SERVER1'
    Role         = 'Interface'
    Environment  = 'Production'
}

```

In case you already have an object, but you only need one or two extra properties, you can simply add that property by using `Select-Object`:

```powershell
Get-ChildItem | Select-Object FullName, Name, 
    @{Name='DateTime'; Expression={Get-Date}}, 
    @{Name='PropertieName'; Expression={'CustomValue'}}

```

All objects can be stored in variables or passed into the pipeline. You could also add these objects to a collection and then show the results at the end.

Collections of objects work well with Export-CSV (and Import-CSV).  Each line of the CSV is an object, each column a property.

Format commands convert objects into text stream for display.  Avoid using Format-* commands until the final step of any data processing, to maintain the usability of the objects.



#### Remarks


**Windows PowerShell** is a shell and scripting component of the Windows Management Framework, an automation/configuration management framework from Microsoft built on the .NET Framework. PowerShell is installed by default on all supported versions of Windows client and server operating systems since Windows 7 / Windows Server 2008 R2. Powershell can be updated at any time by downloading a later version of the [Windows Management Framework](https://msdn.microsoft.com/en-us/powershell/) (WMF). The "Alpha" version of PowerShell 6 is cross-platform (Windows, Linux, and OS X) and needs to be downloaded and installed from [this release page](https://github.com/PowerShell/PowerShell/releases).

Additional resources:

- MSDN Documentation: [https://msdn.microsoft.com/en-us/powershell/scripting/powershell-scripting](https://msdn.microsoft.com/en-us/powershell/scripting/powershell-scripting)
<li>TechNet: [https://technet.microsoft.com/en-us/scriptcenter/dd742419.aspx](https://technet.microsoft.com/en-us/scriptcenter/dd742419.aspx)
<ul>
- [About pages](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.core/about/about_aliases)

