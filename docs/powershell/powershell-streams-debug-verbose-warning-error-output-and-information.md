---
metaTitle: "PowerShell - PowerShell Streams; Debug, Verbose, Warning, Error, Output and Information"
description: "Write-Output, Write Preferences"
---

# PowerShell "Streams"; Debug, Verbose, Warning, Error, Output and Information



## Write-Output


`Write-Output` generates output. This output can go to the next command after the pipeline or to the console so it's simply displayed.

The Cmdlet sends objects down the primary pipeline, also known as the "output stream" or the "success pipeline." To send error objects down the error pipeline, use Write-Error.

```powershell
# 1.) Output to the next Cmdlet in the pipeline
Write-Output 'My text' | Out-File -FilePath "$env:TEMP\Test.txt"

Write-Output 'Bob' | ForEach-Object {
    "My name is $_"
}

# 2.) Output to the console since Write-Output is the last command in the pipeline
Write-Output 'Hello world'

# 3.) 'Write-Output' CmdLet missing, but the output is still considered to be 'Write-Output'
'Hello world'

```


1. The Write-Output cmdlet sends the specified object down the pipeline to the next command.
1. If the command is the last command in the pipeline, the object is displayed in the console.
1. The PowerShell interpreter treats this as an implicit Write-Output.

Because `Write-Output`'s default behavior is to display the objects at the end of a pipeline, it is generally not necessary to use the Cmdlet. For example, `Get-Process | Write-Output` is equivalent to `Get-Process`.



## Write Preferences


Messages can be written with;

```powershell
Write-Verbose "Detailed Message"
Write-Information "Information Message"
Write-Debug "Debug Message"
Write-Progress "Progress Message"
Write-Warning "Warning Message"

```

Each of these has a preference variable;

```powershell
$VerbosePreference = "SilentlyContinue"
$InformationPreference = "SilentlyContinue"
$DebugPreference = "SilentlyContinue"
$ProgressPreference = "Continue"
$WarningPreference = "Continue"

```

The preference variable controls how the message and subsequent execution of the script are handled;

```powershell
$InformationPreference = "SilentlyContinue"
Write-Information "This message will not be shown and execution continues"

$InformationPreference = "Continue"
Write-Information "This message is shown and execution continues"

$InformationPreference = "Inquire"
Write-Information "This message is shown and execution will optionally continue"

$InformationPreference = "Stop"
Write-Information "This message is shown and execution terminates"

```

The color of the messages can be controlled for `Write-Error` by setting;

```powershell
$host.PrivateData.ErrorBackgroundColor = "Black"
$host.PrivateData.ErrorForegroundColor = "Red"

```

Similar settings are available for `Write-Verbose`, `Write-Debug` and `Write-Warning`.



#### Remarks


[https://technet.microsoft.com/en-us/library/hh849921.aspx](https://technet.microsoft.com/en-us/library/hh849921.aspx)

