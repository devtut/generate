---
metaTitle: "PowerShell - Using the Help System"
description: "Updating the Help System, Using Get-Help, Viewing online version of a help topic, Viewing Examples, Viewing the Full Help Page, Viewing help for a specific parameter"
---

# Using the Help System



## Updating the Help System


Beginning with PowerShell 3.0, you can download and update the offline help documentation using a single cmdlet.

```powershell
Update-Help

```

To update help on multiple computers (or computers not connected to the internet).

Run the following on a computer with the help files

`Save-Help -DestinationPath \\Server01\Share\PSHelp -Credential $Cred`

To run on many computers remotely

`Invoke-Command -ComputerName (Get-Content Servers.txt) -ScriptBlock {Update-Help -SourcePath \\Server01\Share\Help -Credential $cred}`



## Using Get-Help


`Get-Help` can be used to view help in PowerShell. You can search for cmdlets, functions, providers or other topics.

In order to view the help documentation about jobs, use:

```powershell
Get-Help about_Jobs

```

You can search for topics using wildcards. If you want to list available help topics with a title starting with `about_`, try:

```powershell
Get-Help about_*

```

If you wanted help on `Select-Object`, you would use:

```powershell
Get-Help Select-Object

```

You can also use the aliases `help` or `man`.



## Viewing online version of a help topic


You can access online help documentation using:

```powershell
Get-Help Get-Command -Online

```



## Viewing Examples


Show usage examples for a specific cmdlet.

```powershell
Get-Help Get-Command -Examples

```



## Viewing the Full Help Page


View the full documentation for the topic.

```powershell
Get-Help Get-Command -Full

```



## Viewing help for a specific parameter


You can view help for a specific parameter using:

```powershell
Get-Help Get-Content -Parameter Path

```



#### Remarks


`Get-Help` is a cmdlet for reading help topics in PowerShell.

Read more a [TechNet](https://technet.microsoft.com/en-us/library/hh849696(v=wps.640).aspx)

