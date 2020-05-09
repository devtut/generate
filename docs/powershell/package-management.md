---
metaTitle: "PowerShell - Package management"
description: "Find a PowerShell module using a pattern, Create the default PowerShell Module Reposity, Find a module by name, Install a Module by name, Uninstall a module my name and version, Update a module by name"
---

# Package management


PowerShell Package Management allows you to find, install, update and uninstall PowerShell Modules and other packages.

[PowerShellGallery.com](https://PowerShellGallery.com) is the default source for PowerShell modules. You can also browse the site for available packages, command and preview the code.



## Find a PowerShell module using a pattern


To find a module that ends with `DSC`

```powershell
Find-Module -Name *DSC

```



## Create the default PowerShell Module Reposity


If for some reason, the default PowerShell module repository `PSGallery` gets removed.  You will need to create it.  This is the command.

```powershell
Register-PSRepository -Default 

```



## Find a module by name


```powershell
Find-Module -Name <Name>

```



## Install a Module by name


```powershell
Install-Module -Name <name>

```



## Uninstall a module my name and version


```powershell
Uninstall-Module -Name <Name> -RequiredVersion <Version>

```



## Update a module by name


```powershell
Update-Module -Name <Name>

```

