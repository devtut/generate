---
metaTitle: "PowerShell - Desired State Configuration"
description: "Simple example - Enabling WindowsFeature, Starting DSC (mof) on remote machine, Importing psd1 (data file) into local variable, List available DSC Resources, Importing resources for use in DSC"
---

# Desired State Configuration



## Simple example - Enabling WindowsFeature


```powershell
configuration EnableIISFeature
{
    node localhost
    {
        WindowsFeature IIS
        {
            Ensure = “Present”
            Name = “Web-Server”                      
        }
    }
}

```

If you run this configuration in Powershell (EnableIISFeature), it will produce a localhost.mof file. This is the "compiled" configuration you can run on a machine.

To test the DSC configuration on your localhost, you can simply invoke the following:

```powershell
Start-DscConfiguration -ComputerName localhost -Wait 

```



## Starting DSC (mof) on remote machine


Starting a DSC on a remote machine is almost just as simple. Assuming you've already set up Powershell remoting (or enabled WSMAN).

```powershell
$remoteComputer = "myserver.somedomain.com"
$cred = (Get-Credential)
Start-DSCConfiguration -ServerName $remoteComputer -Credential $cred -Verbose

```

**Nb:** Assuming you have compiled a configuration for your node on your localmachine (and that the file myserver.somedomain.com.mof is present prior to starting the configuration)



## Importing psd1 (data file) into local variable


Sometimes it can be useful to test your Powershell data files and iterate through the nodes and servers.

Powershell 5 (WMF5) added this neat little feature for doing this called Import-PowerShellDataFile .

Example:

```powershell
$data = Import-PowerShellDataFile -path .\MydataFile.psd1
$data.AllNodes

```



## List available DSC Resources


To list available DSC resources on your authoring node:

```

Get-DscResource 

```

This will list all resources for all installed modules (that are in your PSModulePath) on your authoring node.

To list all available DSC resources that can be found in the online sources (PSGallery ++) on WMF 5 :

```powershell
Find-DSCResource

```



## Importing resources for use in DSC


Before you can use a resource in a configuration, you must explicitly import it. Just having it installed on your computer, will not let you use the resource implicitly.

Import a resource by using Import-DscResource .

Example showing how to import the PSDesiredStateConfiguration resource and the File resource.

```powershell
Configuration InstallPreReqs
{
   param(); # params to DSC goes here.

   Import-DscResource PSDesiredStateConfiguration 

   File CheckForTmpFolder {
        Type = 'Directory'
        DestinationPath = 'C:\Tmp'
        Ensure = "Present"
    }
 }

```

**Note**: In order for DSC Resources to work, you must have the modules installed on the target machines when running the configuration. If you don't have them installed, the configuration will fail.

