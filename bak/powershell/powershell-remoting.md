---
metaTitle: "PowerShell - Powershell Remoting"
description: "Enabling PowerShell Remoting, Connecting to a Remote Server via PowerShell, Run commands on a Remote Computer, A best practise for automatically cleaning-up PSSessions"
---

# Powershell Remoting




## Enabling PowerShell Remoting


PowerShell remoting must first be enabled on the server to which you wish to remotely connect.

```powershell
Enable-PSRemoting -Force

```

This command does the following:

- Runs the Set-WSManQuickConfig cmdlet, which performs the following tasks:
- Starts the WinRM service.
- Sets the startup type on the WinRM service to Automatic.
- Creates a listener to accept requests on any IP address, if one does not already exist.
- Enables a firewall exception for WS-Management communications.
- Registers the Microsoft.PowerShell and Microsoft.PowerShell.Workflow session configurations, if it they are not already registered.
- Registers the Microsoft.PowerShell32 session configuration on 64-bit computers, if it is not already registered.
- Enables all session configurations.
- Changes the security descriptor of all session configurations to allow remote access.
- Restarts the WinRM service to make the preceding changes effective.

### Only for non-domain environments

For servers in an AD Domain the PS remoting authentication is done through Kerberos ('Default'), or NTLM ('Negotiate'). If you want to allow remoting to a non-domain server you have two options.

Either set up WSMan communication over HTTPS (which requires certificate generation) or enable basic authentication which sends your credentials across the wire base64-encoded (that's basically the same as plain-text so be careful with this).

In either case you'll have to add the remote systems to your WSMan trusted hosts list.

### Enabling Basic Authentication

```powershell
Set-Item WSMan:\localhost\Service\AllowUnencrypted $true  

```

Then on the computer you wish to connect **from**, you must tell it to trust the computer you're connecting **to**.

```powershell
Set-Item WSMan:\localhost\Client\TrustedHosts '192.168.1.1,192.168.1.2'

```

```powershell
Set-Item WSMan:\localhost\Client\TrustedHosts *.contoso.com

```

```powershell
Set-Item WSMan:\localhost\Client\TrustedHosts *

```

**Important**: You must tell your client to trust the computer addressed in the way you want to connect (e.g. if you connect via IP, it must trust the IP not the hostname)



## Connecting to a Remote Server via PowerShell


Using credentials from your local computer:

```powershell
Enter-PSSession 192.168.1.1

```

Prompting for credentials on the remote computer

```powershell
Enter-PSSession 192.168.1.1 -Credential $(Get-Credential)

```



## Run commands on a Remote Computer


Once Powershell remoting is enabled (Enable-PSRemoting) You can run commands on the remote computer like this:

```powershell
Invoke-Command -ComputerName "RemoteComputerName" -ScriptBlock {
    Write host "Remote Computer Name: $ENV:ComputerName"
}

```

The above method creates a temporary session and closes it right after the command or scriptblock ends.

To leave the session open and run other command in it later, you need to create a remote session first:

```powershell
$Session = New-PSSession -ComputerName "RemoteComputerName"

```

Then you can use this session each time you invoke commands on the remote computer:

```powershell
Invoke-Command -Session $Session -ScriptBlock {
    Write host "Remote Computer Name: $ENV:ComputerName"
}

Invoke-Command -Session $Session -ScriptBlock {
    Get-Date
}

```

If you need to use different Credentials, you can add them with the `-Credential` Parameter:

```powershell
$Cred = Get-Credential
Invoke-Command -Session $Session -Credential $Cred -ScriptBlock {...}

```

### Remoting serialization warning

> 
Note:
<p>It is important to know that remoting serializes PowerShell
objects on the remote system and deserializes them on your end of the
remoting session, i.e. they are converted to XML during transport and
lose all of their methods.</p>


```powershell
$output = Invoke-Command -Session $Session -ScriptBlock {
    Get-WmiObject -Class win32_printer
}

$output | Get-Member -MemberType Method

  TypeName: Deserialized.System.Management.ManagementObject#root\cimv2\Win32_Printer

Name     MemberType Definition
----     ---------- ----------
GetType  Method     type GetType()
ToString Method     string ToString(), string ToString(string format, System.IFormatProvi...

```

Whereas you have the methods on the regular PS object:

```powershell
Get-WmiObject -Class win32_printer | Get-Member -MemberType Method

 TypeName: System.Management.ManagementObject#root\cimv2\Win32_Printer

Name                  MemberType Definition                                                                                                                          
----                  ---------- ----------                                                                                                                          
CancelAllJobs         Method     System.Management.ManagementBaseObject CancelAllJobs()                                                                              
GetSecurityDescriptor Method     System.Management.ManagementBaseObject GetSecurityDescriptor()                                                                      
Pause                 Method     System.Management.ManagementBaseObject Pause()                                                                                      
PrintTestPage         Method     System.Management.ManagementBaseObject PrintTestPage()                                                                              
RenamePrinter         Method     System.Management.ManagementBaseObject RenamePrinter(System.String NewPrinterName)                                                  
Reset                 Method     System.Management.ManagementBaseObject Reset()                                                                                      
Resume                Method     System.Management.ManagementBaseObject Resume()                                                                                     
SetDefaultPrinter     Method     System.Management.ManagementBaseObject SetDefaultPrinter()                                                                          
SetPowerState         Method     System.Management.ManagementBaseObject SetPowerState(System.UInt16 PowerState, System.String Time)                                  
SetSecurityDescriptor Method     System.Management.ManagementBaseObject SetSecurityDescriptor(System.Management.ManagementObject#Win32_SecurityDescriptor Descriptor)

```

### Argument Usage

To use arguments as parameters for the remote scripting block, one might either use the `ArgumentList` parameter of `Invoke-Command`, or use the `$Using:` syntax.

Using `ArgumentList` with unnamed parameters (i.e. in the order they are passed to the scriptblock):

```powershell
$servicesToShow = "service1"
$fileName = "C:\temp\servicestatus.csv"
Invoke-Command -Session $session -ArgumentList $servicesToShow,$fileName -ScriptBlock {
    Write-Host "Calling script block remotely with $($Args.Count)"
    Get-Service -Name $args[0]
    Remove-Item -Path $args[1] -ErrorAction SilentlyContinue -Force
}

```

Using `ArgumentList` with named parameters:

```powershell
$servicesToShow = "service1"
$fileName = "C:\temp\servicestatus.csv"
Invoke-Command -Session $session -ArgumentList $servicesToShow,$fileName -ScriptBlock {
    Param($serviceToShowInRemoteSession,$fileToDelete)

    Write-Host "Calling script block remotely with $($Args.Count)"
    Get-Service -Name $serviceToShowInRemoteSession
    Remove-Item -Path $fileToDelete -ErrorAction SilentlyContinue -Force
}

```

Using `$Using:` syntax:

```powershell
$servicesToShow = "service1"
$fileName = "C:\temp\servicestatus.csv"
Invoke-Command -Session $session -ScriptBlock {
    Get-Service $Using:servicesToShow
    Remove-Item -Path $fileName -ErrorAction SilentlyContinue -Force
}

```



## A best practise for automatically cleaning-up PSSessions


When a remote session is created via the `New-PSsession` cmdlet, the PSSession persists until the current PowerShell session ends.
Meaning that, by default, the `PSSession` and all associated resources will continue to be used until the current PowerShell session ends.

Multiple active `PSSessions` can become a strain on resources, particularly for long running or interlinked scripts that create hundreds of `PSSessions` in a single PowerShell session.

It is best practise to explicitly remove each `PSSession` after it is finished being used. [1]

The following code template utilises `try-catch-finally` in order to achieve the above, combining error handling with a secure way to ensure all created `PSSessions` are removed when they are finished being used:

```powershell
try
{
    $session = New-PSsession -Computername "RemoteMachineName"
    Invoke-Command -Session $session -ScriptBlock {write-host "This is running on $ENV:ComputerName"}
}
catch
{
    Write-Output "ERROR: $_"
}
finally
{
    if ($session)
    {
        Remove-PSSession $session
    }
}

```

References:
[1] [https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.core/new-pssession](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.core/new-pssession)



#### Remarks


- [about_Remote](https://technet.microsoft.com/en-us/library/hh847900.aspx)
- [about_RemoteFAQ](https://technet.microsoft.com/en-us/library/hh847845.aspx)
- [about_RemoteTroubleshooting](https://technet.microsoft.com/en-us/library/hh847850.aspx)

