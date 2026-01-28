---
metaTitle: "PowerShell - Powershell profiles"
description: "Create an basic profile"
---

# Powershell profiles




## Create an basic profile


A PowerShell profile is used to load user defined variables and functions automatically.

PowerShell profiles are not automatically created for users.

To create a PowerShell profile `C:>New-Item -ItemType File $profile`.

If you are in ISE you can use the built in editor `C:>psEdit $profile`

An easy way to get started with your personal profile for the current host is to save some text to path stored in the `$profile`-variable

```powershell
"#Current host, current user" > $profile

```

Further modification to the profile can be done using PowerShell ISE, notepad, Visual Studio Code or any other editor.

The `$profile`-variable returns the current user profile for the current host by default, but you can access the path to the machine-policy (all users) and/or the profile for all hosts (console, ISE, 3rd party) by using it's properties.

```powershell
PS> $PROFILE | Format-List -Force

AllUsersAllHosts       : C:\Windows\System32\WindowsPowerShell\v1.0\profile.ps1
AllUsersCurrentHost    : C:\Windows\System32\WindowsPowerShell\v1.0\Microsoft.PowerShell_profile.ps1
CurrentUserAllHosts    : C:\Users\user\Documents\WindowsPowerShell\profile.ps1
CurrentUserCurrentHost : C:\Users\user\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1
Length                 : 75

PS> $PROFILE.AllUsersAllHosts
C:\Windows\System32\WindowsPowerShell\v1.0\profile.ps1

```



#### Remarks


Profile file is a powershell script that will run while the powershell console is starting. This way we can have our environment prepared for us each time we start new powershell session.

Typical things we want to do on powershell start are:

- importing modules we use often (ActiveDirectory, Exchange, some specific DLL)
- logging
- changing the prompt
- diagnostics

There are several profile files and locations that have different uses and also hierarchy of start-up order:

|Host|User|Path|Start order|Variable
|---|---|---|---|---|---|---|---|---|---
|All|All|%WINDIR%\System32\WindowsPowerShell\v1.0\profile.ps1|1|$profile.AllUsersAllHosts
|All|Current|%USERPROFILE%\Documents\WindowsPowerShell\profile.ps1|3|$profile.CurrentUserAllHosts
|Console|All|%WINDIR%\System32\WindowsPowerShell\v1.0\Microsoft.PowerShell_profile.ps1|2|$profile.AllUsersCurrentHost
|Console|Current|%USERPROFILE%\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1|4|$profile.CurrentUserCurrentHost
|ISE|All|%WINDIR%\System32\WindowsPowerShell\v1.0\Microsoft.PowerShellISE_profile.ps1|2|$profile.AllUsersCurrentHost
|ISE|Current|%USERPROFILE%\Documents\WindowsPowerShell\Microsoft.PowerShellISE_profile.ps1|4|$profile.CurrentUserCurrentHost

