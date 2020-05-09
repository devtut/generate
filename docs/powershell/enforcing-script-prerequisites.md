---
metaTitle: "PowerShell - Enforcing script prerequisites"
description: "Enforce minimum version of powershell host, Enforce running the script as admininstrator"
---

# Enforcing script prerequisites



## Enforce minimum version of powershell host


```powershell
#requires -version 4

```

After trying to run this script in lower version, you will see this error message

> 
.\script.ps1 : The script 'script.ps1' cannot be run because it contained a "#requires" statement at line 1 for Windows PowerShell version 5.0. The version required by the script does not match the currently running version of Windows PowerShell version 2.0.




## Enforce running the script as admininstrator


```powershell
#requires -RunAsAdministrator

```

After trying to run this script without admin privileges, you will see this error message

> 
.\script.ps1 : The script 'script.ps1' cannot be run because it contains a "#requires" statement for running as Administrator. The current Windows PowerShell session is not running as Administrator. Start Windows PowerShell by using the Run as Administrator option, and then try running the script again.




#### Syntax


- #Requires -Version <N>[.<n>]
- #Requires –PSSnapin <PSSnapin-Name> [-Version <N>[.<n>]]
- #Requires -Modules { <Module-Name> | <Hashtable> }
- #Requires –ShellId <ShellId>
- #Requires -RunAsAdministrator



#### Remarks


`#requires` statement can be placed on any line in the script (it doesn't have to be the first line) but it must be the first statement on that line.

Multiple `#requires` statements may be used in one script.

For more reference, please refer to official documentation on Technet -  [about_about_Requires](https://technet.microsoft.com/en-us/library/hh847765(v=wps.620).aspx).

