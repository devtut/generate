---
metaTitle: "PowerShell - WMI and CIM"
description: "Querying objects, Classes and namespaces"
---

# WMI and CIM



## Querying objects


CIM/WMI is most commonly used to query information or configuration on a device. Thof a class that represents a configuration, process, user etc. In PowerShell there are multiple ways to access these classes and instances, but the most common ways are by using the `Get-CimInstance` (CIM) or `Get-WmiObject` (WMI) cmdlets.

### List all objects for CIM-class

You can list all instances of a class.

**CIM:**

```powershell
> Get-CimInstance -ClassName Win32_Process

ProcessId Name                         HandleCount WorkingSetSize VirtualSize  
--------- ----                         ----------- -------------- -----------  
0         System Idle Process          0           4096           65536        
4         System                       1459        32768          3563520      
480       Secure System                0           3731456        0            
484       smss.exe                     52          372736         2199029891072
....
....

```

**WMI:**

```powershell
Get-WmiObject -Class Win32_Process

```

### Using a filter

You can apply a filter to only get specific instances of a CIM/WMI-class. Filters are written using `WQL` (default) or CQL (add `-QueryDialect CQL`). `-Filter` uses the `WHERE`-part of a full WQL/CQL-query.

**CIM:**

```powershell
Get-CimInstance -ClassName Win32_Process -Filter "Name = 'powershell.exe'"

ProcessId Name           HandleCount WorkingSetSize VirtualSize  
--------- ----           ----------- -------------- -----------  
4800      powershell.exe 676         88305664       2199697199104

```

**WMI:**

```powershell
Get-WmiObject -Class Win32_Process -Filter "Name = 'powershell.exe'"

...
Caption                    : powershell.exe
CommandLine                : "C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe" 
CreationClassName          : Win32_Process
CreationDate               : 20160913184324.393887+120
CSCreationClassName        : Win32_ComputerSystem
CSName                     : STACKOVERFLOW-PC
Description                : powershell.exe
ExecutablePath             : C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe
ExecutionState             : 
Handle                     : 4800
HandleCount                : 673
....

```

### Using a WQL-query:

You can also use a WQL/CQL-query to query and filter instances.

**CIM:**

```powershell
Get-CimInstance -Query "SELECT * FROM Win32_Process WHERE Name = 'powershell.exe'"

ProcessId Name           HandleCount WorkingSetSize VirtualSize  
--------- ----           ----------- -------------- -----------  
4800      powershell.exe 673         88387584       2199696674816

```

Querying objects in a different namespace:

**CIM:**

```powershell
> Get-CimInstance -Namespace "root/SecurityCenter2" -ClassName AntiVirusProduct


displayName              : Windows Defender
instanceGuid             : {D68DDC3A-831F-4fae-9E44-DA132C1ACF46}
pathToSignedProductExe   : %ProgramFiles%\Windows Defender\MSASCui.exe
pathToSignedReportingExe : %ProgramFiles%\Windows Defender\MsMpeng.exe
productState             : 397568
timestamp                : Fri, 09 Sep 2016 21:26:41 GMT
PSComputerName           : 

```

**WMI:**

```powershell
> Get-WmiObject -Namespace "root\SecurityCenter2" -Class AntiVirusProduct

__GENUS                  : 2
__CLASS                  : AntiVirusProduct
__SUPERCLASS             : 
__DYNASTY                : AntiVirusProduct
__RELPATH                : AntiVirusProduct.instanceGuid="{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}"
__PROPERTY_COUNT         : 6
__DERIVATION             : {}
__SERVER                 : STACKOVERFLOW-PC
__NAMESPACE              : ROOT\SecurityCenter2
__PATH                   : \\STACKOVERFLOW-PC\ROOT\SecurityCenter2:AntiVirusProduct.instanceGuid="{D68DDC3A-831F-4fae-9E44-DA132C1ACF46}"
displayName              : Windows Defender
instanceGuid             : {D68DDC3A-831F-4fae-9E44-DA132C1ACF46}
pathToSignedProductExe   : %ProgramFiles%\Windows Defender\MSASCui.exe
pathToSignedReportingExe : %ProgramFiles%\Windows Defender\MsMpeng.exe
productState             : 397568
timestamp                : Fri, 09 Sep 2016 21:26:41 GMT
PSComputerName           : STACKOVERFLOW-PC

```



## Classes and namespaces


There are many classes available in CIM and WMI which are separated into multiple namespaces. The most common (and default) namespace in Windows is `root/cimv2`. To find the righ class, it can useful to list all or search.

### List available classes

You can list all available classes in the default namespace (`root/cimv2`) on a computer.

**CIM:**

```powershell
Get-CimClass

```

**WMI:**

```powershell
Get-WmiObject -List

```

### Search for a class

You can search for specific classes using wildcards. Ex: Find classes containing the word `process`.

**CIM:**

```powershell
> Get-CimClass -ClassName "*Process*"

   NameSpace: ROOT/CIMV2

CimClassName                        CimClassMethods      CimClassProperties                                                           
------------                        ---------------      ------------------                                                           
Win32_ProcessTrace                  {}                   {SECURITY_DESCRIPTOR, TIME_CREATED, ParentProcessID, ProcessID...}           
Win32_ProcessStartTrace             {}                   {SECURITY_DESCRIPTOR, TIME_CREATED, ParentProcessID, ProcessID...}           
Win32_ProcessStopTrace              {}                   {SECURITY_DESCRIPTOR, TIME_CREATED, ParentProcessID, ProcessID...}           
CIM_Process                         {}                   {Caption, Description, InstallDate, Name...}                                 
Win32_Process                       {Create, Terminat... {Caption, Description, InstallDate, Name...}                                 
CIM_Processor                       {SetPowerState, R... {Caption, Description, InstallDate, Name...}                                 
Win32_Processor                     {SetPowerState, R... {Caption, Description, InstallDate, Name...}
...

```

**WMI:**

```powershell
Get-WmiObject -List -Class "*Process*"

```

### List classes in a different namespace

The root namespace is simply called `root`. You can list classes in another namespace using the `-NameSpace` parameter.

**CIM:**

```powershell
> Get-CimClass -Namespace "root/SecurityCenter2"   

   NameSpace: ROOT/SecurityCenter2

CimClassName                        CimClassMethods      CimClassProperties                                                           
------------                        ---------------      ------------------
....                                                   
AntiSpywareProduct                  {}                   {displayName, instanceGuid, pathToSignedProductExe, pathToSignedReportingE...
AntiVirusProduct                    {}                   {displayName, instanceGuid, pathToSignedProductExe, pathToSignedReportingE...
FirewallProduct                     {}                   {displayName, instanceGuid, pathToSignedProductExe, pathToSignedReportingE...

```

**WMI:**

```powershell
Get-WmiObject -Class "__Namespace" -Namespace "root"

```

### List available namespaces

To find available child-namespaces of `root` (or another namespace), query the objects in the `__NAMESPACE`-class for that namespace.

**CIM:**

```powershell
> Get-CimInstance -Namespace "root" -ClassName "__Namespace"

Name            PSComputerName
----            --------------
subscription                  
DEFAULT                       
CIMV2                         
msdtc                         
Cli                           
SECURITY                      
HyperVCluster                 
SecurityCenter2               
RSOP                          
PEH                           
StandardCimv2                 
WMI                           
directory                     
Policy                        
virtualization                
Interop                       
Hardware                      
ServiceModel                  
SecurityCenter                
Microsoft                     
aspnet                        
Appv

```

**WMI:**

```powershell
Get-WmiObject -List -Namespace "root"

```



#### Remarks


### CIM vs WMI

As of PowerShell 3.0, there are two ways to work with management classes in PowerShell, WMI and CIM. PowerShell 1.0 and 2.0 only supported the WMI-module which is now superseeded by the new and improved CIM-module. In a later release of PowerShell, the WMI-cmdlets will be removed.

Comparison of CIM and WMI-modules:

|CIM-cmdlet|WMI-cmdlet|What it does
|---|---|---|---|---|---|---|---|---|---
|Get-CimInstance|Get-WmiObject|Gets CIM/WMI-objects for a class
|Invoke-CimMethod|Invoke-WmiMethod|Invokes a CIM/WMI-class method
|Register-CimIndicationEvent|Register-WmiEvent|Registers event for a CIM/WMI-class
|Remove-CimInstance|Remove-WmiObject|Remove CIM/WMI-object
|Set-CimInstance|Set-WmiInstance|Updates/Saves CIM/WMI-object
|Get-CimAssociatedInstance|N/A|Get associated instances (linked object/classes)
|Get-CimClass|Get-WmiObject -List|List CIM/WMI-classes
|New-CimInstance|N/A|Create new CIM-object
|Get-CimSession|N/A|Lists CIM-sessions
|New-CimSession|N/A|Create new CIM-session
|New-CimSessionOption|N/A|Creates object with session options; protocol, encoding, disable encryption etc. (for use with `New-CimSession`)
|Remove-CimSession|N/A|Removes/Stops CIM-session

### Additional resources

[Should I use CIM or WMI with Windows PowerShell? @ Hey, Scripting Guy! Blog](https://blogs.technet.microsoft.com/heyscriptingguy/2016/02/08/should-i-use-cim-or-wmi-with-windows-powershell/)

