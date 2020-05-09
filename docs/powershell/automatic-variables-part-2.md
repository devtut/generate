---
metaTitle: "PowerShell - Automatic Variables - part 2"
description: "$PSVersionTable"
---

# Automatic Variables - part 2


Topic "Automatic Variables" already has 7 examples listed and we can't add more.  This topic will have a continuation of Automatic Variables.

Automatic Variables are variables that store state information for PowerShell. These variables are created and maintained by Windows PowerShell.



## $PSVersionTable


Contains a read-only hash table (Constant, AllScope) that displays details about the version of PowerShell that is running in the current session.

```powershell
$PSVersionTable    #this call results in this:
Name                           Value
----                           -----
PSVersion                      5.0.10586.117
PSCompatibleVersions           {1.0, 2.0, 3.0, 4.0...}
BuildVersion                   10.0.10586.117
CLRVersion                     4.0.30319.42000
WSManStackVersion              3.0
PSRemotingProtocolVersion      2.3
SerializationVersion           1.1.0.1

```

The fastest way to get a version of PowerShell running:

```powershell
$PSVersionTable.PSVersion  
# result :
Major  Minor  Build  Revision
-----  -----  -----  --------
5      0      10586  117

```



#### Remarks


Not sure if this is the best way to handle documenting Automatic Variables, yet this is better than nothing.  Please comment if you find a better way :)

