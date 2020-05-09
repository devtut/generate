---
metaTitle: "PowerShell - ActiveDirectory module"
description: "Users, Module, Groups, Computers, Objects"
---

# ActiveDirectory module




## Users


Retrieve Active Directory User

```powershell
Get-ADUser -Identity JohnSmith

```

Retrieve All Properties Associated with User

```powershell
Get-ADUser -Identity JohnSmith -Properties *

```

Retrieve Selected Properties for User

```powershell
Get-ADUser -Identity JohnSmith -Properties * | Select-Object -Property sAMAccountName, Name, Mail

```

New AD User

```powershell
New-ADUser -Name "MarySmith" -GivenName "Mary" -Surname "Smith" -DisplayName "MarySmith" -Path "CN=Users,DC=Domain,DC=Local"

```



## Module


```powershell
#Add the ActiveDirectory Module to current PowerShell Session
Import-Module ActiveDirectory 

```



## Groups


Retrieve Active Directory Group

```powershell
Get-ADGroup -Identity "My-First-Group" #Ensure if group name has space quotes are used

```

Retrieve All Properties Associated with Group

```powershell
Get-ADGroup -Identity "My-First-Group" -Properties *

```

Retrieve All Members of a Group

```powershell
Get-ADGroupMember -Identity "My-First-Group" | Select-Object -Property sAMAccountName
Get-ADgroup "MY-First-Group" -Properties Members | Select -ExpandProperty Members

```

Add AD User to an AD Group

```powershell
Add-ADGroupMember -Identity "My-First-Group" -Members "JohnSmith"

```

New AD Group

```powershell
New-ADGroup -GroupScope Universal -Name "My-Second-Group"

```



## Computers


Retrieve AD Computer

```powershell
Get-ADComputer -Identity "JohnLaptop"

```

Retrieve All Properties Associated with Computer

```powershell
Get-ADComputer -Identity "JohnLaptop" -Properties *

```

Retrieve Select Properties of Computer

```powershell
Get-ADComputer -Identity "JohnLaptop" -Properties * | Select-Object -Property Name, Enabled

```



## Objects


Retrieve an Active Directory Object

```powershell
#Identity can be ObjectGUID, Distinguished Name or many more
Get-ADObject -Identity "ObjectGUID07898" 

```

Move an Active Directory Object

```powershell
Move-ADObject -Identity "CN=JohnSmith,OU=Users,DC=Domain,DC=Local" -TargetPath "OU=SuperUser,DC=Domain,DC=Local"

```

Modify an Active Directory Object

```powershell
Set-ADObject -Identity "CN=My-First-Group,OU=Groups,DC=Domain,DC=local" -Description "This is My First Object Modification"

```



#### Remarks


Please remember that PowerShell's Help System is one of the best resources you can possibly utilize.

```powershell
Get-Help Get-ADUser -Full
Get-Help Get-ADGroup -Full
Get-Help Get-ADComputer -Full
Get-Help Get-ADObject -Full

```

All of the help documentation will provide examples, syntax and parameter help.

