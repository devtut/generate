---
metaTitle: "PowerShell - Aliases"
description: "Get-Alias, Set-Alias"
---

# Aliases



## Get-Alias


To list all aliases and their functions:

```powershell
Get-Alias

```

To get all aliases for specific cmdlet:

```powershell
PS C:\> get-alias -Definition Get-ChildItem

CommandType     Name                                               Version    Source
-----------     ----                                               -------    ------
Alias           dir -> Get-ChildItem
Alias           gci -> Get-ChildItem
Alias           ls -> Get-ChildItem

```

To find aliases by matching:

```powershell
PS C:\> get-alias -Name p*

CommandType     Name                                               Version    Source
-----------     ----                                               -------    ------
Alias           popd -> Pop-Location
Alias           proc -> Get-Process
Alias           ps -> Get-Process
Alias           pushd -> Push-Location
Alias           pwd -> Get-Location

```



## Set-Alias


This cmdlet allows you to create new alternate names for exiting cmdlets

```powershell
PS C:\> Set-Alias -Name proc -Value Get-Process
PS C:\> proc

Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id  SI ProcessName
-------  ------    -----      ----- -----   ------     --  -- -----------
    292      17    13052      20444 ...19     7.94    620   1 ApplicationFrameHost
....

```

Keep in mind that any alias you create will be persisted only in current session. When you start new session you need to create your aliases again. Powershell Profiles (see [topic not yet created]) are great for these purposes.



#### Remarks


Powershell naming system has quite strict rules of naming cmdlets (Verb-Noun template; see [topic not yet created] for more information). But it is not really convenient to write `Get-ChildItems` every time you want to list files in directory interactively.<br />
Therefore Powershell enables using shortcuts - aliases - instead of cmdlet names.

You can write `ls`, `dir` or `gci` instead of `Get-ChildItem` and get the same result. Alias is equivalent to its cmdlet.

Some of the common aliases are:

|alias|cmdlet
|---|---|---|---|---|---|---|---|---|---
|%, foreach|For-EachObject
|?, where|Where-Object
|cat, gc, type|Get-Content
|cd, chdir, sl|Set-Location
|cls, clear|Clear-Host
|cp, copy, cpi|Copy-Item
|dir/ls/gci|Get-ChildItem
|echo, write|Write-Output
|fl|Format-List
|ft|Format-Table
|fw|Format-Wide
|gc, pwd|Get-Location
|gm|Get-Member
|iex|Invoke-Expression
|ii|Invoke-Item
|mv, move|Move-Item
|rm, rmdir, del, erase, rd, ri|Remove-Item
|sleep|Start-Sleep
|start, saps|Start-Process

In the table above, you can see how aliases enabled simulating commands known from other environments (cmd, bash), hence increased discoverability.

