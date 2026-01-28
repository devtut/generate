---
metaTitle: "PowerShell - Calculated Properties"
description: "Display file size in KB - Calculated Properties"
---

# Calculated Properties


Calculated Properties in Powershell are custom derived(Calculated) properties. It lets the user to format a certain property in a way he want it to be. The calculation(expression) can be a quite possibly anything.



## Display file size in KB - Calculated Properties


Let's consider the below snippet,

```powershell
Get-ChildItem -Path C:\MyFolder | Select-Object Name, CreationTime, Length

```

It simply output the folder content with the selected properties. Something like,

[<img src="https://i.stack.imgur.com/4IJGG.png" alt="Plain Properties" />](https://i.stack.imgur.com/4IJGG.png)

What if I want to display the file size in KB ? This is where calcualted properties comes handy.

```powershell
Get-ChildItem C:\MyFolder | Select-Object Name, @{Name="Size_In_KB";Expression={$_.Length / 1Kb}}

```

Which produces,

[<img src="https://i.stack.imgur.com/KPeVM.png" alt="enter image description here" />](https://i.stack.imgur.com/KPeVM.png)

The `Expression` is what holds the calculation for calculated property. And yes, it can be anything!

