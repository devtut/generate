---
metaTitle: "PowerShell - Naming Conventions"
description: "Functions"
---

# Naming Conventions




## Functions


```powershell
Get-User()

```


- Use **Verb-Noun** pattern while naming a function.
- Verb implies an action e.g. `Get`, `Set`, `New`, `Read`, `Write` and many more. See [approved verbs](https://msdn.microsoft.com/en-us/library/ms714428(v=vs.85).aspx).
- Noun should be singular even if it acts on multiple items. `Get-User()` may return one or multiple users.
- Use Pascal case for both Verb and Noun. E.g. `Get-UserLogin()`

