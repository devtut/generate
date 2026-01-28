---
metaTitle: "PowerShell - Environment Variables"
description: "Windows environment variables are visible as a PS drive called Env:,  Instant call of Environment Variables with $env:"
---

# Environment Variables



## Windows environment variables are visible as a PS drive called Env:


You can see list with all environment variables with:<br />
Get-Childitem env:



##  Instant call of Environment Variables with $env:


```powershell
$env:COMPUTERNAME

```

