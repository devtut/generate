---
metaTitle: "PowerShell - Common parameters"
description: "ErrorAction parameter"
---

# Common parameters



## ErrorAction parameter


Possible values are `Continue | Ignore | Inquire | SilentlyContinue | Stop | Suspend`.

Value of this parameter will determine how the cmdlet will handle non-terminating errors (those generated from Write-Error for example; to learn more about error handling see [**topic not yet created**]).

Default value (if this parameter is omitted) is `Continue`.

### -ErrorAction Continue

This option will produce an error message and will continue with execution.

```powershell
PS C:\> Write-Error "test" -ErrorAction Continue ; Write-Host "Second command"

```

[<img src="https://i.stack.imgur.com/r9jzQ.png" alt="-ErorrAction Continue" />](https://i.stack.imgur.com/r9jzQ.png)

### -ErrorAction Ignore

This option will not produce any error message and will continue with execution. Also no errors will be added to `$Error` automatic variable.<br />
This option was introduced in v3.

```powershell
PS C:\> Write-Error "test" -ErrorAction Ignore ; Write-Host "Second command"

```

[<img src="https://i.stack.imgur.com/sLtQW.png" alt="-ErorrAction Ignore" />](https://i.stack.imgur.com/sLtQW.png)

### -ErrorAction Inquire

This option will produce an error message and will prompt user to choose an action to take.

```powershell
PS C:\> Write-Error "test" -ErrorAction Inquire ; Write-Host "Second command"

```

[<img src="https://i.stack.imgur.com/ewOoW.png" alt="-ErorrAction Inquire" />](https://i.stack.imgur.com/ewOoW.png)

### -ErrorAction SilentlyContinue

This option will not produce an error message and will continue with execution. All errors will be added to `$Error` automatic variable.

```powershell
PS C:\> Write-Error "test" -ErrorAction SilentlyContinue ; Write-Host "Second command"

```

[<img src="https://i.stack.imgur.com/cfTx7.png" alt="-ErorrAction SilentlyContinue" />](https://i.stack.imgur.com/cfTx7.png)

### -ErrorAction Stop

This option will produce an error message and will not continue with execution.

```powershell
PS C:\> Write-Error "test" -ErrorAction Stop ; Write-Host "Second command"

```

[<img src="https://i.stack.imgur.com/50WP7.png" alt="-ErorrAction Stop" />](https://i.stack.imgur.com/50WP7.png)

### -ErrorAction Suspend

Only available in Powershell Workflows. When used, if the command runs into an error, the workflow is suspended. This allows investigation of such error and gives a possibility to resume the workflow. To learn more about Workflow system, see [topic not yet created].



#### Remarks


Common parameters can be used with any cmdlet (that means as soon as you mark your function as cmdlet [see `CmdletBinding()`], you get all of these parameters for free).

Here is the list of all common parameters (alias is in parenthesis after corresponding parameter):

```powershell
-Debug (db)
-ErrorAction (ea)
-ErrorVariable (ev)
-InformationAction (ia) # introduced in v5
-InformationVariable (iv) # introduced in v5
-OutVariable (ov)
-OutBuffer (ob)
-PipelineVariable (pv)
-Verbose (vb) 
-WarningAction (wa)
-WarningVariable (wv)
-WhatIf (wi)
-Confirm (cf)

```

