---
metaTitle: "PowerShell - Error handling"
description: "Error Types"
---

# Error handling




## Error Types


An error is an error, one might wonder how could there be types in it. Well, with powershell the error broadly falls into two criteria,

- Terminating error
- Non-Terminating error

As the name says Terminating errors will terminate the execution and a Non-Terminating Errors let the execution continue to next statement.

> 
<p>This is true assuming that **$ErrorActionPreference** value is default
(Continue). $ErrorActionPreference is a [Prefrence Variable](https://technet.microsoft.com/en-us/library/hh847796.aspx) which
tells powershell what to do in case of an "Non-Terminating" error.</p>


**Terminating error**

A terminating error can be handled with a typical try catch, as below

```powershell
Try
{
    Write-Host "Attempting Divide By Zero"
    1/0
}
Catch
{
    Write-Host "A Terminating Error: Divide by Zero Caught!" 
}

```

The above snippet will execute and the error will be caught thru the catch block.

**Non-Terminating Error**

A Non-Terminating error in the other hand will not be caught in the catch block by default. The reason behind that is a Non-Terminating error is not considered a critical error.

```powershell
Try
{
    Stop-Process -Id 123456
}
Catch
{
    Write-Host "Non-Terminating Error: Invalid Process ID"
}

```

If you execute the above the line you wont get the output from catch block as since the error is not considered critical and the execution will simply continue from next command. However, the error will be displayed in the console. To handle a Non-Terminating error, you simple have to change the error preference.

```powershell
Try
{
    Stop-Process -Id 123456 -ErrorAction Stop
}
Catch
{
    "Non-Terminating Error: Invalid Process ID"
}

```

Now, with the updated Error preference, this error will be considered a Terminating error and will be caught in the catch block.

**Invoking Terminating & Non-Terminating Errors:**

**Write-Error** cmdlet simply writes the error to the invoking host program. It doesn't stop the execution.  Where as **throw** will give you a terminating error and stop the execution

```powershell
Write-host "Going to try a non terminating Error "
Write-Error "Non terminating" 
Write-host "Going to try a terminating Error "
throw "Terminating Error " 
Write-host "This Line wont be displayed" 

```

