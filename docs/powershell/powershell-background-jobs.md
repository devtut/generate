---
metaTitle: "PowerShell - PowerShell Background Jobs"
description: "Basic job creation, Basic job management"
---

# PowerShell Background Jobs


Jobs were introduced in PowerShell 2.0 and helped to solve a problem inherent in the command-line tools. In a nutshell, if you start a long running task, your prompt is unavailable until the task finishes. As an example of a long running task, think of this simple PowerShell command:

**Get-ChildItem -Path c:\ -Recurse**

It will take a while to fetch full directory list of your C: drive.
If you run it as Job then the console will get the control back and you can capture the result later on.



## Basic job creation


Start a Script Block as background job:

```powershell
$job = Start-Job -ScriptBlock {Get-Process}

```

Start a script as background job:

```powershell
$job = Start-Job -FilePath "C:\YourFolder\Script.ps1"

```

Start a job using `Invoke-Command` on a remote machine:

```powershell
$job = Invoke-Command -ComputerName "ComputerName" -ScriptBlock {Get-Service winrm} -JobName "WinRM" -ThrottleLimit 16 -AsJob

```

Start job as a different user (Prompts for password):

```powershell
Start-Job -ScriptBlock {Get-Process} -Credential "Domain\Username"

```

Or

```powershell
Start-Job -ScriptBlock {Get-Process} -Credential (Get-Credential)

```

Start job as a different user (No prompt):

```powershell
$username = "Domain\Username" 
$password = "password"
$secPassword = ConvertTo-SecureString -String $password -AsPlainText -Force
$credentials = New-Object System.Management.Automation.PSCredential -ArgumentList @($username, $secPassword)
Start-Job -ScriptBlock {Get-Process} -Credential $credentials

```



## Basic job management


Get a list of all jobs in the current session:

```powershell
Get-Job

```

Waiting on a job to finish before getting the result:

```powershell
$job | Wait-job | Receive-Job 

```

Timeout a job if it runs too long (10 seconds in this example)

```powershell
$job | Wait-job -Timeout 10

```

Stopping a job (completes all tasks that are pending in that job queue before ending):

```powershell
$job | Stop-Job 

```

Remove job from current session's background jobs list:

```powershell
$job | Remove-Job

```

**Note**: The following will only work on `Workflow` Jobs.

Suspend a `Workflow` Job (Pause):

```powershell
$job | Suspend-Job 

```

Resume a `Workflow` Job:

```powershell
$job | Resume-Job 

```



#### Remarks


PowerShell Jobs run in a new process.  This has pros and cons which are related.

Pros:

1. The job runs in a clean process, including environment.
1. The job can run asynchronously to your main PowerShell process

Cons:

1. Process environment changes will not be present in the job.
<li>Parameters pass to and returned results are serialized.
<ul>
1. This means if you change a parameter object while the job is running it will not be reflected in the job.
1. This also means if an object cannot be serialized you cannot pass or return it (although PowerShell may Copy any parameters and pass/return a PSObject.)
</ul>
</li>

