---
metaTitle: "PowerShell - PowerShell Workflows"
description: "Workflow with Input Parameters, Simple Workflow Example, Run Workflow as a Background Job, Add a Parallel Block to a Workflow"
---

# PowerShell Workflows


PowerShell Workflow is a feature that was introduced starting with PowerShell version 3.0. Workflow definitions look very similar to PowerShell function definitions, however they execute within the Windows Workflow Foundation environment, instead of directly in the PowerShell engine.

Several unique "out of box" features are included with the Workflow engine, most notably, job persistence.



## Workflow with Input Parameters


Just like PowerShell functions, workflows can accept input parameter. Input parameters can optionally be bound to a specific data type, such as a string, integer, etc. Use the standard `param` keyword to define a block of input parameters, directly after the workflow declaration.

```powershell
workflow DoSomeWork {
  param (
    [string[]] $ComputerName
  )
  Get-Process -ComputerName $ComputerName
}

DoSomeWork -ComputerName server01, server02, server03

```



## Simple Workflow Example


```powershell
workflow DoSomeWork {
  Get-Process -Name notepad | Stop-Process 
}

```

This is a basic example of a PowerShell Workflow definition.



## Run Workflow as a Background Job


PowerShell Workflows are inherently equipped with the ability to run as a background job. To call a workflow as a PowerShell background job, use the `-AsJob` parameter when invoking the workflow.

```powershell
workflow DoSomeWork {
  Get-Process -ComputerName server01
  Get-Process -ComputerName server02
  Get-Process -ComputerName server03
}

DoSomeWork -AsJob

```



## Add a Parallel Block to a Workflow


```powershell
workflow DoSomeWork {
  parallel {
    Get-Process -ComputerName server01
    Get-Process -ComputerName server02
    Get-Process -ComputerName server03
  }
}

```

One of the unique features of PowerShell Workflow is the ability to define a block of activities as parallel. To use this feature, use the `parallel` keyword inside your Workflow.

Calling workflow activities in parallel may help to improve performance of your workflow.



#### Remarks


The PowerShell Workflow feature is exclusively supported on the Microsoft Windows platform, under PowerShell Desktop Edition. PowerShell Core Edition, which is supported on Linux, Mac, and Windows, does not support the PowerShell Workflow feature.

When authoring a PowerShell Workflow, keep in mind that workflows call activities, not cmdlets. You can still call cmdlets from a PowerShell Workflow, but the Workflow Engine will implicitly wrap the cmdlet invocation in an `InlineScript` activity. You can also explicitly wrap code inside of the `InlineScript` activity, which executes PowerShell code; by default the `InlineScript` activity runs in a separate process, and returns the result to the calling Workflow.

