---
metaTitle: "PowerShell - Introduction to Psake"
description: "Basic outline, FormatTaskName example, Run Task conditionally, ContinueOnError"
---

# Introduction to Psake



## Basic outline


```powershell
Task Rebuild -Depends Clean, Build  {
   "Rebuild"
 }

Task Build {
   "Build"
 }

Task Clean {
   "Clean"
 }

Task default -Depends Build

```



## FormatTaskName example


```powershell
# Will display task as:
# -------- Rebuild --------
# -------- Build --------
FormatTaskName "-------- {0} --------"  

# will display tasks in yellow colour:
# Running Rebuild  
FormatTaskName {
    param($taskName)
    "Running $taskName" - foregroundcolor yellow
}

Task Rebuild -Depends Clean, Build  {
   "Rebuild"
 }

Task Build {
   "Build"
 }

Task Clean {
   "Clean"
 }

Task default -Depends Build

```



## Run Task conditionally


```powershell
propreties { 
    $isOk = $false
}

# By default the Build task won't run, unless there is a param $true
Task Build -precondition { return $isOk } {
   "Build"
 }

Task Clean {
   "Clean"
 }

Task default -Depends Build

```



## ContinueOnError


```powershell
Task Build -depends Clean {
   "Build"
 }

Task Clean -ContinueOnError {
   "Clean"
    throw "throw on purpose, but the task will continue to run"
 }

Task default -Depends Build

```



#### Syntax


- Task - main function to execute a step of your build script
- Depends - property that specify what the current step depends upon
- default - there must always be a default task that will get executed if no initial task is specified
- FormatTaskName - specifies how each step is displayed in the result window.



#### Remarks


[psake](https://github.com/psake/psake) is a build automation tool written in PowerShell, and is inspired by Rake (Ruby make) and Bake (Boo make).  It is used to create builds using dependency pattern.
Documentation available [here](http://psake.readthedocs.io/en/latest/)

