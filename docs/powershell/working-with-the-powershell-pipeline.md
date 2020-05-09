---
metaTitle: "PowerShell - Working with the PowerShell pipeline"
description: "Writing Functions with Advanced Lifecycle, Basic Pipeline Support in Functions, Working concept of pipeline"
---

# Working with the PowerShell pipeline


PowerShell introduces an object pipelining model, which allows you to send whole objects down through the pipeline to consuming commandlets or (at least) the output. In contrast to classical string-based pipelining, information in piped objects don't have to be on specific positions. Commandlets can declare to interact with Objects from the pipeline as input, while return values are sent to the pipeline automatically.



## Writing Functions with Advanced Lifecycle


This example shows how a function can accept pipelined input, and iterate efficiently.

Note, that the `begin` and `end` structures of the function are optional when pipelining, but that `process` is required when using `ValueFromPipeline` or `ValueFromPipelineByPropertyName`.

```powershell
function Write-FromPipeline{
    [CmdletBinding()]
    param(
        [Parameter(ValueFromPipeline)]
        $myInput
    )
    begin {
        Write-Verbose -Message "Beginning Write-FromPipeline" 
    }
    process {
        Write-Output -InputObject $myInput
    }
    end {
        Write-Verbose -Message "Ending Write-FromPipeline"
    }
}

$foo = 'hello','world',1,2,3

$foo | Write-FromPipeline -Verbose

```

Output:

```powershell
VERBOSE: Beginning Write-FromPipeline
hello
world
1
2
3
VERBOSE: Ending Write-FromPipeline

```



## Basic Pipeline Support in Functions


This is an example of a function with the simplest possible support for pipelining.<br />
Any function with pipeline support must have at least one parameter with the ParameterAttribute `ValueFromPipeline` or `ValueFromPipelineByPropertyName` set, as shown below.

```powershell
function Write-FromPipeline {
    param(
        [Parameter(ValueFromPipeline)]  # This sets the ParameterAttribute
        [String]$Input
    )
    Write-Host $Input
}

$foo = 'Hello World!'

$foo | Write-FromPipeline

```

Output:

`Hello World!`

Note: In PowerShell 3.0 and above, Default Values for ParameterAttributes is supported. In earlier versions, you must specify `ValueFromPipeline=$true`.



## Working concept of pipeline


In a pipeline series each function runs parallel to the others, like parallel threads. The first processed object is transmitted to the next pipeline and the next processing is immediately executed in another thread. This explains the high speed gain compared to the standard `ForEach`

```powershell
@( bigFile_1, bigFile_2, ..., bigFile_n) | Copy-File | Encrypt-File | Get-Md5

```


1. step - copy the first file (in `Copy-file` Thread)
<li>step - copy
second file (in `Copy-file` Thread) and simultaneously Encrypt the
first (in `Encrypt-File`)</li>
<li>step -
copy third file (in `Copy-file` Thread) and simultaneously encrypt second file (in `Encrypt-File`) and simultaneously `get-Md5` of the first (in `Get-Md5`)</li>



#### Syntax


- BEGIN The first block. Executed once at the beginning. The pipeline input here is $null, as it has not been set.
- PROCESS The second block. Executed for each element of the pipeline. The pipeline parameter is equal to the currently processed element.
- END Last block. Executed once at the end. The pipeline parameter is equal to the last element of the input, because it has not been changed since it was set.



#### Remarks


In most cases, the input of the pipeline will be an array of objects. Although the behavior of the `PROCESS{}` block may seem similar to the `foreach{}` block, skipping an element in the array requires a different process.

If, like in `foreach{}`, you used `continue` inside the `PROCESS{}` block, it would break the pipeline, skipping all following statements including the `END{}` block. Instead, use `return` - it will only end the `PROCESS{}` block for the current element and move to the next.

In some cases, there is a need to output the result of functions with different encoding. The encoding of the output of the CmdLets is controlled by the `$OutputEncoding` variable. When the output is intended to be put into a pipeline to native applications, it might be a good idea to fix the encoding to match the target `$OutputEncoding = [Console]::OutputEncoding`

**Additional references:**

Blog article with more insight about `$OutputEncoding`
[https://blogs.msdn.microsoft.com/powershell/2006/12/11/outputencoding-to-the-rescue/](https://blogs.msdn.microsoft.com/powershell/2006/12/11/outputencoding-to-the-rescue/)

