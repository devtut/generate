---
metaTitle: "PowerShell - Automatic Variables"
description: "$OFS, $?, $null, $error, $pid, Boolean values, $_ / $PSItem"
---

# Automatic Variables


Automatic Variables are created and maintained by Windows PowerShell. One has the ability to call a variable just about any name in the book; The only exceptions to this are the variables that are already being managed by PowerShell. These variables, without a doubt, will be the most repetitious objects you use in PowerShell next to functions (like **$?** - indicates Success/ Failure status of the last operation)



## $OFS


Variable called Output Field Separator contains string value that is used when converting an array to a string. By default `$OFS = " "` (**a space**), but it can be changed:

```powershell
PS C:\> $array = 1,2,3
PS C:\> "$array" # default OFS will be used
1 2 3
PS C:\> $OFS = ",." # we change OFS to comma and dot
PS C:\> "$array"
1,.2,.3

```



## $?


Contains status of the last operation. When there is no error, it is set to `True`:

```powershell
PS C:\> Write-Host "Hello"
Hello
PS C:\> $?
True

```

If there is some error, it is set to `False`:

```powershell
PS C:\> wrt-host
wrt-host : The term 'wrt-host' is not recognized as the name of a cmdlet, function, script file, or operable program.
Check the spelling of the name, or if a path was included, verify that the path is correct and try again.
At line:1 char:1
+ wrt-host
+ ~~~~~~~~
    + CategoryInfo          : ObjectNotFound: (wrt-host:String) [], CommandNotFoundException
    + FullyQualifiedErrorId : CommandNotFoundException

PS C:\> $?
False

```



## $null


`$null` is used to represent absent or undefined value.<br />
`$null` can be used as an empty placeholder for empty value in arrays:

```powershell
PS C:\> $array = 1, "string", $null
PS C:\> $array.Count
3

```

When we use the same array as the source for `ForEach-Object`, it will process all three items (including $null):

```powershell
PS C:\> $array | ForEach-Object {"Hello"}
Hello
Hello
Hello

```

Be careful! This means that `ForEach-Object` **WILL** process even `$null` all by itself:

```powershell
PS C:\> $null | ForEach-Object {"Hello"} # THIS WILL DO ONE ITERATION !!!
Hello

```

Which is very unexpected result if you compare it to classic `foreach` loop:

```powershell
PS C:\> foreach($i in $null) {"Hello"} # THIS WILL DO NO ITERATION
PS C:\>

```



## $error


Array of most recent error objects. The first one in the array is the most recent one:

```powershell
PS C:\> throw "Error" # resulting output will be in red font
Error
At line:1 char:1
+ throw "Error"
+ ~~~~~~~~~~~~~
    + CategoryInfo          : OperationStopped: (Error:String) [], RuntimeException
    + FullyQualifiedErrorId : Error

PS C:\> $error[0] # resulting output will be normal string (not red    )
Error
At line:1 char:1
+ throw "Error"
+ ~~~~~~~~~~~~~
    + CategoryInfo          : OperationStopped: (Error:String) [], RuntimeException
    + FullyQualifiedErrorId : Error

```

Usage hints: When using the `$error` variable in a format cmdlet (e.g. format-list), be aware to use the `-Force` switch. Otherwise the format cmdlet is going to output the `$error`contents in above shown manner.

Error entries can be removed via e.g. `$Error.Remove($Error[0])`.



## $pid


Contains process ID of the current hosting process.

```powershell
PS C:\> $pid
26080

```



## Boolean values


`$true` and `$false` are two variables that represent logical TRUE and FALSE.

Note that you have to specify the dollar sign as the first character (which is different from C#).

```powershell
$boolExpr = "abc".Length -eq 3 # length of "abc" is 3, hence $boolExpr will be True
if($boolExpr -eq $true){
    "Length is 3"
}
# result will be "Length is 3" 
$boolExpr -ne $true
#result will be False

```

Notice that when you use boolean true/false in your code you write `$true` or `$false`, but when Powershell returns a boolean, it looks like `True` or `False`



## $_ / $PSItem


Contains the object/item currently being processed by the pipeline.

```powershell
PS C:\> 1..5 | % { Write-Host "The current item is $_" }
The current item is 1
The current item is 2
The current item is 3
The current item is 4
The current item is 5

```

`$PSItem` and `$_` are identical and can be used interchangeably, but `$_` is by far the most commonly used.



#### Syntax


- `$$` - Contains the last token in the last line received by the session.
- `$^` - Contains the first token in the last line received by the session.
- `$?` - Contains the execution status of the last operation.
- `$_` - Contains the current object in the pipeline

