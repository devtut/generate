---
metaTitle: "PowerShell - Variables in PowerShell"
description: "Removing a variable, Simple variable, Scope, Arrays, List Assignment of Multiple Variables, Reading a CmdLet Output"
---

# Variables in PowerShell


Variables are used for storing values. Let the value be of any type , we need to store it somewhere so that we can use it throughout the console/script. Variable names in PowerShell begin with a **$**, as in **$Variable1**, and values are assigned using **=**, like **$Variable1 = "Value 1"**.PowerShell supports a huge number of variable types; such as text strings, integers, decimals, arrays, and even advanced types like version numbers or IP addresses.



## Removing a variable


To remove a variable from memory, one can use the `Remove-Item` cmdlet. Note: The variable name does NOT include the `$`.

```powershell
Remove-Item Variable:\foo

```

`Variable` has a provider to allow most *-item cmdlets to work much like file systems.

Another method to remove variable is to use Remove-Variable cmdlet and its alias rv

```powershell
$var = "Some Variable" #Define variable 'var' containing the string 'Some Variable'
$var #For test and show string 'Some Variable' on the console

Remove-Variable -Name var
$var 

#also can use alias 'rv'
rv var

```



## Simple variable


All variables in powershell begin with a US dollar sign (`$`). The simplest example of this is:

```powershell
$foo = "bar"

```

This statement allocates a variable called `foo` with a string value of "bar".



## Scope


The default [scope](https://technet.microsoft.com/en-us/library/hh847849.aspx) for a variable is the enclosing container. If outside a script, or other container then the scope is `Global`. To specify a [scope](https://technet.microsoft.com/en-us/library/hh847849.aspx), it is prefixed to the variable name `$scope:varname` like so:

```powershell
$foo = "Global Scope"
function myFunc {
    $foo = "Function (local) scope"
    Write-Host $global:foo
    Write-Host $local:foo
    Write-Host $foo
}
myFunc
Write-Host $local:foo
Write-Host $foo

```

Output:



## Arrays


Array declaration in Powershell is almost the same as instantiating any other variable, i.e. you use a `$name =` syntax. The items in the array are declared by separating them by commas(`,`):

```powershell
$myArrayOfInts = 1,2,3,4
$myArrayOfStrings = "1","2","3","4"

```

### Adding to an arry

Adding to an array is as simple as using the `+` operator:

```powershell
$myArrayOfInts = $myArrayOfInts + 5
//now contains 1,2,3,4 & 5!

```

### Combining arrays together

Again this is as simple as using the `+` operator

```powershell
$myArrayOfInts = 1,2,3,4
$myOtherArrayOfInts = 5,6,7
$myArrayOfInts = $myArrayOfInts + $myOtherArrayOfInts
//now 1,2,3,4,5,6,7

```



## List Assignment of Multiple Variables


Powershell allows multiple assignment of variables and treats almost everything like an array or list. This means that instead of doing something like this:

```powershell
$input = "foo.bar.baz"
$parts = $input.Split(".")
$foo = $parts[0]
$bar = $parts[1]
$baz = $parts[2]

```

You can simply do this:

```powershell
$foo, $bar, $baz = $input.Split(".")

```

Since Powershell treats assignments in this manner like lists, if there are more values in the list than items in your list of variables to assign them to, the last variable becomes an array of the remaining values. This means you can also do things like this:

```powershell
$foo, $leftover = $input.Split(".") #Sets $foo = "foo", $leftover = ["bar","baz"]
$bar = $leftover[0] # $bar = "bar"
$baz = $leftover[1] # $baz = "baz"

```



## Reading a CmdLet Output


By Default, powershell would return the output to the calling Entity. Consider Below Example,

```powershell
Get-Process -Name excel   

```

This would simply, return the running process which matches the name excel, to the calling entity. In this case, the PowerShell Host. It prints something like,

```powershell
Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id  SI ProcessName                                                                                                                     
-------  ------    -----      ----- -----   ------     --  -- -----------                                                                                                                     
   1037      54    67632      62544   617     5.23   4544   1 EXCEL 

```

Now if you assign the output to a variable, it simply wont print anything. And of course the variable holds the output. (Be it a string, Object - Any type for that matter)

```powershell
$allExcel = Get-Process -Name excel

```

So, lets say you have a scenario where you want to assign a variable by a Dynamic name, you can use the **`-OutVariable`** parameter

```powershell
Get-Process -Name excel -OutVariable AllRunningExcel

```

Note that the '$' is missing here. A major difference between these two assignments is that, it also prints the output apart from assigning it to the variable AllRunningExcel. You can also choose to assign it to an another variable.

```powershell
$VarOne = Get-Process -Name excel -OutVariable VarTwo

```

Albeit, the above scenario is very rare, both variables $VarOne & $VarTwo will have the same value.

Now consider this,

```powershell
Get-Process -Name EXCEL -OutVariable MSOFFICE
Get-Process -Name WINWORD -OutVariable +MSOFFICE

```

The first statement would simply get excel process & assign it to MSOFFICE variable, and next would get ms word processes running and "Append" it to  the existing value of MSOFFICE. It would look something like this,

```powershell
Handles  NPM(K)    PM(K)      WS(K) VM(M)   CPU(s)     Id  SI ProcessName                                                                                                                     
-------  ------    -----      ----- -----   ------     --  -- -----------                                                                                                                     
   1047      54    67720      64448   618     5.70   4544   1 EXCEL                                                                                                                           
   1172      70    50052      81780   584     1.83  14968   1 WINWORD     

```

