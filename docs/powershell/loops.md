---
metaTitle: "PowerShell - Loops"
description: "Foreach, For, ForEach() Method, ForEach-Object, Continue, Break, While, Do"
---

# Loops


A loop is a sequence of instruction(s) that is continually repeated until a certain condition is reached. Being able to have your program repeatedly execute a block of code is one of the most basic but useful tasks in programming. A loop lets you write a very simple statement to produce a significantly greater result simply by repetition. If the condition has been reached, the next instruction "falls through" to the next sequential instruction or branches outside the loop.



## Foreach


`ForEach` has two different meanings in PowerShell. One is a [keyword](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.core/about/about_language_keywords) and the other is an alias for the [ForEach-Object](http://stackoverflow.com/documentation/powershell/1067/loops/3665/foreach-object#t=201609240419334042438) cmdlet. The former is described here.

This example demonstrates printing all items in an array to the console host:

```powershell
$Names = @('Amy', 'Bob', 'Celine', 'David')

ForEach ($Name in $Names)
{
    Write-Host "Hi, my name is $Name!"
}

```

This example demonstrates capturing the output of a ForEach loop:

```powershell
$Numbers = ForEach ($Number in 1..20) {
    $Number # Alternatively, Write-Output $Number
}

```

Like the last example, this example, instead, demonstrates creating an array prior to storing the loop:

```powershell
$Numbers = @()
ForEach ($Number in 1..20)
{
    $Numbers += $Number
}

```



## For


```powershell
for($i = 0; $i -le 5; $i++){
    "$i"
}

```

A typical use of the for loop is to operate on a subset of the values in an array.
In most cases, if you want to iterate all values in an array, consider using a foreach statement.



## ForEach() Method


Instead of the `ForEach-Object` cmdlet, the here is also the possibility to use a `ForEach` method directly on object arrays like so

```powershell
(1..10).ForEach({$_ * $_})

```

or - if desired - the parentheses around the script block can be omitted

```powershell
(1..10).ForEach{$_ * $_}  

```

Both will result in the output below

```powershell
1
4
9
16
25
36
49
64
81
100  

```



## ForEach-Object


The `ForEach-Object` cmdlet works similarly to the [`foreach`](http://stackoverflow.com/documentation/powershell/1067/loops/3429/foreach#t=201609240419334042438) statement, but takes its input from the pipeline.

### Basic usage

```powershell
$object | ForEach-Object {
    code_block
}

```

Example:

```powershell
$names = @("Any","Bob","Celine","David")
$names | ForEach-Object {
    "Hi, my name is $_!"
}

```

`Foreach-Object` has two default aliases, `foreach` and `%` (shorthand syntax). Most common is `%` because `foreach` can be confused with the [foreach statement](http://stackoverflow.com/documentation/powershell/1067/loops/3429/foreach). Examples:

```powershell
$names | % {  
    "Hi, my name is $_!"
} 

$names | foreach {  
    "Hi, my name is $_!"
} 

```

### Advanced usage

`Foreach-Object` stands out from the alternative `foreach` solutions because it's a cmdlet which means it's designed to use the pipeline. Because of this, it has support for three scriptblocks just like a cmdlet or advanced function:

- **Begin**: Executed once before looping through the items that arrive from the pipeline. Usually used to create functions for use in the loop, creating variables, opening connections (database, web +) etc.
- **Process**: Executed once per item arrived from the pipeline. "Normal" foreach codeblock. This is the default used in the examples above when the parameter isn't specified.
- **End**: Executed once after processing all items. Usually used to close connections, generate a report etc.

Example:

```powershell
"Any","Bob","Celine","David" | ForEach-Object -Begin {
    $results = @()
} -Process {
    #Create and store message
    $results += "Hi, my name is $_!"
} -End {
    #Count messages and output
    Write-Host "Total messages: $($results.Count)"
    $results
}

```



## Continue


The `Continue` operator works in `For`, `ForEach`, `While` and `Do` loops. It skips the current iteration of the loop, jumping to the top of the innermost loop.

```powershell
$i =0
while ($i -lt 20) {
    $i++ 
    if ($i -eq 7) { continue }
    Write-Host $I
}

```

The above will output 1 to 20 to the console but miss out the number 7.

**Note**: When using a pipeline loop you should use `return` instead of `Continue`.



## Break


The `break` operator will exit a program loop immediately. It can be used in `For`, `ForEach`, `While` and `Do` loops or in a `Switch` Statement.

```powershell
$i = 0
while ($i -lt 15) {
    $i++ 
    if ($i -eq 7) {break}
    Write-Host $i
}

```

The above will count to 15 but stop as soon as 7 is reached.

**Note**: When using a pipeline loop, `break` will behave as `continue`. To simulate `break` in the pipeline loop you need to incorporate some additional logic, cmdlet, etc. It is easier to stick with non-pipeline loops if you need to use `break`.

**Break Labels**

Break can also call a label that was placed in front of the instantiation of a loop:

```powershell
$i = 0
:mainLoop While ($i -lt 15) {
    Write-Host $i -ForegroundColor 'Cyan'
    $j = 0
    While ($j -lt 15) {
        Write-Host $j -ForegroundColor 'Magenta'
        $k = $i*$j
        Write-Host $k -ForegroundColor 'Green'
        if ($k -gt 100) {
            break mainLoop
        }
        $j++
    }
    $i++
}

```

**Note:** This code will increment `$i` to `8` and `$j` to `13` which will cause `$k` to equal `104`. Since `$k` exceed `100`, the code will then break out of both loops.



## While


A while loop will evaluate a condition and if true will perform an action. As long as the condition evaluates to true the action will continue to be performed.

```powershell
while(condition){
  code_block
}

```

The following example creates a loop that will count down from 10 to 0

```powershell
$i = 10
while($i -ge 0){
    $i
    $i--
}

```

Unlike the [`Do`](//stackoverflow.com/documentation/powershell/1067/loops/3884/do#t=201703011018154712687)-While loop the condition is evaluated prior to the action's first execution. The action will not be performed if the initial condition evaluates to false.

Note: When evaluating the condition, PowerShell will treat the existence of a return object as true. This can be used in several ways but below is an example to monitor for a process. This example will spawn a notepad process and then sleep the current shell as long as that process is running. When you manually close the notepad instance the while condition will fail and the loop will break.

```powershell
Start-Process notepad.exe
while(Get-Process notepad -ErrorAction SilentlyContinue){
  Start-Sleep -Milliseconds 500
}

```



## Do


Do-loops are useful when you always want to run a codeblock at least once. A Do-loop will evaluate the condition after executing the codeblock, unlike a while-loop which does it before executing the codeblock.

You can use do-loops in two ways:

<li>
Loop **while** the condition is true:

```powershell
Do {
    code_block
} while (condition)

```


</li>
<li>
Loop **until** the condition is true, in other words, loop while the condition is false:

```powershell
Do {
    code_block
} until (condition)

```


</li>

Real Examples:

```powershell
$i = 0

Do {
    $i++
    "Number $i"
} while ($i -ne 3)

Do {
    $i++
    "Number $i"
} until ($i -eq 3)

```

Do-While and Do-Until are antonymous loops. If the code inside the same, the condition will be reversed. The example above illustrates this behaviour.



#### Syntax


<li>
for ( <Initialization>; <Condition>; <Repetition> ) { <Script_Block> }
</li>
<li>
<Collection> | Foreach-Object { <Script_Block_with_$__as_current_item> }
</li>
<li>
foreach ( <Item> in <Collection> ) { <Script_Block> }
</li>
<li>
while ( <Condition> ){ <Script_Block> }
</li>
<li>
do { <Script_Block> } while ( <Condition> )
</li>
<li>
do { <Script_Block> } until ( <Condition> )
</li>
<li>
<Collection>.foreach( { <Script_Block_with_$__as_current_item> } )
</li>



#### Remarks


### Foreach

There are multiple ways to run a foreach-loop in PowerShell and they all bring their own advantages and disadvantages:

|Solution|Advantages|Disadvantages
|---|---|---|---|---|---|---|---|---|---
|Foreach statement|Fastest. Works best with static collections (stored in a variable).|No pipeline input or output
|ForEach() Method|Same scriptblock syntax as `Foreach-Object`, but faster. Works best with static collections (stored in a variable). Supports pipeline output.|No support for pipeline input. Requires PowerShell 4.0 or greater
|Foreach-Object (cmdlet)|Supports pipeline input and output. Supports begin and end-scriptblocks for initialization and closing of connections etc. Most flexible solution.|Slowest

### Performance

```powershell
$foreach = Measure-Command { foreach ($i in (1..1000000)) { $i * $i } }
$foreachmethod = Measure-Command { (1..1000000).ForEach{ $_ * $_ } }
$foreachobject = Measure-Command { (1..1000000) | ForEach-Object { $_ * $_ } }

"Foreach: $($foreach.TotalSeconds)"
"Foreach method: $($foreachmethod.TotalSeconds)"
"ForEach-Object: $($foreachobject.TotalSeconds)"

Example output:

Foreach: 1.9039875
Foreach method: 4.7559563
ForEach-Object: 10.7543821

```

While `Foreach-Object` is the slowest, it's pipeline-support might be useful as it lets you process items as they arrive (while reading a file, receiving data etc.). This can be very useful when working with big data and low memory as you don't need to load all the data to memory before processing.

