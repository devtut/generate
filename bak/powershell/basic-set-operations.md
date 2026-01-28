---
metaTitle: "PowerShell - Basic Set Operations"
description: "Filtering: Where-Object / where / ?, Ordering: Sort-Object / sort, Grouping: Group-Object / group, Projecting: Select-Object / select"
---

# Basic Set Operations


A set is a collection of items which can be anything. Whatever operator we need to work on these sets are in short the **set operators** and the operation is  also known as **set operation**. Basic set operation includes Union, Intersection as well as addition, subtraction, etc.



## Filtering: Where-Object / where / ?


Filter an enumeration by using a conditional expression

Synonyms:

```powershell
Where-Object
where
?

```

Example:

```powershell
$names = @( "Aaron", "Albert", "Alphonse","Bernie", "Charlie", "Danny", "Ernie", "Frank")

$names | Where-Object { $_ -like "A*" }
$names | where { $_ -like "A*" }
$names | ? { $_ -like "A*" }

```

Returns:

> 
<p>Aaron<br />
Albert<br />
Alphonse</p>




## Ordering: Sort-Object / sort


Sort an enumeration in either ascending or descending order

Synonyms:

```powershell
Sort-Object
sort

```

Assuming:

```powershell
$names = @( "Aaron", "Aaron", "Bernie", "Charlie", "Danny" )

```

Ascending sort is the default:

```powershell
$names | Sort-Object
$names | sort

```

> 
<p>Aaron<br />
Aaron<br />
Bernie<br />
Charlie<br />
Danny</p>


To request descending order:

```powershell
$names | Sort-Object -Descending
$names | sort -Descending

```

> 
<p>Danny<br />
Charlie<br />
Bernie<br />
Aaron<br />
Aaron</p>


You can sort using an expression.

```powershell
$names | Sort-Object { $_.length }

```

> 
<p>Aaron<br />
Aaron<br />
Danny<br />
Bernie<br />
Charlie</p>




## Grouping: Group-Object / group


You can group an enumeration based on an expression.

Synonyms:

```powershell
Group-Object
group

```

Examples:

```powershell
$names = @( "Aaron", "Albert", "Alphonse","Bernie", "Charlie", "Danny", "Ernie", "Frank")

$names | Group-Object -Property Length
$names | group -Property Length

```

Response:

|Count|Name|Group
|---|---|---|---|---|---|---|---|---|---
|4|5|{Aaron, Danny, Ernie, Frank}
|2|6|{Albert, Bernie}
|1|8|{Alphonse}
|1|7|{Charlie}



## Projecting: Select-Object / select


Projecting an enumeration allows you to extract specific members of each object, to extract all the details, or to compute values for each object

Synonyms:

```powershell
Select-Object
select

```

Selecting a subset of the properties:

```powershell
$dir = dir "C:\MyFolder"

$dir | Select-Object Name, FullName, Attributes
$dir | select Name, FullName, Attributes

```

|Name|FullName|Attributes
|---|---|---|---|---|---|---|---|---|---
|Images|C:\MyFolder\Images|Directory
|data.txt|C:\MyFolder\data.txt|Archive
|source.c|C:\MyFolder\source.c|Archive

Selecting the first element, and show all its properties:

```powershell
$d | select -first 1 *

```

|
|---|---|---|---|---|---|---|---|---|---
|PSPath
|PSParentPath
|PSChildName
|PSDrive
|PSProvider
|PSIsContainer
|BaseName
|Mode
|Name
|Parent
|Exists
|Root
|FullName
|Extension
|CreationTime
|CreationTimeUtc
|LastAccessTime
|LastAccessTimeUtc
|LastWriteTime
|LastWriteTimeUtc
|Attributes



#### Syntax


&lt;li>
Group-Object
&lt;/li>
&lt;li>
Group-Object -Property &lt;propertyName>
&lt;/li>
&lt;li>
Group-Object -Property &lt;propertyName>, &lt;propertyName2>
&lt;/li>
&lt;li>
Group-Object -Property &lt;propertyName> -CaseSensitive
&lt;/li>
&lt;li>
Group-Object -Property &lt;propertyName> -Culture &lt;culture>
&lt;/li>
&lt;li>
Group-Object -Property &lt;ScriptBlock>
&lt;/li>
&lt;li>
Sort-Object
&lt;/li>
&lt;li>
Sort-Object -Property &lt;propertyName>
&lt;/li>
&lt;li>
Sort-Object -Property &lt;ScriptBlock>
&lt;/li>
&lt;li>
Sort-Object -Property &lt;propertyName>, &lt;propertyName2>
&lt;/li>
&lt;li>
Sort-Object -Property &lt;propertyObject> -CaseSensitive
&lt;/li>
&lt;li>
Sort-Object -Property &lt;propertyObject> -Descending
&lt;/li>
&lt;li>
Sort-Object -Property &lt;propertyObject> -Unique
&lt;/li>
&lt;li>
Sort-Object -Property &lt;propertyObject> -Culture &lt;culture>
&lt;/li>

