---
metaTitle: "PowerShell - Working with Objects"
description: "Updating Objects, Creating a new object, Examining an object, Creating Instances of Generic Classes"
---

# Working with Objects



## Updating Objects


### Adding properties

If you'd like to add properties to an existing object, you can use the Add-Member cmdlet.  With PSObjects, values are kept in a type of "Note Properties"

```powershell
$object = New-Object -TypeName PSObject -Property @{
        Name = $env:username
        ID = 12
        Address = $null
    }

Add-Member -InputObject $object -Name "SomeNewProp" -Value "A value" -MemberType NoteProperty

# Returns
PS> $Object
Name ID Address SomeNewProp
---- -- ------- -----------
nem  12         A value

```

You can also add properties with Select-Object Cmdlet (so called calculated properties):

```powershell
$newObject = $Object | Select-Object *, @{label='SomeOtherProp'; expression={'Another value'}}

# Returns
PS> $newObject
Name ID Address SomeNewProp SomeOtherProp
---- -- ------- ----------- -------------
nem  12         A value     Another value

```

The command above can be shortened to this:

```powershell
$newObject = $Object | Select *,@{l='SomeOtherProp';e={'Another value'}}

```

### Removing properties

You can use the Select-Object Cmdlet to remove properties from an object:

```powershell
$object = $newObject | Select-Object * -ExcludeProperty ID, Address

# Returns
PS> $object
Name SomeNewProp SomeOtherProp
---- ----------- -------------
nem  A value     Another value

```



## Creating a new object


PowerShell, unlike some other scripting languages, sends objects through the pipeline.  What this means is that when you send data from one command to another, it's essential to be able to create, modify, and collect objects.

Creating an object is simple.  Most objects you create will be custom objects in PowerShell, and the type to use for that is PSObject.  PowerShell will also allow you to create any object you could create in .NET.

Here's an example of creating a new objects with a few properties:

### Option 1: New-Object

```powershell
$newObject = New-Object -TypeName PSObject -Property @{
    Name = $env:username
    ID = 12
    Address = $null
}

# Returns
PS> $newObject
Name ID Address
---- -- -------
nem  12

```

You can store the object in a variable by prefacing the command with `$newObject =`

You may also need to store collections of objects.  This can be done by creating an empty collection variable, and adding objects to the collection, like so:

```powershell
$newCollection = @()
$newCollection += New-Object -TypeName PSObject -Property @{
    Name = $env:username
    ID = 12
    Address = $null
}

```

You may then wish to iterate through this collection object by object.  To do that, locate the Loop section in the documentation.

### Option 2: Select-Object

A less common way of creating objects that you'll still find on the internet is the following:

```powershell
$newObject = 'unuseddummy' | Select-Object -Property Name, ID, Address
$newObject.Name = $env:username
$newObject.ID = 12

# Returns
PS> $newObject
Name ID Address
---- -- -------
nem  12

```

### Option 3: pscustomobject type accelerator (PSv3+ required)

The ordered type accelerator forces PowerShell to keep our properties in the order that we defined them. You don't need the ordered type accelerator to use `[PSCustomObject]`:

```powershell
$newObject = [PSCustomObject][Ordered]@{
    Name = $env:Username
    ID = 12
    Address = $null
}

# Returns
PS> $newObject
Name ID Address
---- -- -------
nem  12

```



## Examining an object


Now that you have an object, it might be good to figure out what it is.  You can use the Get-Member cmdlet to see what an object is and what it contains:

```powershell
Get-Item c:\windows | Get-Member

```

This yields:

```powershell
TypeName: System.IO.DirectoryInfo

```

Followed by a list of properties and methods the object has.

Another way to get the type of an object is to use the GetType method, like so :

```powershell
C:\> $Object = Get-Item C:\Windows
C:\> $Object.GetType()

IsPublic IsSerial Name                                     BaseType
-------- -------- ----                                     --------
True     True     DirectoryInfo                            System.IO.FileSystemInfo

```

To view a list of properties the object has, along with their values, you can use the Format-List cmdlet with its Property parameter set to : * (meaning all).

Here is a example, with the resulting output :

```powershell
C:\> Get-Item C:\Windows | Format-List -Property *


PSPath            : Microsoft.PowerShell.Core\FileSystem::C:\Windows
PSParentPath      : Microsoft.PowerShell.Core\FileSystem::C:\
PSChildName       : Windows
PSDrive           : C
PSProvider        : Microsoft.PowerShell.Core\FileSystem
PSIsContainer     : True
Mode              : d-----
BaseName          : Windows
Target            : {}
LinkType          :
Name              : Windows
Parent            :
Exists            : True
Root              : C:\
FullName          : C:\Windows
Extension         :
CreationTime      : 30/10/2015 06:28:30
CreationTimeUtc   : 30/10/2015 06:28:30
LastAccessTime    : 16/08/2016 17:32:04
LastAccessTimeUtc : 16/08/2016 16:32:04
LastWriteTime     : 16/08/2016 17:32:04
LastWriteTimeUtc  : 16/08/2016 16:32:04
Attributes        : Directory

```



## Creating Instances of Generic Classes


Note: examples written for PowerShell 5.1
You can create instances of Generic Classes

```powershell
#Nullable System.DateTime
[Nullable[datetime]]$nullableDate = Get-Date -Year 2012
$nullableDate
$nullableDate.GetType().FullName
$nullableDate = $null
$nullableDate

#Normal System.DateTime
[datetime]$aDate = Get-Date -Year 2013
$aDate
$aDate.GetType().FullName
$aDate = $null #Throws exception when PowerShell attempts to convert null to 

```

Gives the output:

```powershell
Saturday, 4 August 2012 08:53:02
System.DateTime
Sunday, 4 August 2013 08:53:02
System.DateTime
Cannot convert null to type "System.DateTime".
At line:14 char:1
+ $aDate = $null
+ ~~~~~~~~~~~~~~
    + CategoryInfo          : MetadataError: (:) [], ArgumentTransformationMetadataException
    + FullyQualifiedErrorId : RuntimeException

```

Generic Collections are also possible

```powershell
[System.Collections.Generic.SortedDictionary[int, String]]$dict = [System.Collections.Generic.SortedDictionary[int, String]]::new()
$dict.GetType().FullName

$dict.Add(1, 'a')
$dict.Add(2, 'b')
$dict.Add(3, 'c')


$dict.Add('4', 'd') #powershell auto converts '4' to 4
$dict.Add('5.1', 'c') #powershell auto converts '5.1' to 5

$dict

$dict.Add('z', 'z') #powershell can't convert 'z' to System.Int32 so it throws an error

```

Gives the output:

```powershell
System.Collections.Generic.SortedDictionary`2[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]

Key Value
--- -----
  1 a
  2 b
  3 c
  4 d
  5 c
Cannot convert argument "key", with value: "z", for "Add" to type "System.Int32": "Cannot convert value "z" to type "System.Int32". Error: "Input string was not in a correct format.""
At line:15 char:1
+ $dict.Add('z', 'z') #powershell can't convert 'z' to System.Int32 so  ...
+ ~~~~~~~~~~~~~~~~~~~
    + CategoryInfo          : NotSpecified: (:) [], MethodException
    + FullyQualifiedErrorId : MethodArgumentConversionInvalidCastArgument

```

