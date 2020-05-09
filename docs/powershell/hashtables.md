---
metaTitle: "PowerShell - HashTables"
description: "Access a hash table value by key., Creating a Hash Table, Add a key value pair to an existing hash table, Looping over a hash table, Enumerating through keys and Key-Value Pairs, Remove a key value pair from an existing hash table"
---

# HashTables




## Access a hash table value by key.


An example of defining a hash table and accessing a value by the key

```powershell
$hashTable = @{
    Key1 = 'Value1'
    Key2 = 'Value2'
}
$hashTable.Key1
#output
Value1

```

An example of accessing a key with invalid characters for a property name:

```powershell
$hashTable = @{
    'Key 1' = 'Value3'
    Key2 = 'Value4'
}
$hashTable.'Key 1'
#Output
Value3

```



## Creating a Hash Table


Example of creating an empty HashTable:

```powershell
$hashTable = @{}

```

Example of creating a HashTable with data:

```powershell
$hashTable = @{
    Name1 = 'Value'
    Name2 = 'Value'
    Name3 = 'Value3'
}

```



## Add a key value pair to an existing hash table


An example, to add a "Key2" key with a value of "Value2" to the hash table, using the addition operator:

```powershell
$hashTable = @{
    Key1 = 'Value1'
}
$hashTable += @{Key2 = 'Value2'}
$hashTable

#Output

Name                           Value
----                           -----
Key1                           Value1
Key2                           Value2

```

An example, to add a "Key2" key with a value of "Value2" to the hash table using the Add method:

```powershell
$hashTable = @{
    Key1 = 'Value1'
}
$hashTable.Add("Key2", "Value2")
$hashTable

#Output

Name                           Value
----                           -----
Key1                           Value1
Key2                           Value2

```



## Looping over a hash table


```powershell
$hashTable = @{
    Key1 = 'Value1'
    Key2 = 'Value2'
}

foreach($key in $hashTable.Keys)
{
    $value = $hashTable.$key
    Write-Output "$key : $value"
}
#Output
Key1 : Value1
Key2 : Value2

```



## Enumerating through keys and Key-Value Pairs


**Enumerating through Keys**

```powershell
foreach ($key in $var1.Keys) {
    $value = $var1[$key]
    # or
    $value = $var1.$key 
}

```

**Enumerating through Key-Value Pairs**

```powershell
foreach ($keyvaluepair in $var1.GetEnumerator()) {
    $key1 = $_.Key1
    $val1 = $_.Val1
}

```



## Remove a key value pair from an existing hash table


An example, to remove a "Key2" key with a value of "Value2" from the hash table, using the remove operator:

```powershell
$hashTable = @{
    Key1 = 'Value1'
    Key2 = 'Value2'
}
$hashTable.Remove("Key2", "Value2")
$hashTable

#Output

Name                           Value
----                           -----
Key1                           Value1

```



#### Remarks


An important concept which relies on Hash Tables is [Splatting](http://stackoverflow.com/documentation/powershell/5647/splatting).  It is very useful for making a large number of calls with repetitive parameters.

