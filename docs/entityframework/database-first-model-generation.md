---
metaTitle: "Entity Framework - Database first model generation"
description: "Generating model from database, Adding data annotations to the generated model"
---

# Database first model generation



## Generating model from database


In `Visual Studio` go to your `Solution Explorer` then click on `Project` you will be adding model <kbd>Right mouse</kbd>. Choose `ADO.NET Entity Data Model`

[<img src="http://i.stack.imgur.com/mCO7W.png" alt="enter image description here" />](http://i.stack.imgur.com/mCO7W.png)

Then choose `Generate from database` and click `Next` in next window click `New Connection...` and point to the database you want to generate model from (Could be `MSSQL`, `MySQL` or `Oracle`)

[<img src="http://i.stack.imgur.com/JvFsr.png" alt="enter image description here" />](http://i.stack.imgur.com/JvFsr.png)

After you done this click `Test Connection` to see if you have configured connection properly (do not go any further if it fails here).

Click `Next` then choose options that you want (like style for generating entity names or to add foreign keys).

Click `Next` again, at this point you should have model generated from database.



## Adding data annotations to the generated model


In T4 code-generation strategy used by Entity Framework 5 and higher, data annotation attributes are not included by default. To include data annotations on top of certain property every model regeneration, open template file included with EDMX (with `.tt` extension) then add a `using` statement under `UsingDirectives` method like below:

```cs
foreach (var entity in typeMapper.GetItemsToGenerate<EntityType>
(itemCollection))
{
    fileManager.StartNewFile(entity.Name + ".cs");
    BeginNamespace(code);
#>
<#=codeStringGenerator.UsingDirectives(inHeader: false)#>
using System.ComponentModel.DataAnnotations;  // --> add this line

```

As an example, suppose the template should include `KeyAttribute` which indicates a primary key property. To insert `KeyAttribute` automatically while regenerating model, find part of code containing `codeStringGenerator.Property` as below:

```cs
var simpleProperties = typeMapper.GetSimpleProperties(entity);
    if (simpleProperties.Any())
    {
        foreach (var edmProperty in simpleProperties)
        {
#>
    <#=codeStringGenerator.Property(edmProperty)#>
<#
        }
    }

```

Then, insert an if-condition to check key property as this:

```cs
var simpleProperties = typeMapper.GetSimpleProperties(entity);
    if (simpleProperties.Any())
    {
        foreach (var edmProperty in simpleProperties)
        {
             if (ef.IsKey(edmProperty)) { 
#>    [Key]
<#      } 
#>
    <#=codeStringGenerator.Property(edmProperty)#>
<#
        }
    }

```

By applying changes above, all generated model classes will have `KeyAttribute` on their primary key property after updating model from database.

**Before**

```cs
using System;

public class Example
{
    public int Id { get; set; }
    public string Name { get; set; }
}

```

**After**

```cs
using System;
using System.ComponentModel.DataAnnotations;

public class Example
{
    [Key]
    public int Id { get; set; }
    public string Name { get; set; }
}

```

