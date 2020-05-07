---
metaTitle: "Entity Framework - .t4 templates in entity-framework"
description: "Dynamically adding Interfaces to model, Adding XML Documentation to Entity Classes"
---

# .t4 templates in entity-framework



## Dynamically adding Interfaces to model


When working with existing model that is quite big and is being regenerated quite often in cases where abstraction needed it might be costly to manually go around redecorating model with interfaces. In such cases one might want to add some dynamic behavior to model generation.

Following example will show how automatically add interfaces on classes that have specific column names:

In your model go to `.tt` file modify the `EntityClassOpening` method in following way, this will add `IPolicyNumber` interface on entities that have `POLICY_NO` column, and `IUniqueId` on `UNIQUE_ID`

```cs
public string EntityClassOpening(EntityType entity)
{
    var stringsToMatch = new Dictionary<string,string> { { "POLICY_NO", "IPolicyNumber" }, { "UNIQUE_ID", "IUniqueId" } };
    return string.Format(
        CultureInfo.InvariantCulture,
        "{0} {1}partial class {2}{3}{4}",
        Accessibility.ForType(entity),
        _code.SpaceAfter(_code.AbstractOption(entity)),
        _code.Escape(entity),
        _code.StringBefore(" : ", _typeMapper.GetTypeName(entity.BaseType)),
        stringsToMatch.Any(o => entity.Properties.Any(n => n.Name == o.Key)) ? " : " + string.Join(", ", stringsToMatch.Join(entity.Properties, l => l.Key, r => r.Name, (l,r) =>  l.Value)) : string.Empty);
}

```

This is one specific case but it shows a power of being able to modify `.tt` templates.



## Adding XML Documentation to Entity Classes


On every generated model classes there are no documentation comments added by default. If you want to use XML documentation comments for every generated entity classes, find this part inside **[modelname]**.tt (**modelname** is current EDMX file name):

```cs
foreach (var entity in typeMapper.GetItemsToGenerate<EntityType>(itemCollection))
{
    fileManager.StartNewFile(entity.Name + ".cs");
    BeginNamespace(code); // used to write model namespace
#>
<#=codeStringGenerator.UsingDirectives(inHeader: false)#> 

```

You can add the XML documentation comments before `UsingDirectives` line as shown in example below:

```cs
foreach (var entity in typeMapper.GetItemsToGenerate<EntityType>(itemCollection))
{
    fileManager.StartNewFile(entity.Name + ".cs");
    BeginNamespace(code);
#>
/// <summary>
/// <#=entity.Name#> model entity class.
/// </summary>
<#=codeStringGenerator.UsingDirectives(inHeader: false)#> 

```

The generated documentation comment should be includes entity name as given below.

```cs
/// <summary>
/// Example model entity class.
/// </summary>
public partial class Example
{
    // model contents
}

```

