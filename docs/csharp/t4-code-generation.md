---
metaTitle: "C# | T4 Code Generation"
description: "Runtime Code Generation"
---

# T4 Code Generation



## Runtime Code Generation


```cs
<#@ template language="C#" #> //Language of your project 
<#@ assembly name="System.Core" #>
<#@ import namespace="System.Linq" #>
<#@ import namespace="System.Text" #>
<#@ import namespace="System.Collections.Generic" #>

```



#### Syntax


- **T4 Syntax**
- `<#@...#>` //Declaring properties including templates, assemblies and namespaces and the language the template uses
- `Plain Text` //Declaring text that can be looped through for the files generated
- `<#=...#>` //Declaring Scripts
- `<#+...#>` //Declaring scriptlets
- `<#...#>` //Declaring text blocks

