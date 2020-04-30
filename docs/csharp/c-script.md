---
metaTitle: "C# Script"
description: "Simple code evaluation"
---

# C# Script



## Simple code evaluation


You can evaluate any valid C# code:

```cs
int value = await CSharpScript.EvaluateAsync<int>("15 * 89 + 95");
var span = await CSharpScript.EvaluateAsync<TimeSpan>("new DateTime(2016,1,1) - DateTime.Now");

```

If type is not specified, the result is `object`:

```cs
object value = await CSharpScript.EvaluateAsync("15 * 89 + 95");

```

