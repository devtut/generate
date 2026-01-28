---
metaTitle: ".NET Framework - Parallel processing using .Net framework"
description: "Parallel Extensions"
---

# Parallel processing using .Net framework




## Parallel Extensions


Parallel extensions have been introduced along with the Task Parallel Library to achieve data Parallelism. Data parallelism refers to scenarios in which the same operation is performed concurrently (that is, in parallel) on elements in a source collection or array. The .NET provides new constructs to achieve data parallelism by using Parallel.For and Parallel.Foreach constructs.

```dotnet
//Sequential version

foreach (var item in sourcecollection){

Process(item);

}

// Parallel equivalent

Parallel.foreach(sourcecollection, item => Process(item));

```

The above mentioned Parallel.ForEach construct utilizes the multiple cores and thus enhances the performance in the same fashion.

