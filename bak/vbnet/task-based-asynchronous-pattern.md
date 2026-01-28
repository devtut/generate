---
metaTitle: "Visual Basic .NET - Task-based asynchronous pattern"
description: "Basic usage of Async/Await, Using TAP with LINQ"
---

# Task-based asynchronous pattern



## Basic usage of Async/Await


You can start some slow process in parallel and then collect the results when they are done:

```vb
Public Sub Main()
    Dim results = Task.WhenAll(SlowCalculation, AnotherSlowCalculation).Result
    
    For Each result In results
        Console.WriteLine(result)
    Next
End Sub

Async Function SlowCalculation() As Task(Of Integer)
     Await Task.Delay(2000)

     Return 40
End Function

Async Function AnotherSlowCalculation() As Task(Of Integer)
    Await Task.Delay(2000)

    Return 60
End Function

```

After two seconds both the results will be available.



## Using TAP with LINQ


You can create an `IEnumerable` of `Task` by passing `AddressOf AsyncMethod` to the **LINQ** `Select` method and then start and wait all the results with `Task.WhenAll`

If your method has parameters matching the previous **LINQ** chain call, they will be automatically mapped.

```vb
Public Sub Main()
    Dim tasks = Enumerable.Range(0, 100).Select(AddressOf TurnSlowlyIntegerIntoString)
        
    Dim resultingStrings = Task.WhenAll(tasks).Result
    
    For Each value In resultingStrings
        Console.WriteLine(value)
    Next 
End Sub

Async Function TurnSlowlyIntegerIntoString(input As Integer) As Task(Of String)
    Await Task.Delay(2000)
    
    Return input.ToString()
End Function

```

To map different arguments you can replace `AddressOf Method` with a lambda:

```vb
Function(linqData As Integer) MyNonMatchingMethod(linqData, "Other parameter")

```

