---
metaTitle: "Visual Basic .NET - Short-Circuiting Operators (AndAlso - OrElse)"
description: "OrElse Usage, AndAlso Usage, Avoiding NullReferenceException"
---

# Short-Circuiting Operators (AndAlso - OrElse)



## OrElse Usage


```vb
' The OrElse operator is the homologous of AndAlso. It lets us perform a boolean 
' comparison evaluating the second condition only if the first one is False

If testFunction(5) = True OrElse otherFunction(4) = True Then
    ' If testFunction(5) is True, otherFunction(4) is not called.
    ' Insert code to be executed.
End If

```



## AndAlso Usage


```vb
' Sometimes we don't need to evaluate all the conditions in an if statement's boolean check.

' Let's suppose we have a list of strings:

Dim MyCollection as List(Of String) = New List(of String)()

' We want to evaluate the first value inside our list:

If MyCollection.Count > 0 And MyCollection(0).Equals("Somevalue")
    Console.WriteLine("Yes, I've found Somevalue in the collection!")
End If 

' If MyCollection is empty, an exception will be thrown at runtime.
' This because it evaluates both first and second condition of the 
' if statement regardless of the outcome of the first condition.

' Now let's apply the AndAlso operator

If MyCollection.Count > 0 AndAlso MyCollection(0).Equals("Somevalue")
    Console.WriteLine("Yes, I've found Somevalue in the collection!")
End If 

' This won't throw any exception because the compiler evaluates just the first condition.
' If the first condition returns False, the second expression isn't evaluated at all.

```



## Avoiding NullReferenceException


### OrElse

```vb
Sub Main()
    Dim elements As List(Of Integer) = Nothing

    Dim average As Double = AverageElementsOrElse(elements)
    Console.WriteLine(average) ' Writes 0 to Console

    Try
        'Throws ArgumentNullException
        average = AverageElementsOr(elements)
    Catch ex As ArgumentNullException
        Console.WriteLine(ex.Message)
    End Try
End Sub

Public Function AverageElementsOrElse(ByVal elements As IEnumerable(Of Integer)) As Double
    ' elements.Count is not called if elements is Nothing so it cannot crash
    If (elements Is Nothing OrElse elements.Count = 0) Then
        Return 0
    Else
        Return elements.Average()
    End If
End Function

Public Function AverageElementsOr(ByVal elements As IEnumerable(Of Integer)) As Double
    ' elements.Count is always called so it can crash if elements is Nothing
    If (elements Is Nothing Or elements.Count = 0) Then
        Return 0
    Else
        Return elements.Average()
    End If
End Function

```

### AndAlso

```vb
Sub Main()
    Dim elements As List(Of Integer) = Nothing

    Dim average As Double = AverageElementsAndAlso(elements)
    Console.WriteLine(average) ' Writes 0 to Console

    Try
        'Throws ArgumentNullException
        average = AverageElementsAnd(elements)
    Catch ex As ArgumentNullException
        Console.WriteLine(ex.Message)
    End Try
End Sub

Public Function AverageElementsAndAlso(ByVal elements As IEnumerable(Of Integer)) As Double
    ' elements.Count is not called if elements is Nothing so it cannot crash
    If (Not elements Is Nothing AndAlso elements.Count > 0) Then
        Return elements.Average()
    Else
        Return 0
    End If
End Function

Public Function AverageElementsAnd(ByVal elements As IEnumerable(Of Integer)) As Double
    ' elements.Count is always called so it can crash if elements is Nothing
    If (Not elements Is Nothing And elements.Count > 0) Then
        Return elements.Average()
    Else
        Return 0
    End If
End Function

```

[Visual Basic 14.0 introduced the null conditional operator](http://stackoverflow.com/documentation/vb.net/1501/visual-basic-14-0-features/4878/null-conditional-operator), allowing to rewrite the functions in a cleaner way, mimicking the behavior of the `AndAlso` version of the example.



#### Syntax


- result = expression1 AndAlso expression2
- result = expression1 OrElse expression2



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|result|Required. Any Boolean expression. The result is the Boolean result of comparison of the two expressions.
|expression1|Required. Any Boolean expression.
|expression2|Required. Any Boolean expression.



#### Remarks


**'AndAlso'** and **'OrElse'** are **ShortCircuiting** operators that means that the execution is shorter because the compiler doesn't evaluate all the expressions in a boolean comparision if the first one provides the desidered result.

