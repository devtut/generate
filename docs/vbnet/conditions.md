---
metaTitle: "Visual Basic .NET - Conditions"
description: "If operator, IF...Then...Else"
---

# Conditions




## If operator


```vb
If(condition > value, "True", "False")

```

We can use the **If** operator instead of **If...Then...Else..End If** statement blocks.

Consider the following example:

```vb
If 10 > 9 Then
     MsgBox("True")
Else
     MsgBox("False")
End If

```

is the same as

```vb
MsgBox(If(10 > 9, "True", "False"))

```

`If()` uses **short-circuit** evaluation, which means that it will only evaluate the arguments it uses. If the condition is false (or a `Nullable` that is `Nothing`), the first alternative will not be evaluated at all, and none of its side effects will be observed. This is effectively the same as C#'s [ternary operator](//stackoverflow.com/documentation/c%23/18/operators/6029/ternary-operator) in the form of `condition?a:b`.

This is especially useful in avoiding exceptions:

```vb
Dim z As Integer = If(x = 0, 0, y/x)

```

We all know that dividing by zero will throw an exception, but `If()` here guards against this by short-circuiting to only the expression that the condition has already ensured is valid.

Another example:

```vb
Dim varDate as DateTime = If(varString <> "N/A", Convert.ToDateTime(varString), Now.Date)

```

If `varString <> "N/A"` evaluates to `False`, it will assign `varDate`'s value as `Now.Date` without evaluating the first expression.

Older versions of VB do not have the `If()` operator and have to make do with the `IIf()` built-in function. As it's a function, not an operator, it does **not** short-circuit; all expressions are evaluated, with all possible side-effects, including performance penalties, changing state, and throwing exceptions. (Both of the above examples that avoid exceptions would throw if converted to `IIf`.) If any of these side effects present a problem, there's no way to use an inline conditional; instead, rely on `If..Then` blocks as usual.



## IF...Then...Else


```vb
Dim count As Integer = 0
Dim message As String

If count = 0 Then
    message = "There are no items."
ElseIf count = 1 Then
    message = "There is 1 item."
Else
    message = "There are " & count & " items."
End If

```

