---
metaTitle: "Excel VBA - Conditional statements"
description: "The If statement"
---

# Conditional statements




## The If statement


The `If` control statement allows different code to be executed depending upon the evaluation of a conditional (Boolean) statement. A conditional statement is one that evaluates to either `True` or `False`, e.g. `x > 2`.

There are three patterns that can be used when implementing an `If` statement, which are described below. Note that an `If` conditional evaluation is always followed by a `Then`.

**1. Evaluating one `If` conditional statement and doing something if it is `True`**

**Single line `If` statement**

This is the shortest way to use an `If` and it is useful when only one statement needs to be carried out upon  a `True` evaluation. When using this syntax, all of the code must be on a single line. Do not include an `End If` at the end of the line.

```vb
If [Some condition is True] Then [Do something]

```

**`If` block**

If multiple lines of code need to be executed upon a `True` evaluation, an `If` block may be used.

```vb
If [Some condition is True] Then
  [Do some things]
End If

```

Note that, if a multi-line `If` block is used, a corresponding `End If` is required.

**2. Evaluating one conditional `If` statement, doing one thing if it is `True` and doing something else if it is `False`**

**Single line `If`, `Else` statement**

This may be used if one statement is to be carried out upon a `True` evaluation and a different statement is to be carried out on a `False` evaluation. Be careful using this syntax, as it is often less clear to readers that there is an `Else` statement. When using this syntax, all of the code must be on a single line. Do not include an `End If` at the end of the line.

```vb
If [Some condition is True] Then [Do something] Else [Do something else]

```

**`If`, `Else` block**

Use an `If`, `Else` block to add clarity to your code, or if multiple lines of code need to be executed under either a `True` or a `False` evaluation.

```vb
If [Some condition is True] Then
   [Do some things]
Else
   [Do some other things]
End If

```

Note that, if a multi-line `If` block is used, a corresponding `End If` is required.

**3. Evaluating many conditional statements, when preceding statements are all `False`, and doing something different for each one**

This pattern is the most general use of `If` and would be used when there are many non-overlapping conditions that require different treatment. Unlike the first two patterns, this case requires the use of an `If` block, even if only one line of code will be executed for each condition.

**`If`, `ElseIf`, `...`, `Else` block**

Instead of having to create many `If` blocks one below another, an `ElseIf` may be used evaluate an extra condition. The `ElseIf` is only evaluated if any preceding `If` evaluation is `False`.

```vb
If [Some condition is True] Then
   [Do some thing(s)]
ElseIf [Some other condition is True] Then
   [Do some different thing(s)]
Else    'Everything above has evaluated to False
   [Do some other thing(s)]
End If

```

As many `ElseIf` control statements may be included between an `If` and an `End If` as required. An `Else` control statement is not required when using `ElseIf` (although it is recommended), but if it is included, it must be the final control statement before the `End If`.

