---
metaTitle: "Operator Precedence"
description: "Simple Operator Precedence Examples in python."
---

# Operator Precedence


Python operators have a set **order of precedence**, which determines what operators are evaluated first in a potentially ambiguous expression. For instance, in the expression 3 * 2 + 7, first 3 is multiplied by 2, and then the result is added to 7, yielding 13. The expression is not evaluated the other way around, because * has a higher precedence than +.

Below is a list of operators by precedence, and a brief description of what they (usually) do.



## Simple Operator Precedence Examples in python.


Python follows PEMDAS rule. PEMDAS stands for Parentheses, Exponents, Multiplication and Division, and Addition and Subtraction.

Example:

```
>>> a, b, c, d = 2, 3, 5, 7
>>> a ** (b + c)  # parentheses
256
>>> a * b ** c  # exponent: same as `a * (b ** c)`
7776
>>> a + b * c / d  # multiplication / division: same as `a + (b * c / d)`
4.142857142857142

```

Extras: mathematical rules hold, but [not always](https://docs.python.org/3/tutorial/floatingpoint.html):

```
>>> 300 / 300 * 200
200.0
>>> 300 * 200 / 300
200.0
>>> 1e300 / 1e300 * 1e200
1e+200
>>> 1e300 * 1e200 / 1e300
inf

```



#### Remarks


From the Python documentation:

> 
The following table summarizes the operator precedences in Python, from lowest precedence (least binding) to highest precedence (most binding). Operators in the same box have the same precedence. Unless the syntax is explicitly given, operators are binary. Operators in the same box group left to right (except for comparisons, including tests, which all have the same precedence and chain from left to right and exponentiation, which groups from right to left).


|Operator|Description
|------
|lambda|Lambda expression
|if – else|Conditional expression
|or|Boolean OR
|and|Boolean AND
|not x|Boolean NOT
|in, not in, is, is not, <, <=, >, >=, <>, !=, ==|Comparisons, including membership tests and identity tests
|||Bitwise OR
|^|Bitwise XOR
|&|Bitwise AND
|<<, >>|Shifts
|+, -|Addition and subtraction
|*, /, //, %|Multiplication, division, remainder [8]
|+x, -x, ~x|Positive, negative, bitwise NOT
|**|Exponentiation [9]
|x[index], x[index:index], x(arguments...), x.attribute|Subscription, slicing, call, attribute reference
|(expressions...), [expressions...], {key: value...}, expressions...|Binding or tuple display, list display, dictionary display, string conversion

