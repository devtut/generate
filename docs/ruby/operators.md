---
metaTitle: "Ruby - Operators"
description: "Comparison Operators, Assignment Operators"
---

# Operators



## Comparison Operators


|Operator|Description
|---|---|---|---|---|---|---|---|---|---
|`==`|`true` if the two values are equal.
|`!=`|`true` if the two values are **not** equal.
|`<`|`true` if the value of the operand on the left is **less than** the value on the right.
|`>`|`true` if the value of the operand on the left is **greater than** the value on the right.
|`>=`|`true` if the value of the operand on the left is **greater than** **or** **equal to** the value on the right.
|`<=`|`true` if the value of the operand on the left is **less than** **or** **equal to** the value on the right.
|`<=>`|`0` if the value of the operand on the left is **equal to** the value on the right,<BR>`1` if the value of the operand on the left is **greater than** the value on the right,<BR>`-1` if the value of the operand on the left is **less than** the value on the right.



## Assignment Operators


### Simple Assignment

`=` is a simple assignment. It creates a new local variable if the variable was not previously referenced.

```ruby
x is 3, y is 9

```

### Parallel Assignment

Variables can also be assigned in parallel, e.g. `x, y = 3, 9`. This is especially useful for swapping values:

```ruby
x is 9, y is 3

```

### Abbreviated Assignment

It's possible to mix operators and assignment. For example:

```ruby
x is 1, y is 2
x is now 3

```

Various operations can be used in abbreviated assignment:

|Operator|Description|Example|Equivalent to
|---|---|---|---|---|---|---|---|---|---
|`+=`|Adds and reassigns the variable|`x += y`|`x = x + y`
|`-=`|Subtracts and reassigns the variable|`x -= y`|`x = x - y`
|`*=`|Multiplies and reassigns the variable|`x *= y`|`x = x * y`
|`/=`|Divides and reassigns the variable|`x /= y`|`x = x / y`
|`%=`|Divides, takes the remainder, and reassigns the variable|`x %= y`|`x = x % y`
|`**=`|Calculates the exponent and reassigns the variable|`x **= y`|`x = x ** y`

