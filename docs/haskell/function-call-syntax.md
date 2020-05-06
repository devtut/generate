---
metaTitle: "Haskell - Function call syntax"
description: "Parentheses in a basic function call, Parentheses in embedded function calls, Partial application - Part 1, Partial application - Part 2"
---

# Function call syntax


Haskell's function call syntax, explained with comparisons to C-style languages where applicable. This is aimed at people who are coming to Haskell from a background in C-style languages.



## Parentheses in a basic function call


For a C-style function call, e.g.

```hs
plus(a, b); // Parentheses surrounding only the arguments, comma separated

```

Then the equivalent Haskell code will be

```hs
(plus a b) -- Parentheses surrounding the function and the arguments, no commas

```

In Haskell, parentheses are not explicitly required for function application, and are only used to disambiguate expressions, like in mathematics; so in cases where the brackets surround all the text in the expression, the parentheses are actually not needed, and the following is also equivalent:

```hs
plus a b -- no parentheses are needed here!

```

It is important to remember that while in C-style languages, the function



## Parentheses in embedded function calls


In the previous example, we didn't end up needing the parentheses, because they did not affect the meaning of the statement. However, they are often necessary in more complex expression, like the one below.<br />
In C:

```hs
plus(a, take(b, c));

```

In Haskell this becomes:

```hs
(plus a (take b c))
-- or equivalently, omitting the outermost parentheses
plus a (take b c)

```

Note, that this is not equivalent to:

```hs
plus a take b c -- Not what we want!

```

One might think that because the compiler knows that `take` is a function, it would be able to know that you want to apply it to the arguments `b` and `c`, and pass its result to `plus`.<br />
However, in Haskell, functions often take other functions as arguments, and little actual distinction is made between functions and other values; and so the compiler cannot assume your intention simply because `take` is a function.

And so, the last example is analogous to the following C function call:

```hs
plus(a, take, b, c); // Not what we want!

```



## Partial application - Part 1


In Haskell, functions can be partially applied; we can think of all functions as taking a single argument, and returning a modified function for which that argument is constant. To illustrate this, we can bracket functions as follows:

```hs
(((plus) 1) 2)

```

Here, the function `(plus)` is applied to `1` yielding the function `((plus) 1)`, which is applied to `2`, yielding the function `(((plus) 1) 2)`. Because `plus 1 2` is a function which takes no arguments, you can consider it a plain value; however in Haskell, there is little distinction between functions and values.

To go into more detail, the function `plus` is a function that adds its arguments.<br />
The function `plus 1` is a function that adds `1` to its argument.<br />
The function `plus 1 2` is a function that adds `1` to `2`, which is always the value `3`.



## Partial application - Part 2


As another example, we have the function `map`, which takes a function and a list of values, and applies the function to each value of the list:

```hs
map :: (a -> b) -> [a] -> [b]

```

Let's say we want to increment each value in a list. You may decide to define your own function, which adds one to its argument, and `map` that function over your list

```hs
addOne x = plus 1 x
map addOne [1,2,3]

```

but if you have another look at `addOne`'s definition, with parentheses added for emphasis:

```hs
(addOne) x = ((plus) 1) x

```

The function `addOne`, when applied to any value `x`, is the same as the partially applied function `plus 1` applied to `x`. This means the functions `addOne` and `plus 1` are identical, and we can avoid defining a new function by just replacing `addOne` with `plus 1`, remembering to use parentheses to isolate `plus 1` as a subexpression:

```hs
map (plus 1) [1,2,3]

```



#### Remarks


In general, the rule for converting a C-style function call to Haskell, in any context (assignment, return, or embedded in another call), is to replace the commas in the C-style argument list with whitespace, and move the opening parenthesis from the C-style call to contain the function name and its parameters.

If any expressions are wrapped entirely in parentheses, these (external) pairs of parentheses can be removed for readability, as they do not affect the meaning of the expression.<br />
There are some other circumstances where parentheses can be removed, but this only affects readability and maintainability.

