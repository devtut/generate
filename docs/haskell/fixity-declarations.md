---
metaTitle: "Haskell - Fixity declarations"
description: "Associativity, Binding precedence, Example declarations"
---

# Fixity declarations



## Associativity


`infixl` vs `infixr` vs `infix` describe on which sides the parens will be grouped. For example, consider the following fixity declarations (in base)

```hs
infixl 6 -
infixr 5 :
infix  4 ==

```

The `infixl` tells us that `-` has left associativity, which means that `1 - 2 - 3 - 4` gets parsed as

```hs
((1 - 2) - 3) - 4

```

The `infixr` tells us that `:` has right associativity, which means that `1 : 2 : 3 : []` gets parsed as

```hs
1 : (2 : (3 : []))

```

The `infix` tells us that `==` cannot be used without us including parenthesis, which means that `True == False == True` is a syntax error. On the other hand, `True == (False == True)` or `(True == False) == True` are fine.

Operators without an explicit fixity declaration are `infixl 9`.



## Binding precedence


The number that follows the associativity information describes in what order the operators are applied. It must always be between `0` and `9` inclusive. This is commonly referred to as how tightly the operator binds. For example, consider the following fixity declarations (in base)

```hs
infixl 6 +
infixl 7 *

```

Since `*` has a higher binding precedence than `+` we read `1 * 2 + 3` as

```hs
(1 * 2) + 3

```

In short, the higher the number, the closer the operator will "pull" the parens on either side of it.

### Remarks

<li>
Function application **always** binds higher than operators, so `f x `op` g y` must be interpreted as `(f x)`op`(g y)` no matter what the operator ``op`` and its fixity declaration are.
</li>
<li>
If the binding precedence is omitted in a fixity declaration (for example we have `infixl *!?`) the default is `9`.
</li>



## Example declarations


- `infixr 5 ++`
- `infixl 4 <*>, <*, *>, <**>`
- `infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR``
- `infix 4 ==, /=, <, <=, >=, >`
- `infix ??`



#### Syntax


1. infix [integer] ops
1. infixl [integer] ops
1. infixr [integer] ops



#### Parameters


|Declaration component|Meaning
|---|---|---|---|---|---|---|---|---|---
|`infixr`|the operator is right-associative
|`infixl`|the operator is left-associative
|`infix`|the operator is non-associative
|optional digit|binding precedence of the operator (range 0...9, default 9)
|`op1, ... , opn`|operators



#### Remarks


To parse expressions involving operators and functions, Haskell uses fixity declarations to figure out where parenthesis go. In order, it

1. wraps function applications in parens
1. uses binding precendence to wrap groups of terms all seperated by operators of the same precedence
1. uses the associativity of those operators to figure out how to add parens to these groups

Notice that we assume here that the operators in any given group from step 2 must all have the same associativity. In fact, Haskell will reject any program where this condition is not met.

As an example of the above algorithm, we can step though the process of adding parenthesis to `1 + negate 5 * 2 - 3 * 4 ^ 2 ^ 1`.

```hs
infixl 6 +
infixl 6 -
infixl 7 *
infixr 8 ^

```


1. `1 + (negate 5) * 2 - 3 * 4 ^ 2 ^ 1`
1. `1 + ((negate 5) * 2) - (3 * (4 ^ 2 ^ 1))`
1. `(1 + ((negate 5) * 2)) - (3 * (4 ^ (2 ^ 1)))`

More details in section [4.4.2 of the Haskell 98 report](https://www.haskell.org/onlinereport/decls.html).

