---
metaTitle: "Haskell - Tuples (Pairs, Triples, ...)"
description: "Extract tuple components, Strictness of matching a tuple, Construct tuple values, Write tuple types, Pattern Match on Tuples, Apply a binary function to a tuple (uncurrying), Apply a tuple function to two arguments (currying), Swap pair components"
---

# Tuples (Pairs, Triples, ...)



## Extract tuple components


Use the `fst` and `snd` functions (from `Prelude` or `Data.Tuple`) to extract the first and second component of pairs.

```hs
fst (1, 2) -- evaluates to 1

snd (1, 2) -- evaluates to 2

```

Or use pattern matching.

```hs
case (1, 2) of (result, _) => result -- evaluates to 1

case (1, 2) of (_, result) => result -- evaluates to 2

```

Pattern matching also works for tuples with more than two components.

```hs
case (1, 2, 3) of (result, _, _) => result -- evaluates to 1

case (1, 2, 3) of (_, result, _) => result -- evaluates to 2

case (1, 2, 3) of (_, _, result) => result -- evaluates to 3

```

Haskell does not provide standard functions like `fst` or `snd` for tuples with more than two components. The [`tuple`](https://hackage.haskell.org/package/tuple) library on Hackage provides such functions in the [`Data.Tuple.Select`](https://hackage.haskell.org/package/tuple/docs/Data-Tuple-Select.html) module.



## Strictness of matching a tuple


The pattern `(p1, p2)` is strict in the outermost tuple constructor, which can lead to [unexpected strictness behaviour](http://stackoverflow.com/q/39159825/477476). For example, the following expression diverges (using `Data.Function.fix`):

```hs
fix $ \(x, y) -> (1, 2)

```

since the match on `(x, y)` is strict in the tuple constructor. However, the following expression, using an [irrefutable pattern](https://stackoverflow.com/documentation/haskell/3798/strictness/20142/lazy-patterns#t=201608280608557384509), evaluates to `(1, 2)` as expected:

```hs
fix $ \ ~(x, y) -> (1, 2)

```



## Construct tuple values


Use parentheses and commas to create tuples. Use one comma to create a pair.

```hs
(1, 2)

```

Use more commas to create tuples with more components.

```hs
(1, 2, 3)

(1, 2, 3, 4)

```

Note that it is also possible to declare tuples using in their unsugared form.

```hs
(,) 1 2     -- equivalent to (1,2)
(,,) 1 2 3  -- equivalent to (1,2,3)

```

Tuples can contain values of different types.

```hs
("answer", 42, '?')

```

Tuples can contain complex values such as lists or more tuples.

```hs
([1, 2, 3], "hello", ('A', 65))

(1, (2, (3, 4), 5), 6)

```



## Write tuple types


Use parentheses and commas to write tuple types. Use one comma to write a pair type.

```hs
(Int, Int)

```

Use more commas to write tuple types with more components.

```hs
(Int, Int, Int)

(Int, Int, Int, Int)

```

Tuples can contain values of different types.

```hs
(String, Int, Char)

```

Tuples can contain complex values such as lists or more tuples.

```hs
([Int], String, (Char, Int))

(Int, (Int, (Int, Int), Int), Int)

```



## Pattern Match on Tuples


Pattern matching on tuples uses the tuple constructors. To match a pair for example, we'd use the `(,)` constructor:

```hs
myFunction1 (a, b) = ...

```

We use more commas to match tuples with more components:

```hs
myFunction2 (a, b, c) = ...

myFunction3 (a, b, c, d) = ...

```

Tuple patterns can contain complex patterns such as list patterns or more tuple patterns.

```hs
myFunction4 ([a, b, c], d, e) = ...

myFunction5 (a, (b, (c, d), e), f) = ...

```



## Apply a binary function to a tuple (uncurrying)


Use the `uncurry` function (from `Prelude` or `Data.Tuple`) to convert a binary function to a function on tuples.

```hs
uncurry (+) (1, 2) -- computes 3

uncurry map (negate, [1, 2, 3]) -- computes [-1, -2, -3]

uncurry uncurry ((+), (1, 2)) -- computes 3

map (uncurry (+)) [(1, 2), (3, 4), (5, 6)] -- computes [3, 7, 11]

uncurry (curry f) -- computes the same as f

```



## Apply a tuple function to two arguments (currying)


Use the `curry` function (from `Prelude` or `Data.Tuple`) to convert a function that takes tuples to a function that takes two arguments.

```hs
curry fst 1 2 -- computes 1

curry snd 1 2 -- computes 2

curry (uncurry f) -- computes the same as f

import Data.Tuple (swap)
curry swap 1 2 -- computes (2, 1)

```



## Swap pair components


Use `swap` (from `Data.Tuple`) to swap the components of a pair.

```hs
import Data.Tuple (swap)
swap (1, 2) -- evaluates to (2, 1)

```

Or use pattern matching.

```hs
case (1, 2) of (x, y) => (y, x) -- evaluates to (2, 1)

```



#### Remarks


<li>
Haskell does not support tuples with one component natively.
</li>
<li>
Units (written `()`) can be understood as tuples with zero components.
</li>
<li>
There are no predefined functions to extract components of tuples with more than two components. If you feel that you need such functions, consider using a custom data type with record labels instead of the tuple type. Then you can use the record labels as functions to extract the components.
</li>

