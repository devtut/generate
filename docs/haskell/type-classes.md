---
metaTitle: "Haskell - Type Classes"
description: "Eq, Ord, Monoid, Num, Maybe and the Functor Class, Type class inheritance: Ord type class"
---

# Type Classes


Typeclasses in Haskell are a means of defining the behaviour associated with a type separately from that type's definition. Whereas, say, in Java, you'd define the behaviour as part of the type's definition -- i.e. in an interface, abstract class or concrete class -- Haskell keeps these two things separate.

There are a number of typeclasses already defined in Haskell's `base` package. The relationship between these is illustrated in the Remarks section below.



## Eq


All basic datatypes (like `Int`, `String`, `Eq a => [a]`) from Prelude except for functions and `IO` have instances of `Eq`. If a type instantiates `Eq` it means that we know how to compare two values for **value** or **structural** equality.

```hs
> 3 == 2 
False
> 3 == 3
True

```

### Required methods

- `(==) :: Eq a => a -> a -> Boolean` or `(/=) :: Eq a => a -> a -> Boolean` (if only one is implemented, the other defaults to the negation of the defined one)

### Defines

- `(==) :: Eq a => a -> a -> Boolean`
- `(/=) :: Eq a => a -> a -> Boolean`

### Direct superclasses

None

### Notable subclasses

- [`Ord`](https://stackoverflow.com/documentation/haskell/1879/type-classes/7440/ord)



## Ord


Types instantiating `Ord` include, e.g., `Int`, `String`, and `[a]` (for types `a` where there's an `Ord a` instance). If a type instantiates `Ord` it means that we know a “natural” ordering of values of that type. Note, there are often many possible choices of the “natural” ordering of a type and `Ord` forces us to favor one.

`Ord` provides the standard `(<=)`, `(<)`, `(>)`, `(>=)` operators but interestingly defines them all using a custom algebraic data type

```hs
data Ordering = LT | EQ | GT

compare :: Ord a => a -> a -> Ordering

```

### Required methods

- `compare :: Ord a => a -> a -> Ordering` or `(<=) :: Ord a => a -> a -> Boolean` (the standard’s default `compare` method uses `(<=)` in its implementation)

### Defines

- `compare :: Ord a => a -> a -> Ordering`
- `(<=) :: Ord a => a -> a -> Boolean`
- `(<) :: Ord a => a -> a -> Boolean`
- `(>=) :: Ord a => a -> a -> Boolean`
- `(>) :: Ord a => a -> a -> Boolean`
- `min :: Ord a => a -> a -> a`
- `max :: Ord a => a -> a -> a`

### Direct superclasses

- [`Eq`](https://stackoverflow.com/documentation/haskell/1879/type-classes/7439/eq)



## Monoid


Types instantiating [`Monoid`](http://stackoverflow.com/documentation/haskell/2211/monoid#t=201607241111111418916) include lists, numbers, and functions with `Monoid` return values, among others. To instantiate `Monoid` a type must support an associative binary operation (`mappend` or `(<>)`) which combines its values, and have  a special "zero" value (`mempty`) such that combining a value with it does not change that value:

```hs
mempty  <>  x == x
x <>  mempty  == x

x <> (y <> z) == (x <> y) <> z

```

Intuitively, `Monoid` types are "list-like" in that they support appending values together. Alternatively, `Monoid` types can be thought of as sequences of values for which we care about the order but not the grouping. For instance, a binary tree is a `Monoid`, but using the `Monoid` operations we cannot witness its branching structure, only a traversal of its values (see `Foldable` and `Traversable`).

### Required methods

- `mempty :: Monoid m => m`
- `mappend :: Monoid m => m -> m -> m`

### Direct superclasses

None



## Num


The most general class for number types, more precisely for [rings](https://en.wikipedia.org/wiki/Ring_(mathematics)), i.e. numbers that can be added and subtracted and multiplied in the usual sense, but not necessarily divided.

This class contains both integral types (`Int`, `Integer`, `Word32` etc.) and fractional types (`Double`, `Rational`, also complex numbers etc.). In case of finite types, the semantics are generally understood as **modular arithmetic**, i.e. with over- and underflow<sup>†</sup>.

Note that the rules for the numerical classes are much less strictly obeyed than the [monad](http://stackoverflow.com/documentation/haskell/2968/monads/6144/monad-laws-and-the-maybe-monad#t=201610010024186706477) or monoid laws, or those for [equality comparison](http://stackoverflow.com/documentation/haskell/2264/standard-and-popular-type-classes/7439/eq). In particular, floating-point numbers generally obey laws only in a approximate sense.

### The methods

<li>
`fromInteger :: Num a => Integer -> a`. convert an integer to the general number type (wrapping around the range, if necessary). Haskell [number literals](http://stackoverflow.com/documentation/haskell/369/overloaded-literals/1243/integer-numeral) can be understood as a monomorphic `Integer` literal with the general conversion around it, so you can use the literal `5` in both an `Int` context and a `Complex Double` setting.
</li>
<li>
`(+) :: Num a => a -> a -> a`. Standard addition, generally understood as associative and commutative, i.e.,

```hs
  a + (b + c) ≡ (a + b) + c
  a + b ≡ b + a

```


</li>
<li>
`(-) :: Num a => a -> a -> a`. Subtraction, which is the inverse of addition:

```hs
  (a - b) + b ≡ (a + b) - b ≡ a

```


</li>
<li>
`(*) :: Num a => a -> a -> a`. Multiplication, an associative operation that's distributive over addition:

```hs
  a * (b * c) ≡ (a * b) * c
  a * (b + c) ≡ a * b + a * c

```


for the most common instances, multiplication is also commutative, but this is definitely not a requirement.
</li>
<li>
`negate :: Num a => a -> a`. The full name of the unary negation operator. `-1` is syntactic sugar for `negate 1`.

```hs
  -a ≡ negate a ≡ 0 - a

```


</li>
<li>
`abs :: Num a => a -> a`. The absolute-value function always gives a non-negative result of the same magnitude

```hs
  abs (-a) ≡ abs a
  abs (abs a) ≡ abs a

```


`abs a ≡ 0` should only happen if `a ≡ 0`.
For [real](http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#t:Real) types it's clear what non-negative means: you always have `abs a >= 0`. Complex etc. types don't have a well-defined ordering, however the result of `abs` should always lie in the real subset<sup>‡</sup> (i.e. give a number that could also be written as a single number literal without negation).
</li>
<li>
`signum :: Num a => a -> a`. The sign function, according to the name, yields only `-1` or `1`, depending on the sign of the argument. Actually, that's only true for nonzero real numbers; in general `signum` is better understood as the **normalising** function:

```hs
  abs (signum a) ≡ 1   -- unless a≡0
  signum a * abs a ≡ a -- This is required to be true for all Num instances

```


Note that [section 6.4.4 of the Haskell 2010 Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1390006.4.4) explicitly requires this last equality to hold for any valid `Num` instance.
</li>

Some libraries, notably [linear](http://hackage.haskell.org/package/linear) and [hmatrix](http://hackage.haskell.org/package/hmatrix), have a much laxer understanding of what the `Num` class is for: they treat it just as **a way to overload the arithmetic operators**. While this is pretty straightforward for `+` and `-`, it already becomes troublesome with `*` and more so with the other methods. For instance, **should `*` mean matrix multiplication or element-wise multiplication?**
<br>
It is arguably a bad idea to define such non-number instances; please consider dedicated classes such as [`VectorSpace`](http://hackage.haskell.org/package/vector-space-0.10.2/docs/Data-VectorSpace.html).

<sup>†</sup>
<sub>In particular, the “negatives” of unsigned types are wrapped around to large positive, e.g. `(-4 :: Word32) == 4294967292`.</sub>

<sup>‡</sup>
<sub>This is widely **not** fulfilled: vector types do not have a real subset. The controversial `Num`-instances for such types generally define `abs` and `signum` element-wise, which mathematically speaking doesn't really make sense.</sub>



## Maybe and the Functor Class


In Haskell, data types can have arguments just like functions. Take the `Maybe` type for example.

`Maybe` is a very useful type which allows us to represent the idea of failure, or the possiblity thereof. In other words, if there is a possibility that a computation will fail, we use the `Maybe` type there. `Maybe` acts kind of like a wrapper for other types, giving them additional functionality.

Its actual declaration is fairly simple.

```hs
Maybe a = Just a | Nothing

```

What this tells is that a `Maybe` comes in two forms, a `Just`, which represents success, and a `Nothing`, which represents failure. `Just` takes one argument which determines the type of the `Maybe`, and `Nothing` takes none. For example, the value `Just "foo"` will have type `Maybe String`, which is a string type wrapped with the additional `Maybe` functionality. The value `Nothing` has type `Maybe a` where `a` can be any type.

This idea of wrapping types to give them additional functionality is a very useful one, and is applicable to more than just `Maybe`. Other examples include the `Either`, `IO` and list types, each providing different functionality.  However, there are some actions and abilities which are common to all of these wrapper types. The most notable of these is the ability to modify the encapsulated value.

It is common to think of these kinds of types as boxes which can have values placed in them. Different boxes hold different values and do different things, but none are useful without being able to access the contents within.

To encapsulate this idea, Haskell comes with a standard typeclass, named `Functor`. It is defined as follows.

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b

```

As can be seen, the class has a single function, `fmap`, of two arguments. The first argument is a function from one type, `a`, to another, `b`. The second argument is a functor (wrapper type) containing a value of type `a`. It returns a functor (wrapper type) containing a value of type `b`.

In simple terms, `fmap` takes a function and applies to the value inside of a functor. It is the only function necessary for a type to be a member of the `Functor` class, but it is extremely useful. Functions operating on functors that have more specific applications can be found in the `Applicative` and `Monad` typeclasses.



## Type class inheritance: Ord type class


Haskell supports a notion of class extension. For example, the class `Ord` inherits all of the operations in `Eq`, but in addition has a `compare` function that returns an `Ordering` between values. `Ord` may also contain the common order comparison operators, as well as a `min` method and a `max` method.

The `=>` notation has the same meaning as it does in a function signature and requires type `a` to implement `Eq`, in order to implement `Ord`.

```hs
data Ordering = EQ | LT | GT

class Eq a => Ord a where
    compare :: Ord a => a -> a -> Ordering
    (<)     :: Ord a => a -> a -> Bool
    (<=)    :: Ord a => a -> a -> Bool
    (>)     :: Ord a => a -> a -> Bool
    (>=)    :: Ord a => a -> a -> Bool
    min     :: Ord a => a -> a -> a
    max     :: Ord a => a -> a -> a

```

All of the methods following `compare` can be derived from it in a number of ways:

```hs
x < y   = compare x y == LT
x <= y  = x < y || x == y -- Note the use of (==) inherited from Eq
x > y   = not (x <= y)
x >= y  = not (x < y)

min x y = case compare x y of
               EQ -> x
               LT -> x
               GT -> y

max x y = case compare x y of
               EQ -> x
               LT -> y
               GT -> x

```

Type classes that themselves extend `Ord` must implement at least either the `compare` method or the `(<=)`  method themselves, which builds up the directed inheritance lattice.



#### Remarks


The following diagram taken from the [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) article shows the relationship between the various typeclasses in Haskell.

[<img src="https://i.stack.imgur.com/Fph6t.png" alt="Relationships among standard Haskell type classes, Figure 1 as published in Typeclassopedia." />](https://i.stack.imgur.com/Fph6t.png)

