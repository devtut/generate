---
metaTitle: "Haskell - Type algebra"
description: "Addition and multiplication, Functions, Natural numbers in type algebra, Recursive types, Derivatives"
---

# Type algebra



## Addition and multiplication


The addition and multiplication have equivalents in this type algebra. They correspond to the **tagged unions** and **product types**.

```hs
data Sum a b = A a | B b
data Prod a b = Prod a b

```

We can see how the number of inhabitants of every type corresponds to the operations of the algebra.

Equivalently, we can use `Either` and `(,)` as type constructors for the addition and the multiplication. They are isomorphic to our previously defined types:

```hs
type Sum' a b = Either a b
type Prod' a b = (a,b)

```

The expected results of addition and multiplication are followed by the type algebra up to isomorphism. For example, we can see an isomorphism between 1 + 2, 2 + 1 and 3; as 1 + 2 = 3 = 2 + 1.

```hs
data Color = Red | Green | Blue

f :: Sum () Bool -> Color
f (Left ())     = Red
f (Right True)  = Green
f (Right False) = Blue

g :: Color -> Sum () Bool
g Red   = Left ()
g Green = Right True
g Blue  = Right False

f' :: Sum Bool () -> Color
f' (Right ())   = Red
f' (Left True)  = Green
f' (Left False) = Blue

g' :: Color -> Sum Bool ()
g' Red   = Right ()
g' Green = Left True
g' Blue  = Left False

```

### Rules of addition and multiplication

The common rules of commutativity, associativity and distributivity are valid because there are trivial isomorphisms between the following types:

```hs
-- Commutativity
Sum a b           <=> Sum b a
Prod a b          <=> Prod b a
-- Associativity
Sum (Sum a b) c   <=> Sum a (Sum b c)
Prod (Prod a b) c <=> Prod a (Prod b c)
-- Distributivity
Prod a (Sum b c)  <=> Sum (Prod a b) (Prod a c)

```



## Functions


Functions can be seen as exponentials in our algebra. As we can see, if we take a type `a` with n instances and a type `b` with m instances, the type `a -> b` will have m to the power of n instances.

As an example, `Bool -> Bool` is isomorphic to `(Bool,Bool)`, as 2*2 = 2².

```hs
iso1 :: (Bool -> Bool) -> (Bool,Bool)
iso1 f = (f True,f False)

iso2 :: (Bool,Bool) -> (Bool -> Bool)
iso2 (x,y) = (\p -> if p then x else y)

```



## Natural numbers in type algebra


We can draw a connection between the Haskell types and the natural numbers. This connection can be made assigning to every type the number of inhabitants it has.

### Finite union types

For finite types, it suffices to see that we can assign a natural type to every number, based in the number of constructors. For example:

```hs
type Color = Red | Yellow | Green

```

would be **3**. And the `Bool` type would be **2**.

```hs
type Bool = True | False

```

### Uniqueness up to isomorphism

We have seen that multiple types would correspond to a single number, but in this case, they would be isomorphic. This is to say that there would be a pair of morphisms `f` and `g`, whose composition would be the identity, connecting the two types.

```hs
f :: a -> b
g :: b -> a

f . g == id == g . f

```

In this case, we would say that the types are **isomorphic**. We will consider two types equal in our algebra as long as they are isomorphic.

For example, two different representations of the number two are trivally isomorphic:

```hs
type Bit  = I    | O
type Bool = True | False

bitValue :: Bit -> Bool
bitValue I = True
bitValue O = False

booleanBit :: Bool -> Bit
booleanBit True  = I
booleanBit False = O

```

Because we can see `bitValue . booleanBit == id == booleanBit . bitValue`

### One and Zero

The representation of the number **1** is obviously a type with only one constructor. In Haskell, this type is canonically the type `()`, called Unit. Every other type with only one constructor is isomorphic to `()`.

And our representation of **0** will be a type without constructors. This is the **Void** type in Haskell, as defined in `Data.Void`. This would be equivalent to a unhabited type, wihtout data constructors:

```hs
data Void

```



## Recursive types


### Lists

Lists can be defined as:

```hs
data List a = Nil | Cons a (List a) 

```

If we translate this into our type algebra, we get

> 
List(a) = 1 + a * List(a)


But we can now substitute **List(a)** again in this expression multiple times, in order to get:

> 
List(a) =  1 + a + a*a + a*a*a + a*a*a*a + ...


This makes sense if we see a list as a type that can contain only one value, as in `[]`; or every value of type `a`, as in `[x]`; or two values of type `a`, as in `[x,y]`; and so on. The theoretical definition of List that we should get from there would be:

```hs
-- Not working Haskell code!
data List a = Nil
            | One a
            | Two a a
            | Three a a a 
            ...

```

### Trees

We can do the same thing with binary trees, for example. If we define them as:

```hs
data Tree a = Empty | Node a (Tree a) (Tree a)

```

We get the expression:

> 
Tree(a) = 1 + a * Tree(a) * Tree(a)


And if we make the same substitutions again and again, we would obtain the following sequence:

> 
Tree(a) = 1 + a + 2 (a*a) + 5 (a*a*a) + 14 (a*a*a*a) + ...


The coefficients we get here correspond to the Catalan numbers sequence, and the n-th catalan number is precisely the number of possible binary trees with n nodes.



## Derivatives


The derivative of a type is the type of its type of one-hole contexts. This is the type that we would get if we make a type variable disappear in every possible point and sum the results.

As an example, we can take the triple type `(a,a,a)`, and derive it, obtaining

```hs
data OneHoleContextsOfTriple = (a,a,()) | (a,(),a) | ((),a,a)

```

This is coherent with our usual definition of derivation, as:

> 
d/da (a*a*a) = 3*a*a


More on this topic can be read on [this article](http://strictlypositive.org/diff.pdf).

