---
metaTitle: "Haskell - Common monads as free monads"
description: "Free Empty ~~ Identity, Free Identity ~~ (Nat,) ~~ Writer Nat, Free Maybe ~~ MaybeT (Writer Nat), Free (Writer w) ~~ Writer [w], Free (Const c) ~~ Either c, Free (Reader x) ~~ Reader (Stream x)"
---

# Common monads as free monads




## Free Empty ~~ Identity


Given

```hs
data Empty a

```

we have

```hs
data Free Empty a
     = Pure a
-- the Free constructor is impossible!

```

which is isomorphic to

```hs
data Identity a
     = Identity a

```



## Free Identity ~~ (Nat,) ~~ Writer Nat


Given

```hs
data Identity a = Identity a

```

we have

```hs
data Free Identity a
     = Pure a
     | Free (Identity (Free Identity a))

```

which is isomorphic to

```hs
data Deferred a
     = Now a
     | Later (Deferred a)

```

or equivalently (if you promise to evaluate the fst element first) `(Nat, a)`, aka `Writer Nat a`, with

```hs
data Nat = Z | S Nat
data Writer Nat a = Writer Nat a

```



## Free Maybe ~~ MaybeT (Writer Nat)


Given

```hs
data Maybe a = Just a
             | Nothing

```

we have

```hs
data Free Maybe a
     = Pure a
     | Free (Just (Free Maybe a))
     | Free Nothing

```

which is equivalent to

```hs
data Hopes a
     = Confirmed a
     | Possible (Hopes a)
     | Failed

```

or equivalently (if you promise to evaluate the fst element first) `(Nat, Maybe a)`, aka `MaybeT (Writer Nat) a` with

```hs
data Nat = Z | S Nat
data Writer Nat a = Writer Nat a
data MaybeT (Writer Nat) a = MaybeT (Nat, Maybe a)

```



## Free (Writer w) ~~ Writer [w]


Given

```hs
data Writer w a = Writer w a

```

we have

```hs
data Free (Writer w) a
     = Pure a
     | Free (Writer w (Free (Writer w) a))

```

which is isomorphic to

```hs
data ProgLog w a
     = Done a
     | After w (ProgLog w a)

```

or, equivalently, (if you promise to evaluate the log first), `Writer [w] a`.



## Free (Const c) ~~ Either c


Given

```hs
data Const c a = Const c

```

we have

```hs
data Free (Const c) a
     = Pure a
     | Free (Const c)

```

which is isomorphic to

```hs
data Either c a
     = Right a
     | Left c

```



## Free (Reader x) ~~ Reader (Stream x)


Given

```hs
data Reader x a = Reader (x -> a)

```

we have

```hs
data Free (Reader x) a
     = Pure a
     | Free (x -> Free (Reader x) a)

```

which is isomorphic to

```hs
data Demand x a
     = Satisfied a
     | Hungry (x -> Demand x a)

```

or equivalently `Stream x -> a` with

```hs
data Stream x = Stream x (Stream x)

```

