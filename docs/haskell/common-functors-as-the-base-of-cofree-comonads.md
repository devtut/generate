---
metaTitle: "Haskell - Common functors as the base of cofree comonads"
description: "Cofree Empty ~~ Empty, Cofree (Const c) ~~ Writer c, Cofree Identity ~~ Stream, Cofree Maybe ~~ NonEmpty, Cofree (Writer w) ~~ WriterT w Stream, Cofree (Either e) ~~ NonEmptyT (Writer e), Cofree (Reader x) ~~ Moore x"
---

# Common functors as the base of cofree comonads




## Cofree Empty ~~ Empty


Given

```hs
data Empty a

```

we have

```hs
data Cofree Empty a
   --  = a :< ...  not possible!

```



## Cofree (Const c) ~~ Writer c


Given

```hs
data Const c a = Const c

```

we have

```hs
data Cofree (Const c) a
     = a :< Const c

```

which is isomorphic to

```hs
data Writer c a = Writer c a

```



## Cofree Identity ~~ Stream


Given

```hs
data Identity a = Identity a

```

we have

```hs
data Cofree Identity a
     = a :< Identity (Cofree Identity a)

```

which is isomorphic to

```hs
data Stream a = Stream a (Stream a)

```



## Cofree Maybe ~~ NonEmpty


Given

```hs
data Maybe a = Just a
             | Nothing

```

we have

```hs
data Cofree Maybe a
     = a :< Just (Cofree Maybe a)
     | a :< Nothing

```

which is isomorphic to

```hs
data NonEmpty a
     = NECons a (NonEmpty a)
     | NESingle a

```



## Cofree (Writer w) ~~ WriterT w Stream


Given

```hs
data Writer w a = Writer w a

```

we have

```hs
data Cofree (Writer w) a
     = a :< (w, Cofree (Writer w) a)

```

which is equivalent to

```hs
data Stream (w,a)
     = Stream (w,a) (Stream (w,a))

```

which can properly be written as `WriterT w Stream` with

```hs
data WriterT w m a = WriterT (m (w,a))

```



## Cofree (Either e) ~~ NonEmptyT (Writer e)


Given

```hs
data Either e a = Left e
                | Right a

```

we have

```hs
data Cofree (Either e) a
     = a :< Left e
     | a :< Right (Cofree (Either e) a)

```

which is isomorphic to

```hs
data Hospitable e a
     = Sorry_AllIHaveIsThis_Here'sWhy a e
     | EatThis a (Hospitable e a)

```

or, if you promise to only evaluate the log after the complete result, `NonEmptyT (Writer e) a` with

```hs
data NonEmptyT (Writer e) a = NonEmptyT (e,a,[a])

```



## Cofree (Reader x) ~~ Moore x


Given

```hs
data Reader x a = Reader (x -> a)

```

we have

```hs
data Cofree (Reader x) a
     = a :< (x -> Cofree (Reader x) a)

```

which is isomorphic to

```hs
data Plant x a
     = Plant a (x -> Plant x a)

```

aka [Moore machine](http://hackage.haskell.org/package/machines-0.6.1/docs/Data-Machine-Moore.html#t:Moore).

