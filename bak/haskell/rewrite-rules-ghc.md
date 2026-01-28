---
metaTitle: "Haskell - Rewrite rules (GHC)"
description: "Using rewrite rules on overloaded functions"
---

# Rewrite rules (GHC)



## Using rewrite rules on overloaded functions


[In this question](http://stackoverflow.com/q/32130011/477476), @Viclib asked about using rewrite rules to exploit typeclass laws to eliminate some overloaded function calls:

> 
Mind the following class:

```hs
class ListIsomorphic l where
    toList    :: l a -> [a]
    fromList  :: [a] -> l a

```


I also demand that `toList . fromList == id`. How do I write rewrite rules to tell GHC to make that substitution?


This is a somewhat tricky use case for GHC's rewrite rules mechanism, because [overloaded functions are rewritten into their specific instance methods](http://stackoverflow.com/a/9815210/477476) by rules that are implicitly created behind the scenes by GHC (so something like `fromList :: Seq a -> [a]` would be rewritten into `Seq$fromList` etc.).

However, by first rewriting `toList` and `fromList` into non-inlined non-typeclass methods, [we can protect them from premature rewriting](http://stackoverflow.com/a/32133083/477476), and preserve them until the rule for the composition can fire:

```hs
{-# RULES
  "protect toList"   toList = toList';
  "protect fromList" fromList = fromList';
  "fromList/toList"  forall x . fromList' (toList' x) = x; #-}

{-# NOINLINE [0] fromList' #-}
fromList' :: (ListIsomorphic l) => [a] -> l a
fromList' = fromList

{-# NOINLINE [0] toList' #-}
toList' :: (ListIsomorphic l) => l a -> [a]
toList' = toList

```

