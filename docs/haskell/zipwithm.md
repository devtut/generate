---
metaTitle: "Haskell - zipWithM"
description: "Calculatings sales prices"
---

# zipWithM


`zipWithM` is to `zipWith` as `mapM` is to `map`: it lets you combine two lists using a monadic function.

From the module [`Control.Monad`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad.html#v:zipWithM)



## Calculatings sales prices


Suppose you want to see if a certain set of sales prices makes sense for a store.

The items originally cost $5, so you don't want to accept the sale if the sales price is less for any of them, but you do want to know what the new price is otherwise.

Calculating one price is easy: you calculate the sales price, and return `Nothing` if you don't get a profit:

```hs
calculateOne :: Double -> Double -> Maybe Double
calculateOne price percent = let newPrice = price*(percent/100)
                             in if newPrice < 5 then Nothing else Just newPrice

```

To calculate it for the entire sale, `zipWithM` makes it really simple:

```hs
calculateAllPrices :: [Double] -> [Double] -> Maybe [Double]
calculateAllPrices prices percents = zipWithM calculateOne prices percents

```

This will return `Nothing` if any of the sales prices are below $5.



#### Syntax


- zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]

