---
metaTitle: "Haskell - Arrows"
description: "Function compositions with multiple channels"
---

# Arrows



## Function compositions with multiple channels


[`Arrow`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Arrow.html#t:Arrow) is, vaguely speaking, the class of morphisms that compose like functions, with both serial composition and “parallel composition”. While it is most interesting as a **generalisation** of functions, the `Arrow (->)` instance itself is already quite useful. For instance, the following function:

```hs
spaceAround :: Double -> [Double] -> Double
spaceAround x ys = minimum greater - maximum smaller
 where (greater, smaller) = partition (>x) ys

```

can also be written with arrow combinators:

```hs
spaceAround x = partition (>x) >>> minimum *** maximum >>> uncurry (-)

```

This kind of composition can best be visualised with a diagram:

```

                      ──── minimum ────
                   ╱           *            ╲
──── partition (>x) >>>        *        >>>  uncurry (-) ───
                   ╲           *            ╱
                       ──── maximum ──── 

```

Here,

<li>
The [`>>>` operator](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Arrow.html#v:-62--62--62-) is just a flipped version of the ordinary `.` composition operator (there's also a `<<<` version that composes right-to-left). It pipes the data from one processing step to the next.
</li>
<li>
the out-going `╱` `╲` indicate the data flow is split up in two “channels”. In terms of Haskell types, this is realised with tuples:

```hs
partition (>x) :: [Double] -> ([Double], [Double])

```


splits up the flow in two `[Double]` channels, whereas

```hs
uncurry (-) :: (Double,Double) -> Double

```


merges two `Double` channels.
</li>
<li>
[`***`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Arrow.html#v:-42--42--42-) is the parallel<sup>†</sup> composition operator. It lets `maximum` and `minimum` operate independently on different channels of the data. For functions, the signature of this operator is

```hs
(***) :: (b->c) -> (β->γ) -> (b,β)->(c,γ)

```


</li>

<sup>†</sup><sub>At least in the **Hask** category (i.e. in the `Arrow (->)` instance), `f***g` does not actually compute `f` and `g` in parallel as in, on different threads. This would theoretically be possible, though.</sub>

