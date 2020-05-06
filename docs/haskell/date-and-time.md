---
metaTitle: "Haskell - Date and Time"
description: "Finding Today's Date, Adding, Subtracting and Comparing Days"
---

# Date and Time



## Finding Today's Date


Current date and time can be found with `getCurrentTime`:

```hs
import Data.Time

print =<< getCurrentTime
-- 2016-08-02 12:05:08.937169 UTC

```

Alternatively, just the date is returned by `fromGregorian`:

```hs
fromGregorian 1984 11 17  -- yields a Day

```



## Adding, Subtracting and Comparing Days


Given a `Day`, we can perform simple arithmetic and comparisons, such as adding:

```hs
import Data.Time

addDays 1 (fromGregorian 2000 1 1)
-- 2000-01-02
addDays 1 (fromGregorian 2000 12 31)
-- 2001-01-01

```

Subtract:

```hs
addDays (-1) (fromGregorian 2000 1 1)
-- 1999-12-31

addDays (-1) (fromGregorian 0 1 1)
-- -0001-12-31
-- wat

```

and even find the difference:

```hs
diffDays (fromGregorian 2000 12 31) (fromGregorian 2000 1 1)
365

```

note that the order matters:

```hs
diffDays (fromGregorian 2000 1 1) (fromGregorian 2000 12 31)
-365

```



#### Syntax


<li>
[addDays](http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Calendar.html#v:addDays) :: Integer -> Day -> Day
</li>
<li>
[diffDays](http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Calendar.html#v:diffDays) :: Day -> Day -> Integer
</li>
<li>
[fromGregorian](http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Calendar.html#v:fromGregorian) :: Integer -> Int -> Int -> Day

```hs
 convert from proleptic Gregorian calendar. First argument is year, second month number (1-12), third day (1-31). Invalid values will be clipped to the correct range, month first, then day.

```


</li>
<li>
[getCurrentTime](http://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Clock.html#v:getCurrentTime) :: IO UTCTime
</li>



#### Remarks


The `Data.Time` module from [`time` package](http://hackage.haskell.org/package/time) provides support for retrieving & manipulating date & time values:

