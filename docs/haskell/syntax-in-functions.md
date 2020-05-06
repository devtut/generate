---
metaTitle: "Haskell - Syntax in Functions"
description: "Pattern Matching, Using where and guards, Guards"
---

# Syntax in Functions



## Pattern Matching


Haskell supports pattern matching expressions in both function definition and through `case` statements.

A case statement is much like a switch in other languages, except it supports all of Haskell's types.

Let's start simple:

```hs
longName :: String -> String
longName name = case name of
                   "Alex"  -> "Alexander"
                   "Jenny" -> "Jennifer"
                   _       -> "Unknown"  -- the "default" case, if you like

```

Or, we could define our function like an equation which would be pattern matching, just without using a `case` statement:

```hs
longName "Alex"  = "Alexander"
longName "Jenny" = "Jennifer"
longName _       = "Unknown"

```

A more common example is with the `Maybe` type:

```hs
data Person = Person { name :: String, petName :: (Maybe String) }

hasPet :: Person -> Bool
hasPet (Person _ Nothing) = False
hasPet _ = True  -- Maybe can only take `Just a` or `Nothing`, so this wildcard suffices

```

Pattern matching can also be used on lists:

```hs
isEmptyList :: [a] -> Bool
isEmptyList [] = True
isEmptyList _  = False

addFirstTwoItems :: [Int] -> [Int]
addFirstTwoItems []        = []
addFirstTwoItems (x:[])    = [x]
addFirstTwoItems (x:y:ys)  = (x + y) : ys

```

Actually, Pattern Matching can be used on any constructor for any type class.
E.g. the constructor for lists is `:` and for tuples `,`



## Using where and guards


Given this function:

```hs
annualSalaryCalc :: (RealFloat a) => a -> a -> String
annualSalaryCalc hourlyRate weekHoursOfWork
  | hourlyRate * (weekHoursOfWork * 52) <= 40000  = "Poor child, try to get another job"
  | hourlyRate * (weekHoursOfWork * 52) <= 120000 = "Money, Money, Money!"
  | hourlyRate * (weekHoursOfWork * 52) <= 200000 = "Ri¢hie Ri¢h"
  | otherwise = "Hello Elon Musk!"

```

We can use `where` to avoid the repetition and make our code more readable. See the alternative function below, using `where`:

```hs
annualSalaryCalc' :: (RealFloat a) => a -> a -> String
annualSalaryCalc' hourlyRate weekHoursOfWork
  | annualSalary <= smallSalary  = "Poor child, try to get another job"
  | annualSalary <= mediumSalary = "Money, Money, Money!"
  | annualSalary <= highSalary   = "Ri¢hie Ri¢h"
  | otherwise = "Hello Elon Musk!"
  where 
      annualSalary = hourlyRate * (weekHoursOfWork * 52)
      (smallSalary, mediumSalary, highSalary)  = (40000, 120000, 200000)

```

As observed, we used the `where` in the end of the function body eliminating the repetition of the calculation (`hourlyRate * (weekHoursOfWork * 52)`) and we also used `where` to organize the salary range.

The naming of common sub-expressions can also be achieved with `let` expressions, but only the `where` syntax makes it possible for **guards** to refer to those named sub-expressions.



## Guards


A function can be defined using guards, which can be thought of classifying behaviour according to input.

Take the following function definition:

```hs
absolute :: Int -> Int  -- definition restricted to Ints for simplicity
absolute n = if (n < 0) then (-n) else n

```

We can rearrange it using guards:

```hs
absolute :: Int -> Int
absolute n 
  | n < 0 = -n
  | otherwise = n

```

In this context `otherwise` is a meaningful alias for `True`, so it should always be the last guard.

