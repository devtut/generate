---
metaTitle: "Visual Basic .NET - Random"
description: "Declaring an instance, Generate a random number from an instance of Random"
---

# Random


The Random class is used to generate non-negative pseudo-random integers that are not truly random, but are for general purposes close enough.

The sequence is calculated using an initial number (called the **Seed**) In earlier versions of .net, this seed number was the same every time an application was run. So what would happen was that you would get the same sequence of pseudo-random numbers every time the application was executed. Now, the seed is based on the time the object is declared.



## Declaring an instance


```vb
Dim rng As New Random()

```

This declares an instance of the Random class called `rng`. In this case, the current time at the point where the object is created is used to calculate the seed. This is the most common usage, but has its own problems as we shall see later in the remarks

Instead of allowing the program to use the current time as part of the calculation for the initial seed number, you can specify the initial seed number. This can be any 32 bit integer literal, constant or variable. See below for examples. Doing this means that your instance will generate the same sequence of pseudo-random numbers, which can be useful in certain situations.

```vb
Dim rng As New Random(43352)

```

or

```vb
Dim rng As New Random(x)

```

where `x` has been declared elsewhere in your program as an Integer constant or variable.



## Generate a random number from an instance of Random


The following example declares a new instance of the Random class and then uses the method `.Next` to generate the next number in the sequence of pseudo-random numbers.

```vb
Dim rnd As New Random
Dim x As Integer
x = rnd.Next

```

The last line above will generate the next pseudo-random number and assign it to `x`. This number will be in the range of 0 - 2147483647. However, you can also specify the range of numbers to be generated as in the example below.

```vb
x = rnd.Next(15, 200)

```

Please note however, that using these parameters, range of numbers will be between 15 or above and 199 or below.

You can also generate floating point numbers of the type Double by using `.NextDouble` e.g

```vb
Dim rnd As New Random
Dim y As Double
y = rnd.NextDouble()

```

You cannot however specify a range for this. It will always be in the range of 0.0 to less than 1.0.



#### Remarks


Finally, a note about randomization. As mentioned earlier, when you declare an instance of `Random` without any parameters, the constructor will use the current time as part of the calculation to create the initial seed number. Normally this is OK.

However. If you re-declare new instances over a very short space of time, each time the seed number is calculated, the time could be the same. Consider this code.

```vb
For i As Integer = 1 To 100000
    Dim rnd As New Random
    x = rnd.Next
Next

```

Because computers are very quick these days, this code will take a fraction of a second to run and on several dequential iterations of the loop, the system time will not have changed. So, the seed number will not change and the random number will be the same. If you want to generate lots of random numbers, declare the instance of random outside the loop in this simple example.

```vb
Dim rnd As New Random
For i As Integer = 1 To 100000
    x = rnd.Next
Next

```

**The basic rule of thumb is don't re-instantiate random number generator over short periods of time.**

