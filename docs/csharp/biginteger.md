---
metaTitle: "C# | BigInteger"
description: "Calculate the First 1,000-Digit Fibonacci Number"
---

# BigInteger



## Calculate the First 1,000-Digit Fibonacci Number


Include `using System.Numerics` and add a reference to `System.Numerics` to the project.

```cs
using System;
using System.Numerics;

namespace Euler_25
{
    class Program
    {
        static void Main(string[] args)
        {
            BigInteger l1 = 1;
            BigInteger l2 = 1;
            BigInteger current = l1 + l2;
            while (current.ToString().Length < 1000)
            {
                l2 = l1;
                l1 = current;
                current = l1 + l2;
            }
            Console.WriteLine(current);
        }
    }
}

```

This simple algorithm iterates through Fibonacci numbers until it reaches one at least 1000 decimal digits in length, then prints it out. This value is significantly larger than even a `ulong` could hold.

Theoretically, the only limit on the `BigInteger` class is the amount of RAM your application can consume.

Note: `BigInteger` is only available in .NET 4.0 and higher.



#### Remarks


### When To Use

`BigInteger` objects are by their very nature very heavy on RAM. Consequently, they should only be used when absolutely necessary, ie for numbers on a truly astronomical scale.

Further to this, all arithmetic operations on these objects are an order of magnitude slower than their primitive counterparts, this problem gets further compounded as the number grows as they are not of a fixed size. It is therefore feasibly possible for a rogue `BigInteger` to cause a crash by consuming all of the available RAM.

### Alternatives

If speed is imperative to your solution it may be more efficient to implement this functionality yourself using a class wrapping a `Byte[]` and overloading the necessary operators yourself. However, this does require a significant amount of extra effort.

