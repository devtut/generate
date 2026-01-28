---
metaTitle: "C# | Making a variable thread safe"
description: "Controlling access to a variable in a Parallel.For loop"
---

# Making a variable thread safe



## Controlling access to a variable in a Parallel.For loop


```cs
using System;
using System.Threading;
using System.Threading.Tasks;

class Program
{
    static void Main( string[] args )
    {
        object sync = new object();
        int sum = 0;
        Parallel.For( 1, 1000, ( i ) => {
            lock( sync ) sum = sum + i; // lock is necessary

            // As a practical matter, ensure this `parallel for` executes
            // on multiple threads by simulating a lengthy operation.
            Thread.Sleep( 1 );
        } );
        Console.WriteLine( "Correct answer should be 499500.  sum is: {0}", sum );
    }
}

```

It is not sufficient to just do `sum = sum + i` without the lock because the read-modify-write operation is not atomic.  A thread will overwrite any external modifications to `sum` that occur after it has read the current value of `sum`, but before it stores the modified value of `sum + i` back into `sum`.

