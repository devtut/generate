---
metaTitle: ".NET Framework - Using Progress<T> and IProgress<T>"
description: "Simple Progress reporting, Using IProgress<T>"
---

# Using Progress<T> and IProgress<T>



## Simple Progress reporting


`IProgress<T>` can be used to report progress of some procedure to another procedure. This example shows how you can create a basic method that reports its progress.

```dotnet
void Main()
{
    IProgress<int> p = new Progress<int>(progress =>
    {
        Console.WriteLine("Running Step: {0}", progress);
    });
    LongJob(p);
}

public void LongJob(IProgress<int> progress)
{
    var max = 10;
    for (int i = 0; i < max; i++)
    {
        progress.Report(i);
    }
}

```

Output:

```dotnet
Running Step: 0
Running Step: 3
Running Step: 4
Running Step: 5
Running Step: 6
Running Step: 7
Running Step: 8
Running Step: 9
Running Step: 2
Running Step: 1

```

Note that when you this code runs, you may see numbers be output out of order. This is because the `IProgress<T>.Report()` method is run asynchronously, and is therefore not as suitable for situations where the progress must be reported in order.



## Using IProgress<T>


It's important to note that the `System.Progress<T>` class does not have the `Report()` method available on it. This method was implemented explicitly from the `IProgress<T>` interface, and therefore must be called on a `Progress<T>` when it's cast to an `IProgress<T>`.

```dotnet
var p1 = new Progress<int>();
p1.Report(1); //compiler error, Progress does not contain method 'Report'

IProgress<int> p2 = new Progress<int>();
p2.Report(2); //works

var p3 = new Progress<int>();
((IProgress<int>)p3).Report(3); //works

```

