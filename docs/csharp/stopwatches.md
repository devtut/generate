---
metaTitle: "C# | Stopwatches"
description: "IsHighResolution, Creating an Instance of a Stopwatch"
---

# Stopwatches



## IsHighResolution


- The IsHighResolution property indicates whether the timer is based on a high-resolution performance counter or based on the DateTime class.
- This field is read-only.

```cs
// Display the timer frequency and resolution.
if (Stopwatch.IsHighResolution)
{
    Console.WriteLine("Operations timed using the system's high-resolution performance counter.");
}
else 
{
    Console.WriteLine("Operations timed using the DateTime class.");
}

long frequency = Stopwatch.Frequency;
Console.WriteLine("  Timer frequency in ticks per second = {0}",
    frequency);
long nanosecPerTick = (1000L*1000L*1000L) / frequency;
Console.WriteLine("  Timer is accurate within {0} nanoseconds", 
    nanosecPerTick);
}

```

[https://dotnetfiddle.net/ckrWUo](https://dotnetfiddle.net/ckrWUo)

The timer used by the Stopwatch class depends on the system hardware and operating system. IsHighResolution is true if the Stopwatch timer is based on a high-resolution performance counter. Otherwise, IsHighResolution is false, which indicates that the Stopwatch timer is based on the system timer.

Ticks in Stopwatch are machine/OS dependent, thus you should never count on the ration of Stopwatch ticks to seconds to be the same between two systems, and possibly even on the same system after a reboot.  Thus, you can never count on Stopwatch ticks to be the same interval as DateTime/TimeSpan ticks.

To get system-independent time, make sure to use the Stopwatch’s Elapsed or ElapsedMilliseconds properties, which already take the Stopwatch.Frequency (ticks per second) into account.

Stopwatch should always be used over Datetime for timing processes as it is more lightweight and uses Dateime if it cant use a high-resolution performance counter.

[Source](http://geekswithblogs.net/BlackRabbitCoder/archive/2012/01/12/c.net-little-pitfalls-stopwatch-ticks-are-not-timespan-ticks.aspx)



## Creating an Instance of a Stopwatch


A Stopwatch instance can measure elapsed time over several intervals with the total elapsed time being all individual intervals added together. This gives a reliable method of measuring elapsed time between two or more events.

```cs
Stopwatch stopWatch = new Stopwatch();
stopWatch.Start();

double d = 0;
for (int i = 0; i < 1000 * 1000 * 1000; i++)
{
    d += 1;
}

stopWatch.Stop();
Console.WriteLine("Time elapsed: {0:hh\\:mm\\:ss\\.fffffff}", stopWatch.Elapsed);

```

`Stopwach` is in `System.Diagnostics` so you need to add `using System.Diagnostics;` to your file.



#### Syntax


- stopWatch.Start() - Starts Stopwatch.
- stopWatch.Stop()  - Stops Stopwatch.
- stopWatch.Elapsed - Gets the total elapsed time measured by the current interval.



#### Remarks


Stopwatches are often used in benchmarking programs to time code and see how optimal different segments of code take to run.

