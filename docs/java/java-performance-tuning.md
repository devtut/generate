---
metaTitle: "Java Performance Tuning"
description: "Reducing amount of Strings, An evidence-based approach to Java performance tuning, General approach"
---

# Java Performance Tuning



## Reducing amount of Strings


In Java, it's too "easy" to create many String instances which are not needed. That and other reasons might cause your program to have lots of Strings that the GC is busy cleaning up.

Some ways you might be creating String instances:

```java
myString += "foo";

```

Or worse, in a loop or recursion:

```java
for (int i = 0; i < N; i++) {
    myString += "foo" + i;
}

```

The problem is that each `+` creates a new String (usually, since new compilers optimize some cases). A possible optimization can be made using `StringBuilder` or `StringBuffer`:

```java
StringBuffer sb = new StringBuffer(myString);
for (int i = 0; i < N; i++) {
    sb.append("foo").append(i);
}
myString = sb.toString();

```

If you build long Strings often (SQLs for example), use a String building API.

Other things to consider:

- Reduce usage of `replace`, `substring` etc.
- Avoid `String.toArray()`, especially in frequently accessed code.
- Log prints which are destined to be filtered (due to log level for example) should not be generated (log level should be checked in advance).
- Use libraries like [this](https://commons.apache.org/proper/commons-lang/) if necessary.
- StringBuilder is better if the variable is used in a non-shared manner (across threads).



## An evidence-based approach to Java performance tuning


Donald Knuth is often quoted as saying this:

> 
"Programmers waste enormous amounts of time thinking about, or worrying about, the speed of noncritical parts of their programs, and these attempts at efficiency actually have a strong negative impact when debugging and maintenance are considered. **We should forget about small efficiencies, say about 97% of the time**: premature optimization is the root of all evil. Yet we should not pass up our opportunities in that critical 3%."


[source](https://en.wikiquote.org/wiki/Donald_Knuth#Computer_Programming_as_an_Art_.281974.29)

Bearing that sage advice in mind, here is the recommended procedure for optimizing programs:

<li>
First of all, design and code your program or library with a focus on simplicity and correctness.  To start with, don't spend much effort on performance.
</li>
<li>
Get it to a working state, and (ideally) develop unit tests for the key parts of the codebase.
</li>
<li>
Develop an application level performance benchmark.  The benchmark should cover the performance critical aspects of your application, and should perform a range of tasks that are typical of how the application will be used in production.
</li>
<li>
Measure the performance.
</li>
<li>
Compare the measured performance against your criteria for how fast the application needs to be.  (Avoid unrealistic, unattainable or unquantifiable criteria such as "as fast as possible".)
</li>
<li>
If you have met the criteria, STOP.  You job is done.  (Any further effort is probably a waste of time.)
</li>
<li>
Profile the application while it is running your performance benchmark.
</li>
<li>
Examine the profiling results and pick the biggest (unoptimized) "performance hotspots"; i.e. sections of the code where the application seems to be spending the most time.
</li>
<li>
Analyse the hotspot code section to try to understand why it is a bottleneck, and think of a way to make it faster.
</li>
<li>
Implement that as a proposed code change, test and debug.
</li>
<li>
Rerun the benchmark to see if the code change has improved the performance:
<ul>
1. If Yes, then return to step 4.
1. If No, then abandon the change and return to step 9.  If you are making no progress, pick a different hotspot for your attention.
</ul>
</li>

Eventually you will get to a point where the application is either fast enough, or you have considered all of the significant hotspots.  At this point you need to stop this approach.  If a section of code is consuming (say) 1% of the overall time, then even a 50% improvement is only going to make the application 0.5% faster overall.

Clearly, there is a point beyond which hotspot optimization is a waste of effort.  If you get to that point, you need to take a more radical approach.  For example:

- Look at the algorithmic complexity of your core algorithms.
- If the application is spending a lot of time garbage collection, look for ways to reduce the rate of object creation.
- If key parts of the application are CPU intensive and single-threaded, look for opportunities for parallelism.
- If the application is already multi-threaded, look for concurrency bottlenecks.

But wherever possible, rely on tools and measurement rather than instinct to direct your optimization effort.



## General approach


The internet is packed with tips for performance improvement of Java programs. Perhaps the number one tip is awareness. That means:

- Identify possible performance problems and bottlenecks.
- Use analyzing and testing tools.
- Know good practices and bad practices.

The first point should be done during the design stage if speaking about a new system or module. If speaking about legacy code, analyzing and testing tools come into the picture. The most basic tool for analyzing your JVM performance is JVisualVM, which is included in the JDK.

The third point is mostly about experience and extensive research, and of course raw tips that will show up on this page and others, like [this](http://javaperformancetuning.com/tips/rawtips.shtml).

