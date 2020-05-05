---
metaTitle: "JVM Flags"
description: "-XXaggressive, -XXallocClearChunks, -XXallocClearChunkSize, -XXcallProfiling, -XXdisableFatSpin, -XXdisableGCHeuristics, -XXdumpSize, -XXexitOnOutOfMemory"
---

# JVM Flags



## -XXaggressive


`-XXaggressive` is a collection of configurations that make the JVM perform at a high speed and reach a stable state as soon as possible. To achieve this goal, the JVM uses more internal resources at startup; however, it requires less adaptive optimization once the goal is reached. We recommend that you use this option for long-running, memory-intensive applications that work alone.

Usage:

```java
-XXaggressive:<param>

```

|<param>|Description
|---|---|---|---|---|---|---|---|---|---
|`opt`|Schedules adaptive optimizations earlier and enables new optimizations, which are expected to be the default in future releases.
|`memory`|Configures the memory system for memory-intensive workloads and sets an expectation to enable large amounts of memory resources to ensure high throughput. JRockit JVM will also use large pages, if available.



## -XXallocClearChunks


This option allows you to clear a TLA for references and values at TLA allocation time and pre-fetch the next chunk. When an integer, a reference, or anything else is declared, it has a default value of 0 or null (depending upon type). At the appropriate time, you will need to clear these references and values to free the memory on the heap so Java can use- or reuse- it. You can do either when the object is allocated or, by using this option, when you request a new TLA.

Usage:

```java
-XXallocClearChunks

```

```java
-XXallocClearChunks=<true | false>

```

The above is a boolean option and is generally recommended on IA64 systems; ultimately, its use depends upon the application. If you want to set the size of chunks cleared, combine this option with `-XXallocClearChunkSize`. If you use this flag but do not specify a boolean value, the default is `true`.



## -XXallocClearChunkSize


When used with `-XXallocClearChunkSize`, this option sets the size of the chunks to be cleared. If this flag is used but no value is specified, the default is 512 bytes.

Usage:

```java
-XXallocClearChunks -XXallocClearChunkSize=<size>[k|K][m|M][g|G]

```



## -XXcallProfiling


This option enables the use of call profiling for code optimizations. Profiling records useful runtime statistics specific to the application and can—in many cases—increase performance because JVM can then act on those statistics.

> 
Note: This option is supported with the JRockit JVM R27.3.0 and later version. It may become default in future versions.


Usage:

```java
java -XXcallProfiling myApp

```

This option is disabled by default. You must enable it to use it.



## -XXdisableFatSpin


This option disables the fat lock spin code in Java, allowing threads that block trying to acquire a fat lock go to sleep directly.

Objects in Java become a lock as soon as any thread enters a synchronized block on that object. All locks are held (that is, stayed locked) until released by the locking thread. If the lock is not going to be released very fast, it can be inflated to a “fat lock.” “Spinning” occurs when a thread that wants a specific lock continuously checks that lock to see if it is still taken, spinning in a tight loop as it makes the check. Spinning against a fat lock is generally beneficial although, in some instances, it can be expensive and might affect performance. `-XXdisableFatSpin` allows you to turn off spinning against a fat lock and eliminate the potential performance hit.

Usage:

```java
-XXdisableFatSpin

```



## -XXdisableGCHeuristics


This option disables the garbage collector strategy changes. Compaction heuristics and nursery size heuristics are not affected by this option. By default, the garbage collection heuristics are enabled.

Usage:

```java
-XXdisableFatSpin

```



## -XXdumpSize


This option causes a dump file to be generated and allows you to specify the relative size of that file (that is, small, medium, or large).

Usage:

```java
-XXdumpsize:<size>

```

|<size>|Description
|---|---|---|---|---|---|---|---|---|---
|`none`|Does not generate a dump file.
|`small`|On Windows, a small dump file is generated (on Linux a full core dump is generated). A small dump only include the thread stacks including their traces and very little else. This was the default in the JRockit JVM 8.1 with service packs 1 and 2, as well as 7.0 with service pack 3 and higher).
|`normal`|Causes a normal dump to be generated on all platforms. This dump file includes all memory except the java heap. This is the default value for the JRockit JVM 1.4.2 and later.
|`large`|Includes everything that is in memory, including the Java heap. This option makes `-XXdumpSize` equivalent to `-XXdumpFullState`.



## -XXexitOnOutOfMemory


This option makes JRockit JVM exit on the first occurrence of an out of memory error. It can be used if you prefer restarting an instance of JRockit JVM rather than handling out of memory errors. Enter this command at startup to force JRockit JVM to exit on the first occurrence of an out of memory error.

Usage:

```java
-XXexitOnOutOfMemory

```



#### Remarks


It is strongly recommended that you use these options only:

- If you have a thorough understanding of your system.
- Are aware that, if used improperly, these options can have negative effect on the stability or performance of your system.

Information gathered from [official Java documentation](http://docs.oracle.com/cd/E13150_01/jrockit_jvm/jrockit/jrdocs/refman/optionXX.html).

