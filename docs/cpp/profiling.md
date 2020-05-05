---
metaTitle: "C++ | Profiling"
description: "Profiling with gcc and gprof, Generating callgraph diagrams with gperf2dot, Profiling CPU Usage with gcc and Google Perf Tools"
---

# Profiling



## Profiling with gcc and gprof


The GNU gprof profiler, [gprof](https://sourceware.org/binutils/docs/gprof/), allows you to profile your code. To use it, you need to perform the following steps:

<li>
Build the application with settings for generating profiling information
</li>
<li>
Generate profiling information by running the built application
</li>
<li>
View the generated profiling information with gprof
</li>

In order to build the application with settings for generating profiling information, we add the `-pg` flag. So, for example, we could use

```cpp
$ gcc -pg *.cpp -o app

```

or

```cpp
$ gcc -O2 -pg *.cpp -o app

```

and so forth.

Once the application, say `app`, is built, execute it as usual:

```cpp
$ ./app

```

This should produce a file called `gmon.out`.

To see the profiling results, now run

```cpp
$ gprof app gmon.out

```

(note that we provide both the application as well as the generated output).

Of course, you can also pipe or redirect:

```cpp
$ gprof app gmon.out | less

```

and so forth.

The result of the last command should be a table, whose rows are the functions, and whose columns indicate the number of calls, total time spent, self time spent (that is, time spent in the function excluding calls to children).



## Generating callgraph diagrams with gperf2dot


For more complex applications, flat execution profiles may be difficult to follow. This is why many profiling tools also generate some form of annotated callgraph information.

[gperf2dot](https://github.com/jrfonseca/gprof2dot) converts text output from many profilers (Linux perf, callgrind, oprofile etc.) into a callgraph diagram. You can use it by running your profiler (example for `gprof`):

```cpp
# compile with profiling flags  
g++ *.cpp -pg
# run to generate profiling data                                            
./main
# translate profiling data to text, create image     
gprof ./main | gprof2dot -s | dot -Tpng -o output.png

```

[<img src="http://i.stack.imgur.com/QDQA0.png" alt="enter image description here" />](http://i.stack.imgur.com/QDQA0.png)



## Profiling CPU Usage with gcc and Google Perf Tools


[Google Perf Tools](https://github.com/gperftools/gperftools) also provides a CPU profiler, with a slightly friendlier interface. To use it:

1. [Install Google Perf Tools](https://github.com/gperftools/gperftools)
1. Compile your code as usual
1. Add the `libprofiler` profiler library to your library load path at runtime
1. Use `pprof` to generate a flat execution profile, or a callgraph diagram

For example:

```cpp
# compile code
g++ -O3 -std=c++11 main.cpp -o main

# run with profiler
LD_PRELOAD=/usr/local/lib/libprofiler.so CPUPROFILE=main.prof CPUPROFILE_FREQUENCY=100000 ./main

```

where:

- `CPUPROFILE` indicates the output file for profiling data
- `CPUPROFILE_FREQUENCY` indicates the profiler sampling frequency;

Use `pprof` to post-process the profiling data.

You can generate a flat call profile as text:

```cpp
$ pprof --text ./main main.prof
PROFILE: interrupts/evictions/bytes = 67/15/2016
pprof --text --lines ./main main.prof
Using local file ./main.
Using local file main.prof.
Total: 67 samples
      22  32.8%  32.8%       67 100.0% longRunningFoo ??:0
      20  29.9%  62.7%       20  29.9% __memmove_ssse3_back /build/eglibc-3GlaMS/eglibc-2.19/string/../sysdeps/x86_64/multiarch/memcpy-ssse3-back.S:1627
       4   6.0%  68.7%        4   6.0% __memmove_ssse3_back /build/eglibc-3GlaMS/eglibc-2.19/string/../sysdeps/x86_64/multiarch/memcpy-ssse3-back.S:1619
       3   4.5%  73.1%        3   4.5% __random_r /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random_r.c:388
       3   4.5%  77.6%        3   4.5% __random_r /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random_r.c:401
       2   3.0%  80.6%        2   3.0% __munmap /build/eglibc-3GlaMS/eglibc-2.19/misc/../sysdeps/unix/syscall-template.S:81
       2   3.0%  83.6%       12  17.9% __random /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random.c:298
       2   3.0%  86.6%        2   3.0% __random_r /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random_r.c:385
       2   3.0%  89.6%        2   3.0% rand /build/eglibc-3GlaMS/eglibc-2.19/stdlib/rand.c:26
       1   1.5%  91.0%        1   1.5% __memmove_ssse3_back /build/eglibc-3GlaMS/eglibc-2.19/string/../sysdeps/x86_64/multiarch/memcpy-ssse3-back.S:1617
       1   1.5%  92.5%        1   1.5% __memmove_ssse3_back /build/eglibc-3GlaMS/eglibc-2.19/string/../sysdeps/x86_64/multiarch/memcpy-ssse3-back.S:1623
       1   1.5%  94.0%        1   1.5% __random /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random.c:293
       1   1.5%  95.5%        1   1.5% __random /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random.c:296
       1   1.5%  97.0%        1   1.5% __random_r /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random_r.c:371
       1   1.5%  98.5%        1   1.5% __random_r /build/eglibc-3GlaMS/eglibc-2.19/stdlib/random_r.c:381
       1   1.5% 100.0%        1   1.5% rand /build/eglibc-3GlaMS/eglibc-2.19/stdlib/rand.c:28
       0   0.0% 100.0%       67 100.0% __libc_start_main /build/eglibc-3GlaMS/eglibc-2.19/csu/libc-start.c:287
       0   0.0% 100.0%       67 100.0% _start ??:0
       0   0.0% 100.0%       67 100.0% main ??:0
       0   0.0% 100.0%       14  20.9% rand /build/eglibc-3GlaMS/eglibc-2.19/stdlib/rand.c:27
       0   0.0% 100.0%       27  40.3% std::vector::_M_emplace_back_aux ??:0

```

... or you can generate an annotated callgraph in a pdf with:

```cpp
pprof --pdf ./main main.prof > out.pdf

```

