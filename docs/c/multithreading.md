---
metaTitle: "Multithreading"
description: "C11 Threads simple example"
---

# Multithreading


In C11 there is a standard thread library, `<threads.h>`, but no known compiler that yet implements it. Thus, to use multithreading in C you must use platform specific implementations such as the POSIX threads library (often referred to as pthreads) using the `pthread.h` header.



## C11 Threads simple example


```c
#include <threads.h>
#include <stdio.h>

int run(void *arg)
{
    printf("Hello world of C11 threads.");

    return 0;
}

int main(int argc, const char *argv[])
{
    thrd_t thread;
    int result;

    thrd_create(&thread, run, NULL);

    thrd_join(&thread, &result);

    printf("Thread return %d at the end\n", result);
}

```



#### Syntax


- thrd_t // Implementation-defined complete object type identifying a thread
- int thrd_create( thrd_t *thr, thrd_start_t func, void *arg ); // Creates a thread
- int thrd_equal( thrd_t thr0, thrd_t thr1 ); // Check if arguments refer to the same thread
- thr_t thrd_current(void); // Returns identifier of the thread that calls it
- int thrd_sleep( const struct timespec *duration, struct timespec *remaining ); // Suspend call thread execution for at least a given time
- void thrd_yield(void); // Permit other threads to run instead of the thread that calls it
- _Noreturn void thrd_exit( int res ); // Terminates the thread the thread that calls it
- int thrd_detatch( thrd_t thr; // Detaches a given thread from the current environment
- int thrd_join( thrd_t thr, int *res ); // Blocks the current thread until the given thread finishes



#### Remarks


Using threads can introduce extra undefined behavior such as a [https://stackoverflow.com/documentation/c/364/undefined-behavior/2622/data-race#t=201706130820201457052](https://stackoverflow.com/documentation/c/364/undefined-behavior/2622/data-race#t=201706130820201457052). For race-free access to variables that are shared between different threads C11 provides the `mtx_lock()` mutex functionality or the (optional) [https://stackoverflow.com/documentation/c/4924/atomics#t=201706150835215525448](https://stackoverflow.com/documentation/c/4924/atomics#t=201706150835215525448) data-types and associated functions in `stdatomic.h`.

