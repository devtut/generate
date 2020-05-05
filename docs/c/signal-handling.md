---
metaTitle: "Signal handling"
description: "Signal Handling with “signal()”"
---

# Signal handling



## Signal Handling with “signal()”


[Signal numbers](https://en.wikipedia.org/wiki/C_signal_handling#Standard_signals) can be synchronous (like `SIGSEGV` – segmentation fault) when they are triggered by a malfunctioning of the program itself or asynchronous (like `SIGINT` - interactive attention) when they are initiated from outside the program, e.g by a keypress as `Cntrl-C`.

The `signal()` function is part of the ISO C standard and can be used to assign a function to handle a specific signal

```c
#include <stdio.h>  /* printf() */
#include <stdlib.h> /* abort()  */
#include <signal.h> /* signal() */

void handler_nonportable(int sig)
{
    /* undefined behavior, maybe fine on specific platform */
    printf("Catched: %d\n", sig);
    
    /* abort is safe to call */
    abort();
}

sig_atomic_t volatile finished = 0;

void handler(int sig)
{
    switch (sig) {
    /* hardware interrupts should not return */
    case SIGSEGV:
    case SIGFPE:
    case SIGILL:

```

```c
      /* quick_exit is safe to call */
      quick_exit(EXIT_FAILURE);

```

```c
      /* use _Exit in pre-C11 */
      _Exit(EXIT_FAILURE);

```

```c
    default:
       /* Reset the signal to the default handler, 
          so we will not be called again if things go
          wrong on return. */
      signal(sig, SIG_DFL);
      /* let everybody know that we are finished */
      finished = sig;
      return;
   }
}

int main(void)
{

    /* Catch the SIGSEGV signal, raised on segmentation faults (i.e NULL ptr access */
    if (signal(SIGSEGV, &handler) == SIG_ERR) {
        perror("could not establish handler for SIGSEGV");
        return EXIT_FAILURE;
    }

    /* Catch the SIGTERM signal, termination request */
    if (signal(SIGTERM, &handler) == SIG_ERR) {
        perror("could not establish handler for SIGTERM");
        return EXIT_FAILURE;
    }

    /* Ignore the SIGINT signal, by setting the handler to `SIG_IGN`. */
    signal(SIGINT, SIG_IGN);


    /* Do something that takes some time here, and leaves
       the time to terminate the program from the keyboard. */

    /* Then: */

    if (finished) {
       fprintf(stderr, "we have been terminated by signal %d\n", (int)finished);
        return EXIT_FAILURE;
    }


    /* Try to force a segmentation fault, and raise a SIGSEGV */
    {
      char* ptr = 0;
      *ptr = 0;
    }

    /* This should never be executed */
    return EXIT_SUCCESS;
}

```

Using `signal()` imposes important limitations what you are allowed to do inside the signal handlers, see the remarks for further information.

[POSIX](http://stackoverflow.com/documentation/posix/4532/signals#t=201608081940432865722) recommends the usage of `sigaction()` instead of `signal()`, due to its underspecified behavior and significant implementation variations. POSIX also defines [many more signals](https://en.wikipedia.org/wiki/Unix_signal#POSIX_signals) than ISO C standard, including `SIGUSR1` and `SIGUSR2`, which can be used freely by the programmer for any purpose.



#### Syntax


- void (*signal(int sig, void (*func)(int)))(int);



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|sig|The signal to set the signal handler to, one of `SIGABRT`, `SIGFPE`, `SIGILL`, `SIGTERM`, `SIGINT`, `SIGSEGV` or some implementation defined value
|func|The signal handler, which is either of the following: `SIG_DFL`, for the default handler, `SIG_IGN` to ignore the signal, or a function pointer with the signature `void foo(int sig);`.



#### Remarks


The usage of signal handlers with only the guarantees from the C standard imposes various limitations what can, or can't be done in the user defined signal handler.

<li>
If the user defined function returns while handling `SIGSEGV`, `SIGFPE`, `SIGILL` or any other implementation-defined hardware interrupt, the behavior is undefined by the C standard. This is because C's interface doesn't give means to change the faulty state (e.g after a division by `0`) and thus when returning the program is in exactly the same erroneous state than before the hardware interrupt occurred.
</li>
<li>
If the user defined function was called as the result of a call to `abort`, or `raise`, the signal handler is not allowed to call `raise`, again.
</li>
<li>
Signals can arrive in the middle of any operation, and therefore the indivisibility of operations can in generally not be guaranteed nor does signal handling work well with optimization. Therefore all modifications to data in a signal handler must be to variables
<ul>
- of type `sig_atomic_t` (all versions) or a lock-free atomic type (since C11, optional)
- that are `volatile` qualified.

Other functions from the C standard library will usually not respect these restrictions, because they may change variables in the global state of the program. The C standard only makes guarantees for `abort`, `_Exit` (since C99), `quick_exit` (since C11), `signal` (for the same signal number), and some atomic operations (since C11).

Behavior is undefined by the C standard if any of the rules above are violated. Platforms may have specific extensions, but these are generally not portable beyond that platform.

<li>
Usually systems have their own list of functions that are **asynchronous signal safe**, that is of C library functions that can be used from a signal handler. E.g often `printf` is among these function.
</li>
<li>
In particular the C standard doesn't define much about the interaction with its threads interface (since C11) or any platform specific thread libraries such as POSIX threads. Such platforms have to specify the interaction of such thread libraries with signals by themselves.
</li>

