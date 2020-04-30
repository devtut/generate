---
metaTitle: "Atomics"
description: "atomics and operators"
---

# Atomics



## atomics and operators


Atomic variables can be accessed concurrently between different threads without creating race conditions.

```c
/* a global static variable that is visible by all threads */
static unsigned _Atomic active = ATOMIC_VAR_INIT(0);


int myThread(void* a) {
  ++active;         // increment active race free
  // do something
  --active;         // decrement active race free
  return 0;
}

```

All lvalue operations (operations that modify the object) that are allowed for the base type are allowed and will not lead to race conditions between different threads that access them.

- Operations on atomic objects are generally orders of magnitude slower than normal arithmetic operations. This also includes simple load or store operations. So you should only use them for critical tasks.
- Usual arithmetic operations and assignment such as `a = a+1;` are in fact three operations on `a`: first a load, then addition and finally a store. This is **not** race free. Only the operation `a += 1;` and `a++;` are.



#### Syntax


- `#ifdef __STDC_NO_ATOMICS__`
- `# error this implementation needs atomics`
- `#endif`
- `#include <stdatomic.h>`
- unsigned _Atomic counter = ATOMIC_VAR_INIT(0);



#### Remarks


Atomics as part of the C language are an optional feature that is available since C11.

Their purpose is to ensure race-free access to variables that are shared between different threads. Without atomic qualification, the state of a shared variable would be undefined if two threads access it concurrently. Eg an increment operation (`++`) could be split into several assembler instructions, a read, the addition itself and a store instruction. If another thread would be doing the same operation their two instruction sequences could be intertwined and lead to an inconsistent result.

<li>
**Types:** All object types with the exception of array types can be qualified with `_Atomic`.
</li>
<li>
**Operators:** All read-modify-write operators (e.g `++` or `*=`) on these are guaranteed to be atomic.
</li>
<li>
**Operations:** There are some other operations that are specified as type generic functions, e.g `atomic_compare_exchange`.
</li>
<li>
**Threads:** Access to them is guaranteed not to produce data race when they are accessed by different threads.
</li>
<li>
**Signal handlers:** Atomic types are called **lock-free** if all operations on them are stateless. In that case they can also be used to deal state changes between normal control flow and a signal handler.
</li>
<li>
There is only one data type that is guaranteed to be lock-free: `atomic_flag`. This is a minimal type who's operations are intended to map to efficient test-and-set hardware instructions.
</li>

Other means to avoid race conditions are available in C11's thread interface, in particular a mutex type `mtx_t` to mutually exclude threads from accessing critical data or critical sections of code. If atomics are not available, these must be used to prevent races.

