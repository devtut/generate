---
metaTitle: "C++11 Memory Model"
description: "Need for Memory Model, Fence example"
---

# C++11 Memory Model




## Need for Memory Model


```cpp
int x, y;
bool ready = false;

void init()
{
  x = 2;
  y = 3;
  ready = true;
}
void use()
{
  if (ready)
    std::cout << x + y;
}

```

One thread calls the `init()` function while another thread (or signal handler) calls the `use()` function. One might expect that the `use()` function will either print `5` or do nothing. This may not always be the case for several reasons:

<li>
The CPU may reorder the writes that happen in `init()` so that the code that  actually executes might look like:

```cpp
void init()
{
  ready = true;
  x = 2;
  y = 3;
}

```


</li>
<li>
The CPU may reorder the reads that happen in `use()` so that the actually executed code might become:

```cpp
void use()
{
  int local_x = x;
  int local_y = y;
  if (ready)
    std::cout << local_x + local_y;
}

```


</li>
<li>
An optimizing C++ compiler may decide to reorder the program in similar way.
</li>

Such reordering cannot change the behavior of a program running in single thread because a thread cannot interleave the calls to `init()` and `use()`. On the other hand in a multi-threaded setting one thread may see part of the writes performed by the other thread where it may happen that `use()` may see `ready==true` and garbage in `x` or `y` or both.

The C++ Memory Model allows the programmer to specify which reordering operations are permitted and which are not, so that a multi-threaded program would also be able to behave as expected. The example above can be rewritten in thread-safe way like this:

```cpp
int x, y;
std::atomic<bool> ready{false};

void init()
{
  x = 2;
  y = 3;
  ready.store(true, std::memory_order_release);
}
void use()
{
  if (ready.load(std::memory_order_acquire))
    std::cout << x + y;
}

```

Here `init()` performs **atomic store-release** operation. This not only stores the value `true` into `ready`, but also tells the compiler that it cannot move this operation before write operations that are **sequenced before** it.

The `use()` function does an **atomic load-acquire** operation. It reads the current value of `ready` and also forbids the compiler from placing read operations that are **sequenced after** it to **happen before** the **atomic load-acquire**.

These atomic operations also cause the compiler to put whatever hardware instructions are needed to inform the CPU to refrain from the unwanted reorderings.

Because the **atomic store-release** is to the same memory location as the **atomic load-acquire**, the memory model stipulates that if the **load-acquire** operation sees the value written by the **store-release** operation, then all writes performed by `init()`'s thread prior to that **store-release** will be visible to loads that `use()`'s thread executes after its **load-acquire**. That is if `use()` sees `ready==true`, then it is guaranteed to see `x==2` and `y==3`.

Note that the compiler and the CPU are still allowed to write to `y` before writing to `x`, and similarly the reads from these variables in `use()` can happen in any order.



## Fence example


The example above can also be implemented with fences and relaxed atomic operations:

```cpp
int x, y;
std::atomic<bool> ready{false};

void init()
{
  x = 2;
  y = 3;
  atomic_thread_fence(std::memory_order_release);
  ready.store(true, std::memory_order_relaxed);
}
void use()
{
  if (ready.load(std::memory_order_relaxed))
  {
    atomic_thread_fence(std::memory_order_acquire);
    std::cout << x + y;
  }
}

```

If the atomic load operation sees the value written by the atomic store then the store happens before the load, and so do the fences: the release fence happens before the acquire fence making the writes to `x` and `y` that precede the release fence to become visible to the `std::cout` statement that follows the acquire fence.

A fence might be beneficial if it can reduce the overall number of acquire, release or other synchronization operations. For example:

```cpp
void block_and_use()
{
  while (!ready.load(std::memory_order_relaxed))
    ;
  atomic_thread_fence(std::memory_order_acquire);
  std::cout << x + y;
}

```

The `block_and_use()` function spins until the `ready` flag is set with the help of relaxed atomic load. Then a single acquire fence is used to provide the needed memory ordering.



#### Remarks


Different threads trying to access the same memory location participate in a **data race** if at least one of the operations is a modification (also known as **store operation**). These **data races** cause **undefined behavior**. To avoid them one needs to prevent these threads from concurrently executing such conflicting operations.

Synchronization primitives (mutex, critical section and the like) can guard such accesses. The Memory Model introduced in C++11 defines two new portable ways to synchronize access to memory in multi-threaded environment: atomic operations and fences.

### Atomic Operations

It is now possible to read and write to given memory location by the use of **atomic load** and **atomic store** operations. For convenience these are wrapped in the `std::atomic<t>` template class. This class wraps a value of type `t` but this time **loads** and **stores** to the object are atomic.

The template is not available for all types. Which types are available is implementation specific, but this usually includes most (or all) available integral types as well as pointer types. So that `std::atomic<unsigned>` and `std::atomic<std::vector<foo> *>` should be available, while `std::atomic<std::pair<bool,char>>` most probably wont be.

Atomic operations have the following properties:

- All atomic operations can be performed concurrently from multiple threads without causing undefined behavior.
- An **atomic load** will see either the initial value which the atomic object was constructed with, or the value written to it via some **atomic store** operation.
- **Atomic stores** to the same atomic object are ordered the same in all threads. If a thread has already seen the value of some **atomic store** operation, subsequent **atomic load** operations will see either the same value, or the value stored by subsequent **atomic store** operation.
- **Atomic read-modify-write** operations allow **atomic load** and **atomic store** to happen without other **atomic store** in between. For example one can atomically increment a counter from multiple threads, and no increment will be lost regardless of the contention between the threads.
- Atomic operations receive an optional `std::memory_order` parameter which defines what additional properties the operation has regarding other memory locations.

|std::memory_order|Meaning
|------
|`std::memory_order_relaxed`|no additional restrictions
|`std::memory_order_release` → `std::memory_order_acquire`|if `load-acquire` sees the value stored by `store-release` then stores **sequenced before** the `store-release` happen before loads sequenced after the **load acquire**
|`std::memory_order_consume`|like `memory_order_acquire` but only for dependent loads
|`std::memory_order_acq_rel`|combines `load-acquire` and `store-release`
|`std::memory_order_seq_cst`|sequential consistency

These memory order tags allow three different memory ordering disciplines: **sequential consistency**, **relaxed**, and **release-acquire** with its sibling **release-consume**.

### Sequential Consistency

If no memory order is specified for an atomic operation, the order defaults to **sequential consistency**. This mode can also be explicitly selected by tagging the operation with `std::memory_order_seq_cst`.

With this order no memory operation can cross the atomic operation. All memory operations sequenced before the atomic operation happen before the atomic operation and the atomic operation happens before all memory operations that are sequenced after it. This mode is probably the easiest one to reason about but it also leads to the greatest penalty to performance. It also prevents all compiler optimizations that might otherwise try to reorder operations past the atomic operation.

### Relaxed Ordering

The opposite to **sequential consistency** is the **relaxed** memory ordering. It is selected with the `std::memory_order_relaxed` tag. Relaxed atomic operation will impose no restrictions on other memory operations. The only effect that remains, is that the operation is itself still atomic.

### Release-Acquire Ordering

An **atomic store** operation can be tagged with `std::memory_order_release` and an **atomic load** operation can be tagged with `std::memory_order_acquire`. The first operation is called **(atomic) store-release** while the second is called **(atomic) load-acquire**.

When **load-acquire** sees the value written by a **store-release** the following happens: all store operations sequenced before the **store-release** become visible to (**happen before**) load operations that are sequenced after the **load-acquire**.

Atomic read-modify-write operations can also receive the cumulative tag `std::memory_order_acq_rel`. This makes the **atomic load** portion of the operation an **atomic load-acquire** while the **atomic store** portion becomes **atomic store-release**.

The compiler is not allowed to move store operations after an **atomic store-release** operation. It is also not allowed to move load operations before **atomic load-acquire** (or **load-consume**).

Also note that there is no **atomic load-release** or **atomic store-acquire**. Attempting to create such operations makes them **relaxed** operations.

### Release-Consume Ordering

This combination is similar to **release-acquire**, but this time the **atomic load** is tagged with `std::memory_order_consume` and becomes **(atomic) load-consume** operation. This mode is the same as **release-acquire** with the only difference that among the load operations sequenced after the **load-consume** only these depending on the value loaded by the **load-consume** are ordered.

### Fences

Fences also allow memory operations to be ordered between threads. A fence is either a release fence or acquire fence.

If a release fence happens before an acquire fence, then stores sequenced before the release fence are visible to loads sequenced after the acquire fence. To guarantee that the release fence happens before the acquire fence one may use other synchronization primitives including relaxed atomic operations.

