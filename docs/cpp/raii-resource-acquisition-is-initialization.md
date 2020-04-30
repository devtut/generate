---
metaTitle: "RAII: Resource Acquisition Is Initialization"
description: "Locking, Finally/ScopeExit, ScopeSuccess (c++17), ScopeFail (c++17)"
---

# RAII: Resource Acquisition Is Initialization



## Locking


Bad locking:

```cpp
std::mutex mtx;

void bad_lock_example() {
    mtx.lock();
    try
    {
        foo();
        bar();
        if (baz()) {
            mtx.unlock();   // Have to unlock on each exit point.
            return;
        }
        quux();
        mtx.unlock();       // Normal unlock happens here.
    }
    catch(...) {
        mtx.unlock();       // Must also force unlock in the presence of
        throw;              // exceptions and allow the exception to continue.
    }
}

```

That is the wrong way to implement the locking and unlocking of the mutex. To ensure the correct release of the mutex with `unlock()` requires the programer to make sure that all the flows resulting in the exiting of the function result in a call to `unlock()`. As shown above this is a brittle processes as it requires any maintainers to continue following the pattern manually.

Using an appropriately crafted class to implement RAII, the problem is trivial:

```cpp
std::mutex mtx;

void good_lock_example() {
    std::lock_guard<std::mutex> lk(mtx);   // constructor locks.
                                           // destructor unlocks. destructor call
                                           // guaranteed by language.
    foo();
    bar();
    if (baz()) {
        return;
    }
    quux();
}

```

`lock_guard` is an extremely simple class template that simply calls `lock()` on its argument in its constructor, keeps a reference to the argument, and calls `unlock()` on the argument in its destructor. That is, when the `lock_guard` goes out of scope, the `mutex` is guaranteed to be unlocked. It doesn't matter if the reason it went out of scope is an exception or an early return - all cases are handled; regardless of the control flow, we have guaranteed that we will unlock correctly.



## Finally/ScopeExit


For cases when we don't want to write special classes to handle some resource, we may write a generic class:

```cpp
template<typename Function>
class Finally final
{
public:
    explicit Finally(Function f) : f(std::move(f)) {}
    ~Finally() { f(); } // (1) See below

    Finally(const Finally&) = delete;
    Finally(Finally&&) = default;
    Finally& operator =(const Finally&) = delete;
    Finally& operator =(Finally&&) = delete;
private:
    Function f;
};
// Execute the function f when the returned object goes out of scope.
template<typename Function>
auto onExit(Function &&f) { return Finally<std::decay_t<Function>>{std::forward<Function>(f)}; }

```

And its example usage

```cpp
void foo(std::vector<int>& v, int i)
{
    // ...

    v[i] += 42;
    auto autoRollBackChange = onExit([&](){ v[i] -= 42; });

    // ... code as recursive call `foo(v, i + 1)`
}

```

Note (1): Some discussion about destructor definition has to be considered to handle exception:

- `~Finally() noexcept { f(); }`: `std::terminate` is called in case of exception
- `~Finally() noexcept(noexcept(f())) { f(); }`: terminate() is called only in case of exception during stack unwinding.
- `~Finally() noexcept { try { f(); } catch (...) { /* ignore exception (might log it) */} }` No `std::terminate` called, but we cannot handle error (even for non stack unwinding).



## ScopeSuccess (c++17)


Thanks to `int std::uncaught_exceptions()`, we can implement action which executes only on success (no thrown exception in scope).
Previously `bool std::uncaught_exception()` just allows to detect if **any** stack unwinding is running.

```cpp
#include <exception>
#include <iostream>

template <typename F>
class ScopeSuccess
{
private:
    F f;
    int uncaughtExceptionCount = std::uncaught_exceptions();
public:
    explicit ScopeSuccess(const F& f) : f(f) {}
    ScopeSuccess(const ScopeSuccess&) = delete;
    ScopeSuccess& operator =(const ScopeSuccess&) = delete;

    // f() might throw, as it can be caught normally.
    ~ScopeSuccess() noexcept(noexcept(f())) {
        if (uncaughtExceptionCount == std::uncaught_exceptions()) {
            f();
        }
    }
};

struct Foo {
    ~Foo() {
        try {
            ScopeSuccess logSuccess{[](){std::cout << "Success 1\n";}};
            // Scope succeeds,
            // even if Foo is destroyed during stack unwinding
            // (so when 0 < std::uncaught_exceptions())
            // (or previously std::uncaught_exception() == true)
        } catch (...) {
        }
        try {
            ScopeSuccess logSuccess{[](){std::cout << "Success 2\n";}};

            throw std::runtime_error("Failed"); // returned value
                                                // of std::uncaught_exceptions increases
        } catch (...) { // returned value of std::uncaught_exceptions decreases
        }
    }

};

int main()
{
    try {
        Foo foo;

        throw std::runtime_error("Failed"); // std::uncaught_exceptions() == 1
    } catch (...) { // std::uncaught_exceptions() == 0
    }
}

```

Output:

```cpp
Success 1

```



## ScopeFail (c++17)


Thanks to `int std::uncaught_exceptions()`, we can implement action which executes only on failure (thrown exception in scope).
Previously `bool std::uncaught_exception()` just allows to detect if **any** stack unwinding is running.

```cpp
#include <exception>
#include <iostream>

template <typename F>
class ScopeFail
{
private:
    F f;
    int uncaughtExceptionCount = std::uncaught_exceptions();
public:
    explicit ScopeFail(const F& f) : f(f) {}
    ScopeFail(const ScopeFail&) = delete;
    ScopeFail& operator =(const ScopeFail&) = delete;

    // f() should not throw, else std::terminate is called.
    ~ScopeFail() {
        if (uncaughtExceptionCount != std::uncaught_exceptions()) {
            f();
        }
    }
};

struct Foo {
    ~Foo() {
        try {
            ScopeFail logFailure{[](){std::cout << "Fail 1\n";}};
            // Scope succeeds,
            // even if Foo is destroyed during stack unwinding
            // (so when 0 < std::uncaught_exceptions())
            // (or previously std::uncaught_exception() == true)
        } catch (...) {
        }
        try {
            ScopeFail logFailure{[](){std::cout << "Failure 2\n";}};

            throw std::runtime_error("Failed"); // returned value
                                                // of std::uncaught_exceptions increases
        } catch (...) { // returned value of std::uncaught_exceptions decreases
        }
    }

};

int main()
{
    try {
        Foo foo;

        throw std::runtime_error("Failed"); // std::uncaught_exceptions() == 1
    } catch (...) { // std::uncaught_exceptions() == 0
    }
}

```

Output:

```cpp
Failure 2

```



#### Remarks


RAII stands for **R**esource **A**cquisition **I**s **I**nitialization. Also occasionally referred to as SBRM (Scope-Based Resource Management) or RRID (Resource Release Is Destruction), RAII is an idiom used to tie resources to object lifetime. In C++, the destructor for an object always runs when an object goes out of scope - we can take advantage of that to tie resource cleanup into object destruction.

Any time you need to acquire some resource (e.g. a lock, a file handle, an allocated buffer) that you will eventually need to release, you should consider using an object to handle that resource management for you. Stack unwinding will happen regardless of exception or early scope exit, so the resource handler object will clean up the resource for you without you having to carefully consider all possible current and future code paths.

It's worth noting that RAII doesn't completely free the developer of thinking about the lifetime of resources.
One case is, obviously, a crash or exit() call, which will prevent destructors from being called. Since the OS will clean up process-local resources like memory after a process ends, this is not a problem in most cases.
However with system resources (i.e. named pipes, lock files, shared memory) you still need facilities to deal with the case where a process didn't clean up after itself, i.e. on startup test if the lock file is there, if it is, verify the process with the pid actually exists, then act accordingly.

Another situation is when a unix process calls a function from the exec-family, i.e. after a fork-exec to create a new process. Here, the child process will have a full copy of the parents memory (including the RAII objects) but once exec was called, none of the destructors will be called in that process.
On the other hand, if a process is forked and neither of the processes call exec, all resources are cleaned up in both processes.
This is correct only for all resources that were actually duplicated in the fork, but with system resources, both processes will only have a reference to the resource (i.e. the path to a lock file) and will both try to release it individually, potentially causing the other process to fail.

