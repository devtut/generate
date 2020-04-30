---
metaTitle: "Linkage specifications"
description: "Signal handler for Unix-like operating system, Making a C library header compatible with C++"
---

# Linkage specifications


A linkage specification tells the compiler to compile declarations in a way that allows them to be linked together with declarations written in another language, such as C.



## Signal handler for Unix-like operating system


Since a signal handler will be called by the kernel using the C calling convention, we must tell the compiler to use the C calling convention when compiling the function.

```cpp
volatile sig_atomic_t death_signal = 0;
extern "C" void cleanup(int signum) {
    death_signal = signum;
}
int main() {
    bind(...);
    listen(...);
    signal(SIGTERM, cleanup);
    while (int fd = accept(...)) {
        if (fd == -1 && errno == EINTR && death_signal) {
            printf("Caught signal %d; shutting down\n", death_signal);
            break;
        }
        // ...
    }
}

```



## Making a C library header compatible with C++


A C library header can usually be included into a C++ program, since most declarations are valid in both C and C++. For example, consider the following `foo.h`:

```cpp
typedef struct Foo {
    int bar;
} Foo;
Foo make_foo(int);

```

The definition of `make_foo` is separately compiled and distributed with the header in object form.

A C++ program can `#include <foo.h>`, but the compiler will not know that the `make_foo` function is defined as a C symbol, and will probably try to look for it with a mangled name, and fail to locate it. Even if it can find the definition of `make_foo` in the library, not all platforms use the same calling conventions for C and C++, and the C++ compiler will use the C++ calling convention when calling `make_foo`, which is likely to cause a segmentation fault if `make_foo` is expecting to be called with the C calling convention.

The way to remedy this problem is to wrap almost all the declarations in the header in an `extern "C"` block.

```cpp
#ifdef __cplusplus
extern "C" {
#endif

typedef struct Foo {
    int bar;
} Foo;
Foo make_foo(int);

#ifdef __cplusplus
}  /* end of "extern C" block */
#endif

```

Now when `foo.h` is included from a C program, it will just appear as ordinary declarations, but when `foo.h` is included from a C++ program, `make_foo` will be inside an `extern "C"` block and the compiler will know to look for an unmangled name and use the C calling convention.



#### Syntax


- extern **string-literal** { **declaration-seq**(**opt**) }
- extern **string-literal** **declaration**



#### Remarks


The standard requires all compilers to support `extern "C"` in order to allow C++ to be compatible with C, and `extern "C++"`, which may be used to override an enclosing linkage specification and restore the default. Other supported linkage specifications are [implementation-defined](http://stackoverflow.com/documentation/c%2b%2b/1363/implementation-defined-behavior).

