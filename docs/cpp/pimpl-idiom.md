---
metaTitle: "C++ | Pimpl Idiom"
description: "Basic Pimpl idiom"
---

# Pimpl Idiom



## Basic Pimpl idiom


In the header file:

```cpp
// widget.h

#include <memory>  // std::unique_ptr
#include <experimental/propagate_const>

class Widget
{
    public:
        Widget();
        ~Widget();
        void DoSomething();

    private:
        // the pImpl idiom is named after the typical variable name used
        // ie, pImpl:
        struct Impl;                    // forward declaration
        std::experimental::propagate_const<std::unique_ptr< Impl >> pImpl;  // ptr to actual implementation
};

```

In the implementation file:

```cpp
// widget.cpp

#include "widget.h"
#include "reallycomplextype.h" // no need to include this header inside widget.h

struct Widget::Impl
{
    // the attributes needed from Widget go here
    ReallyComplexType rct;
};

Widget::Widget() :
    pImpl(std::make_unique<Impl>())
{}

Widget::~Widget() = default;

void Widget::DoSomething()
{
    // do the stuff here with pImpl
}

```

The `pImpl` contains the `Widget` state (or some/most of it).  Instead of the `Widget` description of state being exposed in the header file, it can be only exposed within the implementation.

`pImpl` stands for "pointer to implementation".  The "real" implementation of `Widget` is in the `pImpl`.

Danger: Note that for this to work with `unique_ptr`, `~Widget()` must be implemented at a point in a file where the `Impl` is fully visible.  You can `=default` it there, but if `=default` where `Impl` is undefined, the program may easily become ill-formed, no diagnostic required.



#### Remarks


The **pimpl idiom** (**p**ointer to **impl**ementation, sometimes referred to as  **opaque pointer** or **cheshire cat technique**), reduces the compilation times of a class by moving all its private data members into a struct defined in the .cpp file.

The class owns a pointer to the implementation. This way, it can be forward declared, so that the header file does not need to `#include` classes that are used in private member variables.

When using the pimpl idiom, changing a private data member does not require recompiling classes that depend on it.

