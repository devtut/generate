---
metaTitle: "Side by Side Comparisons of classic C++ examples solved via C++ vs C++11 vs C++14 vs C++17"
description: "Looping through a container"
---

# Side by Side Comparisons of classic C++ examples solved via C++ vs C++11 vs C++14 vs C++17



## Looping through a container


In C++, looping through a sequence container `c` can be done using indexes as follows:

```cpp
for(size_t i = 0; i < c.size(); ++i) c[i] = 0;

```

While simple, such writings are subject to common semantic errors, like wrong comparison operator, or wrong indexing variable:

```cpp
for(size_t i = 0; i <= c.size(); ++j) c[i] = 0;
                     ^~~~~~~~~~~~~~^

```

Looping can also be achieved for all containers using iterators, with similar drawbacks:

```cpp
for(iterator it = c.begin(); it != c.end(); ++it) (*it) = 0;

```

C++11 introduced range-based for loops and `auto` keyword, allowing the code to become:

```cpp
for(auto& x : c) x = 0;

```

Here the only parameters are the container `c`, and a variable `x` to hold the current value. This prevents the semantics errors previously pointed.

According to the C++11 standard, the underlying implementation is equivalent to:

```cpp
for(auto begin = c.begin(), end = c.end(); begin != end; ++begin)
{
    // ...
}

```

In such implementation, the expression `auto begin = c.begin(), end = c.end();` forces `begin` and `end` to be of the same type, while `end` is never incremented, nor dereferenced. So the range-based for loop only works for containers defined by a pair iterator/iterator. The C++17 standard relaxes this constraint by changing the implementation to:

```cpp
auto begin = c.begin();
auto end = c.end();
for(; begin != end; ++begin)
{
    // ...
}

```

Here `begin` and `end` are allowed to be of different types, as long as they can be compared for inequality. This allows to loop through more containers, e.g. a container defined by a pair iterator/sentinel.

