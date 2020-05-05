---
metaTitle: "C++ | decltype"
description: "Basic Example, Another example"
---

# decltype


The keyword `decltype` can be used to get the type of a variable, function or an expression.



## Basic Example


This example just illustrates how this keyword can be used.

```cpp
int a = 10;

// Assume that type of variable 'a' is not known here, or it may
// be changed by programmer (from int to long long, for example).
// Hence we declare another variable, 'b' of the same type using 
// decltype keyword.
decltype(a) b; // 'decltype(a)' evaluates to 'int'

```

If, for example, someone changes, type of 'a' to:

`float a=99.0f;`

Then the type of variable `b` now automatically becomes `float`.



## Another example


Let's say we have vector:

```cpp
std::vector<int> intVector;

```

And we want to declare an iterator for this vector. An obvious idea is to use `auto`. However, it may be needed just declare an iterator variable (and not to assign it to anything). We would do:

```cpp
vector<int>::iterator iter;

```

However, with `decltype` it becomes easy and less error prone (if type of `intVector` changes).

```cpp
decltype(intVector)::iterator iter;

```

Alternatively:

```cpp
decltype(intVector.begin()) iter;

```

In second example, the return type of `begin` is used to determine the actual type, which is `vector<int>::iterator`.

If we need a const_iterator, we just need to use `cbegin`:

```cpp
decltype(intVector.cbegin()) iter; // vector<int>::const_iterator

```

