---
metaTitle: "Parameter packs"
description: "A template with a parameter pack, Expansion of a parameter pack"
---

# Parameter packs



## A template with a parameter pack


```cpp
template<class ... Types> struct Tuple {};

```

A parameter pack is a template parameter accepting zero or more template arguments. If a template has at least one parameter pack is a **variadic template**.



## Expansion of a parameter pack


The pattern `parameter_pack ...` is expanded into a list of comma-separated substitutions of `parameter_pack` with each one of its parameters

```cpp
template<class T> // Base of recursion
void variadic_printer(T last_argument) {
    std::cout << last_argument;
}

template<class T, class ...Args> 
void variadic_printer(T first_argument, Args... other_arguments) {
  std::cout << first_argument << "\n";
  variadic_printer(other_arguments...); // Parameter pack expansion
}

```

The code above invoked with `variadic_printer(1, 2, 3, "hello");` prints

```cpp
1
2
3
hello

```

