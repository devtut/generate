---
metaTitle: "Inline variables"
description: "Defining a static data member in the class definition"
---

# Inline variables


An inline variable is allowed to be defined in multiple translation units without violating the [One Definition Rule](http://stackoverflow.com/documentation/c%2b%2b/4907/one-definition-rule-odr). If it is multiply defined, the linker will merge all definitions into a single object in the final program.



## Defining a static data member in the class definition


A static data member of the class may be fully defined within the class definition if it is declared `inline`. For example, the following class may be defined in a header. Prior to C++17, it would have been necessary to provide a `.cpp` file to contain the definition of `Foo::num_instances` so that it would be defined only once, but in C++17 the multiple definitions of the inline variable `Foo::num_instances` all refer to the same `int` object.

```cpp
// warning: not thread-safe...
class Foo {
  public:
    Foo() { ++num_instances; }
    ~Foo() { --num_instances; }
    inline static int num_instances = 0;
};

```

As a special case, a `constexpr` static data member is implicitly inline.

```cpp
class MyString {
  public:
    MyString() { /* ... */ }
    // ...
    static constexpr int max_size = INT_MAX / 2;
};
// in C++14, this definition was required in a single translation unit:
// constexpr int MyString::max_size;

```

