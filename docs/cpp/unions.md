---
metaTitle: "C++ | Unions"
description: "Undefined Behavior, Basic Union Features, Typical Use"
---

# Unions



## Undefined Behavior


```cpp
union U {
    int a;
    short b;
    float c;
};
U u;

u.a = 10;
if (u.b == 10) {
   // this is undefined behavior since 'a' was the last member to be
   // written to. A lot of compilers will allow this and might issue a
   // warning, but the result will be "as expected"; this is a compiler
   // extension and cannot be guaranteed across compilers (i.e. this is
   // not compliant/portable code).
}

```



## Basic Union Features


Unions are a specialized struct within which all members occupy overlapping memory.

```cpp
union U {
    int a;
    short b;
    float c;
};
U u;

//Address of a and b will be equal
(void*)&u.a == (void*)&u.b;
(void*)&u.a == (void*)&u.c;

//Assigning to any union member changes the shared memory of all members
u.c = 4.f;
u.a = 5;
u.c != 4.f;

```



## Typical Use


Unions are useful for minimizing memory usage for exclusive data, such as when implementing mixed data types.

```cpp
struct AnyType {
    enum {
        IS_INT,
        IS_FLOAT
    } type;
    
    union Data {
        int as_int;
        float as_float;
    } value;

    AnyType(int i) : type(IS_INT) { value.as_int = i; }
    AnyType(float f) : type(IS_FLOAT) { value.as_float = f; }

    int get_int() const {
        if(type == IS_INT)
            return value.as_int;
        else
            return (int)value.as_float;
    }
    
    float get_float() const {
        if(type == IS_FLOAT)
            return value.as_float;
        else
            return (float)value.as_int;
    }
};

```



#### Remarks


Unions are very useful tools, but come with a few important caveats:

<li>
It is undefined behavior, per the C++ standard, to access an element of a union that was not the most recently modified member. Although a lot of C++ compilers permit this access in well defined ways, these are extensions and cannot be guaranteed across compilers.
<p>A [`std::variant`](http://en.cppreference.com/w/cpp/utility/variant) (since C++17) is like a union, only it tells you what it currently contains (part of its visible state is the type of the value it holds at a
given moment: it enforces value access happening only to that type).</p>
</li>
<li>
Implementations do not necessarily align members of different sizes to the same address.
</li>

