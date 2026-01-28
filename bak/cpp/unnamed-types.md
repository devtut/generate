---
metaTitle: "C++ | Unnamed types"
description: "Unnamed classes, As a type alias, Anonymous members, Anonymous Union"
---

# Unnamed types



## Unnamed classes


Unlike a named class or struct, unnamed classes and structs must be instantiated where they are defined, and cannot have constructors or destructors.

```cpp
struct {
    int foo;
    double bar;
} foobar;

foobar.foo = 5;
foobar.bar = 4.0;

class {
    int baz;
public:
    int buzz;
    
    void setBaz(int v) {
        baz = v;
    }
} barbar;

barbar.setBaz(15);
barbar.buzz = 2;

```



## As a type alias


Unnamed class types may also be used when creating type aliases, i.e. via `typedef` and `using`:

```cpp
using vec2d = struct {
    float x;
    float y;
};

```

```cpp
typedef struct {
    float x;
    float y;
} vec2d;

```

```cpp
vec2d pt;
pt.x = 4.f;
pt.y = 3.f;

```



## Anonymous members


As a non-standard extension to C++, common compilers allow the use of classes as anonymous members.

```cpp
struct Example {
    struct {
        int inner_b;
    };
    
    int outer_b;
    
    //The anonymous struct's members are accessed as if members of the parent struct
    Example() : inner_b(2), outer_b(4) {
        inner_b = outer_b + 2;
    }
};

Example ex;

//The same holds true for external code referencing the struct
ex.inner_b -= ex.outer_b;

```



## Anonymous Union


Member names of an anonymous union belong to the scope of the union declaration an must be distinct to all other names of this scope. The example here has the same construction as example [Anonymous Members](http://stackoverflow.com/documentation/c%2B%2B/2704/unnamed-types/9055/anonymous-members#t=201609090658106572273) using "struct" but is standard conform.

```cpp
struct Sample {
    union {
        int a;
        int b;
    };
    int c;
};
int main()
{
  Sample sa;
  sa.a =3;
  sa.b =4;
  sa.c =5;
}

```

