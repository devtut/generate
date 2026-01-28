---
metaTitle: "C++ | Trailing return type"
description: "Avoid qualifying a nested type name, Lambda expressions"
---

# Trailing return type



## Avoid qualifying a nested type name


```cpp
class ClassWithAReallyLongName {
  public:
    class Iterator { /* ... */ };
    Iterator end();
};

```

Defining the member `end` with a trailing return type:

```cpp
auto ClassWithAReallyLongName::end() -> Iterator { return Iterator(); }

```

Defining the member `end` without a trailing return type:

```cpp
ClassWithAReallyLongName::Iterator ClassWithAReallyLongName::end() { return Iterator(); }

```

The trailing return type is looked up in the scope of the class, while a leading return type is looked up in the enclosing namespace scope and can therefore require "redundant" qualification.



## Lambda expressions


A lambda can **only** have a trailing return type; the leading return type syntax is not applicable to lambdas. Note that in many cases it is not necessary to specify a return type for a lambda at all.

```cpp
struct Base {};
struct Derived1 : Base {};
struct Derived2 : Base {};
auto lambda = [](bool b) -> Base* { if (b) return new Derived1; else return new Derived2; };
// ill-formed: auto lambda = Base* [](bool b) { ... };

```



#### Syntax


- **function_name** ( [**function_args**] ) [**function_attributes**] [**function_qualifiers**] `->` **trailing-return-type** [**requires_clause**]



#### Remarks


The above syntax shows a full function declaration using a trailing type, where square brackets indicate an optional part of the function declaration (like the argument list if a no-arg function).

Additionally, the syntax of the trailing return type prohibits defining a class, union, or enum type inside a trailing return type (note that this is not allowed in a leading return type either). Other than that, types can be spelled the same way after the `->` as they would be elsewhere.

