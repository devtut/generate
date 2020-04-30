---
metaTitle: "More undefined behaviors in C++"
description: "Referring to non-static members in initializer lists"
---

# More undefined behaviors in C++


More examples on how C++ can go wrong.

Continuation from [Undefined Behavior](http://stackoverflow.com/documentation/c%2b%2b/1812/undefined-behavior#t=201705050731006995217)



## Referring to non-static members in initializer lists


Referring to non-static members in initializer lists before the constructor has started executing can result in undefined behavior. This results since not all members are constructed at this time. From the standard draft:

> 
<p>§12.7.1: For an object with a non-trivial constructor, referring to
any non-static member or base class of the object before the
constructor begins execution results in undefined behavior.</p>


**Example**

```cpp
struct W { int j; };
struct X : public virtual W { };
struct Y {
   int *p;
   X x;
   Y() : p(&x.j) { // undefined, x is not yet constructed
   }
};

```

