---
metaTitle: "Function Template Overloading"
description: "What is a valid function template overloading?"
---

# Function Template Overloading



## What is a valid function template overloading?


A function template can be overloaded under the rules for non-template function overloading (same name, but different parameter types) and in addition to that, the overloading is valid if

- The return type is different, or
- The template parameter list is different, except for the naming of parameters and the presence of default arguments (they are not part of the signature)

For a normal function, comparing two parameter types is is easy for the compiler, since it has all informat. But a type within a template may not be determined yet. Therefore, the rule for when two parameter types are equal is approximative here and says that the non-depependend types and values need to match and the spelling of dependent types and expressions needs to be the same (more precisely, they need to conform to the so-called ODR-rules), except that template parameters may be renamed. However, if under such different spellings, two values within the types are deemed different, but will always instantiate to the same values, the overloading is invalid, but no diagnostic is required from the compiler.

```cpp
template<typename T>
void f(T*) { }

template<typename T>
void f(T) { }

```

This is a valid overload, as "T" and "T*" are different spellings. But the following is invalid, with no diagnostic required

```cpp
template<typename T>
void f(T (*x)[sizeof(T) + sizeof(T)]) { }

template<typename T>
void f(T (*x)[2 * sizeof(T)]) { }

```



#### Remarks


- A normal function is never related to a function template, despite same name, same type.
- A normal function call and a generated function template call are different even if they share the same name, same return type and same argument list

