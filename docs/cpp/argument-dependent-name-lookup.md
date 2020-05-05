---
metaTitle: "C++ | Argument Dependent Name Lookup"
description: "What functions are found"
---

# Argument Dependent Name Lookup



## What functions are found


Functions are found by first collecting a set of **"associated classes"** and **"associated namespaces"** that include one ore more of the following, depending on the argument type `T`. First, let us show the rules for classes, enumeration and class template specialization names.

- If `T` is a nested class, member enumeration, then the surrounding class of it.
- If `T` is an enumeration (it may **also** be a class member!), the innermost namespace of it.
- If `T` is a class (it may **also** be nested!), all its base classes and the class itself. The innermost namespace of all associated classes.
- If `T` is a `ClassTemplate<TemplateArguments>` (this is **also** a class!), the classes and namespaces associated with the template type arguments, the namespace of any template template argument and the surrounding class of any template template argument, if a template argument is a member template.

Now there are a few rules for builtin types as well

- If `T` is a pointer to `U` or array of `U`, the classes and namespaces associated with `U`. Example: `void (*fptr)(A); f(fptr);`, includes the namespaces and classes associated with `void(A)` (see next rule).
- If `T` is a function type, the classes and namespaces associated with parameter and return types. Example: `void(A)` would includes the namespaces and classes associated with `A`.
- If `T` is a pointer to member, the classes and namespaces associated with the member type (may apply to both pointer to member functions and pointer to data member!). Example: `B A::*p; void (A::*pf)(B); f(p); f(pf);` includes the namespaces and classes associated with `A`, `B`, `void(B)` (which applies bullet above for function types).

**All functions and templates within all associated namespaces are found by argument dependent lookup.** In addition, **namespace-scope friend functions declared in associated classes are found**, which are normally not visible. Using directives are ignored, however.

All of the following example calls are valid, without qualifying `f` by the namespace name in the call.

```cpp
namespace A {
   struct Z { };
   namespace I { void g(Z); }
   using namespace I;

   struct X { struct Y { }; friend void f(Y) { } };
   void f(X p) { }
   void f(std::shared_ptr<X> p) { }
}

// example calls
f(A::X());
f(A::X::Y());
f(std::make_shared<A::X>());

g(A::Z()); // invalid: "using namespace I;" is ignored!

```

