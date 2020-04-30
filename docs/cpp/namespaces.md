---
metaTitle: "Namespaces"
description: "What are namespaces?, Argument Dependent Lookup, Extending namespaces, Using directive, Making namespaces, Unnamed/anonymous namespaces, Compact nested namespaces, Inline namespace, Namespace alias, Aliasing a long namespace, Alias Declaration scope"
---

# Namespaces


Used to prevent name collisions when using multiple libraries, a namespace is a declarative prefix for functions, classes, types, etc.



## What are namespaces?


A C++ namespace is a collection of C++ entities (functions, classes, variables), whose names are prefixed by the name of the namespace. When writing code within a namespace, named entities belonging to that namespace need not be prefixed with the namespace name, but entities outside of it must use the fully qualified name.  The fully qualified name has the format `<namespace>::<entity>`. Example:

```cpp
namespace Example
{
  const int test = 5;

  const int test2 = test + 12; //Works within `Example` namespace
}

const int test3 = test + 3; //Fails; `test` not found outside of namespace.

const int test3 = Example::test + 3; //Works; fully qualified name used.

```

Namespaces are useful for grouping related definitions together. Take the analogy of a shopping mall.  Generally a shopping mall is split up into several stores, each store selling items from a specific category.  One store might sell electronics, while another store might sell shoes.  These logical separations in store types help the shoppers find the items they're looking for.  Namespaces help c++ programmers, like shoppers, find the functions, classes, and variables they're looking for by organizing them in a logical manner.  Example:

```cpp
namespace Electronics
{
    int TotalStock;
    class Headphones
    {
        // Description of a Headphone (color, brand, model number, etc.)
    };
    class Television
    {
        // Description of a Television (color, brand, model number, etc.)
    };
}

namespace Shoes
{
    int TotalStock;
    class Sandal
    {
        // Description of a Sandal (color, brand, model number, etc.)
    };
    class Slipper
    {
        // Description of a Slipper (color, brand, model number, etc.)
    };
}

```

There is a single namespace predefined, which is the global namespace that has no name, but can be denoted by `::`. Example:

```cpp
void bar() {
    // defined in global namespace
}
namespace foo {
    void bar() {
        // defined in namespace foo
    }
    void barbar() {
        bar();   // calls foo::bar()
        ::bar(); // calls bar() defined in global namespace
    }
}

```



## Argument Dependent Lookup


When calling a function without an explicit namespace qualifier, the compiler can choose to call a function within a namespace if one of the parameter types to that function is also in that namespace. This is called "Argument Dependent Lookup", or ADL:

```cpp
namespace Test
{
  int call(int i);

  class SomeClass {...};

  int call_too(const SomeClass &data);
}

call(5); //Fails. Not a qualified function name.

Test::SomeClass data;

call_too(data); //Succeeds

```

`call` fails because none of its parameter types come from the `Test` namespace. `call_too` works because `SomeClass` is a member of `Test` and therefore it qualifies for ADL rules.

### When does ADL not occur

ADL does not occur if normal unqualified lookup finds a class member, a function that has been declared at block scope, or something that is not of function type. For example:

```cpp
void foo();
namespace N {
    struct X {};
    void foo(X ) { std::cout << '1'; }
    void qux(X ) { std::cout << '2'; }
}

struct C {
    void foo() {}
    void bar() {
        foo(N::X{}); // error: ADL is disabled and C::foo() takes no arguments
    }
};

void bar() {
    extern void foo(); // redeclares ::foo
    foo(N::X{});       // error: ADL is disabled and ::foo() doesn't take any arguments
}

int qux;

void baz() {
    qux(N::X{}); // error: variable declaration disables ADL for "qux"
}

```



## Extending namespaces


A useful feature of `namespace`s is that you can expand them (add members to it).

```cpp
namespace Foo
{
    void bar() {}
}

//some other stuff

namespace Foo
{
    void bar2() {}
}

```



## Using directive


The keyword ['using'](http://stackoverflow.com/documentation/c%2B%2B/4891/keywords/19096/using#t=20160909070552352037) has three flavors. Combined with keyword 'namespace' you write a 'using directive':

If you don't want to write `Foo::` in front of every stuff in the namespace `Foo`, you can use `using namespace Foo;` to import every single thing out of `Foo`.

```cpp
namespace Foo
{
    void bar() {}
    void baz() {}
}

//Have to use Foo::bar()
Foo::bar();

//Import Foo
using namespace Foo;
bar(); //OK
baz(); //OK

```

It is also possible to import selected entities in a namespace rather than the whole namespace:

```cpp
using Foo::bar;
bar(); //OK, was specifically imported
baz(); // Not OK, was not imported

```

A word of caution: `using namespace`s in header files is seen as bad style in most cases. If this is done, the namespace is imported in **every** file that includes the header. Since there is no way of "un-`using`" a namespace, this can lead to namespace pollution (more or unexpected symbols in the global namespace) or, worse, conflicts. See this example for an illustration of the problem:

```cpp
/***** foo.h *****/
namespace Foo
{
    class C;
}

/***** bar.h *****/
namespace Bar
{
    class C;
}

/***** baz.h *****/
#include "foo.h"
using namespace Foo;

/***** main.cpp *****/
#include "bar.h"
#include "baz.h"

using namespace Bar;
C c; // error: Ambiguity between Bar::C and Foo::C

```

A **using-directive** cannot occur at class scope.



## Making namespaces


Creating a namespace is really easy:

```cpp
//Creates namespace foo
namespace Foo
{
    //Declares function bar in namespace foo
    void bar() {}
}

```

To call `bar`, you have to specify the namespace first, followed by the scope resolution operator `::`:

```cpp
Foo::bar();

```

It is allowed to create one namespace in another, for example:

```cpp
namespace A
{
    namespace B
    {
        namespace C
        {
            void bar() {}
        }
    }
}

```

The above code could be simplified to the following:

```cpp
namespace A::B::C
{
    void bar() {}
}

```



## Unnamed/anonymous namespaces


An unnamed namespace can be used to ensure names have internal linkage (can only be referred to by the current translation unit). Such a namespace is defined in the same way as any other namespace, but without the name:

```cpp
namespace {
    int foo = 42;
}

```

`foo` is only visible in the translation unit in which it appears.

It is recommended to never use unnamed namespaces in header files as this gives a version of the content for every translation unit it is included in. This is especially important if you define non-const globals.

```cpp
// foo.h
namespace {
    std::string globalString;
}

// 1.cpp
#include "foo.h" //< Generates unnamed_namespace{1.cpp}::globalString ...

globalString = "Initialize";

// 2.cpp
#include "foo.h" //< Generates unnamed_namespace{2.cpp}::globalString ...

std::cout << globalString; //< Will always print the empty string

```



## Compact nested namespaces


```cpp
namespace a {
  namespace b {
    template<class T>
    struct qualifies : std::false_type {};
  }
}

namespace other {
  struct bob {};
}

namespace a::b {
  template<>
  struct qualifies<::other::bob> : std::true_type {};
}

```

You can enter both the `a` and `b` namespaces in one step with `namespace a::b` starting in C++17.



## Inline namespace


`inline namespace` includes the content of the inlined namespace in the enclosing namespace, so

```cpp
namespace Outer
{
    inline namespace Inner
    {
        void foo();
    }
}

```

is mostly equivalent to

```cpp
namespace Outer
{

    namespace Inner
    {
        void foo();
    }

    using Inner::foo;
}

```

but element from `Outer::Inner::` and those associated into `Outer::` are identical.

So following is equivalent

```cpp
Outer::foo();
Outer::Inner::foo();

```

The alternative `using namespace Inner;` would not be equivalent for some tricky parts as template specialization:

For

```cpp
#include <outer.h> // See below

class MyCustomType;
namespace Outer
{
    template <>
    void foo<MyCustomType>() { std::cout << "Specialization"; }
}

```


<li>
The inline namespace allows the specialization of `Outer::foo`

```cpp
// outer.h
// include guard omitted for simplification

namespace Outer
{
    inline namespace Inner
    {
        template <typename T>
        void foo() { std::cout << "Generic"; }
    }
}

```


</li>
<li>
Whereas the `using namespace` doesn't allow the specialization of `Outer::foo`

```cpp
// outer.h
// include guard omitted for simplification

namespace Outer
{
    namespace Inner
    {
        template <typename T>
        void foo() { std::cout << "Generic"; }
    }
    using namespace Inner;
    // Specialization of `Outer::foo` is not possible
    // it should be `Outer::Inner::foo`.
}

```


</li>

Inline namespace is a way to allow several version to cohabit and defaulting to the `inline` one

```cpp
namespace MyNamespace
{
    // Inline the last version
    inline namespace Version2
    {
        void foo(); // New version
        void bar();
    }

    namespace Version1 // The old one
    {
        void foo();
    }

}

```

And with usage

```cpp
MyNamespace::Version1::foo(); // old version
MyNamespace::Version2::foo(); // new version
MyNamespace::foo();           // default version : MyNamespace::Version1::foo();

```



## Namespace alias


A namespace can be given an alias (**i.e.,** another name for the same namespace) using the `namespace` **identifier** `=` syntax.  Members of the aliased namespace can be accessed by qualifying them with the name of the alias. In the following example, the nested namespace `AReallyLongName::AnotherReallyLongName` is inconvenient to type, so the function `qux` locally declares an alias `N`. Members of that namespace can then be accessed simply using `N::`.

```cpp
namespace AReallyLongName {
    namespace AnotherReallyLongName {
        int foo();
        int bar();
        void baz(int x, int y);
    }
}
void qux() {
    namespace N = AReallyLongName::AnotherReallyLongName;
    N::baz(N::foo(), N::bar());
}

```



## Aliasing a long namespace


This is usually used for renaming or shortening long namespace references such referring to components of a library.

```cpp
namespace boost
{
    namespace multiprecision
    {
        class Number ...
    }
}

namespace Name1 = boost::multiprecision;


//    Both Type declarations are equivalent
boost::multiprecision::Number X   //    Writing the full namespace path, longer
Name1::Number Y                   //    using the name alias, shorter

```



## Alias Declaration scope


Alias Declaration are affected by preceding **using** statements

```cpp
namespace boost
{
    namespace multiprecision
    {
        class Number ...
    }
}


using namespace boost;

//   Both Namespace are equivalent 
namespace Name1 = boost::multiprecision;
namespace Name2 = multiprecision;

```

However, it is easier to get confused over which namespace you are aliasing when you have something like this:

```cpp
namespace boost
{
    namespace multiprecision
    {
        class Number ...
    }
}

namespace numeric
{
    namespace multiprecision
    {
        class Number ...
    }
}

using namespace numeric;
using namespace boost;

//    Not recommended as 
//    its not explicitly clear whether Name1 refers to
//    numeric::multiprecision or boost::multiprecision
namespace Name1 = multiprecision;

//    For clarity, its recommended to use absolute paths
//    instead
namespace Name2 = numeric::multiprecision;
namespace Name3 = boost::multiprecision;

```



#### Syntax


- namespace **identifier**(**opt**) { **declaration-seq** }
- inline namespace **identifier**(**opt**) { **declaration-seq** } /* since C++11 */
- inline(**opt**) namespace **attribute-specifier-seq** **identifier**(**opt**) { **declaration-seq** } /* since C++17 */
- namespace **enclosing-namespace-specifier** :: **identifier** { **declaration-seq** } /* since C++17 */
- namespace **identifier** = **qualified-namespace-specifier**;
- using namespace **nested-name-specifier**(**opt**) **namespace-name**;
- **attribute-specifier-seq** using namespace **nested-name-specifier**(**opt**) **namespace-name**; /* since C++11 */



#### Remarks


The [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) `namespace` has three different meanings depending on context:

<li>
When followed by an optional name and a brace-enclosed sequence of declarations, it [defines a new namespace](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces) or [extends an existing namespace](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/1623/extending-namespaces) with those declarations. If the name is omitted, the namespace is an [unnamed namespace](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/4851/unnamed-anonymous-namespaces).
</li>
<li>
When followed by a name and an equal sign, it declares a [namespace alias](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/28691/namespace-alias).
</li>
<li>
When preceded by `using` and followed by a namespace name, it forms a [**using directive**](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/1624/using-namespaces#t=201608060458446751916), which allows names in the given namespace to be found by unqualified name lookup (but does not redeclare those names in the current scope). A **using-directive** cannot occur at class scope.
</li>

`using namespace std;` is discouraged. Why? Because `namespace std` is huge! This means that there is a high chance that names will collide:

```cpp
//Really bad!
using namespace std;

//Calculates p^e and outputs it to std::cout
void pow(double p, double e) { /*...*/ }

//Calls pow
pow(5.0, 2.0); //Error! There is already a pow function in namespace std with the same signature,
               //so the call is ambiguous

```

