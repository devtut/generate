---
metaTitle: "Templates"
description: "Function Templates, Basic Class Template, Variadic template data structures, Argument forwarding, Template Specialization, Partial template specialization, Alias template, Template template parameters, Declaring non-type template arguments with auto, Non-type template parameter, Explicit instantiation, Default template parameter value"
---

# Templates


Classes, functions, and (since C++14) variables can be templated. A template is a piece of code with some free parameters that will become a concrete class, function, or variable when all parameters are specified. Parameters can be types, values, or themselves templates. A well-known template is `std::vector`, which becomes a concrete container type when the element type is specified, **e.g.,** `std::vector<int>`.



## Function Templates


Templating can also be applied to functions (as well as the more traditional structures) with the same effect.

```cpp
// 'T' stands for the unknown type
// Both of our arguments will be of the same type.
template<typename T>
void printSum(T add1, T add2)
{
    std::cout << (add1 + add2) << std::endl;
}

```

This can then be used in the same way as structure templates.

```cpp
printSum<int>(4, 5);
printSum<float>(4.5f, 8.9f);

```

In both these case the template argument is used to replace the types of the parameters; the result works just like a normal C++ function (if the parameters don't match the template type the compiler applies the standard conversions).

One additional property of template functions (unlike template classes) is that the compiler can infer the template parameters based on the parameters passed to the function.

```cpp
printSum(4, 5);     // Both parameters are int.
                    // This allows the compiler deduce that the type
                    // T is also int.

printSum(5.0, 4);   // In this case the parameters are two different types.
                    // The compiler is unable to deduce the type of T
                    // because there are contradictions. As a result
                    // this is a compile time error.

```

This feature allows us to simplify code when we combine template structures and functions. There is a common pattern in the standard library that allows us to make `template structure X` using a helper function `make_X()`.

```cpp
// The make_X pattern looks like this.
// 1) A template structure with 1 or more template types.
template<typename T1, typename T2>
struct MyPair
{
    T1      first;
    T2      second;
};
// 2) A make function that has a parameter type for
//    each template parameter in the template structure.
template<typename T1, typename T2>
MyPair<T1, T2> make_MyPair(T1 t1, T2 t2)
{
    return MyPair<T1, T2>{t1, t2};
}

```

How does this help?

```cpp
auto val1 = MyPair<int, float>{5, 8.7};     // Create object explicitly defining the types
auto val2 = make_MyPair(5, 8.7);            // Create object using the types of the paramters.
                                            // In this code both val1 and val2 are the same
                                            // type.

```

Note: This is not designed to shorten the code. This is designed to make the code more robust. It allows the types to be changed by changing the code in a single place rather than in multiple locations.



## Basic Class Template


The basic idea of a class template is that the template parameter gets substituted by a type at compile time. The result is that the same class can be reused for multiple types. The user specifies which type will be used when a variable of the class is declared. Three examples of this are shown in `main()`:

```cpp
#include <iostream>
using std::cout;

template <typename T>         // A simple class to hold one number of any type
class Number {
public:
    void setNum(T n);         // Sets the class field to the given number
    T plus1() const;          // returns class field's "follower"
private:
    T num;                    // Class field
};

template <typename T>         // Set the class field to the given number
void Number<T>::setNum(T n) {
    num = n;
}

template <typename T>         // returns class field's "follower"
T Number<T>::plus1() const {
    return num + 1;
}

int main() {
    Number<int> anInt;        // Test with an integer (int replaces T in the class)
    anInt.setNum(1);
    cout << "My integer + 1 is " << anInt.plus1() << "\n";     // Prints 2

    Number<double> aDouble;   // Test with a double
    aDouble.setNum(3.1415926535897);
    cout << "My double + 1 is " << aDouble.plus1() << "\n";    // Prints 4.14159

    Number<float> aFloat;     // Test with a float
    aFloat.setNum(1.4);
    cout << "My float + 1 is " << aFloat.plus1() << "\n";      // Prints 2.4

    return 0;  // Successful completion
}

```



## Variadic template data structures


It is often useful to define classes or structures that have a variable number and type of data members which are defined at compile time. The canonical example is `std::tuple`, but sometimes is it is necessary to define your own custom structures. Here is an example that defines the structure using compounding (rather than inheritance as with `std::tuple`. Start with the general (empty) definition, which also serves as the base-case for recrusion termination in the later specialisation:

```cpp
template<typename ... T>
struct DataStructure {};

```

This already allows us to define an empty structure, `DataStructure<> data`, albeit that isn't very useful yet.

Next comes the recursive case specialisation:

```cpp
template<typename T, typename ... Rest>
struct DataStructure<T, Rest ...>
{
    DataStructure(const T& first, const Rest& ... rest)
        : first(first)
        , rest(rest...)
    {}
    
    T first;                                
    DataStructure<Rest ... > rest;
};

```

This is now sufficient for us to create arbitrary data structures, like `DataStructure<int, float, std::string> data(1, 2.1, "hello")`.

So what's going on? First, note that this is a specialisation whose requirement is that at least one variadic template parameter (namely `T` above) exists, whilst not caring about the specific makeup of the pack `Rest`. Knowing that `T` exists allows the definition of its data member, `first`. The rest of the data is recursively packaged as `DataStructure<Rest ... > rest`. The constructor initiates both of those members, including a recursive constructor call to the `rest` member.

To understand this better, we can work through an example: suppose you have a declaration `DataStructure<int, float> data`. The declaration first matches against the specialisation, yielding a structure with `int first` and `DataStructure<float> rest` data members. The `rest` definition again matches this specialisation, creating its own `float first` and `DataStructure<> rest` members. Finally this last `rest` matches against the base-case defintion, producing an empty structure.

You can visualise this as follows:

```cpp
DataStructure<int, float>
   -> int first
   -> DataStructure<float> rest
         -> float first
         -> DataStructure<> rest
              -> (empty)

```

Now we have the data structure, but its not terribly useful yet as we cannot easily access the individual data elements (for example to access the last member of `DataStructure<int, float, std::string> data` we would have to use `data.rest.rest.first`, which is not exactly user-friendly). So we add a `get` method to it (only needed in the specialisation as the base-case structure has no data to `get`):

```cpp
template<typename T, typename ... Rest>
struct DataStructure<T, Rest ...>
{
    ...
    template<size_t idx>
    auto get()
    {
        return GetHelper<idx, DataStructure<T,Rest...>>::get(*this);
    }
    ...
};

```

As you can see this `get` member function is itself templated - this time on the index of the member that is needed (so usage can be things like `data.get<1>()`, similar to `std::tuple`). The actual work is done by a static function in a helper class, `GetHelper`. The reason we can't define the required functionality directly in `DataStructure`'s `get` is because (as we will shortly see) we would need to specialise on `idx` - but it isn't possible to specialise a template member function without specialising the containing class template. Note also the use of a C++14-style `auto` here makes our lives significantly simpler as otherwise we would need quite a complicated expression for the return type.

So on to the helper class. This time we will need an empty forward declaration and two specialisations. First the declaration:

```cpp
template<size_t idx, typename T>
struct GetHelper;

```

Now the base-case (when `idx==0`). In this case we just return the `first` member:

```cpp
template<typename T, typename ... Rest>
struct GetHelper<0, DataStructure<T, Rest ... >>
{
    static T get(DataStructure<T, Rest...>& data)
    {
        return data.first;
    }
};

```

In the recursive case, we decrement `idx` and invoke the `GetHelper` for the `rest` member:

```cpp
template<size_t idx, typename T, typename ... Rest>
struct GetHelper<idx, DataStructure<T, Rest ... >>
{
    static auto get(DataStructure<T, Rest...>& data)
    {
        return GetHelper<idx-1, DataStructure<Rest ...>>::get(data.rest);
    }
};

```

To work through an example, suppose we have `DataStructure<int, float> data` and we need `data.get<1>()`. This invokes `GetHelper<1, DataStructure<int, float>>::get(data)` (the 2nd specialisation), which in turn invokes `GetHelper<0, DataStructure<float>>::get(data.rest)`, which finally returns (by the 1st specialisation as now `idx` is 0) `data.rest.first`.

So that's it! Here is the whole functioning code, with some example use in the `main` function:

```cpp
#include <iostream>

template<size_t idx, typename T>
struct GetHelper;

template<typename ... T>
struct DataStructure
{
};

template<typename T, typename ... Rest>
struct DataStructure<T, Rest ...>
{
    DataStructure(const T& first, const Rest& ... rest)
        : first(first)
        , rest(rest...)
    {}
    
    T first;
    DataStructure<Rest ... > rest;
    
    template<size_t idx>
    auto get()
    {
        return GetHelper<idx, DataStructure<T,Rest...>>::get(*this);
    }
};

template<typename T, typename ... Rest>
struct GetHelper<0, DataStructure<T, Rest ... >>
{
    static T get(DataStructure<T, Rest...>& data)
    {
        return data.first;
    }
};

template<size_t idx, typename T, typename ... Rest>
struct GetHelper<idx, DataStructure<T, Rest ... >>
{
    static auto get(DataStructure<T, Rest...>& data)
    {
        return GetHelper<idx-1, DataStructure<Rest ...>>::get(data.rest);
    }
};

int main()
{
    DataStructure<int, float, std::string> data(1, 2.1, "Hello");
        
    std::cout << data.get<0>() << std::endl;
    std::cout << data.get<1>() << std::endl;
    std::cout << data.get<2>() << std::endl;
    
    return 0;
}

```



## Argument forwarding


Template may accept both lvalue and rvalue references using **forwarding reference**:

```cpp
template <typename T>
void f(T &&t);

```

In this case, the real type of `t` will be deduced depending on the context:

```cpp
struct X { };

X x;
f(x); // calls f<X&>(x)
f(X()); // calls f<X>(x)

```

In the first case, the type `T` is deduced as **reference to `X`** (`X&`), and the type of `t` is **lvalue reference to `X`**, while in the second case the type of `T` is deduced as `X` and the type of `t` as **rvalue reference to `X`** (`X&&`).

**Note:** It is worth noticing that in the first case, `decltype(t)` is the same as `T`, but not in the second.

In order to perfectly forward `t` to another function ,whether it is an lvalue or rvalue reference, one must use `std::forward`:

```cpp
template <typename T>
void f(T &&t) {
    g(std::forward<T>(t));
}

```

Forwarding references may be used with variadic templates:

```cpp
template <typename... Args>
void f(Args&&... args) {
    g(std::forward<Args>(args)...);
}

```

**Note:** Forwarding references can only be used for template parameters, for instance, in the following code, `v` is a rvalue reference, not a forwarding reference:

```cpp
#include <vector>

template <typename T>
void f(std::vector<T> &&v);

```



## Template Specialization


You can define implementation for specific instantiations of a template class/method.

For example if you have:

```cpp
template <typename T>
T sqrt(T t) { /* Some generic implementation */ }

```

You can then write:

```cpp
template<>
int sqrt<int>(int i) { /* Highly optimized integer implementation */ }

```

Then a user that writes `sqrt(4.0)` will get the generic implementation whereas `sqrt(4)` will get the specialized implementation.



## Partial template specialization


In contrast of a full template specialization partial template specialization allows to introduce template with some of the arguments of existing template fixed. Partial template specialization is only available for template class/structs:

```cpp
// Common case:
template<typename T, typename U>
struct S {
    T t_val;
    U u_val;
};

// Special case when the first template argument is fixed to int
template<typename V>
struct S<int, V> {
    double another_value;
    int foo(double arg) {// Do something}
};

```

As shown above, partial template specializations may introduce completely different sets of data and function members.

When a partially specialized template is instantiated, the most suitable specialization is selected. For example, let's define a template and two partial specializations:

```cpp
template<typename T, typename U, typename V>
struct S {
    static void foo() {
        std::cout << "General case\n";
    }
};

template<typename U, typename V>
struct S<int, U, V> {
    static void foo() {
        std::cout << "T = int\n";
    }
};

template<typename V>
struct S<int, double, V> {
    static void foo() {
        std::cout << "T = int, U = double\n";
    }
};

```

Now the following calls:

```cpp
S<std::string, int, double>::foo();
S<int, float, std::string>::foo();
S<int, double, std::string>::foo();

```

will print

```cpp
General case
T = int
T = int, U = double

```

Function templates may only be fully specialized:

```cpp
template<typename T, typename U>
void foo(T t, U u) {
    std::cout << "General case: " << t << " " << u << std::endl;
}

// OK.
template<>
void foo<int, int>(int a1, int a2) {
    std::cout << "Two ints: " << a1 << " " << a2 << std::endl;
}

void invoke_foo() {
    foo(1, 2.1); // Prints "General case: 1 2.1"
    foo(1,2);    // Prints "Two ints: 1 2"
}

// Compilation error: partial function specialization is not allowed.
template<typename U>
void foo<std::string, U>(std::string t, U u) {
    std::cout << "General case: " << t << " " << u << std::endl;
}

```



## Alias template


Basic example:

```cpp
template<typename T> using pointer = T*;

```

This definition makes `pointer<T>` an alias of `T*`. For example:

```cpp
pointer<int> p = new int; // equivalent to: int* p = new int;

```

Alias templates cannot be specialized. However, that functionality can be obtained indirectly by having them refer to a nested type in a struct:

```cpp
template<typename T>
 struct nonconst_pointer_helper { typedef T* type; };

template<typename T>
 struct nonconst_pointer_helper<T const> { typedef T* type; };

template<typename T> using nonconst_pointer = nonconst_pointer_helper<T>::type;

```



## Template template parameters


Sometimes we would like to pass into the template a template type without fixing its values. This is what template template parameters are created for. Very simple template template parameter examples:

```cpp
template <class T>
struct Tag1 { };

template <class T>
struct Tag2 { };

template <template <class> class Tag>
struct IntTag {
   typedef Tag<int> type;
};

int main() {
   IntTag<Tag1>::type t;
}

```

```cpp
#include <vector>
#include <iostream>

template <class T, template <class...> class C, class U>
C<T> cast_all(const C<U> &c) {
   C<T> result(c.begin(), c.end());
   return result;
}

int main() {
   std::vector<float> vf = {1.2, 2.6, 3.7};
   auto vi = cast_all<int>(vf);
   for(auto &&i: vi) {
      std::cout << i << std::endl;
   }
}

```



## Declaring non-type template arguments with auto


Prior to C++17, when writing a template non-type parameter, you had to specify its type first. So a common pattern became writing something like:

```cpp
template <class T, T N>
struct integral_constant {
    using type = T;
    static constexpr T value = N;
};

using five = integral_constant<int, 5>;

```

But for complicated expressions, using something like this involves having to write `decltype(expr), expr` when instantiating templates. The solution is to simplify this idiom and simply allow `auto`:

```cpp
template <auto N>
struct integral_constant {
    using type = decltype(N); 
    static constexpr type value = N;
};

using five = integral_constant<5>;

```

### Empty custom deleter for unique_ptr

A nice motivating example can come from trying to combine the empty base optimization with a custom deleter for `unique_ptr`. Different C API deleters have different return types, but we don't care - we just want something to work for any function:

```cpp
template <auto DeleteFn>
struct FunctionDeleter {
    template <class T>
    void operator()(T* ptr) const {
        DeleteFn(ptr);
    }
};

template <T, auto DeleteFn>
using unique_ptr_deleter = std::unique_ptr<T, FunctionDeleter<DeleteFn>>;

```

And now you can simply use any function pointer that can take an argument of type `T` as a template non-type parameter, regardless of return type, and get a no-size overhead `unique_ptr` out of it:

```cpp
unique_ptr_deleter<std::FILE, std::fclose> p;

```



## Non-type template parameter


Apart from types as a template parameter we are allowed to declare
values of constant expressions meeting one of the following criteria:

- integral or enumeration type,
- pointer to object  or pointer to function,
- lvalue reference to object or lvalue  reference to function,
- pointer to member,
- `std::nullptr_t`.

Like all template parameters, non-type template parameters can be explicitly specified, defaulted, or derived implicitly via Template Argument Deduction.

Example of non-type template parameter usage:

```cpp
#include <iostream>

template<typename T, std::size_t size>
std::size_t size_of(T (&anArray)[size])  // Pass array by reference. Requires.
{                                        // an exact size. We allow all sizes
    return size;                         // by using a template "size".
}

int main()
{
    char anArrayOfChar[15];
    std::cout << "anArrayOfChar: " << size_of(anArrayOfChar) << "\n";

    int  anArrayOfData[] = {1,2,3,4,5,6,7,8,9};
    std::cout << "anArrayOfData: " << size_of(anArrayOfData) << "\n";
}

```

Example of explicitly specifying both type and non-type template parameters:

```cpp
#include <array>
int main ()
{
    std::array<int, 5> foo; // int is a type parameter, 5 is non-type
}

```

Non-type template parameters are one of the ways to achieve template
recurrence and enables to do [Metaprogramming](http://stackoverflow.com/documentation/c%2b%2b/462/metaprogramming).



## Explicit instantiation


An explicit instantiation definition creates and declares a concrete class, function, or variable from a template, without using it just yet. An explicit instantiation can be referenced from other translation units. This can be used to avoid defining a template in a header file, if it will only be instantiated with a finite set of arguments. For example:

```cpp
// print_string.h
template <class T>
void print_string(const T* str);

// print_string.cpp
#include "print_string.h"
template void print_string(const char*);
template void print_string(const wchar_t*);

```

Because `print_string<char>` and `print_string<wchar_t>` are explicitly instantiated in `print_string.cpp`, the linker will be able to find them even though the `print_string` template is not defined in the header. If these explicit instantiation declarations were not present, a linker error would likely occur. See [Why can templates only be implemented in the header file?](http://stackoverflow.com/questions/495021/why-can-templates-only-be-implemented-in-the-header-file)

If an explicit instantiation definition is preceded by the `extern` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords), it becomes an explicit instantiation **declaration** instead. The presence of an explicit instantiation declaration for a given specialization  prevents the implicit instantiation of the given specialization within the current translation unit. Instead, a reference to that specialization that would otherwise cause an implicit instantiation can refer to an explicit instantiation definition in the same or another TU.

`foo.h`

```cpp
#ifndef FOO_H
#define FOO_H
template <class T> void foo(T x) {
    // complicated implementation
}
#endif

```

`foo.cpp`

```cpp
#include "foo.h"
// explicit instantiation definitions for common cases
template void foo(int);
template void foo(double);

```

`main.cpp`

```cpp
#include "foo.h"
// we already know foo.cpp has explicit instantiation definitions for these
extern template void foo(double);
int main() {
    foo(42);   // instantiates foo<int> here;
               // wasteful since foo.cpp provides an explicit instantiation already!
    foo(3.14); // does not instantiate foo<double> here;
               // uses instantiation of foo<double> in foo.cpp instead
}

```



## Default template parameter value


Just like in case of the function arguments, template parameters can have their default values. All template parameters with a default value have to be declared at the end of the template parameter list. The basic idea is that the template parameters with default value can be omitted while template instantiation.

Simple example of default template parameter value usage:

```cpp
template <class T, size_t N = 10>
struct my_array {
    T arr[N];
};

int main() {
    /* Default parameter is ignored, N = 5 */
    my_array<int, 5> a;

    /* Print the length of a.arr: 5 */
    std::cout << sizeof(a.arr) / sizeof(int) << std::endl;

    /* Last parameter is omitted, N = 10 */
    my_array<int> b;

    /* Print the length of a.arr: 10 */
    std::cout << sizeof(b.arr) / sizeof(int) << std::endl;
}

```



#### Syntax


- template < **template-parameter-list** > **declaration**
- export template < **template-parameter-list** > **declaration** /* until C++11 */
- template <> **declaration**
- template **declaration**
- extern template **declaration** /* since C++11 */
- template < **template-parameter-list** > class ...(**opt**) **identifier**(**opt**)
- template < **template-parameter-list** > class **identifier**(**opt**) = **id-expression**
- template < **template-parameter-list** > typename ...(**opt**) **identifier**(**opt**) /* since C++17 */
- template < **template-parameter-list** > typename **identifier**(**opt**) = **id-expression** /* since C++17 */
- **postfix-expression** . template **id-expression**
- **postfix-expression** -> template **id-expression**
- **nested-name-specifier** `template` **simple-template-id** `::`



#### Remarks


The word `template` is a [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) with five different meanings in the C++ language, depending on the context.

<li>
When followed by a list of template parameters enclosed in `<>`, it declares a template such as a [class template](http://stackoverflow.com/documentation/c%2b%2b/460/templates/3999/basic-class-template), a [function template](http://stackoverflow.com/documentation/c%2b%2b/460/templates/1517/function-templates), or a [partial specialization](http://stackoverflow.com/documentation/c%2b%2b/460/templates/6253/partial-template-specialization) of an existing template.

```cpp
template <class T>
void increment(T& x) { ++x; }

```


</li>
<li>
When followed by an **empty** `<>`, it declares a an [explicit (full) specialization](http://stackoverflow.com/documentation/c%2b%2b/460/templates/5054/template-specialization).

```cpp
template <class T>
void print(T x);

template <> // <-- keyword used in this sense here
void print(const char* s) {
    // output the content of the string
    printf("%s\n", s);
}

```


</li>
<li>
When followed by a declaration without `<>`, it forms an [explicit instantiation](http://stackoverflow.com/documentation/c%2b%2b/460/templates/28734/explicit-instantiation) declaration or definition.

```cpp
template <class T>
std::set<T> make_singleton(T x) { return std::set<T>(x); }

template std::set<int> make_singleton(int x); // <-- keyword used in this sense here

```


</li>
<li>
Within a template parameter list, it introduces a [template template parameter](http://stackoverflow.com/documentation/c%2b%2b/460/templates/10838/template-template-parameters).

```cpp
template <class T, template <class U> class Alloc>
//                 ^^^^^^^^ keyword used in this sense here
class List {
    struct Node {
        T value;
        Node* next;
    };
    Alloc<Node> allocator;
    Node* allocate_node() {
        return allocator.allocate(sizeof(T));
    }
    // ...
};

```


</li>
<li>
After the scope resolution operator `::` and the class member access operators `.` and `->`, it specifies that the following name is a template.

```cpp
struct Allocator {
    template <class T>
    T* allocate();
};

template <class T, class Alloc>
class List {
    struct Node {
        T value;
        Node* next;
    }
    Alloc allocator;
    Node* allocate_node() {
        // return allocator.allocate<Node>();       // error: < and > are interpreted as
                                                    // comparison operators
        return allocator.template allocate<Node>(); // ok; allocate is a template
        //               ^^^^^^^^ keyword used in this sense here
    }
};

```


</li>

Before C++11, a template could be declared with the `export` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords), making it into an **exported** template. An exported template's definition does not need to be present in every translation unit in which the template is instantiated. For example, the following was supposed to work:

`foo.h`:

```cpp
#ifndef FOO_H
#define FOO_H
export template <class T> T identity(T x);
#endif

```

`foo.cpp`:

```cpp
#include "foo.h"
template <class T> T identity(T x) { return x; }

```

`main.cpp`:

```cpp
#include "foo.h"
int main() {
    const int x = identity(42); // x is 42
}

```

Due to difficulty of implementation, the `export` keyword was not supported by most major compilers. It was removed in C++11; now, it is illegal to use the `export` keyword at all. Instead, it is usually necessary to define templates in headers (in contrast to non-template functions, which are usually **not** defined in headers). See [Why can templates only be implemented in the header file?](http://stackoverflow.com/questions/495021/why-can-templates-only-be-implemented-in-the-header-file)

