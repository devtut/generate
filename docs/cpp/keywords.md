---
metaTitle: "Keywords"
description: "asm, explicit, noexcept, typename, sizeof, Different keywords"
---

# Keywords


Keywords have fixed meaning defined by the C++ standard and cannot be used as identifiers. It is illegal to redefine keywords using the preprocessor in any translation unit that includes a standard library header. However, keywords lose their special meaning inside attributes.



## asm


The `asm` keyword takes a single operand, which must be a string literal. It has an implementation-defined meaning, but is typically passed to the implementation's assembler, with the assembler's output being incorporated into the translation unit.

The `asm` statement is a **definition**, not an **expression**, so it may appear either at block scope or namespace scope (including global scope). However, since inline assembly cannot be constrained by the rules of the C++ language, `asm` may not appear inside a `constexpr` function.

Example:

```cpp
[[noreturn]] void halt_system() {
    asm("hlt");
}

```



## explicit


<li>
When applied to a single-argument constructor, prevents that constructor from being used to perform implicit conversions.

```cpp
class MyVector {
  public:
    explicit MyVector(uint64_t size);
};
MyVector v1(100);  // ok
uint64_t len1 = 100;
MyVector v2{len1}; // ok, len1 is uint64_t
int len2 = 100;
MyVector v3{len2}; // ill-formed, implicit conversion from int to uint64_t

```


Since C++11 introduced initializer lists, in C++11 and later, `explicit` can be applied to a constructor with any number of arguments, with the same meaning as in the single-argument case.

```cpp
struct S {
    explicit S(int x, int y);
};
S f() {
    return {12, 34};  // ill-formed
    return S{12, 34}; // ok
}

```


</li>

<li>
When applied to a conversion function, prevents that conversion function from being used to perform implicit conversions.

```cpp
class C {
    const int x;
  public:
    C(int x) : x(x) {}
    explicit operator int() { return x; }
};
C c(42);
int x = c;                   // ill-formed
int y = static_cast<int>(c); // ok; explicit conversion

```


</li>



## noexcept


<li>
A unary operator that determines whether the evaluation of its operand can propagate an exception. Note that the bodies of called functions are not examined, so `noexcept` can yield false negatives. The operand is not evaluated.

```cpp
#include <iostream>
#include <stdexcept>
void foo() { throw std::runtime_error("oops"); }
void bar() {}
struct S {};
int main() {
    std::cout << noexcept(foo()) << '\n'; // prints 0
    std::cout << noexcept(bar()) << '\n'; // prints 0
    std::cout << noexcept(1 + 1) << '\n'; // prints 1
    std::cout << noexcept(S()) << '\n';   // prints 1
}

```


In this example, even though `bar()` can never throw an exception, `noexcept(bar())` is still false because the fact that `bar()` cannot propagate an exception has not been explicitly specified.
</li>
<li>
When declaring a function, specifies whether or not the function can propagate an exception. Alone, it declares that the function cannot propagate an exception. With a parenthesized argument, it declares that the function can or cannot propagate an exception depending on the truth value of the argument.

```cpp
void f1() { throw std::runtime_error("oops"); }
void f2() noexcept(false) { throw std::runtime_error("oops"); }
void f3() {}
void f4() noexcept {}
void f5() noexcept(true) {}
void f6() noexcept {
    try {
        f1();
    } catch (const std::runtime_error&) {}
}

```


In this example, we have declared that `f4`, `f5`, and `f6` cannot propagate exceptions. (Although an exception can be thrown during execution of `f6`, it is caught and not allowed to propagate out of the function.) We have declared that `f2` may propagate an exception. When the `noexcept` specifier is omitted, it is equivalent to `noexcept(false)`, so we have implicitly declared that `f1` and `f3` may propagate exceptions, even though exceptions cannot actually be thrown during the execution of `f3`.
</li>

Whether or not a function is `noexcept` is part of the function's type: that is, in the example above, `f1`, `f2`, and `f3` have different types from `f4`, `f5`, and `f6`. Therefore, `noexcept` is also significant in function pointers, template arguments, and so on.

```cpp
void g1() {}
void g2() noexcept {}
void (*p1)() noexcept = &g1; // ill-formed, since g1 is not noexcept
void (*p2)() noexcept = &g2; // ok; types match
void (*p3)() = &g1;          // ok; types match
void (*p4)() = &g2;          // ok; implicit conversion

```



## typename


<li>
When followed by a qualified name, `typename` specifies that it is the name of a type. This is often required in templates, in particular, when the nested name specifier is a dependent type other than the current instantiation. In this example, `std::decay<T>` depends on the template parameter `T`, so in order to name the nested type `type`, we need to prefix the entire qualified name with `typename`. For more deatils, see [Where and why do I have to put the "template" and "typename" keywords?](http://stackoverflow.com/questions/610245/where-and-why-do-i-have-to-put-the-template-and-typename-keywords)

```cpp
template <class T>
auto decay_copy(T&& r) -> typename std::decay<T>::type;

```


</li>
<li>
Introduces a type parameter in the declaration of a [template](http://stackoverflow.com/documentation/c%2b%2b/460/templates). In this context, it is interchangeable with `class`.

```cpp
template <typename T>
const T& min(const T& x, const T& y) {
    return b < a ? b : a;
} 

```


</li>

<li>
`typename` can also be used when declaring a [template template parameter](http://stackoverflow.com/documentation/c%2b%2b/460/templates/10838/template-template-parameters), preceding the name of the parameter, just like `class`.

```cpp
template <template <class T> typename U>
void f() {
    U<int>::do_it();
    U<double>::do_it();
}

```


</li>



## sizeof


A unary operator that yields the size in bytes of its operand, which may be either an expression or a type. If the operand is an expression, it is not evaluated. The size is a constant expression of type `std::size_t`.

If the operand is a type, it must be parenthesized.

- It is illegal to apply `sizeof` to a function type.
- It is illegal to apply `sizeof` to an incomplete type, including `void`.
- If sizeof is applied to a reference type `T&` or `T&&`, it is equivalent to `sizeof(T)`.
- When `sizeof` is applied to a class type, it yields the number of bytes in a complete object of that type, including any padding bytes in the middle or at the end. Therefore, a `sizeof` expression can never have a value of 0. See [layout of object types](http://stackoverflow.com/documentation/c%2b%2b/9329/layout-of-object-types) for more details.
- The `char`, `signed char`, and `unsigned char` types have a size of 1. Conversely, a byte is defined to be the amount of memory required to store a `char` object. It does not necessarily mean 8 bits, as some systems have `char` objects longer than 8 bits.

If **expr** is an expression, `sizeof(`**expr**`)` is equivalent to `sizeof(T)` where `T` is the type of **expr.**

```cpp
int a[100];
std::cout << "The number of bytes in `a` is: " << sizeof a;
memset(a, 0, sizeof a); // zeroes out the array

```

The `sizeof...` operator yields the number of elements in a parameter pack.

```cpp
template <class... T>
void f(T&&...) {
    std::cout << "f was called with " << sizeof...(T) << " arguments\n";
}

```



## Different keywords


> 
**void C++**


<li>
When used as a function return type, the void keyword specifies that the function does not return a value. When used for a function's parameter list, void specifies that the function takes no parameters. When used in the declaration of a pointer, void specifies that the pointer is "universal."
</li>
<li>
If a pointer's type is void *, the pointer can point to any variable that is not declared with the const or volatile keyword. A void pointer cannot be dereferenced unless it is cast to another type. A void pointer can be converted into any other type of data pointer.
</li>
<li>
A void pointer can point to a function, but not to a class member in C++.

```cpp
void vobject;   // C2182  
void *pv;   // okay  
int *pint; int i;  
int main() {  
pv = &i;  
   // Cast optional in C required in C++  
pint = (int *)pv;  

```


</li>

> 
**Volatile C++**


<li>
A type qualifier that you can use to declare that an object can be modified in the program by the hardware.

```cpp
volatile declarator ;

```


</li>

> 
**virtual C++**


<li>
The virtual keyword declares a virtual function or a virtual base class.

```cpp
virtual [type-specifiers] member-function-declarator  
virtual [access-specifier] base-class-name 

```


</li>

**Parameters**

<li>
**type-specifiers**   Specifies the return type of the virtual member function.
</li>
<li>
**member-function-declarator**   Declares a member function.
</li>
<li>
**access-specifier**   Defines the level of access to the base class, public, protected or private. Can appear before or after the virtual keyword.
</li>
<li>
**base-class-name**  Identifies a previously declared class type
</li>

> 
**this pointer**


<li>
The this pointer is a pointer accessible only within the nonstatic member functions of a class, struct, or union type. It points to the object for which the member function is called. Static member functions do not have a this pointer.

```cpp
this->member-identifier  

```


</li>

An object's this pointer is not part of the object itself; it is not reflected in the result of a sizeof statement on the object. Instead, when a nonstatic member function is called for an object, the address of the object is passed by the compiler as a hidden argument to the function. For example, the following function call:

```cpp
myDate.setMonth( 3 );  

can be interpreted this way:


setMonth( &myDate, 3 );  

The object's address is available from within the member function as the this pointer. Most uses of this are implicit. It is legal, though unnecessary, to explicitly use this when referring to members of the class. For example:


void Date::setMonth( int mn )  
{  
   month = mn;            // These three statements  
   this->month = mn;      // are equivalent  
   (*this).month = mn;  
}  

The expression *this is commonly used to return the current object from a member function:


return *this;  

The this pointer is also used to guard against self-reference:


if (&Object != this) {  
// do not execute in cases of self-reference 

```

> 
**try, throw, and catch Statements (C++)**


1. To implement exception handling in C++, you use try, throw, and catch expressions.
1. First, use a try block to enclose one or more statements that might throw an exception.
1. A throw expression signals that an exceptional condition—often, an error—has occurred in a try block. You can use an object of any type as the operand of a throw expression. Typically, this object is used to communicate information about the error. In most cases, we recommend that you use the std::exception class or one of the derived classes that are defined in the standard library. If one of those is not appropriate, we recommend that you derive your own exception class from std::exception.
1. To handle exceptions that may be thrown, implement one or more catch blocks immediately following a try block. Each catch block specifies the type of exception it can handle.

```

   MyData md;  
try {  
   // Code that could throw an exception  
   md = GetNetworkResource();  
}  
catch (const networkIOException& e) {  
   // Code that executes when an exception of type  
   // networkIOException is thrown in the try block  
   // ...  
   // Log error message in the exception object  
   cerr << e.what();  
}  
catch (const myDataFormatException& e) {  
   // Code that handles another exception type  
   // ...  
   cerr << e.what();  
}  
  
// The following syntax shows a throw expression  
MyData GetNetworkResource()  
{  
   // ...  
   if (IOSuccess == false)  
      throw networkIOException("Unable to connect");  
   // ...  
   if (readError)  
      throw myDataFormatException("Format error");   
   // ...  
}

```

> 
The code after the try clause is the guarded section of code. The throw expression throws—that is, raises—an exception. The code block after the catch clause is the exception handler. This is the handler that catches the exception that's thrown if the types in the throw and catch expressions are compatible.


```

   try {  
   throw CSomeOtherException();  
}  
catch(...) {  
   // Catch all exceptions – dangerous!!!  
   // Respond (perhaps only partially) to the exception, then  
   // re-throw to pass the exception to some other handler  
   // ...  
   throw;  
}

```

> 
**friend (C++)**


<li>
In some circumstances, it is more convenient to grant member-level access to functions that are not members of a class or to all members in a separate class. Only the class implementer can declare who its friends are. A function or class cannot declare itself as a friend of any class. In a class definition, use the friend keyword and the name of a non-member function or other class to grant it access to the private and protected members of your class. In a template definition, a type parameter can be declared as a friend.
</li>
<li>
If you declare a friend function that was not previously declared, that function is exported to the enclosing nonclass scope.

```cpp
class friend F  
friend F;
class ForwardDeclared;// Class name is known.  
class HasFriends  
{  
   friend int ForwardDeclared::IsAFriend();// C2039 error expected  
};  

```


</li>

> 
**friend functions**


<li>
A friend function is a function that is not a member of a class but has access to the class's private and protected members.Friend functions are not considered class members; they are normal external functions that are given special access privileges.
</li>
<li>
Friends are not in the class's scope, and they are not called using the member-selection operators (. and –>) unless they are members of another class.
</li>
<li>
A friend function is declared by the class that is granting access. The friend declaration can be placed anywhere in the class declaration. It is not affected by the access control keywords.

```cpp
#include <iostream>  

using namespace std;  
class Point  
{  
    friend void ChangePrivate( Point & );  
public:  
    Point( void ) : m_i(0) {}  
    void PrintPrivate( void ){cout << m_i << endl; }  

private:  
int m_i;  
};  

void ChangePrivate ( Point &i ) { i.m_i++; }  

int main()  
{  
   Point sPoint;  
   sPoint.PrintPrivate();  
   ChangePrivate(sPoint);  
   sPoint.PrintPrivate();  
    // Output: 0  
           1  
}  

```


</li>

**Class members as friends**

```cpp
class B;  

class A {  
public:  
   int Func1( B& b );  

private:  
   int Func2( B& b );  
};  

class B {  
private:  
int _b;  

   // A::Func1 is a friend function to class B  
   // so A::Func1 has access to all members of B  
   friend int A::Func1( B& );  
};  

int A::Func1( B& b ) { return b._b; }   // OK  
int A::Func2( B& b ) { return b._b; }   // C2248  

```



#### Syntax


- asm (**string-literal**);
- noexcept(**expression**) // meaning 1
- noexcept(**constant-expression**) // meaning 2
- noexcept // meaning 2
- sizeof **unary-expression**
- sizeof(**type-id**)
- sizeof...(**identifier**) // since C++11
- typename **nested-name-specifier** **identifier** // meaning 1
- typename **nested-name-specifier** template(**opt**) **simple-template-id** // meaning 1
- typename **identifier**(**opt**) // meaning 2
- typename... **identifier**(**opt**) // meaning 2; since C++11
- typename **identifier**(**opt**) = **type-id** // meaning 2
- template <**template-parameter-list**> typename ...(**opt**) **identifier**(**opt**) // meaning 3
- template <**template-parameter-list**> typename **identifier**(**opt**) = **id-expression** // meaning 3



#### Remarks


The full list of keywords is as follows:

- [`alignas`](http://stackoverflow.com/documentation/c%2b%2b/9249/alignment/17909/controlling-alignment) (since C++11)
- [`alignof`](http://stackoverflow.com/documentation/c%2b%2b/9249/alignment/17475/querying-the-alignment-of-a-type) (since C++11)
- [`asm`](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18214/asm)
- `auto`: [since C++11](http://stackoverflow.com/documentation/c%2b%2b/7863/type-deduction/25567/auto-type-deduction), [before C++11](http://stackoverflow.com/documentation/c%2b%2b/9225/storage-class-specifiers/28629/auto)
- [`bool`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18416/bool)
- [`break`](http://stackoverflow.com/documentation/c%2b%2b/7841/iteration/18476/break)
- [`case`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18489/case)
- [`catch`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18492/catch)
- [`char`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18494/char)
- [`char16_t`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18501/char16-t) (since C++11)
- [`char32_t`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18502/char32-t) (since C++11)
- [`class`](http://stackoverflow.com/documentation/c%2b%2b/7838/type-keywords/18504/class)
- [`const`](http://stackoverflow.com/documentation/c%2b%2b/7840/variable-declaration-keywords/18509/const)
- [`constexpr`](http://stackoverflow.com/documentation/c%2b%2b/3899/constexpr) (since C++11)
- [`const_cast`](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/11225/casting-away-constness)
- [`continue`](http://stackoverflow.com/documentation/c%2b%2b/7841/iteration/18512/continue)
- [`decltype`](http://stackoverflow.com/documentation/c%2b%2b/7840/variable-declaration-keywords/18513/decltype) (since C++11)
- [`default`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18514/default)
- `delete` [for memory management](http://stackoverflow.com/documentation/c%2b%2b/2873/memory-management), [for functions](http://stackoverflow.com/documentation/c%2b%2b/206/getting-started-with-c-language/25460/function) (since C++11)
- [`do`](http://stackoverflow.com/documentation/c%2b%2b/7841/iteration/18544/do)
- [`double`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18640/double)
- [`dynamic_cast`](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/10518/base-to-derived-conversion)
- [`else`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18548/else)
- [`enum`](http://stackoverflow.com/documentation/c%2b%2b/7838/type-keywords/18566/enum)
- [`explicit`](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18568/explicit)
- [`export`](http://stackoverflow.com/documentation/c%2b%2b/460/templates)
- `extern` [as declaration specifier](http://stackoverflow.com/documentation/c%2b%2b/9225/storage-class-specifiers), [in linkage specification](http://stackoverflow.com/documentation/c%2b%2b/9268/linkage-specifications), [for templates](http://stackoverflow.com/documentation/c%2b%2b/460/templates/28734/explicit-instantiation)
- [`false`](http://stackoverflow.com/documentation/c%2b%2b/7836/literal-keywords/18638/false)
- [`float`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18639/float)
- [`for`](http://stackoverflow.com/documentation/c%2b%2b/7841/iteration/18641/for)
- [`friend`](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/7872/friendship)
- [`goto`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18643/goto)
- [`if`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18547/if)
- `inline` [for functions](http://stackoverflow.com/documentation/c%2b%2b/7150/inline-functions), [for namespaces](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/4556/inline-namespace) (since C++11), [for variables](http://stackoverflow.com/documentation/c%2b%2b/9265/inline-variables) (since C++17)
- [`int`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/17264/int)
- [`long`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18645/long)
- [`mutable`](http://stackoverflow.com/documentation/c%2b%2b/9225/storage-class-specifiers/18647/mutable)
- [`namespace`](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18653/namespace)
- [`new`](http://stackoverflow.com/documentation/c%2b%2b/2873/memory-management)
- [`noexcept`](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18664/noexcept) (since C++11)
- [`nullptr`](http://stackoverflow.com/documentation/c%2b%2b/7836/literal-keywords/18669/nullptr) (since C++11)
- [`operator`](http://stackoverflow.com/documentation/c%2b%2b/562/operator-overloading)
- [`private`](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/1668/access-specifiers)
- [`protected`](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/1668/access-specifiers)
- [`public`](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/1668/access-specifiers)
- [`register`](http://stackoverflow.com/documentation/c%2b%2b/9225/storage-class-specifiers/18681/register)
- [`reinterpret_cast`](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions)
- [`return`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18683/return)
- [`short`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/18646/short)
- [`signed`](http://stackoverflow.com/documentation/c%2b%2b/7840/variable-declaration-keywords/18685/signed)
- [`sizeof`](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18687/sizeof)
- [`static`](http://stackoverflow.com/documentation/c%2b%2b/9225/storage-class-specifiers/18689/static)
- [`static_assert`](http://stackoverflow.com/documentation/c%2b%2b/3822/static-assert) (since C++11)
- [`static_cast`](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions)
- [`struct`](http://stackoverflow.com/documentation/c%2b%2b/7838/type-keywords/18505/struct)
- [`switch`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18490/switch)
- [`template`](http://stackoverflow.com/documentation/c%2b%2b/460/templates)
- [`this`](http://stackoverflow.com/documentation/c%2b%2b/7836/literal-keywords/18758/this)
- [`thread_local`](http://stackoverflow.com/documentation/c%2b%2b/699/threading/18759/thread-local-storage) (since C++11)
- [`throw`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18765/throw)
- [`true`](http://stackoverflow.com/documentation/c%2b%2b/7836/literal-keywords/18637/true)
- [`try`](http://stackoverflow.com/documentation/c%2b%2b/7837/flow-control/18781/try)
- [`typedef`](http://stackoverflow.com/documentation/c%2b%2b/9328/typedef-and-type-aliases)
- [`typeid`](http://stackoverflow.com/documentation/c%2b%2b/3129/rtti-run-time-type-information)
- [`typename`](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/19075/typename)
- [`union`](http://stackoverflow.com/documentation/c%2b%2b/7838/type-keywords/19092/union)
- [`unsigned`](http://stackoverflow.com/documentation/c%2b%2b/7840/variable-declaration-keywords/18686/unsigned)
- `using` [to redeclare a name](http://stackoverflow.com/documentation/c%2b%2b/9301/using-declaration), [to alias a namespace](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/28691/namespace-alias), [to alias a type](http://stackoverflow.com/documentation/c%2b%2b/9328/typedef-and-type-aliases)
- `virtual` [for functions](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/19097/virtual), [for base classes](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/1670/virtual-inheritance)
- [`void`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/19098/void)
- [`volatile`](http://stackoverflow.com/documentation/c%2b%2b/7840/variable-declaration-keywords/19102/volatile)
- [`wchar_t`](http://stackoverflow.com/documentation/c%2b%2b/7839/basic-type-keywords/19113/wchar-t)
- [`while`](http://stackoverflow.com/documentation/c%2b%2b/7841/iteration/25514/while)

The tokens `final` and `override` are not keywords. They may be used as identifiers and have special meaning only in certain contexts.

The tokens `and`, `and_eq`, `bitand`, `bitor`, `compl`, `not`, `not_eq`, `or`, `or_eq`, `xor`, and `xor_eq` are alternative spellings of `&&`, `&=`, `&`, `|`, `~`, `!`, `!=`, `||`, `|=`, `^`, and `^=`, respectively. The standard does not treat them as keywords, but they are keywords for all intents and purposes, since it is impossible to redefine them or use them to mean anything other than the operators they represent.

The following topics contain detailed explanations of many of the keywords in C++, which serve fundamental purposes such as naming basic types or controlling the flow of execution.

<li>
[Basic Type Keywords](http://stackoverflow.com/documentation/c%2B%2B/7839/basic-type-keywords)
</li>
<li>
[Flow Control](http://stackoverflow.com/documentation/c%2B%2B/7837/flow-control)
</li>
<li>
[Iteration](http://stackoverflow.com/documentation/c%2B%2B/7841/iteration)
</li>
<li>
[Literal Keywords](http://stackoverflow.com/documentation/c%2B%2B/7836/literal-keywords)
</li>
<li>
[Type Keywords](http://stackoverflow.com/documentation/c%2B%2B/7838/type-keywords)
</li>
<li>
[Variable Declaration Keywords](http://stackoverflow.com/documentation/c%2B%2B/7840/variable-declaration-keywords)
</li>
<li>
[Classes/Structures](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures)
</li>
<li>
[Storage class specifiers](http://stackoverflow.com/documentation/c%2b%2b/9225/storage-class-specifiers)
</li>

