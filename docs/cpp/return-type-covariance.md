---
metaTitle: "C++ | Return Type Covariance"
description: "2. Covariant result version of the base example, static type checking., 3. Covariant smart pointer result (automated cleanup)., 1. Base example without covariant returns, shows why they're desirable"
---

# Return Type Covariance



## 2. Covariant result version of the base example, static type checking.


```cpp
// 2. Covariant result version of the base example, static type checking.

class Top
{
public:
    virtual Top* clone() const = 0;
    virtual ~Top() = default;       // Necessary for `delete` via Top*.
};

class D : public Top
{
public:
    D* /* ← Covariant return */ clone() const override
    { return new D( *this ); }
};

class DD : public D
{
private:
    int answer_ = 42;

public:
    int answer() const
    { return answer_;}

    DD* /* ← Covariant return */ clone() const override
    { return new DD( *this ); }
};

#include <iostream>
using namespace std;

int main()
{
    DD* p1 = new DD();
    DD* p2 = p1->clone();
    // Correct dynamic type DD for *p2 is guaranteed by the static type checking.

    cout << p2->answer() << endl;          // "42"
    delete p2;
    delete p1;
}

```



## 3. Covariant smart pointer result (automated cleanup).


```cpp
// 3. Covariant smart pointer result (automated cleanup).

#include <memory>
using std::unique_ptr;

template< class Type >
auto up( Type* p ) { return unique_ptr<Type>( p ); }

class Top
{
private:
    virtual Top* virtual_clone() const = 0;

public:
    unique_ptr<Top> clone() const
    { return up( virtual_clone() ); }

    virtual ~Top() = default;       // Necessary for `delete` via Top*.
};

class D : public Top
{
private:
    D* /* ← Covariant return */ virtual_clone() const override
    { return new D( *this ); }

public:
    unique_ptr<D> /* ← Apparent covariant return */ clone() const
    { return up( virtual_clone() ); }
};

class DD : public D
{
private:
    int answer_ = 42;

    DD* /* ← Covariant return */ virtual_clone() const override
    { return new DD( *this ); }

public:
    int answer() const
    { return answer_;}

    unique_ptr<DD> /* ← Apparent covariant return */ clone() const
    { return up( virtual_clone() ); }
};

#include <iostream>
using namespace std;

int main()
{
    auto  p1 = unique_ptr<DD>(new DD());
    auto  p2 = p1->clone();
    // Correct dynamic type DD for *p2 is guaranteed by the static type checking.

    cout << p2->answer() << endl;          // "42"
    // Cleanup is automated via unique_ptr.
 }

```



## 1. Base example without covariant returns, shows why they're desirable


```cpp
// 1. Base example not using language support for covariance, dynamic type checking.

class Top
{
public:
    virtual Top* clone() const = 0;
    virtual ~Top() = default;       // Necessary for `delete` via Top*.
};

class D : public Top
{

public:
    Top* clone() const override
    { return new D( *this ); }
};

class DD : public D
{
private:
    int answer_ = 42;

public:
    int answer() const
    { return answer_;}

    Top* clone() const override
    { return new DD( *this ); }
};

#include <assert.h>
#include <iostream>
#include <typeinfo>
using namespace std;

int main()
{
    cout << boolalpha;

    DD* p1 = new DD();
    Top* p2 = p1->clone();
    bool const  correct_dynamic_type = (typeid( *p2 ) == typeid( DD ));
    cout << correct_dynamic_type << endl;               // "true"

    assert( correct_dynamic_type ); // This is essentially dynamic type checking. :(
    auto p2_most_derived = static_cast<DD*>( p2 );
    cout << p2_most_derived->answer() << endl;          // "42"
    delete p2;
    delete p1;
}

```



#### Remarks


**Covariance** of a parameter or a return value for a virtual member function `m` is where its type `T` gets more specific in a derived class' override of `m`. The type `T` then varies (**variance**) in specificity in the same way (**co**) as the classes providing `m`. C++ provides language support for covariant **return types** that are raw pointers or raw references – the covariance is for the pointee or referent type.

The C++ support is limited to return types because function return values are the only pure **out-arguments** in C++, and covariance is only type safe for a pure out-argument. Otherwise calling code could supply an object of less specific type than the receiving code expects. MIT professor Barbara Liskov investigated this and related variance type safety issues, and it's now known as the Liskov Substitution Principle, or [**LSP**](https://en.wikipedia.org/wiki/Liskov_substitution_principle).

The covariance support essentially helps to avoid downcasting and dynamic type checking.

Since smart pointers are of class type one cannot use the built-in support for covariance directly for smart pointer results, but one can define **apparently covariant** non-`virtual` smart pointer result wrapper functions for a covariant `virtual` function that produces raw pointers.

