---
metaTitle: "C++ | The This Pointer"
description: "this Pointer, Using the this Pointer to Access Member Data, Using the this Pointer to Differentiate Between Member Data and Parameters, this Pointer CV-Qualifiers, this Pointer Ref-Qualifiers"
---

# The This Pointer



## this Pointer


All non-static member functions have a hidden parameter, a pointer to an instance of the class, named `this`; this parameter is silently inserted at the beginning of the parameter list, and handled entirely by the compiler.  When a member of the class is accessed inside a member function, it is silently accessed through `this`; this allows the compiler to use a single non-static member function for all instances, and allows a member function to call other member functions polymorphically.

```cpp
struct ThisPointer {
    int i;

    ThisPointer(int ii);

    virtual void func();

    int  get_i() const;
    void set_i(int ii);
};
ThisPointer::ThisPointer(int ii) : i(ii) {}
// Compiler rewrites as:
ThisPointer::ThisPointer(int ii) : this->i(ii) {}
// Constructor is responsible for turning allocated memory into 'this'.
// As the constructor is responsible for creating the object, 'this' will not be "fully"
// valid until the instance is fully constructed.

/* virtual */ void ThisPointer::func() {
    if (some_external_condition) {
        set_i(182);
    } else {
        i = 218;
    }
}
// Compiler rewrites as:
/* virtual */ void ThisPointer::func(ThisPointer* this) {
    if (some_external_condition) {
        this->set_i(182);
    } else {
        this->i = 218;
    }
}

int  ThisPointer::get_i() const { return i; }
// Compiler rewrites as:
int  ThisPointer::get_i(const ThisPointer* this) { return this->i; }

void ThisPointer::set_i(int ii) { i = ii; }
// Compiler rewrites as:
void ThisPointer::set_i(ThisPointer* this, int ii) { this->i = ii; }

```

In a constructor, `this` can safely be used to (implicitly or explicitly) access any field that has already been initialised, or any field in a parent class; conversely, (implicitly or explicitly) accessing any fields that haven't yet been initialised, or any fields in a derived class, is unsafe (due to the derived class not yet being constructed, and thus its fields neither being initialised nor existing).  It is also unsafe to call virtual member functions through `this` in the constructor, as any derived class functions will not be considered (due to the derived class not yet being constructed, and thus its constructor not yet updating the vtable).

Also note that while in a constructor, the type of the object is the type which that constructor constructs.  This holds true even if the object is declared as a derived type.  For example, in the below example, `ctd_good` and `ctd_bad` are type `CtorThisBase` inside `CtorThisBase()`, and type `CtorThis` inside `CtorThis()`, even though their canonical type is `CtorThisDerived`.  As the more-derived classes are constructed around the base class, the instance gradually goes through the class hierarchy until it is a fully-constructed instance of its intended type.

```cpp
class CtorThisBase {
    short s;

  public:
    CtorThisBase() : s(516) {}
};

class CtorThis : public CtorThisBase {
    int i, j, k;

  public:
    // Good constructor.
    CtorThis() : i(s + 42), j(this->i), k(j) {}

    // Bad constructor.
    CtorThis(int ii) : i(ii), j(this->k), k(b ? 51 : -51) {
        virt_func();
    }

    virtual void virt_func() { i += 2; }
};

class CtorThisDerived : public CtorThis {
    bool b;

  public:
    CtorThisDerived()       : b(true) {}
    CtorThisDerived(int ii) : CtorThis(ii), b(false) {}

    void virt_func() override { k += (2 * i); }
};

// ...

CtorThisDerived ctd_good;
CtorThisDerived ctd_bad(3);

```

With these classes and member functions:

<li>In the good constructor, for `ctd_good`:
<ul>
- `CtorThisBase` is fully constructed by the time the `CtorThis` constructor is entered.  Therefore, `s` is in a valid state while initialising `i`, and can thus be accessed.
- `i` is initialised before `j(this->i)` is reached.  Therefore, `i` is in a valid state while initialising `j`, and can thus be accessed.
- `j` is initialised before `k(j)` is reached.  Therefore, `j` is in a valid state while initialising `k`, and can thus be accessed.

- `k` is initialised after `j(this->k)` is reached.  Therefore, `k` is in an invalid state while initialising `j`, and accessing it causes undefined behaviour.
- `CtorThisDerived` is not constructed until after `CtorThis` is constructed.  Therefore, `b` is in an invalid state while initialising `k`, and accessing it causes undefined behaviour.
- The object `ctd_bad` is still a `CtorThis` until it leaves `CtorThis()`, and will not be updated to use `CtorThisDerived`'s vtable until `CtorThisDerived()`.  Therefore, `virt_func()` will call `CtorThis::virt_func()`, regardless of whether it is intended to call that or `CtorThisDerived::virt_func()`.



## Using the this Pointer to Access Member Data


In this context, using the `this` pointer isn't entirely necessary, but it will make your code clearer to the reader, by indicating that a given function or variable is a member of the class. An example in this situation:

```cpp
// Example for this pointer
#include <iostream>
#include <string>

using std::cout;
using std::endl;

class Class
{
  public:
    Class();
    ~Class();
    int getPrivateNumber () const;
  private:
    int private_number = 42;
};

Class::Class(){}
Class::~Class(){}

int Class::getPrivateNumber() const
{
    return this->private_number;
}

int main()
{
    Class class_example;
    cout << class_example.getPrivateNumber() << endl;
}

```

See it in action [here](http://cpp.sh/9flka).



## Using the this Pointer to Differentiate Between Member Data and Parameters


This is an actual useful strategy to differentiate member data from parameters... Lets take this example :

```cpp
// Dog Class Example
#include <iostream>
#include <string>

using std::cout;
using std::endl;

/*
* @class Dog
*   @member name
*       Dog's name
*   @function bark
*       Dog Barks!
*   @function getName
*       To Get Private
*       Name Variable
*/
class Dog
{
 public:
    Dog(std::string name);
    ~Dog();
    void  bark() const;
    std::string  getName() const;
 private:
    std::string name;
};

Dog::Dog(std::string name)
{
    /*
    *  this->name is the
    *  name variable from 
    *  the class dog . and
    *  name is from the 
    *  parameter of the function
    */
    this->name = name; 
}

Dog::~Dog(){}

void Dog::bark() const
{
  cout << "BARK" << endl;   
}

std::string  Dog::getName() const
{
    return this->name;
}


int main()
{
    Dog dog("Max");
    cout << dog.getName() << endl;
    dog.bark();
}

```

You can see here in the constructor we execute the following:

```cpp
this->name = name; 

```

Here , you can see we are assinging the parameter name to the name of the private variable from the class Dog(this->name) .

To see the output of above code : [http://cpp.sh/75r7](http://cpp.sh/75r7)



## this Pointer CV-Qualifiers


`this` can also be cv-qualified, the same as any other pointer.  However, due to the `this` parameter not being listed in the parameter list, special syntax is required for this; the cv-qualifiers are listed after the parameter list, but before the function's body.

```cpp
struct ThisCVQ {
    void no_qualifier()                {} // "this" is: ThisCVQ*
    void  c_qualifier() const          {} // "this" is: const ThisCVQ*
    void  v_qualifier() volatile       {} // "this" is: volatile ThisCVQ*
    void cv_qualifier() const volatile {} // "this" is: const volatile ThisCVQ*
};

```

As `this` is a parameter, a [function can be overloaded based on its `this` cv-qualifier(s)](http://stackoverflow.com/documentation/c%2B%2B/510/function-overloading/11242/member-function-cv-qualifier-overloading#t=201610191448370301667).

```cpp
struct CVOverload {
    int func()                { return    3; }
    int func() const          { return   33; }
    int func() volatile       { return  333; }
    int func() const volatile { return 3333; }
};

```

When `this` is `const` (including `const volatile`), the function is unable to write to member variables through it, whether implicitly or explicitly.  The sole exception to this is [`mutable` member variables](http://stackoverflow.com/documentation/c%2B%2B/2705/mutable-keyword/9058/non-static-class-member-modifier#t=201610111819273885699), which can be written regardless of const-ness.  Due to this, `const` is used to indicate that the member function doesn't change the object's logical state (the way the object appears to the outside world), even if it does modify the physical state (the way the object looks under the hood).

> 
Logical state is the way the object appears to outside observers.  It isn't directly tied to physical state, and indeed, might not even be stored as physical state.  As long as outside observers can't see any changes, the logical state is constant, even if you flip every single bit in the object.


> 
Physical state, also known as bitwise state, is how the object is stored in memory.  This is the object's nitty-gritty, the raw 1s and 0s that make up its data.  An object is only physically constant if its representation in memory never changes.


Note that C++ bases `const`ness on logical state, not physical state.

```cpp
class DoSomethingComplexAndOrExpensive {
    mutable ResultType cached_result;
    mutable bool state_changed;

    ResultType calculate_result();
    void modify_somehow(const Param& p);

    // ...

  public:
    DoSomethingComplexAndOrExpensive(Param p) : state_changed(true) {
        modify_somehow(p);
    }

    void change_state(Param p) {
        modify_somehow(p);
        state_changed = true;
    }

    // Return some complex and/or expensive-to-calculate result.
    // As this has no reason to modify logical state, it is marked as "const".
    ResultType get_result() const;
};
ResultType DoSomethingComplexAndOrExpensive::get_result() const {
    // cached_result and state_changed can be modified, even with a const "this" pointer.
    // Even though the function doesn't modify logical state, it does modify physical state
    //  by caching the result, so it doesn't need to be recalculated every time the function
    //  is called.  This is indicated by cached_result and state_changed being mutable.

    if (state_changed) {
        cached_result = calculate_result();
        state_changed = false;
    }

    return cached_result;
}

```

Note that while you technically **could** use `const_cast` on `this` to make it non-cv-qualified, you really, ****REALLY**** shouldn't, and should use `mutable` instead.  A `const_cast` is liable to invoke undefined behaviour when used on an object that actually **is** `const`, while `mutable` is designed to be safe to use.  It is, however, possible that you may run into this in extremely old code.

[An exception to this rule is defining non-cv-qualified accessors in terms of `const` accessors; as the object is guaranteed to not be `const` if the non-cv-qualified version is called, there's no risk of UB.](http://stackoverflow.com/documentation/c%2B%2B/2386/const-keyword/16974/avoiding-duplication-of-code-in-const-and-non-const-getter-methods)

```cpp
class CVAccessor {
    int arr[5];

  public:
    const int& get_arr_element(size_t i) const { return arr[i]; }

    int& get_arr_element(size_t i) {
        return const_cast<int&>(const_cast<const CVAccessor*>(this)->get_arr_element(i));
    }
};

```

This prevents unnecessary duplication of code.

As with regular pointers, if `this` is `volatile` (including `const volatile`), it is loaded from memory each time it is accessed, instead of being cached.  This has the same effects on optimisation as declaring any other pointer `volatile` would, so care should be taken.

Note that if an instance is cv-qualified, the only member functions it is allowed to access are member functions whose `this` pointer is at least as cv-qualified as the instance itself:

- Non-cv instances can access any member functions.
- `const` instances can access `const` and `const volatile` functions.
- `volatile` instances can access `volatile` and `const volatile` functions.
- `const volatile` instances can access `const volatile` functions.

This is one of the key tenets of [`const` correctness](http://stackoverflow.com/documentation/c%2B%2B/7217/const-correctness#t=201610111815341169566).

```cpp
struct CVAccess {
    void    func()                {}
    void  func_c() const          {}
    void  func_v() volatile       {}
    void func_cv() const volatile {}
};

CVAccess cva;
cva.func();    // Good.
cva.func_c();  // Good.
cva.func_v();  // Good.
cva.func_cv(); // Good.

const CVAccess c_cva;
c_cva.func();    // Error.
c_cva.func_c();  // Good.
c_cva.func_v();  // Error.
c_cva.func_cv(); // Good.

volatile CVAccess v_cva;
v_cva.func();    // Error.
v_cva.func_c();  // Error.
v_cva.func_v();  // Good.
v_cva.func_cv(); // Good.

const volatile CVAccess cv_cva;
cv_cva.func();    // Error.
cv_cva.func_c();  // Error.
cv_cva.func_v();  // Error.
cv_cva.func_cv(); // Good.

```



## this Pointer Ref-Qualifiers


Similarly to `this` cv-qualifiers, we can also apply **ref-qualifiers** to `*this`.  Ref-qualifiers are used to choose between normal and rvalue reference semantics, allowing the compiler to use either copy or move semantics depending on which are more appropriate, and are applied to `*this` instead of `this`.

Note that despite ref-qualifiers using reference syntax, `this` itself is still a pointer.  Also note that ref-qualifiers don't actually change the type of `*this`; it's just easier to describe and understand their effects by looking at them as if they did.

```cpp
struct RefQualifiers {
    std::string s;

    RefQualifiers(const std::string& ss = "The nameless one.") : s(ss) {}

    // Normal version.
    void func() &  { std::cout << "Accessed on normal instance "    << s << std::endl; }
    // Rvalue version.
    void func() && { std::cout << "Accessed on temporary instance " << s << std::endl; }

    const std::string& still_a_pointer() &  { return this->s; }
    const std::string& still_a_pointer() && { this->s = "Bob"; return this->s; }
};

// ...

RefQualifiers rf("Fred");
rf.func();              // Output:  Accessed on normal instance Fred
RefQualifiers{}.func(); // Output:  Accessed on temporary instance The nameless one

```

A member function cannot have overloads both with and without ref-qualifiers; the programmer has to choose between one or the other.  Thankfully, cv-qualifiers can be used in conjunction with ref-qualifiers, allowing [`const` correctness](http://stackoverflow.com/documentation/c%2B%2B/7217/const-correctness#t=201610111809376589726) rules to be followed.

```cpp
struct RefCV {
    void func() &                {}
    void func() &&               {}
    void func() const&           {}
    void func() const&&          {}
    void func() volatile&        {}
    void func() volatile&&       {}
    void func() const volatile&  {}
    void func() const volatile&& {}
};

```



#### Remarks


The `this` pointer is a keyword for C++ therfore there is no library needed to implement this. And do not forget `this` is a pointer! So you cannot do:

```

this.someMember();

```

As you access member functions or member variables from pointers using the arrow symbol    `->` :

```cpp
this->someMember();

```

Other helpful links to the better understanding of the `this` pointer :

[What is the &#39;this&#39; pointer?](http://stackoverflow.com/questions/16492736/what-is-the-this-pointer)

[http://www.geeksforgeeks.org/this-pointer-in-c/](http://www.geeksforgeeks.org/this-pointer-in-c/)

[https://www.tutorialspoint.com/cplusplus/cpp_this_pointer.htm](https://www.tutorialspoint.com/cplusplus/cpp_this_pointer.htm)

