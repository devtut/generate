---
metaTitle: "Classes/Structures"
description: "Class basics, Final classes and structs, Access specifiers, Inheritance, Friendship, Virtual Inheritance, Accessing class members, Private inheritance: restricting base class interface, Nested Classes/Structures, Member Types and Aliases, Multiple Inheritance, Static class members, Non-static member functions, Unnamed struct/class"
---

# Classes/Structures




## Class basics


A **class** is a user-defined type. A class is introduced with the `class`, `struct` or `union` keyword. In colloquial usage, the term "class" usually refers only to non-union classes.

A class is a collection of **class members**, which can be:

- member variables (also called "fields"),
- member functions (also called "methods"),
- member types or typedefs (e.g. "nested classes"),
- member templates (of any kind: variable, function, class or alias template)

The `class` and `struct` keywords, called **class keys**, are largely interchangeable, except that the default access specifier for members and bases is "private" for a class declared with the `class` key and "public" for a class declared with the `struct` or `union` key (cf. [Access modifiers](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/1668/access-modifiers)).

For example, the following code snippets are identical:

```cpp
struct Vector
{
    int x;
    int y;
    int z;
};
// are equivalent to
class Vector
{
public:
    int x;
    int y;
    int z;
};

```

By declaring a class` a new type is added to your program, and it is possible to instantiate objects of that class by

```cpp
Vector my_vector;

```

Members of a class are accessed using dot-syntax.

```cpp
my_vector.x = 10;
my_vector.y = my_vector.x + 1; // my_vector.y = 11;
my_vector.z = my_vector.y - 4; // my:vector.z = 7;

```



## Final classes and structs


Deriving a class may be forbidden with `final` specifier. Let's declare a final class:

```cpp
class A final {
};

```

Now any attempt to subclass it will cause a compilation error:

```cpp
// Compilation error: cannot derive from final class:
class B : public A {
};

```

Final class may appear anywhere in class hierarchy:

```cpp
class A {
};

// OK.
class B final : public A {
};

// Compilation error: cannot derive from final class B.
class C : public B {
};

```



## Access specifiers


There are three [keywords](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) that act as **access specifiers**. These limit the access to class members  following the specifier, until another specifier changes the access level again:

|Keyword|Description
|------
|`public`|Everyone has access
|`protected`|Only the class itself, derived classes and friends have access
|`private`|Only the class itself and friends have access

When the type is defined using the `class` keyword, the default access specifier is `private`, but if the type is defined using the `struct` keyword, the default access specifier is `public`:

```cpp
struct MyStruct { int x; };
class MyClass { int x; };

MyStruct s;
s.x = 9; // well formed, because x is public

MyClass c;
c.x = 9; // ill-formed, because x is private

```

Access specifiers are mostly used to limit access to internal fields and methods, and force the programmer to use a specific interface, for example to force use of getters and setters instead of referencing a variable directly:

```cpp
class MyClass {

public: /* Methods: */

    int x() const noexcept { return m_x; }
    void setX(int const x) noexcept { m_x = x; }

private: /* Fields: */

    int m_x;

};

```

Using `protected` is useful for allowing certain functionality of the type to be only accessible to the derived classes, for example, in the following code, the method `calculateValue()` is only accessible to classes deriving from the base class `Plus2Base`, such as `FortyTwo`:

```cpp
struct Plus2Base {
    int value() noexcept { return calculateValue() + 2; }
protected: /* Methods: */
    virtual int calculateValue() noexcept = 0;
};
struct FortyTwo: Plus2Base {
protected: /* Methods: */
    int calculateValue() noexcept final override { return 40; }
};

```

Note that the `friend` keyword can be used to add access exceptions to functions or types for accessing protected and private members.

The `public`, `protected`, and `private` keywords can also be used to grant or limit access to base class subobjects. See the [Inheritance](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/1669/inheritance) example.



## Inheritance


Classes/structs can have inheritance relations.

If a class/struct `B` inherits from a class/struct `A`, this means that `B` has as a parent `A`. We say that `B` is a derived class/struct from `A`, and `A` is the base class/struct.

```cpp
struct A
{
public:
    int p1;
protected:
    int p2;
private:
    int p3;
};

//Make B inherit publicly (default) from A
struct B : A
{
};

```

There are 3 forms of inheritance for a class/struct:

- `public`
- `private`
- `protected`

Note that the default inheritance is the same as the default visibility of members: `public` if you use the `struct` keyword, and `private` for the `class` keyword.

It's even possible to have a `class` derive from a `struct` (or vice versa). In this case, the default inheritance is controlled by the child, so a `struct` that derives from a `class` will default to public inheritance, and a `class` that derives from a `struct` will have private inheritance by default.

`public` inheritance:

```cpp
struct B : public A // or just `struct B : A`
{
    void foo()
    {
        p1 = 0; //well formed, p1 is public in B
        p2 = 0; //well formed, p2 is protected in B
        p3 = 0; //ill formed, p3 is private in A
    }
};

B b;
b.p1 = 1; //well formed, p1 is public
b.p2 = 1; //ill formed, p2 is protected
b.p3 = 1; //ill formed, p3 is inaccessible

```

`private` inheritance:

```cpp
struct B : private A
{
    void foo()
    {
        p1 = 0; //well formed, p1 is private in B
        p2 = 0; //well formed, p2 is private in B
        p3 = 0; //ill formed, p3 is private in A
    }
};

B b;
b.p1 = 1; //ill formed, p1 is private
b.p2 = 1; //ill formed, p2 is private
b.p3 = 1; //ill formed, p3 is inaccessible

```

`protected` inheritance:

```cpp
struct B : protected A
{
    void foo()
    {
        p1 = 0; //well formed, p1 is protected in B
        p2 = 0; //well formed, p2 is protected in B
        p3 = 0; //ill formed, p3 is private in A
    }
};

B b;
b.p1 = 1; //ill formed, p1 is protected
b.p2 = 1; //ill formed, p2 is protected
b.p3 = 1; //ill formed, p3 is inaccessible

```

Note that although `protected` inheritance is allowed, the actual use of it is rare. One instance of how `protected` inheritance is used in application is in partial base class specialization (usually referred to as "controlled polymorphism").

When OOP was relatively new, (public) inheritance was frequently said to model an "IS-A" relationship. That is, public inheritance is correct only if an instance of the derived class **is also an** instance of the base class.

This was later refined into the [Liskov Substitution Principle](https://en.wikipedia.org/wiki/Liskov_substitution_principle): public inheritance should only be used when/if an instance of the derived class can be substituted for an instance of the base class under any possible circumstance (and still make sense).

Private inheritance is typically said to embody a completely different relationship: "is implemented in terms of" (sometimes called a "HAS-A" relationship). For example, a `Stack` class could inherit privately from a `Vector` class. Private inheritance bears a much greater similarity to aggregation than to public inheritance.

Protected inheritance is almost never used, and there's no general agreement on what sort of relationship it embodies.



## Friendship


The `friend` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keyword) is used to give other classes and functions access to private and protected members of the class, even through they are defined outside the class`s scope.

```cpp
class Animal{
private:
    double weight;
    double height;
public:
    friend void printWeight(Animal animal);
    friend class AnimalPrinter;
    // A common use for a friend function is to overload the operator<< for streaming. 
    friend std::ostream& operator<<(std::ostream& os, Animal animal);
};

void printWeight(Animal animal)
{
    std::cout << animal.weight << "\n";
}

class AnimalPrinter
{
public:
    void print(const Animal& animal)
    {
        // Because of the `friend class AnimalPrinter;" declaration, we are
        // allowed to access private members here.
        std::cout << animal.weight << ", " << animal.height << std::endl;
    }
}

std::ostream& operator<<(std::ostream& os, Animal animal)
{
    os << "Animal height: " << animal.height << "\n";
    return os;
}

int main() {
    Animal animal = {10, 5};
    printWeight(animal);

    AnimalPrinter aPrinter;
    aPrinter.print(animal);

    std::cout << animal;
}

```

```cpp
10
10, 5
Animal height: 5

```



## Virtual Inheritance


When using inheritance, you can specify the `virtual` keyword:

```cpp
struct A{};
struct B: public virtual A{};

```

When class `B` has virtual base `A` it means that `A` **will reside in most derived class** of inheritance tree, and thus that most derived class is also responsible for initializing that virtual base:

```cpp
struct A
{
    int member;
    A(int param)
    {
        member = param;
    }
};

struct B: virtual A
{
    B(): A(5){}
};

struct C: B
{
    C(): /*A(88)*/ {}
};

void f()
{
    C object; //error since C is not initializing it's indirect virtual base `A`
}

```

If we un-comment `/*A(88)*/` we won't get any error since `C` is now initializing it's indirect virtual base `A`.

Also note that when we're creating variable `object`, most derived class is `C`, so `C` is responsible for creating(calling constructor of) `A` and thus value of `A::member` is `88`, not `5` (as it would be if we were creating object of type `B`).

It is useful when solving the [diamond problem](https://en.wikipedia.org/wiki/Multiple_inheritance#The_diamond_problem).:

```

 A                                        A   A
 / \                                       |   |
B   C                                      B   C
 \ /                                        \ /
  D                                          D
virtual inheritance                   normal inheritance

```

`B` and `C` both inherit from `A`, and `D` inherits from `B` and `C`, so **there are 2 instances of `A` in `D`!** This results in ambiguity when you're accessing member of `A` through `D`, as the compiler has no way of knowing from which class do you want to access that member (the one which `B` inherits, or the one that is inherited by`C`?).

Virtual inheritance solves this problem: Since virtual base resides only in most derived object, there will be only one instance of `A` in `D`.

```cpp
struct A
{
    void foo() {}
};

struct B : public /*virtual*/ A {};
struct C : public /*virtual*/ A {};

struct D : public B, public C
{
    void bar()
    {
        foo(); //Error, which foo? B::foo() or C::foo()? - Ambiguous
    }
};

```

Removing the comments resolves the ambiguity.



## Accessing class members


To access member variables and member functions of an object of a class, the `.` operator is used:

```cpp
struct SomeStruct {
  int a;
  int b;
  void foo() {}
};

SomeStruct var;
// Accessing member variable a in var.
std::cout << var.a << std::endl;
// Assigning member variable b in var.
var.b = 1;
// Calling a member function.
var.foo();

```

When accessing the members of a class via a pointer, the `->` operator is commonly used.  Alternatively, the instance can be dereferenced and the `.` operator used, although this is less common:

```cpp
struct SomeStruct {
  int a;
  int b;
  void foo() {}
};

SomeStruct var;
SomeStruct *p = &var;
// Accessing member variable a in var via pointer.
std::cout << p->a << std::endl;
std::cout << (*p).a << std::endl;
// Assigning member variable b in var via pointer.
p->b = 1;
(*p).b = 1;
// Calling a member function via a pointer.
p->foo();
(*p).foo();

```

When accessing static class members, the `::` operator is used, but on the name of the class instead of an instance of it.  Alternatively, the static member can be accessed from an instance or a pointer to an instance using the `.` or `->` operator, respectively, with the same syntax as accessing non-static members.

```cpp
struct SomeStruct {
  int a;
  int b;
  void foo() {}

  static int c;
  static void bar() {}
};
int SomeStruct::c;

SomeStruct var;
SomeStruct* p = &var;
// Assigning static member variable c in struct SomeStruct.
SomeStruct::c = 5;
// Accessing static member variable c in struct SomeStruct, through var and p.
var.a = var.c;
var.b = p->c;
// Calling a static member function.
SomeStruct::bar();
var.bar();
p->bar();

```

### Background

The `->` operator is needed because the member access operator `.` has precedence over the dereferencing operator `*`.

One would expect that `*p.a` would dereference `p` (resulting in a reference to the object `p` is pointing to) and then accessing its member `a`. But in fact, it tries to access the member `a` of `p` and then dereference it. I.e. `*p.a` is equivalent to `*(p.a)`. In the example above, this would result in a compiler error because of two facts: First, `p` is a pointer and does not have a member `a`. Second, `a` is an integer and, thus, can't be dereferenced.

The uncommonly used solution to this problem would be to explicitly control the precedence: `(*p).a`

Instead, the `->` operator is almost always used. It is a short-hand for first dereferencing the pointer and then accessing it. I.e. `(*p).a` is exactly the same as `p->a`.

The `::` operator is the scope operator, used in the same manner as accessing a member of a namespace.  This is because a static class member is considered to be in that class' scope, but isn't considered a member of instances of that class.  The use of normal `.` and `->` is also allowed for static members, despite them not being instance members, for historical reasons; this is of use for writing generic code in templates, as the caller doesn't need to be concerned with whether a given member function is static or non-static.



## Private inheritance: restricting base class interface


Private inheritance is useful when it is required to restrict the public interface of the class:

```cpp
class A {
public:
    int move();
    int turn();
};

class B : private A {
public:
    using A::turn; 
};

B b;
b.move();  // compile error
b.turn();  // OK

```

This approach efficiently prevents an access to the A public methods by casting to the A pointer or reference:

```cpp
B b; 
A& a = static_cast<A&>(b); // compile error

```

In the case of public inheritance such casting will provide access to all the A public methods despite on alternative ways to prevent this in derived B, like hiding:

```cpp
class B : public A {
private:
    int move();  
};

```

or private using:

```cpp
class B : public A {
private:
    using A::move;  
};

```

then for both cases it is possible:

```cpp
B b;
A& a = static_cast<A&>(b); // OK for public inheritance
a.move(); // OK

```



## Nested Classes/Structures


A `class` or `struct` can also contain another `class`/`struct` definition inside itself, which is called a "nested class"; in this situation, the containing class is referred to as the "enclosing class".  The nested class definition is considered to be a member of the enclosing class, but is otherwise separate.

```cpp
struct Outer {
    struct Inner { };
};

```

From outside of the enclosing class, nested classes are accessed using the scope operator.  From inside the enclosing class, however, nested classes can be used without qualifiers:

```cpp
struct Outer {
    struct Inner { };

    Inner in;
};

// ...

Outer o;
Outer::Inner i = o.in;

```

As with a non-nested `class`/`struct`, member functions and static variables can be defined either within a nested class, or in the enclosing namespace.  However, they cannot be defined within the enclosing class, due to it being considered to be a different class than the nested class.

```cpp
// Bad.
struct Outer {
    struct Inner {
        void do_something();
    };

    void Inner::do_something() {}
};


// Good.
struct Outer {
    struct Inner {
        void do_something();
    };

};

void Outer::Inner::do_something() {}

```

As with non-nested classes, nested classes can be forward declared and defined later, provided they are defined before being used directly.

```cpp
class Outer {
    class Inner1;
    class Inner2;

    class Inner1 {};

    Inner1 in1;
    Inner2* in2p;

  public:
    Outer();
    ~Outer();
};

class Outer::Inner2 {};

Outer::Outer() : in1(Inner1()), in2p(new Inner2) {}
Outer::~Outer() {
    if (in2p) { delete in2p; }
}

```

Prior to C++11, nested classes only had access to type names, `static` members, and enumerators from the enclosing class; all other members defined in the enclosing class were off-limits.

As of C++11, nested classes, and members thereof, are treated as if they were `friend`s of the enclosing class, and can access all of its members, according to the usual access rules; if members of the nested class require the ability to evaluate one or more non-static members of the enclosing class, they must therefore be passed an instance:

```cpp
class Outer {
    struct Inner {
        int get_sizeof_x() {
            return sizeof(x); // Legal (C++11): x is unevaluated, so no instance is required.
        }

        int get_x() {
            return x; // Illegal: Can't access non-static member without an instance.
        }

        int get_x(Outer& o) {
            return o.x; // Legal (C++11): As a member of Outer, Inner can access private members.
        }
    };

    int x;
};

```

Conversely, the enclosing class is **not** treated as a friend of the nested class, and thus cannot access its private members without explicitly being granted permission.

```cpp
class Outer {
    class Inner {
        // friend class Outer;

        int x;
    };

    Inner in;

  public:
    int get_x() {
        return in.x; // Error: int Outer::Inner::x is private.
        // Uncomment "friend" line above to fix.
    }
};

```

Friends of a nested class are not automatically considered friends of the enclosing class; if they need to be friends of the enclosing class as well, this must be declared separately.  Conversely, as the enclosing class is not automatically considered a friend of the nested class, neither will friends of the enclosing class be considered friends of the nested class.

```cpp
class Outer {
    friend void barge_out(Outer& out, Inner& in);

    class Inner {
        friend void barge_in(Outer& out, Inner& in);

        int i;
    };

    int o;
};

void barge_in(Outer& out, Outer::Inner& in) {
    int i = in.i;  // Good.
    int o = out.o; // Error: int Outer::o is private.
}

void barge_out(Outer& out, Outer::Inner& in) {
    int i = in.i;  // Error: int Outer::Inner::i is private.
    int o = out.o; // Good.
}

```

As with all other class members, nested classes can only be named from outside the class if they have public access.  However, you are allowed to access them regardless of access modifier, as long as you don't explicitly name them.

```cpp
class Outer {
    struct Inner {
        void func() { std::cout << "I have no private taboo.\n"; }
    };

  public:
    static Inner make_Inner() { return Inner(); }
};

// ...

Outer::Inner oi; // Error: Outer::Inner is private.

auto oi = Outer::make_Inner(); // Good.
oi.func();                     // Good.
Outer::make_Inner().func();    // Good.

```

You can also create a type alias for a nested class.  If a type alias is contained in the enclosing class, the nested type and the type alias can have different access modifiers.  If the type alias is outside the enclosing class, it requires that either the nested class, or a `typedef` thereof, be public.

```cpp
class Outer {
    class Inner_ {};

  public:
    typedef Inner_ Inner;
};

typedef Outer::Inner  ImOut; // Good.
typedef Outer::Inner_ ImBad; // Error.

// ...

Outer::Inner  oi; // Good.
Outer::Inner_ oi; // Error.
ImOut         oi; // Good.

```

As with other classes, nested classes can both derive from or be derived from by other classes.

```cpp
struct Base {};

struct Outer {
    struct Inner : Base {};
};

struct Derived : Outer::Inner {};

```

This can be useful in situations where the enclosing class is derived from by another class, by allowing the programmer to update the nested class as necessary.  This can be combined with a typedef to provide a consistent name for each enclosing class' nested class:

```cpp
class BaseOuter {
    struct BaseInner_ {
        virtual void do_something() {}
        virtual void do_something_else();
    } b_in;

  public:
    typedef BaseInner_ Inner;

    virtual ~BaseOuter() = default;

    virtual Inner& getInner() { return b_in; }
};

void BaseOuter::BaseInner_::do_something_else() {}

// ---

class DerivedOuter : public BaseOuter {
    // Note the use of the qualified typedef; BaseOuter::BaseInner_ is private.
    struct DerivedInner_ : BaseOuter::Inner {
        void do_something() override {}
        void do_something_else() override;
    } d_in;

  public:
    typedef DerivedInner_ Inner;

    BaseOuter::Inner& getInner() override { return d_in; }
};

void DerivedOuter::DerivedInner_::do_something_else() {}

// ...

// Calls BaseOuter::BaseInner_::do_something();
BaseOuter* b = new BaseOuter;
BaseOuter::Inner& bin = b->getInner();
bin.do_something();
b->getInner().do_something();

// Calls DerivedOuter::DerivedInner_::do_something();
BaseOuter* d = new DerivedOuter;
BaseOuter::Inner& din = d->getInner();
din.do_something();
d->getInner().do_something();

```

In the above case, both `BaseOuter` and `DerivedOuter` supply the member type `Inner`, as `BaseInner_` and `DerivedInner_`, respectively.  This allows nested types to be derived without breaking the enclosing class' interface, and allows the nested type to be used polymorphically.



## Member Types and Aliases


A `class` or `struct` can also define member type aliases, which are type aliases contained within, and treated as members of, the class itself.

```cpp
struct IHaveATypedef {
    typedef int MyTypedef;
};

struct IHaveATemplateTypedef {
    template<typename T>
    using MyTemplateTypedef = std::vector<T>;
};

```

Like static members, these typedefs are accessed using the scope operator, `::`.

```cpp
IHaveATypedef::MyTypedef i = 5; // i is an int.

IHaveATemplateTypedef::MyTemplateTypedef<int> v; // v is a std::vector<int>.

```

As with normal type aliases, each member type alias is allowed to refer to any type defined or aliased before, but not after, its definition.  Likewise, a typedef outside the class definition can refer to any accessible typedefs within the class definition, provided it comes after the class definition.

```cpp
template<typename T>
struct Helper {
    T get() const { return static_cast<T>(42); }
};

struct IHaveTypedefs {
//    typedef MyTypedef NonLinearTypedef; // Error if uncommented.
    typedef int MyTypedef;
    typedef Helper<MyTypedef> MyTypedefHelper;
};

IHaveTypedefs::MyTypedef        i; // x_i is an int.
IHaveTypedefs::MyTypedefHelper hi; // x_hi is a Helper<int>.

typedef IHaveTypedefs::MyTypedef TypedefBeFree;
TypedefBeFree ii;                  // ii is an int.

```

Member type aliases can be declared with any access level, and will respect the appropriate access modifier.

```cpp
class TypedefAccessLevels {
    typedef int PrvInt;

  protected:
    typedef int ProInt;

  public:
    typedef int PubInt;
};

TypedefAccessLevels::PrvInt prv_i; // Error: TypedefAccessLevels::PrvInt is private.
TypedefAccessLevels::ProInt pro_i; // Error: TypedefAccessLevels::ProInt is protected.
TypedefAccessLevels::PubInt pub_i; // Good.

class Derived : public TypedefAccessLevels {
    PrvInt prv_i; // Error: TypedefAccessLevels::PrvInt is private.
    ProInt pro_i; // Good.
    PubInt pub_i; // Good.
};

```

This can be used to provide a level of abstraction, allowing a class' designer to change its internal workings without breaking code that relies on it.

```cpp
class Something {
    friend class SomeComplexType;

    short s;
    // ...

  public:
    typedef SomeComplexType MyHelper;

    MyHelper get_helper() const { return MyHelper(8, s, 19.5, "shoe", false); }

    // ...
};

// ...

Something s;
Something::MyHelper hlp = s.get_helper();

```

In this situation, if the helper class is changed from `SomeComplexType` to some other type, only the `typedef` and the `friend` declaration would need to be modified; as long as the helper class provides the same functionality, any code that uses it as `Something::MyHelper` instead of specifying it by name will usually still work without any modifications.  In this manner, we minimise the amount of code that needs to be modified when the underlying implementation is changed, such that the type name only needs to be changed in one location.

This can also be combined with `decltype`, if one so desires.

```cpp
class SomethingElse {
    AnotherComplexType<bool, int, SomeThirdClass> helper;

  public:
    typedef decltype(helper) MyHelper;

  private:
    InternalVariable<MyHelper> ivh;

    // ...

  public:
    MyHelper& get_helper() const { return helper; }

    // ...
};

```

In this situation, changing the implementation of `SomethingElse::helper` will automatically change the typedef for us, due to `decltype`.  This minimises the number of modifications necessary when we want to change `helper`, which minimises the risk of human error.

As with everything, however, this can be taken too far.  If the typename is only used once or twice internally and zero times externally, for example, there's no need to provide an alias for it.  If it's used hundreds or thousands of times throughout a project, or if it has a long enough name, then it can be useful to provide it as a typedef instead of always using it in absolute terms.  One must balance forwards compatibility and convenience with the amount of unnecessary noise created.

This can also be used with template classes, to provide access to the template parameters from outside the class.

```cpp
template<typename T>
class SomeClass {
    // ...

  public:
    typedef T MyParam;
    MyParam getParam() { return static_cast<T>(42); }
};

template<typename T>
typename T::MyParam some_func(T& t) {
    return t.getParam();
}

SomeClass<int> si;
int i = some_func(si);

```

This is commonly used with containers, which will usually provide their element type, and other helper types, as member type aliases.  Most of the containers in the C++ standard library, for example, provide the following 12 helper types, along with any other special types they might need.

```cpp
template<typename T>
class SomeContainer {
    // ...

  public:
    // Let's provide the same helper types as most standard containers.
    typedef T                                     value_type;
    typedef std::allocator<value_type>            allocator_type;
    typedef value_type&                           reference;
    typedef const value_type&                     const_reference;
    typedef value_type*                           pointer;
    typedef const value_type*                     const_pointer;
    typedef MyIterator<value_type>                iterator;
    typedef MyConstIterator<value_type>           const_iterator;
    typedef std::reverse_iterator<iterator>       reverse_iterator;
    typedef std::reverse_iterator<const_iterator> const_reverse_iterator;
    typedef size_t                                size_type;
    typedef ptrdiff_t                             difference_type;
};

```

Prior to C++11, it was also commonly used to provide a "template `typedef`" of sorts, as the feature wasn't yet available; these have become a bit less common with the introduction of alias templates, but are still useful in some situations (and are combined with alias templates in other situations, which can be very useful for obtaining individual components of a complex type such as a function pointer).  They commonly use the name `type` for their type alias.

```cpp
template<typename T>
struct TemplateTypedef {
    typedef T type;
}

TemplateTypedef<int>::type i; // i is an int.

```

This was often used with types with multiple template parameters, to provide an alias that defines one or more of the parameters.

```cpp
template<typename T, size_t SZ, size_t D>
class Array { /* ... */ };

template<typename T, size_t SZ>
struct OneDArray {
    typedef Array<T, SZ, 1> type;
};

template<typename T, size_t SZ>
struct TwoDArray {
    typedef Array<T, SZ, 2> type;
};

template<typename T>
struct MonoDisplayLine {
    typedef Array<T, 80, 1> type;
};

OneDArray<int, 3>::type     arr1i; // arr1i is an Array<int, 3, 1>.
TwoDArray<short, 5>::type   arr2s; // arr2s is an Array<short, 5, 2>.
MonoDisplayLine<char>::type arr3c; // arr3c is an Array<char, 80, 1>.

```



## Multiple Inheritance


Aside from single inheritance:

```cpp
class A {};
class B : public A {};

```

You can also have multiple inheritance:

```cpp
class A {};
class B {};
class C : public A, public B {};

```

`C` will now have inherit from `A` and `B` at the same time.

***Note: this can lead to ambiguity if the same names are used in multiple inherited `class`s or `struct`s. Be careful!***

*Ambiguity in Multiple Inheritance *

Multiple inheritance may be helpful in certain cases but, sometimes odd sort of problem encounters while using multiple inheritance.

For example: Two base classes have functions with same name which is not overridden in derived class and if you write code to access that function using object of derived class, compiler shows error because, it cannot determine which function to call. Here is a code for this type of ambiguity in multiple inheritance.

```cpp
class base1
{
  public:
     void funtion( )
     { //code for base1 function }  
};
class base2
{
    void function( )
     { // code for base2 function } 
};

class derived : public base1, public base2
{
    
};

int main()
{
    derived obj;
    
  // Error because compiler can't figure out which function to call 
  //either function( ) of base1 or base2 .   
    obj.function( )  
}

```

But, this problem can be solved using scope resolution function to specify which function to class either base1 or base2:

```cpp
int main()
{
    obj.base1::function( );  // Function of class base1 is called. 
    obj.base2::function( );  // Function of class base2 is called.
}

```



## Static class members


A class is also allowed to have `static` members, which can be either variables or functions.  These are considered to be in the class' scope, but aren't treated as normal members; they have static storage duration (they exist from the start of the program to the end), aren't tied to a particular instance of the class, and only one copy exists for the entire class.

```cpp
class Example {
    static int num_instances;      // Static data member (static member variable).
    int i;                         // Non-static member variable.

  public:
    static std::string static_str; // Static data member (static member variable).
    static int static_func();      // Static member function.

    // Non-static member functions can modify static member variables.
    Example() { ++num_instances; }
    void set_str(const std::string& str);
};

int         Example::num_instances;
std::string Example::static_str = "Hello.";

// ...

Example one, two, three;
// Each Example has its own "i", such that:
//  (&one.i != &two.i)
//  (&one.i != &three.i)
//  (&two.i != &three.i).
// All three Examples share "num_instances", such that:
//  (&one.num_instances == &two.num_instances)
//  (&one.num_instances == &three.num_instances)
//  (&two.num_instances == &three.num_instances)

```

Static member variables are not considered to be defined inside the class, only declared, and thus have their definition outside the class definition; the programmer is allowed, but not required, to initialise static variables in their definition.  When defining the member variables, the keyword `static` is omitted.

```cpp
class Example {
    static int num_instances;               // Declaration.

  public:
    static std::string static_str;          // Declaration.

    // ...
};

int         Example::num_instances;         // Definition.  Zero-initialised.
std::string Example::static_str = "Hello."; // Definition.

```

Due to this, static variables can be incomplete types (apart from `void`), as long as they're later defined as a complete type.

```cpp
struct ForwardDeclared;

class ExIncomplete {
    static ForwardDeclared fd;
    static ExIncomplete    i_contain_myself;
    static int             an_array[];
};

struct ForwardDeclared {};

ForwardDeclared ExIncomplete::fd;
ExIncomplete    ExIncomplete::i_contain_myself;
int             ExIncomplete::an_array[5];

```

Static member functions can be defined inside or outside the class definition, as with normal member functions.  As with static member variables, the keyword `static` is omitted when defining static member functions outside the class definition.

```cpp
// For Example above, either...
class Example {
    // ...

  public:
    static int static_func() { return num_instances; }

    // ...

    void set_str(const std::string& str) { static_str = str; }
};

// Or...

class Example { /* ... */ };

int  Example::static_func() { return num_instances; }
void Example::set_str(const std::string& str) { static_str = str; }

```

If a static member variable is declared `const` but not `volatile`, and is of an integral or enumeration type, it can be initialised at declaration, inside the class definition.

```cpp
enum E { VAL = 5 };

struct ExConst {
    const static int ci = 5;              // Good.
    static const E ce = VAL;              // Good.
    const static double cd = 5;           // Error.
    static const volatile int cvi = 5;    // Error.

    const static double good_cd;
    static const volatile int good_cvi;
};

const double ExConst::good_cd = 5;        // Good.
const volatile int ExConst::good_cvi = 5; // Good.

```

As of C++11, static member variables of `LiteralType` types (types that can be constructed at compile time, according to `constexpr` rules) can also be declared as `constexpr`; if so, they must be initialised within the class definition.

```cpp
struct ExConstexpr {
    constexpr static int ci = 5;                      // Good.
    static constexpr double cd = 5;                   // Good.
    constexpr static int carr[] = { 1, 1, 2 };        // Good.
    static constexpr ConstexprConstructibleClass c{}; // Good.
    constexpr static int bad_ci;                      // Error.
};

constexpr int ExConstexpr::bad_ci = 5;                // Still an error.

```

If a `const` or `constexpr` static member variable is **odr-used** (informally, if it has its address taken or is assigned to a reference), then it must still have a separate definition, outside the class definition.  This definition is not allowed to contain an initialiser.

```cpp
struct ExODR {
    static const int odr_used = 5;
};

// const int ExODR::odr_used;

const int* odr_user = & ExODR::odr_used; // Error; uncomment above line to resolve.

```

As static members aren't tied to a given instance, they can be accessed using the scope operator, `::`.

```cpp
std::string str = Example::static_str;

```

They can also be accessed as if they were normal, non-static members.  This is of historical significance, but is used less commonly than the scope operator to prevent confusion over whether a member is static or non-static.

```cpp
Example ex;
std::string rts = ex.static_str;

```

Class members are able to access static members without qualifying their scope, as with non-static class members.

```cpp
class ExTwo {
    static int num_instances;
    int my_num;

  public:
    ExTwo() : my_num(num_instances++) {}

    static int get_total_instances() { return num_instances; }
    int get_instance_number() const { return my_num; }
};

int ExTwo::num_instances;

```

They cannot be `mutable`, nor would they need to be; as they aren't tied to any given instance, whether an instance is or isn't const doesn't affect static members.

```cpp
struct ExDontNeedMutable {
    int immuta;
    mutable int muta;

    static int i;

    ExDontNeedMutable() : immuta(-5), muta(-5) {}
};
int ExDontNeedMutable::i;

// ...

const ExDontNeedMutable dnm;
dnm.immuta = 5; // Error: Can't modify read-only object.
dnm.muta = 5;   // Good.  Mutable fields of const objects can be written.
dnm.i = 5;      // Good.  Static members can be written regardless of an instance's const-ness.

```

Static members respect access modifiers, just like non-static members.

```cpp
class ExAccess {
    static int prv_int;

  protected:
    static int pro_int;

  public:
    static int pub_int;
};

int ExAccess::prv_int;
int ExAccess::pro_int;
int ExAccess::pub_int;

// ...

int x1 = ExAccess::prv_int; // Error: int ExAccess::prv_int is private.
int x2 = ExAccess::pro_int; // Error: int ExAccess::pro_int is protected.
int x3 = ExAccess::pub_int; // Good.

```

As they aren't tied to a given instance, static member functions have no `this` pointer; due to this, they can't access non-static member variables unless passed an instance.

```cpp
class ExInstanceRequired {
    int i;

  public:
    ExInstanceRequired() : i(0) {}

    static void bad_mutate() { ++i *= 5; }                         // Error.
    static void good_mutate(ExInstanceRequired& e) { ++e.i *= 5; } // Good.
};

```

Due to not having a `this` pointer, their addresses can't be stored in pointers-to-member-functions, and are instead stored in normal pointers-to-functions.

```cpp
struct ExPointer {
           void nsfunc() {}
    static void  sfunc() {}
};

typedef void (ExPointer::* mem_f_ptr)();
typedef void (*f_ptr)();

mem_f_ptr p_sf = &ExPointer::sfunc; // Error.
    f_ptr p_sf = &ExPointer::sfunc; // Good.

```

Due to not having a `this` pointer, they also cannot be `const` or `volatile`, nor can they have ref-qualifiers.  They also cannot be virtual.

```cpp
struct ExCVQualifiersAndVirtual {
    static void   func()                {} // Good.
    static void  cfunc() const          {} // Error.
    static void  vfunc() volatile       {} // Error.
    static void cvfunc() const volatile {} // Error.
    static void  rfunc() &              {} // Error.
    static void rvfunc() &&             {} // Error.

    virtual static void vsfunc()        {} // Error.
    static virtual void svfunc()        {} // Error.
};

```

As they aren't tied to a given instance, static member variables are effectively treated as special global variables; they're created when the program starts, and destroyed when it exits, regardless of whether any instances of the class actually exist.  Only a single copy of each static member variable exists (unless the variable is declared `thread_local` (C++11 or later), in which case there's one copy per thread).

Static member variables have the same linkage as the class, whether the class has external or internal linkage.  Local classes and unnamed classes aren't allowed to have static members.



## Non-static member functions


A class can have [non-static member functions](http://stackoverflow.com/documentation/c%2B%2B/5661/non-static-member-functions), which operate on individual instances of the class.

```cpp
class CL {
  public:
    void member_function() {}
};

```

These functions are called on an instance of the class, like so:

```cpp
CL instance;
instance.member_function();

```

They can be defined either inside or outside the class definition; if defined outside, they are specified as being in the class' scope.

```cpp
struct ST {
    void  defined_inside() {}
    void defined_outside();
};
void ST::defined_outside() {}

```

They can be [CV-qualified](http://stackoverflow.com/documentation/c%2B%2B/7146/the-this-pointer/24492/this-pointer-cv-qualifiers#t=201610150101175922343) and/or [ref-qualified](http://stackoverflow.com/documentation/c%2B%2B/7146/the-this-pointer/24493/this-pointer-ref-qualifiers#t=201610150101354472159), affecting how they see the instance they're called upon; the function will see the instance as having the specified cv-qualifier(s), if any.  Which version is called will be based on the instance's cv-qualifiers.  If there is no version with the same cv-qualifiers as the instance, then a more-cv-qualified version will be called if available.

```cpp
struct CVQualifiers {
    void func()                   {} // 1: Instance is non-cv-qualified.
    void func() const             {} // 2: Instance is const.

    void cv_only() const volatile {}
};

CVQualifiers       non_cv_instance;
const CVQualifiers      c_instance;

non_cv_instance.func(); // Calls #1.
c_instance.func();      // Calls #2.

non_cv_instance.cv_only(); // Calls const volatile version.
c_instance.cv_only();      // Calls const volatile version.

```

Member function ref-qualifiers indicate whether or not the function is intended to be called on rvalue instances, and use the same syntax as function cv-qualifiers.

```cpp
struct RefQualifiers {
    void func() &  {} // 1: Called on normal instances.
    void func() && {} // 2: Called on rvalue (temporary) instances.
};

RefQualifiers rf;
rf.func();              // Calls #1.
RefQualifiers{}.func(); // Calls #2.

```

CV-qualifiers and ref-qualifiers can also be combined if necessary.

```cpp
struct BothCVAndRef {
    void func() const& {} // Called on normal instances.  Sees instance as const.
    void func() &&     {} // Called on temporary instances.
};

```

They can also be [virtual](http://stackoverflow.com/documentation/c%2B%2B/1752/virtual-member-functions); this is fundamental to polymorphism, and allows a child class(es) to provide the same interface as the parent class, while supplying their own functionality.

```cpp
struct Base {
    virtual void func() {}
};
struct Derived {
    virtual void func() {}
};

Base* bp = new Base;
Base* dp = new Derived;
bp.func(); // Calls Base::func().
dp.func(); // Calls Derived::func().

```

For more information, see [here](http://stackoverflow.com/documentation/c%2B%2B/5661/non-static-member-functions).



## Unnamed struct/class


**Unnamed `struct`** is allowed (type has no name)

```cpp
void foo()
{
    struct /* No name */ {
        float x;
        float y;
    } point;
    
    point.x = 42;
}

```

or

```cpp
struct Circle
{
    struct /* No name */ {
        float x;
        float y;
    } center; // but a member name
    float radius;
};

```

and later

```cpp
Circle circle;
circle.center.x = 42.f;

```

but NOT **anonymous `struct`** (unnamed type and unnamed object)

```cpp
struct InvalidCircle
{
    struct /* No name */ {
        float centerX;
        float centerY;
    }; // No member either.
    float radius;
};

```

Note: Some compilers allow **anonymous `struct`** as **extension**.

<li>
**lamdba** can be seen as a special **unnamed `struct`**.
</li>
<li>
`decltype` allows to retrieve the type of **unnamed `struct`**:

```cpp
decltype(circle.point) otherPoint;

```


</li>
<li>
**unnamed `struct`** instance can be parameter of template method:

```cpp
void print_square_coordinates()
{
    const struct {float x; float y;} points[] = {
        {-1, -1}, {-1, 1}, {1, -1}, {1, 1}
    };

    // for range relies on `template <class T, std::size_t N> std::begin(T (&)[N])`
    for (const auto& point : points) { 
        std::cout << "{" << point.x << ", " << point.y << "}\n";
    }

    decltype(points[0]) topRightCorner{1, 1};
    auto it = std::find(points, points + 4, topRightCorner);
    std::cout << "top right corner is the "
              << 1 + std::distance(points, it) << "th\n";
}

```


</li>



#### Syntax


- variable.member_var = constant;
- variable.member_function();
- variable_pointer->member_var = constant;
- variable_pointer->member_function();



#### Remarks


Note that the ****only**** difference between the `struct` and `class` keywords is that by default, the member variables, member functions, and base classes of a `struct` are `public`, while in a `class` they are `private`. C++ programmers tend to call it a class if it has constructors and destructors, and the ability to enforce its own invariants; or a struct if it's just a simple collection of values, but the C++ language itself makes no distinction.

