---
metaTitle: "C++ | The Rule of Three, Five, And Zero"
description: "Rule of Zero, Rule of Five, Rule of Three, Self-assignment Protection"
---

# The Rule of Three, Five, And Zero




## Rule of Zero


We can combine the principles of the Rule of Five and [RAII](http://stackoverflow.com/documentation/c%2B%2B/1320/raii-resource-acquisition-is-initialization#t=201607230254049480454) to get a much leaner interface: the Rule of Zero: any resource that needs to be managed should be in its own type. That type would have to follow the Rule of Five, but all users of that resource do not need to write **any** of the five special member functions and can simply `default` all of them.

Using the `Person` class introduced in the [Rule of Three example](http://stackoverflow.com/documentation/c%2b%2b/1206/the-rule-of-three-five-and-zero/9867/rule-of-three#t=201607281743050894887), we can create a resource-managing object for `cstrings`:

```cpp
class cstring {
private:
    char* p;

public:
    ~cstring() { delete [] p; }
    cstring(cstring const& );
    cstring(cstring&& );
    cstring& operator=(cstring const& );
    cstring& operator=(cstring&& );

    /* other members as appropriate */
};

```

And once this is separate, our `Person` class becomes far simpler:

```cpp
class Person {
    cstring name;
    int arg;

public:
    ~Person() = default;
    Person(Person const& ) = default;
    Person(Person&& ) = default;
    Person& operator=(Person const& ) = default;
    Person& operator=(Person&& ) = default;

    /* other members as appropriate */
};

```

The special members in `Person` do not even need to be declared explicitly; the compiler will default or delete them appropriately, based on the contents of `Person`. Therefore, the following is also an example of the rule of zero.

```cpp
struct Person {
    cstring name;
    int arg;
};

```

If `cstring` were to be a move-only type, with a `delete`d copy constructor/assignment operator, then `Person` would automatically be move-only as well.

The term rule of zero was [introduced by R. Martinho Fernandes](https://rmf.io/cxx11/rule-of-zero)



## Rule of Five


C++11 introduces two new special member functions: the move constructor and the move assignment operator. For all the same reasons that you want to follow the [Rule of Three](http://stackoverflow.com/documentation/c%2b%2b/1206/the-rule-of-three-five-and-zero/9867/rule-of-three#t=201607251426414241612) in C++03, you usually want to follow the Rule of Five in C++11: If a class requires ONE of five special member functions, and if move semantics are desired, then it most likely requires ALL FIVE of them.

Note, however, that failing to follow the Rule of Five is usually not considered an error, but a missed optimisation opportunity, as long as the Rule of Three is still followed.  If no move constructor or move assignment operator is available when the compiler would normally use one, it will instead use copy semantics if possible, resulting in a less efficient operation due to unnecessary copy operations.  If move semantics aren't desired for a class, then it has no need to declare a move constructor or assignment operator.

Same example as for the Rule of Three:

```cpp
class Person
{
    char* name;
    int age;

public:
    // Destructor 
    ~Person() { delete [] name; }

    // Implement Copy Semantics
    Person(Person const& other)
        : name(new char[std::strlen(other.name) + 1])
        , age(other.age)
    {
        std::strcpy(name, other.name);
    }
    
    Person &operator=(Person const& other) 
    {
        // Use copy and swap idiom to implement assignment.
        Person copy(other);
        swap(*this, copy);
        return *this;
    }

    // Implement Move Semantics
    // Note: It is usually best to mark move operators as noexcept
    //       This allows certain optimizations in the standard library
    //       when the class is used in a container.

    Person(Person&& that) noexcept
        : name(nullptr)               // Set the state so we know it is undefined
        , age(0)
    {
        swap(*this, that);
    }

    Person& operator=(Person&& that) noexcept
    {
        swap(*this, that);
        return *this;
    }

    friend void swap(Person& lhs, Person& rhs) noexcept
    {
        std::swap(lhs.name, rhs.name);
        std::swap(lhs.age, rhs.age);
    }
};

```

Alternatively, both the copy and move assignment operator can be replaced with a single assignment operator, which takes an instance by value instead of reference or rvalue reference to facilitate using the copy-and-swap idiom.

```cpp
Person& operator=(Person copy)
{
    swap(*this, copy);
    return *this;
}

```

Extending from the Rule of Three to the Rule of Five is important for performance reasons, but is not strictly necessary in most cases. Adding the copy constructor and assignment operator ensures that moving the type will not leak memory (move-constructing will simply fall back to copying in that case), but will be performing copies that the caller probably did not anticipate.



## Rule of Three


The Rule of Three states that if a type ever needs to have a user-defined copy constructor, copy assignment operator, or destructor, then it must have **all three**.

The reason for the rule is that a class which needs any of the three manages some resource (file handles, dynamically allocated memory, etc), and all three are needed to manage that resource consistently. The copy functions deal with how the resource gets copied between objects, and the destructor would destroy the resource, in accord with [RAII principles](http://stackoverflow.com/documentation/c%2B%2B/1320/raii-resource-acquisition-is-initialization#t=201607230301520057618).

Consider a type that manages a string resource:

```cpp
class Person
{
    char* name;
    int age;

public:
    Person(char const* new_name, int new_age)
        : name(new char[std::strlen(new_name) + 1])
        , age(new_age)
    {
       std::strcpy(name, new_name);
    }

    ~Person() {
        delete [] name;
    }
};

```

Since `name` was allocated in the constructor, the destructor deallocates it to avoid leaking memory. But what happens if such an object is copied?

```cpp
int main()
{
    Person p1("foo", 11);
    Person p2 = p1;
}

```

First, `p1` will be constructed. Then `p2` will be copied from `p1`. However, the C++-generated copy constructor will copy each component of the type as-is. Which means that `p1.name` and `p2.name` both point to the **same** string.

When `main` ends, destructors will be called. First `p2`'s destructor will be called; it will delete the string. Then `p1`'s destructor will be called. However, the string is **already deleted**. Calling `delete` on memory that was already deleted yields undefined behavior.

To avoid this, it is necessary to provide a suitable copy constructor. One approach is to implement a reference counted system, where different `Person` instances share the same string data. Each time a copy is performed, the shared reference count is incremented. The destructor then decrements the reference count, only releasing the memory if the count is zero.

Or we could implement [value semantics and deep copying behavior](http://stackoverflow.com/documentation/c%2B%2B/1955/value-and-reference-semantics#t=201607230312543894418):

```cpp
Person(Person const& other)
    : name(new char[std::strlen(other.name) + 1])
    , age(other.age)
{
    std::strcpy(name, other.name);
}

Person &operator=(Person const& other) 
{
    // Use copy and swap idiom to implement assignment
    Person copy(other);
    swap(copy);            //  assume swap() exchanges contents of *this and copy
    return *this;
}

```

Implementation of the copy assignment operator is complicated by the need to release an existing buffer.  The copy and swap technique creates a temporary object which holds a new buffer.  Swapping the contents of `*this` and `copy` gives ownership to `copy` of the original buffer.  Destruction of `copy`, as the function returns, releases the buffer previously owned by `*this`.



## Self-assignment Protection


When writing a copy assignment operator, it is **very** important that it be able to work in the event of self-assignment. That is, it has to allow this:

```cpp
SomeType t = ...;
t = t;

```

Self-assignment usually doesn't happen in such an obvious way. It typically happens via a circuitous route through various code systems, where the location of the assignment simply has two `Person` pointers or references and has no idea that they are the same object.

Any copy assignment operator you write must be able to take this into account.

The typical way to do so is to wrap all of the assignment logic in a condition like this:

```cpp
SomeType &operator=(const SomeType &other)
{
    if(this != &other)
    {
        //Do assignment logic.
    }
    return *this;
}

```

**Note:** It is important to think about self-assignment and ensure that your code behaves correctly when it happens. However, self-assignment is a very rare occurrence and optimizing to prevent it may actually pessimize the normal case. Since the normal case is much more common, pessimizing for self-assignment may well reduce your code efficiency (so be careful using it).

As an example, the normal technique for implementing the assignment operator is the `copy and swap idiom`. The normal implementation of this technique does not bother to test for self-assignment (even though self-assignment is expensive because a copy is made). The reason is that pessimization of the normal case has been shown to be much more costly (as it happens more often).

Move assignment operators must also be protected against self-assignment. However, the logic for many such operators is based on `std::swap`, which can handle swapping from/to the same memory just fine. So if your move assignment logic is nothing more than a series of swap operations, then you do not need self-assignment protection.

If this is not the case, you **must** take similar measures as above.

