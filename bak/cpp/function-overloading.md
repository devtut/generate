---
metaTitle: "C++ | Function Overloading"
description: "What is Function Overloading?, Return Type in Function Overloading, Member Function cv-qualifier Overloading"
---

# Function Overloading


See also separate topic on [Overload Resolution](http://stackoverflow.com/documentation/c%2B%2B/2021/overload-resolution#t=201705051758072433148)



## What is Function Overloading?


Function overloading is having multiple functions declared in the same scope with the exact same name exist in the same place (known as **scope**) differing only in their **signature**, meaning the arguments they accept.

Suppose you are writing a series of functions for generalized printing capabilities, beginning with `std::string`:

```cpp
void print(const std::string &str)
{
    std::cout << "This is a string: " << str << std::endl;
}

```

This works fine, but suppose you want a function that also accepts an `int` and prints that too. You could write:

```cpp
void print_int(int num)
{
    std::cout << "This is an int:  " << num << std::endl;
}

```

But because the two functions accept different parameters, you can simply write:

```cpp
void print(int num)
{
    std::cout << "This is an int: " << num << std::endl;
}

```

Now you have 2 functions, both named `print`, but with different signatures. One accepts `std::string`, the other one an `int`. Now you can call them without worrying about different names:

```cpp
print("Hello world!"); //prints "This is a string: Hello world!"
print(1337);           //prints "This is an int: 1337"

```

Instead of:

```cpp
print("Hello world!");
print_int(1337);

```

When you have overloaded functions, the compiler infers which of the functions to call from the parameters you provide it. Care must be taken when writing function overloads. For example, with implicit type conversions:

```cpp
void print(int num)
{
    std::cout << "This is an int: " << num << std::endl;
}
void print(double num)
{
    std::cout << "This is a double: " << num << std::endl;
}

```

Now it's not immediately clear which overload of `print` is called when you write:

```cpp
print(5);

```

And you might need to give your compiler some clues, like:

```cpp
print(static_cast<double>(5));
print(static_cast<int>(5));
print(5.0);

```

Some care also needs to be taken when writing overloads that accept optional parameters:

```cpp
// WRONG CODE
void print(int num1, int num2 = 0)    //num2 defaults to 0 if not included
{ 
    std::cout << "These are ints: << num1 << " and " << num2 << std::endl;
}
void print(int num)
{
    std::cout << "This is an int: " << num << std::endl;
}

```

Because there's no way for the compiler to tell if a call like `print(17)` is meant for the first or second function because of the optional second parameter, this will fail to compile.



## Return Type in Function Overloading


Note that you cannot overload a function based on its return type. For example:

```cpp
// WRONG CODE
std::string getValue()
{
  return "hello";
}

int getValue()
{
  return 0;
}

int x = getValue();

```

This will cause a compilation error as the compiler will not be able to work out which version of `getValue` to call, even though the return type is assigned to an `int`.



## Member Function cv-qualifier Overloading


Functions within a class can be overloaded for when they are accessed through a cv-qualified reference to that class; this is most commonly used to overload for `const`, but can be used to overload for `volatile` and `const volatile`, too.  This is because all non-static member functions take `this` as a hidden parameter, which the cv-qualifiers are applied to.  This is most commonly used to overload for `const`, but can also be used for `volatile` and `const volatile`.

This is necessary because a member function can only be called if it is at least as cv-qualified as the instance it's called on.  While a non-`const` instance can call both `const` and non-`const` members, a `const` instance can only call `const` members.  This allows a function to have different behaviour depending on the calling instance's cv-qualifiers, and allows the programmer to disallow functions for an undesired cv-qualifier(s) by not providing a version with that qualifier(s).

A class with some basic `print` method could be `const` overloaded like so:

```cpp
#include <iostream>

class Integer
{
    public:
        Integer(int i_): i{i_}{}

        void print()
        {
            std::cout << "int: " << i << std::endl;
        }

        void print() const
        {
            std::cout << "const int: " << i << std::endl;
        }

    protected:
        int i;
};

int main()
{
    Integer i{5};
    const Integer &ic = i;
    
    i.print(); // prints "int: 5"
    ic.print(); // prints "const int: 5"
}

```

This is a key tenet of `const` correctness: By marking member functions as `const`, they are allowed to be called on `const` instances, which in turn allows functions to take instances as `const` pointers/references if they don't need to modify them.  This allows code to specify whether it modifies state by taking unmodified parameters as `const` and modified parameters without cv-qualifiers, making code both safer and more readable.

```cpp
class ConstCorrect 
{
  public:
    void good_func() const 
    {
        std::cout << "I care not whether the instance is const." << std::endl;
    }

    void bad_func() 
    {
        std::cout << "I can only be called on non-const, non-volatile instances." << std::endl;
    }
};

void i_change_no_state(const ConstCorrect& cc) 
{
    std::cout << "I can take either a const or a non-const ConstCorrect." << std::endl;
    cc.good_func(); // Good.  Can be called from const or non-const instance.
    cc.bad_func();  // Error.  Can only be called from non-const instance.
}

void const_incorrect_func(ConstCorrect& cc) 
{
    cc.good_func(); // Good.  Can be called from const or non-const instance.
    cc.bad_func();  // Good.  Can only be called from non-const instance.
}

```

A common usage of this is declaring accessors as `const`, and mutators as non-`const`.

No class members can be modified within a `const` member function. If there is some member that you really need to modify, such as locking a `std::mutex`, you can declare it as [`mutable`](http://stackoverflow.com/documentation/c%2B%2B/2705/mutable-keyword#t=201608042103196324218):

```cpp
class Integer
{
    public:
        Integer(int i_): i{i_}{}

        int get() const
        {
            std::lock_guard<std::mutex> lock{mut};
            return i;
        }

        void set(int i_)
        {
            std::lock_guard<std::mutex> lock{mut};
            i = i_;
        }

    protected:
        int i;
        mutable std::mutex mut;
};

```



#### Remarks


Ambiguities can occur when one type can be implicitly converted into more than one type and there is no matching function for that specific type.

For example:

```cpp
void foo(double, double);
void foo(long, long);

//Call foo with 2 ints
foo(1, 2); //Function call is ambiguous - int can be converted into a double/long at the same time 

```

