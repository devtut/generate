---
metaTitle: "C++ | Memory management"
description: "Free Storage (Heap, Dynamic Allocation ...), Placement new, Stack"
---

# Memory management




## Free Storage (Heap, Dynamic Allocation ...)


The term **'heap'** is a general computing term meaning an area of memory from which portions can be allocated and deallocated independently of the memory provided by the **stack**.

In `C++` the **Standard** refers to this area as the **Free Store** which is considered a more accurate term.

Areas of memory allocated from the **Free Store** may live longer than the original scope in which it was allocated. Data too large to be stored on the stack may also be allocated from the **Free Store**.

Raw memory can be allocated and deallocated by the **new** and **delete** keywords.

```cpp
float *foo = nullptr;
{
    *foo = new float; // Allocates memory for a float
    float bar;              // Stack allocated 
} // End lifetime of bar, while foo still alive

delete foo;          // Deletes the memory for the float at pF, invalidating the pointer
foo = nullptr;       // Setting the pointer to nullptr after delete is often considered good practice

```

It's also possible to allocate fixed size arrays with **new** and **delete**, with a slightly different syntax. Array allocation is not compatible with non-array allocation, and mixing the two will lead to heap corruption. Allocating an array also allocates memory to track the size of the array for later deletion in an implementation-defined way.

```cpp
// Allocates memory for an array of 256 ints
int *foo = new int[256];
// Deletes an array of 256 ints at foo
delete[] foo;

```

When using **new** and **delete** instead [**malloc** and **free**](http://stackoverflow.com/documentation/c/4726/memory-management), the constructor and destructor will get executed (Similar to stack based objects). This is why **new** and **delete** are prefered over **malloc** and **free**.

```cpp
struct ComplexType {
    int a = 0;

    ComplexType() { std::cout << "Ctor" << std::endl; }
    ~ComplexType() { std::cout << "Dtor" << std::endl; }
};

// Allocates memory for a ComplexType, and calls its constructor
ComplexType *foo = new ComplexType();
//Calls the destructor for ComplexType() and deletes memory for a Complextype at pC
delete foo;

```

From C++11 on, the use of [smart pointers](http://stackoverflow.com/documentation/c%2b%2b/509/smart-pointers) is recommended for indicating ownership.

C++14 added `std::make_unique` to the STL, changing the recommendation to favor `std::make_unique` or `std::make_shared` instead of using naked **new** and **delete**.



## Placement new


There are situations when we don't want to rely upon Free Store for allocating memory and we want to use custom memory allocations using `new`.

For these situations we can use `Placement New`, where we can tell `new' operator to allocate memory from a pre-allocated memory location

For example

```cpp
int a4byteInteger;

char *a4byteChar = new (&a4byteInteger) char[4];

```

In this example, the memory pointed by `a4byteChar` is 4 byte allocated to 'stack' via integer variable `a4byteInteger`.

The benefit of this kind of memory allocation is the fact that programmers control the allocation. In the example above, since `a4byteInteger` is allocated on stack, we don't need to make an explicit call to 'delete a4byteChar`.

Same behavior can be achieved for dynamic allocated memory also. For example

```cpp
int *a8byteDynamicInteger = new int[2];

char *a8byteChar = new (a8byteDynamicInteger) char[8];

```

In this case, the memory pointer by `a8byteChar` will be referring to dynamic memory allocated by `a8byteDynamicInteger`. In this case however, we need to explicitly call`delete a8byteDynamicInteger` to release the memory

Another example for C++ Class

```cpp
struct ComplexType {
    int a;

    ComplexType() : a(0) {}
    ~ComplexType() {}
};

int main() {
    char* dynArray = new char[256];

    //Calls ComplexType's constructor to initialize memory as a ComplexType
    new((void*)dynArray) ComplexType();

    //Clean up memory once we're done
    reinterpret_cast<ComplexType*>(dynArray)->~ComplexType();
    delete[] dynArray;

    //Stack memory can also be used with placement new
    alignas(ComplexType) char localArray[256]; //alignas() available since C++11

    new((void*)localArray) ComplexType();

    //Only need to call the destructor for stack memory
    reinterpret_cast<ComplexType*>(localArray)->~ComplexType();

    return 0;
}

```



## Stack


The stack is a small region of memory into which temporary values are placed during execution. Allocating data into the stack is very fast compared to heap allocation, as all the memory has already been assigned for this purpose.

```cpp
int main() {
    int a = 0; //Stored on the stack
    return a;
}

```

The stack is named because chains of function calls will have their temporary memory 'stacked' on top of each other, each one using a separate small section of memory.

```cpp
float bar() {
    //f will be placed on the stack after anything else
    float f = 2;
    return f;
}

double foo() {
    //d will be placed just after anything within main()
    double d = bar();
    return d;
}

int main() {
    //The stack has no user variables stored in it until foo() is called
    return (int)foo();
}

```

Data stored on the stack is only valid so long as the scope that allocated the variable is still active.

```cpp
int* pA = nullptr;

void foo() {
    int b = *pA;
    pA = &b;
}

int main() {
    int a = 5;
    pA = &a;
    foo();
    //Undefined behavior, the value pointed to by pA is no longer in scope
    a = *pA;
}

```



#### Syntax


- ::(**opt**) new (**expression-list**)(**opt**) **new-type-id** **new-initializer**(**opt**)
- ::(**opt**) new (**expression-list**)(**opt**) (**type-id**) **new-initializer**(**opt**)
- ::(**opt**) delete **cast-expression**
- ::(**opt**) delete [] **cast-expression**
- std::unique_ptr<**type-id**> var_name(new **type-id**(**opt**)); //C++11
- std::shared_ptr<**type-id**> var_name(new **type-id**(**opt**)); //C++11
- std::shared_ptr<**type-id**> var_name = std::make_shared<**type-id**>(**opt**); //C++11
- std::unique_ptr<**type-id**> var_name = std::make_unique<**type-id**>(**opt**); //C++14



#### Remarks


A leading `::` forces the new or delete operator to be looked up in global scope, overriding any overloaded class-specific new or delete operators.

The optional arguments following the `new` keyword are usually used to call [placement new](http://stackoverflow.com/documentation/c%2b%2b/2873/memory-management/9740/placement-new), but can also be used to pass additional information to the allocator, such as a tag requesting that memory be allocated from a chosen pool.

The type allocated is usually explicitly specified, **e.g.,** `new Foo`, but can also be written as `auto` (since C++11) or `decltype(auto)` (since C++14) to deduce it from the initializer.

Initialization of the object allocated occurs according to the same rules as initialization of local variables. In particular, the object will be default-initialized if the initializer iso omitted, and when dynamically allocating a scalar type or an array of scalar type, there is no guarantee that the memory will be zeroed out.

An array object created by a **new-expression** must be destroyed using `delete[]`, regardless of whether the **new-expression** was written with `[]` or not. For example:

```cpp
using IA = int[4];
int* pIA = new IA;
delete[] pIA;  // right
// delete pIA;  // wrong

```

