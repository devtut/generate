---
metaTitle: "Pointers"
description: "Pointers for array access, Pointer arithmetic, The asterisk is part of the type, void*, Member access using ->, Generic pointers"
---

# Pointers



## Pointers for array access


This example demonstrates how pointers can be used for C-like access to C# arrays.

```cs
unsafe
{
    var buffer = new int[1024];
    fixed (int* p = &buffer[0])
    {
        for (var i = 0; i < buffer.Length; i++)
        {
            *(p + i) = i;
        }
    }
}

```

The `unsafe` keyword is required because pointer access will not emit any bounds checks that are normally emitted when accessing C# arrays the regular way.

The `fixed` keyword tells the C# compiler to emit instructions to pin the object in an exception-safe way. Pinning is required to ensure that the garbage collector will not move the array in memory, as that would invalidate any pointers pointing within the array.



## Pointer arithmetic


Addition and subtraction in pointers works differently from integers. When a pointer is incremented or decremented, the address it points to is increased or decreased by the size of the referent type.

For example, the type `int` (alias for `System.Int32`) has a size of 4. If an `int` can be stored in address 0, the subsequent `int` can be stored in address 4, and so on. In code:

```cs
var ptr = (int*)IntPtr.Zero;
Console.WriteLine(new IntPtr(ptr)); // prints 0
ptr++;
Console.WriteLine(new IntPtr(ptr)); // prints 4
ptr++;
Console.WriteLine(new IntPtr(ptr)); // prints 8

```

Similarly, the type `long` (alias for `System.Int64`) has a size of 8. If a `long` can be stored in address 0, the subsequent `long`can be stored in address 8, and so on. In code:

```cs
var ptr = (long*)IntPtr.Zero;
Console.WriteLine(new IntPtr(ptr)); // prints 0
ptr++;
Console.WriteLine(new IntPtr(ptr)); // prints 8
ptr++;
Console.WriteLine(new IntPtr(ptr)); // prints 16

```

The type `void` is special and `void` pointers are also special and they are used as catch-all pointers when the type isn't known or doesn't matter. Due to their size-agnostic nature, `void` pointers cannot be incremented or decremented:

```cs
var ptr = (void*)IntPtr.Zero;
Console.WriteLine(new IntPtr(ptr));
ptr++; // compile-time error
Console.WriteLine(new IntPtr(ptr));
ptr++; // compile-time error
Console.WriteLine(new IntPtr(ptr));

```



## The asterisk is part of the type


In C and C++, the asterisk in the declaration of a pointer variable is **part of the expression** being declared. In C#, the asterisk in the declaration is **part of the type**.

In C, C++ and C#, the following snippet declares an `int` pointer:

```cs
int* a;

```

In C and C++, the following snippet declares an `int` pointer and an `int` variable. In C#, it declares two `int` pointers:

```cs
int* a, b; 

```

In C and C++, the following snippet declares two `int` pointers. In C#, it is invalid:

```cs
int *a, *b;

```



## void*


C# inherits from C and C++ the usage of `void*` as a type-agnostic and size-agnostic pointer.

```cs
void* ptr;

```

Any pointer type can be assigned to `void*` using an implicit conversion:

```cs
int* p1 = (int*)IntPtr.Zero;
void* ptr = p1;

```

The reverse requires an explicit conversion:

```cs
int* p1 = (int*)IntPtr.Zero;
void* ptr = p1;
int* p2 = (int*)ptr;

```



## Member access using ->


C# inherits from C and C++ the usage of the symbol `->` as a means of accessing the members of an instance through a typed pointer.

Consider the following struct:

```cs
struct Vector2
{
    public int X;
    public int Y;
}

```

This is an example of the usage of `->` to access its members:

```cs
Vector2 v;
v.X = 5;
v.Y = 10;

Vector2* ptr = &v;
int x = ptr->X;
int y = ptr->Y;
string s = ptr->ToString();

Console.WriteLine(x); // prints 5
Console.WriteLine(y); // prints 10
Console.WriteLine(s); // prints Vector2

```



## Generic pointers


The criteria that a type must satisfy in order to support pointers (see **Remarks**) cannot be expressed in terms of generic constraints. Therefore, any attempt to declare a pointer to a type provided through a generic type parameter will fail.

```cs
void P<T>(T obj) 
    where T : struct
{
    T* ptr = &obj; // compile-time error
}

```



#### Remarks


### Pointers and `unsafe`

Due to their nature, pointers produce unverifiable code. Thus, usage of any pointer type requires an `unsafe` context.

The type `System.IntPtr` is a safe wrapper around a `void*`. It is intended as a more convenient alternative to `void*` when an unsafe context isn't otherwise required to perform the task at hand.

### Undefined behavior

Like in C and C++, incorrect usage of pointers can invoke undefined behavior, with possible side-effects being memory corruption and execution of unintended code. Due to the unverifiable nature of most pointer operations, correct usage of pointers is entirely a responsibility of the programmer.

### Types that support pointers

Unlike C and C++, not all C# types have corresponding pointer types. A type `T` may have a corresponding pointer type if both of the following criteria apply:

- `T` is a struct type or a pointer type.
- `T` contains only members that satisfy both of these criteria recursively.

