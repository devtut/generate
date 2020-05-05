---
metaTitle: "C - Storage Classes"
description: "auto, register, static, typedef, extern, _Thread_local"
---

# Storage Classes


A storage class is used to set the scope of a variable or function. By knowing the storage class of a variable, we can determine the life-time of that variable during the run-time of the program.



## auto


This storage class denotes that an identifier has automatic storage duration. This means once the scope in which the identifier was defined ends, the object denoted by the identifier is no longer valid.

Since all objects, not living in global scope or being declared `static`, have automatic storage duration by default when defined, this keyword is mostly of historical interest and should not be used:

```c
int foo(void)
{
    /* An integer with automatic storage duration. */
    auto int i = 3;

    /* Same */
    int j = 5;

    return 0;
} /* The values of i and j are no longer able to be used. */

```



## register


Hints to the compiler that access to an object should be as fast as possible. Whether the compiler actually uses the hint is implementation-defined; it may simply treat it as equivalent to `auto`.

The only property that is definitively different for all objects that are declared with `register` is that they cannot have their address computed. Thereby `register` can be a good tool to ensure certain optimizations:

```c
register size_t size = 467;

```

is an object that can never **alias** because no code can pass its address to another function where it might be changed unexpectedly.

This property also implies that an array

```c
register int array[5];

```

cannot decay into a pointer to its first element (i.e. `array` turning into `&array[0]`). This means that the elements of such an array cannot be accessed and the array itself cannot be passed to a function.

In fact, the only legal usage of an array declared with a `register` storage class is the `sizeof` operator; any other operator would require the address of the first element of the array. For that reason, arrays generally should not be declared with the `register` keyword since it makes them useless for anything other than size computation of the entire array, which can be done just as easily without the `register` keyword.

The `register` storage class is more appropriate for variables that are defined inside a block and are accessed with high frequency. For example,

```c
/* prints the sum of the first 5 integers*/
/* code assumed to be part of a function body*/ 
{ 
    register int k, sum;
    for(k = 1, sum = 0; k < 6; sum += k, k++);
        printf("\t%d\n",sum);
}

```

The `_Alignof` operator is also allowed to be used with `register` arrays.



## static


The `static` storage class serves different purposes, depending on the location of the declaration in the file:

<li>
To confine the identifier to that [translation unit](https://en.wikipedia.org/wiki/Translation_unit_(programming)) only (scope=file).

```c
/* No other translation unit can use this variable. */
static int i;

/* Same; static is attached to the function type of f, not the return type int. */
static int f(int n);

```


</li>
<li>
To save data for use with the next call of a function (scope=block):

```c
 void foo()
 {
     static int a = 0; /* has static storage duration and its lifetime is the
                        * entire execution of the program; initialized to 0 on 
                        * first function call */ 
     int b = 0; /* b has block scope and has automatic storage duration and 
                 * only "exists" within function */
     
     a += 10;
     b += 10; 

     printf("static int a = %d, int b = %d\n", a, b);
 }

 int main(void)
 {
     int i;
     for (i = 0; i < 5; i++)
     {
         foo();
     }

     return 0;
 }

```


This code prints:

```c
 static int a = 10, int b = 10
 static int a = 20, int b = 10
 static int a = 30, int b = 10
 static int a = 40, int b = 10
 static int a = 50, int b = 10

```


</li>

Static variables retain their value even when called from multiple different threads.

<li>
Used in function parameters to denote an array is expected to have a constant minimum number of elements and a non-null parameter:

```c
/* a is expected to have at least 512 elements. */
void printInts(int a[static 512])
{
    size_t i;
    for (i = 0; i < 512; ++i)
        printf("%d\n", a[i]);
}

```


The required number of items (or even a non-null pointer) is not necessarily checked by the compiler, and compilers are not required to notify you in any way if you don't have enough elements. If a programmer passes fewer than 512 elements or a null pointer, undefined behavior is the result. Since it is impossible to enforce this, extra care must be used when passing a value for that parameter to such a function.
</li>



## typedef


Defines a new type based on an existing type. Its syntax mirrors that of a variable declaration.

```c
/* Byte can be used wherever `unsigned char` is needed */
typedef unsigned char Byte;

/* Integer is the type used to declare an array consisting of a single int */
typedef int Integer[1];

/* NodeRef is a type used for pointers to a structure type with the tag "node" */
typedef struct node *NodeRef;

/* SigHandler is the function pointer type that gets passed to the signal function. */
typedef void (*SigHandler)(int);

```

While not technically a storage class, a compiler will treat it as one since none of the other storage classes are allowed if the `typedef` keyword is used.

The `typedef`s are important and should not be substituted with `#define` macro.

```c
typedef int newType; 
newType *ptr;        // ptr is pointer to variable of type 'newType' aka int

```

However,

```c
#define int newType
newType *ptr;        // Even though macros are exact replacements to words, this doesn't result to a pointer to variable of type 'newType' aka int

```



## extern


Used to **declare an object or function** that is defined elsewhere (and that has **external linkage**). In general, it is used to declare an object or function to be used in a module that is not the one in which the corresponding object or function is defined:

```c
/* file1.c */
int foo = 2;  /* Has external linkage since it is declared at file scope. */

```

```c
/* file2.c */
#include <stdio.h>
int main(void)
{
    /* `extern` keyword refers to external definition of `foo`. */
    extern int foo;
    printf("%d\n", foo);
    return 0;
}

```

Things get slightly more interesting with the introduction of the `inline` keyword in C99:

```c
/* Should usually be place in a header file such that all users see the definition */
/* Hints to the compiler that the function `bar` might be inlined */
/* and suppresses the generation of an external symbol, unless stated otherwise. */
inline void bar(int drink)
{
    printf("You ordered drink no.%d\n", drink);
}

/* To be found in just one .c file.
   Creates an external function definition of `bar` for use by other files.
   The compiler is allowed to choose between the inline version and the external
   definition when `bar` is called. Without this line, `bar` would only be an inline
   function, and other files would not be able to call it. */
extern void bar(int);

```



## _Thread_local


This was a new storage specifier introduced in C11 along with multi-threading. This isn't available in earlier C standards.

Denotes **thread storage duration**. A variable declared with `_Thread_local` storage specifier denotes that the object is **local to that thread** and its lifetime is the entire execution of the thread in which it's created. It can also appear along with `static` or `extern`.

```c
#include <threads.h>
#include <stdio.h>
#define SIZE 5

int thread_func(void *id)
{
    /* thread local variable i. */
    static _Thread_local int i;

    /* Prints the ID passed from main() and the address of the i.
     * Running this program will print different addresses for i, showing
     * that they are all distinct objects. */
    printf("From thread:[%d], Address of i (thread local): %p\n", *(int*)id, (void*)&i);

    return 0;
}

int main(void)
{
    thrd_t id[SIZE];
    int arr[SIZE] = {1, 2, 3, 4, 5};

    /* create 5 threads. */
    for(int i = 0; i < SIZE; i++) {
        thrd_create(&id[i], thread_func, &arr[i]);
    }

    /* wait for threads to complete. */
    for(int i = 0; i < SIZE; i++) {
        thrd_join(id[i], NULL);
    }
}

```



#### Syntax


<li>
[auto|register|static|extern] <Data type> <Variable name>[ = <Value>];
</li>
<li>
[static _Thread_local|extern _Thread_local|_Thread_local] <Data type> <Variable name>[ = <Value>]; /* since >=C11 */
</li>
<li>
Examples:
</li>
<li>
typedef int **foo**;
</li>
<li>
extern int **foo**[2];
</li>



#### Remarks


Storage class specifiers are the keywords which can appear next to the top-level type of a declaration. The use of these keywords affects the storage duration and linkage of the declared object, depending on whether it is declared at file scope or at block scope:

|Keyword|Storage Duration|Linkage|Remarks
|---|---|---|---|---|---|---|---|---|---
|`static`|Static|Internal|Sets internal linkage for objects at file scope; sets static storage duration for objects at block scope.
|`extern`|Static|External|Implied and therefore redundant for objects defined at file scope which also have an initializer. When used in a declaration at file scope without an initializer, hints that the definition is to be found in another translation unit and will be resolved at link-time.
|`auto`|Automatic|Irrelevant|Implied and therefore redundant for objects declared at block scope.
|`register`|Automatic|Irrelevant|Relevant only to objects with automatic storage duration. Provides a hint that the variable should be stored in a register. An imposed constraint is that one cannot use the unary `&` "address of" operator on such an object, and therefore the object cannot be aliased.
|`typedef`|Irrelevant|Irrelevant|Not a storage class specifier in practice, but works like one from a syntactic point of view. The only difference is that the declared identifier is a type, rather than an object.
|`_Thread_local`|Thread|Internal/external|Introduced in C11, to represent **thread storage duration**. If used at block scope, it shall also include `extern` or `static`.

Every object has an associated storage duration (regardless of scope) and linkage (relevant to declarations at file scope only), even when these keywords are omitted.

The ordering of storage class specifiers with respect to top-level type specifiers (`int`, `unsigned`, `short`, etc.) and top-level type qualifiers (`const`, `volatile`) is not enforced, so both of these declarations are valid:

```c
int static const unsigned a = 5; /* bad practice */
static const unsigned int b = 5; /* good practice */

```

It is, however, considered a good practice to put storage class specifiers first, then any type qualifiers, then the type specifier (`void`, `char`, `int`, `signed long`, `unsigned long long`, `long double`...).

Not all storage class specifiers are legal at a certain scope:

```c
register int x; /* legal at block scope, illegal at file scope */
auto int y; /* same */

static int z; /* legal at both file and block scope */
extern int a; /* same */

extern int b = 5; /* legal and redundant at file scope, illegal at block scope */

/* legal because typedef is treated like a storage class specifier syntactically */
int typedef new_type_name;

```

### Storage Duration

Storage duration can be either static or automatic. For a declared object, it is determined depending on its scope and the storage class specifiers.

<a class="remarks-subsection-anchor" name="remarks-static-storage-duration-1"></a>
<h3>Static Storage Duration</h3>

Variables with static storage duration live throughout the whole execution of the program and can be declared both at file scope (with or without `static`) and at block scope (by putting `static` explicitly). They are usually allocated and initialized by the operating system at program startup and reclaimed when the process terminates. In practice, executable formats have dedicated sections for such variables (`data`, `bss` and `rodata`) and these whole sections from the file are mapped into memory at certain ranges.

<a class="remarks-subsection-anchor" name="remarks-thread-storage-duration-2"></a>
<h3>Thread Storage Duration</h3>

This storage duration was introduced in C11. This wasn't available in earlier C standards. Some compilers provide a non-standard extension with similar semantics. For example, gcc supports `__thread` specifier which can be used in earlier C standards which didn't have `_Thread_local`.

Variables with thread storage duration can be declared at both file scope and block scope. If declared at block scope, it shall also use `static` or `extern` storage specifier. Its lifetime is the entire execution the **thread** in which it's created. This is the only storage specifier that can appear alongside another storage specifier.

<a class="remarks-subsection-anchor" name="remarks-automatic-storage-duration-3"></a>
<h3>Automatic Storage Duration</h3>

Variables with automatic storage duration can only be declared at block scope (directly within a function or within a block in that function). They are usable only in the period between entering and leaving the function or block. Once the variable goes out of scope (either by returning from the function or by leaving the block), its storage is automatically deallocated. Any further references to the same variable from pointers are invalid and lead to undefined behaviour.

In typical implementations, automatic variables are located at certain offsets in the stack frame of a function or in registers.

### External and Internal Linkage

Linkage is only relevant to objects (functions and variables) declared at file scope and affects their visibility across different translation units. Objects with external linkage are visible in every other translation unit (provided that the appropriate declaration is included). Objects with internal linkage are not exposed to other translation units and can only be used in the translation unit where they are defined.

