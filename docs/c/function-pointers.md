---
metaTitle: "C - Function Pointers"
description: "Returning Function Pointers from a Function, Best Practices, Introduction, Assigning a Function Pointer, Mnemonic for writing function pointers, Basics"
---

# Function Pointers


Function pointers are pointers that point to functions instead of data types. They can be used to allow variability in the function that is to be called, at run-time.



## Returning Function Pointers from a Function


```c
#include <stdio.h>

enum Op
{
  ADD = '+',
  SUB = '-',
};


/* add: add a and b, return result */    
int add(int a, int b)
{
    return a + b;
}

/* sub: subtract b from a, return result */
int sub(int a, int b)
{
    return a - b;
}

/* getmath: return the appropriate math function */
int (*getmath(enum Op op))(int,int)
{
    switch (op)
    {
        case ADD:
            return &add;
        case SUB:
            return &sub;
        default:
            return NULL;
    }
}

int main(void)
{
    int a, b, c;
    int (*fp)(int,int);

    fp = getmath(ADD);

    a = 1, b = 2;
    c = (*fp)(a, b);
    printf("%d + %d = %d\n", a, b, c);
    return 0;
}

```



## Best Practices


### Using typedef

It might be handy to use a `typedef` instead of declaring the function pointer each time by hand.

The syntax for declaring a `typedef` for a function pointer is:

```c
typedef returnType (*name)(parameters);

```

### Example:

Posit that we have a function, `sort`, that expects a function pointer to a function `compare` such that:

> 
<p>compare -  A compare function for two elements which is to
be supplied to a sort function.</p>
<p>"compare" is expected to return 0 if the two elements are deemed
equal, a positive value if the first element passed is "larger" in some
sense than the latter element and otherwise the function returns a
negative value (meaning that the first element is "lesser" than the latter).</p>


Without a `typedef` we would pass a function pointer as an argument to a function in the following manner:

```c
void sort(int (*compare)(const void *elem1, const void *elem2)) { 
    /* inside of this block, the function is named "compare" */
}

```

With a `typedef`, we'd write:

```c
typedef int (*compare_func)(const void *, const void *);

```

and then we could change the function signature of `sort` to:

```c
void sort(compare_func func) { 
    /* In this block the function is named "func" */
}

```

both definitions of `sort` would accept any function of the form

```c
int compare(const void *arg1, const void *arg2) {
    /* Note that the variable names do not have to be "elem1" and "elem2" */
}

```

Function pointers are the only place where you should include the pointer property of the type, e.g. do not try to define types like `typedef struct something_struct *something_type`. This applies even for a structure with members which are not supposed to accessed directly by API callers, for example the stdio.h `FILE` type (which as you now will notice is not a pointer).

### Taking context pointers.

A function pointer should almost always take a user-supplied void * as a context pointer.

### Example

```c
/* function minimiser, details unimportant */
double findminimum( double (*fptr)(double x, double y, void *ctx), void *ctx)
{
    ...
    /* repeatedly make calls like this */
    temp = (*fptr)(testx, testy, ctx);
}

/* the function we are minimising, sums two cubics */
double *cubics(double x, double y, void *ctx)
{
    double *coeffsx = ctx;
    double *coeffsy = coeffx + 4;

    return coeffsx[0] * x * x * x + coeffsx[1] * x * x + coeffsx[2] * x + coeffsx[3] +
           coeffsy[0] * y * y * y + coeffsy[1] * y * y + coeffsy[2] * y + coeffsy[3];

} 

void caller()
{
    /* context, the coefficients of the cubics */
    double coeffs[8] = {1, 2, 3, 4, 5, 6, 7, 8};
    double min;

    min = findminimum(cubics, coeffs);       
}

```

Using the context pointer means that the extra parameters do not need to be hard-coded into the function pointed to, or require the use globals.

The library function `qsort()` does not follow this rule, and one can often get away without context for trivial comparison functions. But for anything more complicated, the context pointer becomes essential.

### See also

[Functions pointers](http://stackoverflow.com/documentation/c/1108/pointers/7796/functions-pointers#t=201701141233084604166)



## Introduction


Just like `char` and `int`, a function is a fundamental feature of C. As such, you can declare a pointer to one: which means that you can pass **which function to call** to another function to help it do its job. For example, if you had a `graph()` function that displayed a graph, you could pass **which function to graph** into `graph()`.

```c
// A couple of external definitions to make the example clearer
extern unsigned int screenWidth;
extern void plotXY(double x, double y);

// The graph() function.
// Pass in the bounds: the minimum and maximum X and Y that should be plotted.
// Also pass in the actual function to plot.
void graph(double minX, double minY,
           double maxX, double maxY,
           ???? *fn) {            // See below for syntax

    double stepX = (maxX - minX) / screenWidth;
    for (double x=minX; x<maxX; x+=stepX) {

        double y = fn(x);         // Get y for this x by calling passed-in fn()

        if (minY<=y && y<maxY) {
            plotXY(x, y);         // Plot calculated point
        } // if
    } for
} // graph(minX, minY, maxX, maxY, fn)

```

### Usage

So the above code will graph whatever function you passed into it - as long as that function meets certain criteria: namely, that you pass a `double` in and get a `double` out. There are many functions like that - `sin()`, `cos()`, `tan()`, `exp()` etc. - but there are many that aren't, such as `graph()` itself!

### Syntax

So how do you specify which functions you can pass into `graph()` and which ones you can't? The conventional way is by using a syntax that may not be easy to read or understand:

```c
double (*fn)(double); // fn is a pointer-to-function that takes a double and returns one

```

The problem above is that there are two things trying to be defined at the same time: the structure of the function, and the fact that it's a pointer. So, split the two definitions! But by using `typedef`, a better syntax (easier to read & understand) can be achieved.



## Assigning a Function Pointer


```c
#include <stdio.h>

/* increment: take number, increment it by one, and return it */
int increment(int i)
{
    printf("increment %d by 1\n", i);
    return i + 1;
}

/* decrement: take number, decrement it by one, and return it */
int decrement(int i)
{
    printf("decrement %d by 1\n", i);
    return i - 1;
}

int main(void)
{
    int num = 0;          /* declare number to increment */
    int (*fp)(int);       /* declare a function pointer */

    fp = &increment;      /* set function pointer to increment function */
    num = (*fp)(num);     /* increment num */
    num = (*fp)(num);     /* increment num a second time */

    fp = &decrement;      /* set function pointer to decrement function */
    num = (*fp)(num);     /* decrement num */
    printf("num is now: %d\n", num);
    return 0;
}

```



## Mnemonic for writing function pointers


All C functions are in actuality pointers to a spot in the program memory where some code exists. The main use of a function pointer is to provide a "callback" to other functions (or to simulate classes and objects).

The syntax of a function, as defined further down on this page is:

returnType (*name)(parameters)

A mnemonic for writing a function pointer definition is the following procedure:

1. Begin by writing a normal function declaration: `returnType name(parameters)`
1. Wrap the function name with pointer syntax: `returnType (*name)(parameters)`



## Basics


Just like you can have a pointer to an **int**, **char**, **float**, **array/string**, **struct**, etc. - you can have a pointer to a function.

**Declaring the pointer** takes the **return value of the function**, the **name of the function**, and the **type of arguments/parameters it receives**.

Say you have the following function declared and initialized:

```c
int addInt(int n, int m){
    return n+m;
}

```

You can declare and initialize a pointer to this function:

```c
int (*functionPtrAdd)(int, int) = addInt; // or &addInt - the & is optional

```

If you have a void function it could look like this:

```c
void Print(void){
    printf("look ma' - no hands, only pointers!\n");
}

```

Then declaring the pointer to it would be:

```c
void (*functionPtrPrint)(void) = Print;

```

**Accessing** the function itself would require dereferencing the pointer:

```c
sum = (*functionPtrAdd)(2, 3); //will assign 5 to sum
(*functionPtrPrint)(); //will print the text in Print function

```

As seen in more advanced examples in this document, declaring a pointer to a function could get messy if the function is passed more than a few parameters. If you have a few pointers to functions that have identical "structure" (same type of return value, and same type of parameters) it's best to use the **typedef** command to save you some typing, and to make the code more clear:

```c
typedef int (*ptrInt)(int, int);

int Add(int i, int j){
    return i+j;
}

int Multiply(int i, int j){
    return i*j;
}

int main()
{
    ptrInt ptr1 = Add;
    ptrInt ptr2 = Multiply;

    printf("%d\n", (*ptr1)(2,3)); //will print 5
    printf("%d\n", (*ptr2)(2,3)); //will print 6
    return 0;
}

```

You can also create an **Array of function-pointers**. If all the pointers are of the same "structure":

```c
int (*array[2]) (int x, int y); // can hold 2 function pointers
array[0] = Add;
array[1] = Multiply;

```

You can learn more [here](http://stackoverflow.com/a/252750/6296435) and [here](http://stackoverflow.com/a/5488718/6296435).

It is also possible to define an array of function-pointers of different types, though that would require casting when-ever you want to access the specific function. You can learn more [here](http://stackoverflow.com/q/39435158/6296435).



#### Syntax


<li>
returnType (*name)(parameters)
</li>
<li>
typedef returnType (*name)(parameters)
</li>
<li>
<p>typedef returnType Name(parameters);<br />
Name *name;</p>
</li>
<li>
<p>typedef returnType Name(parameters);<br />
typedef Name *NamePtr;</p>
</li>

