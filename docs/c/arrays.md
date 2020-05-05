---
metaTitle: "C - Arrays"
description: "Array length, Iterating through an array efficiently and row-major order, Declaring and initializing an array, Multi-dimensional arrays, Passing multidimensional arrays to a function, Clearing array contents (zeroing), Setting values in arrays, Define array and access array element, Allocate and zero-initialize an array with user defined size, Iterating through an array using pointers"
---

# Arrays


Arrays are derived data types, representing an ordered collection of values ("elements") of another type.  Most arrays in C have a fixed number of elements of any one type, and its representation stores the elements contiguously in memory without gaps or padding.  C allows multidimensional arrays whose elements are other arrays, and also arrays of pointers.

C supports dynamically allocated arrays whose size is determined at run time. C99 and later supports variable length arrays or VLAs.



## Array length


Arrays have fixed lengths that are known within the scope of their declarations.  Nevertheless, it is possible and sometimes convenient to calculate array lengths.  In particular, this can make code more flexible when the array length is determined automatically from an initializer:

```c
int array[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

/* size of `array` in bytes */
size_t size = sizeof(array);

/* number of elements in `array` */
size_t length = sizeof(array) / sizeof(array[0]); 

```

However, in most contexts where an array appears in an expression, it is automatically converted to ("decays to") a pointer to its first element.  The case where an array is the operand of the `sizeof` operator is one of a small number of exceptions.  The resulting pointer is not itself an array, and it does not carry any information about the length of the array from which it was derived.  Therefore, if that length is needed in conjunction with the pointer, such as when the pointer is passed to a function, then it must be conveyed separately.

For example, suppose we want to write a function to return the last element of an array of `int`.  Continuing from the above, we might call it like so:

```c
/* array will decay to a pointer, so the length must be passed separately */
int last = get_last(array, length);

```

The function could be implemented like this:

```c
int get_last(int input[], size_t length) {
    return input[length - 1];
}

```

Note in particular that although the declaration of parameter `input` resembles that of an array, **it in fact declares `input` as a pointer** (to `int`).  It is exactly equivalent to declaring `input` as `int *input`.  The same would be true even if a dimension were given.  This is possible because arrays cannot ever be actual arguments to functions (they decay to pointers when they appear in function call expressions), and it can be viewed as mnemonic.

It is a very common error to attempt to determine array size from a pointer, which cannot work.  DO NOT DO THIS:

```c
int BAD_get_last(int input[]) {
    /* INCORRECTLY COMPUTES THE LENGTH OF THE ARRAY INTO WHICH input POINTS: */
    size_t length = sizeof(input) / sizeof(input[0]));

    return input[length - 1];  /* Oops -- not the droid we are looking for */
}

```

In fact, that particular error is so common that some compilers recognize it and warn about it.  `clang`, for instance, will emit the following warning:

```c
warning: sizeof on array function parameter will return size of 'int *' instead of 'int []' [-Wsizeof-array-argument]
        int length = sizeof(input) / sizeof(input[0]);
                           ^
note: declared here
int BAD_get_last(int input[])
                     ^

```



## Iterating through an array efficiently and row-major order


Arrays in C can be seen as a contiguous chunk of memory. More precisely, the last dimension of the array is the contiguous part. We call this the **row-major order**. Understanding this and the fact that a cache fault loads a complete cache line into the cache when accessing uncached data to prevent subsequent cache faults, we can see why accessing an array of dimension 10000x10000 with `array[0][0]` would **potentially** load `array[0][1]` in cache, but accessing `array[1][0]` right after would generate a second cache fault, since it is `sizeof(type)*10000` bytes away from `array[0][0]`, and therefore certainly not on the same cache line. Which is why iterating like this is inefficient:

```c
#define ARRLEN 10000
int array[ARRLEN][ARRLEN];

size_t i, j;
for (i = 0; i < ARRLEN; ++i)
{
    for(j = 0; j < ARRLEN; ++j)
    {
        array[j][i] = 0;
    }
}

```

And iterating like this is more efficient:

```c
#define ARRLEN 10000
int array[ARRLEN][ARRLEN];

size_t i, j;
for (i = 0; i < ARRLEN; ++i)
{
    for(j = 0; j < ARRLEN; ++j)
    {
        array[i][j] = 0;
    }
}

```

In the same vein, this is why when dealing with an array with one dimension and multiple indexes (let's say 2 dimensions here for simplicity with indexes i and j) it is important to iterate through the array like this:

```c
#define DIM_X 10
#define DIM_Y 20

int array[DIM_X*DIM_Y];

size_t i, j;
for (i = 0; i < DIM_X; ++i)
{
    for(j = 0; j < DIM_Y; ++j)
    {
        array[i*DIM_Y+j] = 0;
    }
}

```

Or with 3 dimensions and indexes i,j and k:

```c
#define DIM_X 10
#define DIM_Y 20
#define DIM_Z 30

int array[DIM_X*DIM_Y*DIM_Z];

size_t i, j, k;
for (i = 0; i < DIM_X; ++i)
{
    for(j = 0; j < DIM_Y; ++j)
    {
        for (k = 0; k < DIM_Z; ++k)
        {
            array[i*DIM_Y*DIM_Z+j*DIM_Z+k] = 0;
        }
    }
}

```

Or in a more generic way, when we have an array with **N1 x N2 x ... x Nd** elements, **d** dimensions and indices noted as **n1,n2,...,nd** the offset is calculated like this

[<img src="http://i.stack.imgur.com/3bQIZ.png" alt="Formula" />](http://i.stack.imgur.com/3bQIZ.png)

Picture/formula taken from: [https://en.wikipedia.org/wiki/Row-major_order](https://en.wikipedia.org/wiki/Row-major_order)



## Declaring and initializing an array


The general syntax for declaring a one-dimensional array is

```c
type arrName[size];

```

where `type` could be any built-in type or user-defined types such as structures, `arrName` is a user-defined identifier, and `size` is an integer constant.

Declaring an array (an array of 10 int variables in this case) is done like this:

```c
int array[10];

```

it now holds indeterminate values. To ensure it holds zero values while declaring, you can do this:

```c
int array[10] = {0};

```

Arrays can also have initializers, this example declares an array of 10 `int`'s, where the first 3 `int`'s will contain the values `1`, `2`, `3`, all other values will be zero:

```c
int array[10] = {1, 2, 3};

```

In the above method of initialization, the first value in the list will be assigned to the first member of the array, the second value will be assigned to the second member of the array and so on. If the list size is smaller than the array size, then as in the above example, the remaining members of the array will be initialized to zeros. With designated list initialization (ISO C99), explicit initialization of the array members is possible. For example,

```c
int array[5] = {[2] = 5, [1] = 2, [4] = 9}; /* array is {0, 2, 5, 0, 9} */

```

In most cases, the compiler can deduce the length of the array for you, this can be achieved by leaving the square brackets empty:

```c
int array[] = {1, 2, 3}; /* an array of 3 int's */
int array[] = {[3] = 8, [0] = 9}; /* size is 4 */

```

Declaring an array of zero length is not allowed.

Variable Length Arrays (VLA for short) were added in C99, and made optional in C11.
They are equal to normal arrays, with one, important, difference: The length doesn't have to be known at compile time. VLA's have automatic storage duration. Only pointers to VLA's can have static storage duration.

```c
size_t m = calc_length(); /* calculate array length at runtime */
int vla[m];               /* create array with calculated length */

```

**Important:**

VLA's are potentially dangerous. If the array `vla` in the example above requires more space on the stack than available, the stack will overflow. Usage of VLA's is therefore often discouraged in style guides and by books and exercises.



## Multi-dimensional arrays


The C programming language allows [multidimensional arrays](https://en.wikipedia.org/wiki/Array_data_structure#Multidimensional_arrays). Here is the general form of a multidimensional array declaration −

```c
type name[size1][size2]...[sizeN];

```

For example, the following declaration creates a three dimensional (5 x 10 x 4) integer array:

```c
int arr[5][10][4];

```

**Two-dimensional Arrays**

The simplest form of multidimensional array is the two-dimensional array. A two-dimensional array is, in essence, a list of one-dimensional arrays. To declare a two-dimensional integer array of dimensions m x n, we can write as follows:

```c
type arrayName[m][n];

```

Where `type` can be any valid C data type (`int`, `float`, etc.) and `arrayName` can be any valid C identifier. A two-dimensional array can be visualized as a table with `m` rows and `n` columns. **Note**: The order **does** matter in C. The array `int a[4][3]` is not the same as the array `int a[3][4]`. The number of rows comes first as C is a **row**-major language.

A two-dimensional array `a`, which contains three rows and four columns can be shown as follows:

[<img src="http://i.stack.imgur.com/whdnE.jpg" alt="visual layout of 2D array as a table" />](http://i.stack.imgur.com/whdnE.jpg)

Thus, every element in the array `a` is identified by an element name of the form `a[i][j]`, where `a` is the name of the array, `i` represents which row, and `j` represents which column. Recall that rows and columns are zero indexed. This is very similar to mathematical notation for subscripting 2-D matrices.

**Initializing Two-Dimensional Arrays**

Multidimensional arrays may be initialized by specifying bracketed values for each row. The following define an array with 3 rows where each row has 4 columns.

```c
int a[3][4] = {  
   {0, 1, 2, 3} ,   /*  initializers for row indexed by 0 */
   {4, 5, 6, 7} ,   /*  initializers for row indexed by 1 */
   {8, 9, 10, 11}   /*  initializers for row indexed by 2 */
};

```

The nested braces, which indicate the intended row, are optional. The following initialization is equivalent to the previous example:

```c
int a[3][4] = {0,1,2,3,4,5,6,7,8,9,10,11};

```

While the method of creating arrays with nested braces is optional, it is strongly encouraged as it is more readable and clearer.

**Accessing Two-Dimensional Array Elements**

An element in a two-dimensional array is accessed by using the subscripts, i.e., row index and column index of the array. For example −

```c
int val = a[2][3];

```

The above statement will take the 4th element from the 3rd row of the array. Let us check the following program where we have used a nested loop to handle a two-dimensional array:

```c
#include <stdio.h>
 
int main () {

   /* an array with 5 rows and 2 columns*/
   int a[5][2] = { {0,0}, {1,2}, {2,4}, {3,6},{4,8}};
   int i, j;
 
   /* output each array element's value */
   for ( i = 0; i < 5; i++ ) {

      for ( j = 0; j < 2; j++ ) {
         printf("a[%d][%d] = %d\n", i,j, a[i][j] );
      }
   }
   
   return 0;
}

```

When the above code is compiled and executed, it produces the following result:

```c
a[0][0]: 0
a[0][1]: 0
a[1][0]: 1
a[1][1]: 2
a[2][0]: 2
a[2][1]: 4
a[3][0]: 3
a[3][1]: 6
a[4][0]: 4
a[4][1]: 8

```

**Three-Dimensional array:**

A 3D array is essentially an array of arrays of arrays: it's an array or collection of 2D arrays, and a 2D array is an array of 1D arrays.

[<img src="http://i.stack.imgur.com/VvvbU.jpg" alt="visual layout of 2D array as a collection of tables" />](http://i.stack.imgur.com/VvvbU.jpg)

**3D array memory map:**

[<img src="http://i.stack.imgur.com/VGy4r.jpg" alt="3D array laid out contiguously in memory" />](http://i.stack.imgur.com/VGy4r.jpg)

**Initializing a 3D Array:**

```c
double cprogram[3][2][4]={ 
{{-0.1, 0.22, 0.3, 4.3}, {2.3, 4.7, -0.9, 2}},
 {{0.9, 3.6, 4.5, 4}, {1.2, 2.4, 0.22, -1}},
 {{8.2, 3.12, 34.2, 0.1}, {2.1, 3.2, 4.3, -2.0}} 
};

```

We can have arrays with any number of dimensions, although it is likely that most of the arrays that are created will be of one or two dimensions.



## Passing multidimensional arrays to a function


Multidimensional arrays follow the same rules as single-dimensional arrays when passing them to a function.  However the combination of decay-to-pointer, operator precedence, and the two different ways to declare a multidimensional array (array of arrays vs array of pointers) may make the declaration of such functions non-intuitive.  The following example shows the correct ways to pass multidimensional arrays.

```c
#include <assert.h>
#include <stdlib.h>

/* When passing a multidimensional array (i.e. an array of arrays) to a
   function, it decays into a pointer to the first element as usual.  But only
   the top level decays, so what is passed is a pointer to an array of some fixed
   size (4 in this case). */
void f(int x[][4]) {
    assert(sizeof(*x) == sizeof(int) * 4);
}

/* This prototype is equivalent to f(int x[][4]).
   The parentheses around *x are required because [index] has a higher
   precedence than *expr, thus int *x[4] would normally be equivalent to int
   *(x[4]), i.e. an array of 4 pointers to int.  But if it's declared as a
   function parameter, it decays into a pointer and becomes int **x, 
   which is not compatable with x[2][4]. */
void g(int (*x)[4]) {
    assert(sizeof(*x) == sizeof(int) * 4);
}

/* An array of pointers may be passed to this, since it'll decay into a pointer
   to pointer, but an array of arrays may not. */
void h(int **x) {
    assert(sizeof(*x) == sizeof(int*));
}

int main(void) {
    int foo[2][4];
    f(foo);
    g(foo);

    /* Here we're dynamically creating an array of pointers.  Note that the 
       size of each dimension is not part of the datatype, and so the type 
       system just treats it as a pointer to pointer, not a pointer to array
       or array of arrays. */
    int **bar = malloc(sizeof(*bar) * 2);
    assert(bar);
    for (size_t i = 0; i < 2; i++) {
        bar[i] = malloc(sizeof(*bar[i]) * 4);
        assert(bar[i]);
    }

    h(bar);
    
    for (size_t i = 0; i < 2; i++) {
        free(bar[i]);
    }
    free(bar);
}

```

### See also

[Passing in Arrays to Functions](http://stackoverflow.com/documentation/c/1006/function-parameters/3892/passing-in-arrays-to-functions#t=201701141223012644924)



## Clearing array contents (zeroing)


Sometimes it's necessary to set an array to zero, after the initialization has been done.

```c
#include <stdlib.h> /* for EXIT_SUCCESS */

#define ARRLEN (10)

int main(void)
{
  int array[ARRLEN]; /* Allocated but not initialised, as not defined static or global. */

  size_t i;
  for(i = 0; i < ARRLEN; ++i)
  {
    array[i] = 0;
  }

  return EXIT_SUCCESS;
}

```

An common short cut to the above loop is to use `memset()` from `<string.h>`. Passing `array` as shown below makes it decay to a pointer to its 1st element.

```c
memset(array, 0, ARRLEN * sizeof (int)); /* Use size explicitly provided type (int here). */

```

or

```c
memset(array, 0, ARRLEN * sizeof *array); /* Use size of type the pointer is pointing to. */

```

As in this example `array` **is** an array and not just a pointer to an array's 1st element (see [Array length](http://stackoverflow.com/documentation/c/322/arrays/1125/array-length#t=201701141205543540386) on why this is important) a third option to 0-out the array is possible:

```

memset(array, 0, sizeof array); /* Use size of the array itself. */

```



## Setting values in arrays


Accessing array values is generally done through square brackets:

```c
int val;
int array[10];

/* Setting the value of the fifth element to 5: */
array[4] = 5;

/* The above is equal to: */
*(array + 4) = 5;

/* Reading the value of the fifth element: */
val = array[4];

```

As a side effect of the operands to the `+` operator being exchangeable (--> commutative law) the following is equivalent:

```c
*(array + 4) = 5;
*(4 + array) = 5;

```

so as well the next statements are equivalent:

```c
array[4] = 5;
4[array] = 5; /* Weird but valid C ... */

```

and those two as well:

```c
val = array[4];
val = 4[array]; /* Weird but valid C ... */

```

C doesn't perform any boundary checks, accessing contents outside of the declared array is undefined ([Accessing memory beyond allocated chunk](http://stackoverflow.com/documentation/c/364/undefined-behavior/2144/accessing-memory-beyond-allocated-chunk#t=201701141217382606962) ):

```c
int val;
int array[10];

array[4] = 5;    /* ok */
val = array[4];  /* ok */
array[19] = 20;  /* undefined behavior */
val = array[15]; /* undefined behavior */

```



## Define array and access array element


```c
#include <stdio.h>
 
#define ARRLEN (10)

int main (void) 
{

   int n[ ARRLEN ]; /* n is an array of 10 integers */
   size_t i, j; /* Use size_t to address memory, that is to index arrays, as its guaranteed to 
                   be wide enough to address all of the possible available memory. 
                   Using signed integers to do so should be considered a special use case, 
                   and should be restricted to the uncommon case of being in the need of 
                   negative indexes. */
 
   /* Initialize elements of array n. */         
   for ( i = 0; i < ARRLEN ; i++ ) 
   {
      n[ i ] = i + 100; /* Set element at location i to i + 100. */
   }
   
   /* Output each array element's value. */
   for (j = 0; j < ARRLEN ; j++ ) 
   {
      printf("Element[%zu] = %d\n", j, n[j] );
   }
 
   return 0;
}

```



## Allocate and zero-initialize an array with user defined size


```c
#include <stdio.h>
#include <stdlib.h>


int main (void)
{
  int * pdata;
  size_t n;

  printf ("Enter the size of the array: ");
  fflush(stdout); /* Make sure the prompt gets printed to buffered stdout. */

  if (1 != scanf("%zu", &n)) /* If zu is not supported (Windows?) use lu. */
  {
    fprintf("scanf() did not read a in proper value.\n");
    exit(EXIT_FAILURE);
  }

  pdata = calloc(n, sizeof *pdata);
  if (NULL == pdata) 
  {
    perror("calloc() failed"); /* Print error. */
    exit(EXIT_FAILURE);
  }

  free(pdata); /* Clean up. */

  return EXIT_SUCCESS;
}

```

This program tries to scan in an unsigned integer value from standard input, allocate a block of memory for an array of `n` elements of type `int` by calling the `calloc()` function. The memory is initialized to all zeros by the latter.

In case of success the memory is releases by the call to `free()`.



## Iterating through an array using pointers


```c
#include <stdio.h>
#define SIZE (10)
int main()
{
    size_t i = 0;
    int *p = NULL;
    int a[SIZE];
    
    /* Setting up the values to be i*i */
    for(i = 0; i < SIZE; ++i) 
    {
        a[i] = i * i;
    }
    
    /* Reading the values using pointers */
    for(p = a; p < a + SIZE; ++p) 
    {
        printf("%d\n", *p);
    }

    return 0;
}

```

Here, in the initialization of `p` in the first `for` loop condition, the array `a` [**decays**](http://stackoverflow.com/questions/1461432/what-is-array-decaying) to a pointer to its first element, as it would in almost all places where such an array variable is used.

Then, the `++p` performs pointer arithmetic on the pointer `p` and walks one by one through the elements of the array, and refers to them by dereferencing them with `*p`.



#### Syntax


- type name[length];  /* Define array of 'type' with name 'name' and length 'length'. */
- int arr[10] = {0};  /* Define an array and initialize ALL elements to 0. */
- int arr[10] = {42};  /* Define an array and initialize 1st elements to 42 an the rest to 0. */
- int arr[] = {4, 2, 3, 1}; /* Define and initialize an array of length 4. */
- arr[n] = value;     /* Set value at index n. */
- value = arr[n];     /* Get value at index n. */



#### Remarks


**Why do we need arrays?**

Arrays provide a way to organize objects into an aggregate with its own significance.  For example, C strings are arrays of characters (`char`s), and a string such as "Hello, World!" has meaning as an aggregate that is not inherent in the characters individually.  Similarly, arrays are commonly used to represent mathematical vectors and matrices, as well as lists of many kinds.  Moreover, without some way to group the elements, one would need to address each individually, such as via separate variables.  Not only is that unwieldy, it does not easily accommodate collections of different lengths.

**Arrays are implicitly converted to pointers in most contexts**.

Except when appearing as the operand of the `sizeof` operator, the `_Alignof` operator (C2011), or the unary `&` (address-of) operator, or as a string literal used to initialize an(other) array, an array is implicitly converted into ("decays to") a pointer to its first element. This implicit conversion is tightly coupled to the definition of the array subscripting operator (`[]`): the expression `arr[idx]` is defined as be equivalent to `*(arr + idx)`.  Furthermore, since pointer arithmetic is commutative, `*(arr + idx)` is also equivalent to `*(idx + arr)`, which in turn is equivalent to`idx[arr]`.  All of those expressions are valid and evaluate to the same value, provided that either `idx` or `arr` is a pointer (or an array, which decays to a pointer),  the other is an integer, and the integer is a valid index into the array to which the pointer points.

As a special case, observe that `&(arr[0])` is equivalent to `&*(arr + 0)`, which simplifies to `arr`.  All of those expressions are interchangeable wherever the last decays to a pointer.  This simply expresses again that an array decays to a pointer to its first element.

In contrast, if the address-of operator is applied to an array of type `T[N]` (**i.e.** `&arr`) then the result has type `T (*)[N]` and points to the whole array.  This is distinct from a pointer to the first array element at least with respect to pointer arithmetic, which is defined in terms of the size of the pointed-to type.

**Function parameters are not arrays**.

```c
void foo(int a[], int n);
void foo(int *a, int n);

```

Although the first declaration of `foo` uses array-like syntax for parameter `a`, such syntax is used to declare a function parameter declares that parameter as a **pointer** to the array's element type.  Thus, the second signature for `foo()` is semantically identical to the first.  This corresponds to the decay of array values to pointers where they appear as arguments to a function **call**, such that if a variable and a function parameter are declared with the same array type then that  variable's value is suitable for use in a function call as the argument associated with the parameter.

