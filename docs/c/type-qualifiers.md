---
metaTitle: "Type Qualifiers"
description: "Unmodifiable (const) variables, Volatile variables"
---

# Type Qualifiers



## Unmodifiable (const) variables


```c
const int a = 0; /* This variable is "unmodifiable", the compiler
                    should throw an error when this variable is changed */
int b = 0; /* This variable is modifiable */

b += 10; /* Changes the value of 'b' */
a += 10; /* Throws a compiler error */

```

The `const` qualification only means that we don't have the right to change the data. It doesn't mean that the value cannot change behind our back.

```c
_Bool doIt(double const* a) {
   double rememberA = *a;
   // do something long and complicated that calls other functions

   return rememberA == *a;
}

```

During the execution of the other calls `*a` might have changed, and so this function may return either `false` or `true`.

### Warning

Variables with `const` qualification could still be changed using pointers:

```c
const int a = 0;

int *a_ptr = (int*)&a; /* This conversion must be explicitly done with a cast */
*a_ptr += 10;          /* This has undefined behavior */

printf("a = %d\n", a); /* May print: "a = 10" */

```

But doing so is an error that leads to undefined behavior. The difficulty here is that this may behave as expected in simple examples as this, but then go wrong when the code grows.



## Volatile variables


The `volatile` keyword tells the compiler that the value of the variable may change at any time as a result of external conditions, not only as a result of program control flow.

The compiler will not optimize anything that has to do with the volatile variable.

```c
volatile int foo; /* Different ways to declare a volatile variable */
int volatile foo;

volatile uint8_t * pReg; /* Pointers to volatile variable */
uint8_t volatile * pReg;

```

There are two main reasons to uses volatile variables:

- To interface with hardware that has memory-mapped I/O registers.
- When using variables that are modified outside the program control flow (e.g., in an interrupt service routine)

Let's see this example:

```c
int quit = false;

void main() 
{
    ... 
    while (!quit) {
      // Do something that does not modify the quit variable
    } 
    ...
}

void interrupt_handler(void) 
{
  quit = true;
}

```

The compiler is allowed to notice the while loop does not modify the `quit` variable and convert the loop to a endless `while (true)` loop. Even if the `quit` variable is set on the signal handler for `SIGINT` and `SIGTERM`, the compiler does not know that.

Declaring `quit` as `volatile` will tell the compiler to not optimize the loop and the problem will be solved.

The same problem happens when accessing hardware, as we see in this example:

```c
uint8_t * pReg = (uint8_t *) 0x1717;

// Wait for register to become non-zero 
while (*pReg == 0) { } // Do something else

```

The behavior of the optimizer is to read the variable's value once, there is no need to reread it, since the value will always be the same. So we end up with an infinite loop. To force the compiler to do what we want, we modify the declaration to:

```c
uint8_t volatile * pReg = (uint8_t volatile *) 0x1717;

```



#### Remarks


Type qualifiers are the keywords which describe additional semantics about a type. They are an integral part of type signatures. They can appear both at the topmost level of a declaration (directly affecting the identifier) or at sub-levels (relevant to pointers only, affecting the pointed-to values):

|Keyword|Remarks
|---|---|---|---|---|---|---|---|---|---
|`const`|Prevents the mutation of the declared object (by appearing at the topmost level) or prevents the mutation of the pointed-to value (by appearing next to a pointer subtype).
|`volatile`|Informs the compiler that the declared object (at topmost level) or the pointed-to value (in pointer subtypes) may change its value as a result of external conditions, not only as a result of program control flow.
|`restrict`|An optimization hint, relevant to pointers only. Declares intent that for the lifetime of the pointer, no other pointers will be used to access the same pointed-to object.

The ordering of type qualifiers with respect to storage class specifiers (`static`, `extern`, `auto`, `register`), type modifiers (`signed`, `unsigned`, `short`, `long`) and type specifiers (`int`, `char`, `double`, etc.) is not enforced, but the good practice is to put them in the aforementioned order:

```c
static const volatile unsigned long int a = 5; /* good practice */
unsigned volatile long static int const b = 5; /* bad practice */

```

### Top-level qualifications

```c
/* "a" cannot be mutated by the program but can change as a result of external conditions */
const volatile int a = 5;

/* the const applies to array elements, i.e. "a[0]" cannot be mutated */    
const int arr[] = { 1, 2, 3 };

/* for the lifetime of "ptr", no other pointer could point to the same "int" object */
int *restrict ptr;

```

### Pointer subtype qualifications

```c
/* "s1" can be mutated, but "*s1" cannot */
const char *s1 = "Hello";

/* neither "s2" (because of top-level const) nor "*s2" can be mutated */
const char *const s2 = "World";

/* "*p" may change its value as a result of external conditions, "**p" and "p" cannot */
char *volatile *p;

/* "q", "*q" and "**q" may change their values as a result of external conditions */
volatile char *volatile *volatile q;

```

