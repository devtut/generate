---
metaTitle: "C - Implementation-defined behaviour"
description: "Right shift of a negative integer, Assigning an out-of-range value to an integer, Allocating zero bytes, Representation of signed integers"
---

# Implementation-defined behaviour



## Right shift of a negative integer


```c
int signed_integer = -1;

// The right shift operation exhibits implementation-defined behavior:
int result = signed_integer >> 1;

```



## Assigning an out-of-range value to an integer


```c
// Supposing SCHAR_MAX, the maximum value that can be represented by a signed char, is
// 127, the behavior of this assignment is implementation-defined:
signed char integer;
integer = 128;

```



## Allocating zero bytes


```c
// The allocation functions have implementation-defined behavior when the requested size
// of the allocation is zero.
void *p = malloc(0);

```



## Representation of signed integers


Each signed integer type may be represented in any one of three formats; it is implementation-defined which one is used.  The implementation in use for any given signed integer type at least as wide as `int` can be determined at runtime from the two lowest-order bits of the representation of value `-1` in that type, like so:

```c
enum { sign_magnitude = 1, ones_compl = 2, twos_compl = 3, };
#define SIGN_REP(T) ((T)-1 & (T)3)

switch (SIGN_REP(long)) {
   case sign_magnitude: { /* do something */ break; }
   case ones_compl:     { /* do otherwise */ break; }
   case twos_compl:     { /* do yet else  */ break; }
   case 0:  { _Static_assert(SIGN_REP(long), "bogus sign representation"); }
}

```

The same pattern applies to the representation of narrower types, but they cannot be tested by this technique because the operands of `&` are subject to "the usual arithmetic conversions" before the result is computed.



#### Remarks


### Overview

The C standard describes the language syntax, the functions provided by the standard library, and the behavior of conforming C processors (roughly speaking, compilers) and conforming C programs.  With respect to behavior, the standard for the most part specifies particular behaviors for programs and processors.  On the other hand, some operations have explicit or implicit **undefined behavior** -- such operations are always to be avoided, as you cannot rely on anything about them.  In between, there are a variety of **implementation defined** behaviors.  These behaviors may vary between C processors, runtimes, and standard libraries (collectively, **implementations**), but they are consistent and reliable for any given implementation, and conforming implementations document their behavior in each of these areas.

It is sometimes reasonable for a program to rely on implementation-defined behavior.  For example, if the program is anyway specific to a particular operating environment then relying on implementation-defined behaviors general to the common processors for that environment is unlikely to be a problem.  Alternatively, one can use conditional compilation directives to select implementation-defined behaviors appropriate for the implementation in use.  In any case, it is essential to know which operations have implementation defined behavior, so as to either avoid them or to make an informed decision about whether and how to use them.

The balance of these remarks constitute a list of all the implementation-defined behaviors and characteristics specified in the C2011 standard, with references to the standard.  Many of them use [the terminology of the standard](http://port70.net/%7Ensz/c/c11/n1570.html#3).  Some others rely more generally on the context of the standard, such as the eight stages of translating source code into a program, or the difference between hosted and freestanding implementations.  Some that may be particularly surprising or notable are presented in bold typeface.  Not all the behaviors described are supported by earlier C standards, but generally speaking, they have implementation-defined behavior in all versions of the standard that support them.

### Programs and Processors

### General

<li>
**The number of bits in one byte** ([3.6/3](http://port70.net/%7Ensz/c/c11/n1570.html#3.6p3)). At least `8`, the actual value can be queried with the macro `CHAR_BIT`.
</li>
<li>
Which output messages are considered "diagnostic messages" ([3.10/1](http://port70.net/%7Ensz/c/c11/n1570.html#3.10p1))
</li>

### Source translation

<li>
The manner in which physical source file multibyte characters are mapped to the source character set ([5.1.1.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.1.2p1)).
</li>
<li>
Whether non-empty sequences of non-newline whitespace are replaced by single spaces during translation phase 3 ([5.1.1.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.1.2p1))
</li>
<li>
The execution-set character(s) to which character literals and characters in string constants are converted (during translation phase 5) when there is otherwise no corresponding character ([5.1.1.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.1.2p1)).
</li>

### Operating environment

<li>
The manner in which the diagnostic messages to be emitted are identified ([5.1.1.3/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.1.3p1)).
</li>
<li>
The name and type of the function called at startup in a freestanding implementation ([5.1.2.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.1p1)).
</li>
<li>
Which library facilities are available in a freestanding implementation, beyond a specified minimal set ([5.1.2.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.1p1)).
</li>
<li>
The effect of program termination in a freestanding environment ([5.1.2.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.1p2)).
</li>
<li>
In a hosted environment, any allowed signatures for the `main()` function other than `int main(int argc, char *arg[])` and `int main(void)` ([5.1.2.2.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.2.1p1)).
</li>
<li>
The manner in which a hosted implementation defines the strings pointed to by the second argument to `main()` ([5.1.2.2.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.2.1p2)).
</li>
<li>
What constitutes an "interactive device" for the purpose of sections [5.1.2.3](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.3) (Program Execution) and [7.21.3](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3) (Files) ([5.1.2.3/7](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.3p7)).
</li>
<li>
Any restrictions on objects referred to by interrupt-handler routines in an optimizing implementation ([5.1.2.3/10](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.3p10)).
</li>
<li>
In a freestanding implementation, whether multiple threads of execution are supported ([5.1.2.4/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.1.2.4p1)).
</li>
<li>
The values of the members of the execution character set ([5.2.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.1p1)).
</li>
<li>
The `char` values corresponding to the defined alphabetic escape sequences ([5.2.2/3](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.2p3)).
</li>
<li>
**The integer and floating-point numeric limits and characteristics** ([5.2.4.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2p1)).
</li>
<li>
The accuracy of floating-point arithmetic operations and of the standard library's conversions from internal floating point representations to string representations ([5.2.4.2.2/6](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2.2p6)).
</li>
<li>
The value of macro `FLT_ROUNDS`, which encodes the default floating-point rounding mode ([5.2.4.2.2/8](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2.2p8)).
</li>
<li>
The rounding behaviors characterized by supported values of `FLT_ROUNDS` greater than 3 or less than -1 ([5.2.4.2.2/8](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2.2p8)).
</li>
<li>
The value of macro `FLT_EVAL_METHOD`, which characterizes floating-point evaluation behavior ([5.2.4.2.2/9](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2.2p9)).
</li>
<li>
The behavior characterized by any supported values of `FLT_EVAL_METHOD` less than -1 ([5.2.4.2.2/9](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2.2p9)).
</li>
<li>
The values of macros `FLT_HAS_SUBNORM`, `DBL_HAS_SUBNORM`, and `LDBL_HAS_SUBNORM`, characterizing whether the standard floating-point formats support subnormal numbers ([5.2.4.2.2/10](http://port70.net/%7Ensz/c/c11/n1570.html#5.2.4.2.2p10))
</li>

### Types

<li>
The result of attempting to (indirectly) access an object with thread storage duration from a thread other than the one with which the object is associated ([6.2.4/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.4p4))
</li>
<li>
The value of a `char` to which a character outside the basic execution set has been assigned ([6.2.5/3](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.5p3)).
</li>
<li>
The supported extended signed integer types, if any, ([6.2.5/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.5p4)), and any extension keywords used to identify them.
</li>
<li>
**Whether `char` has the same representation and behavior as `signed char` or as `unsigned char`** ([6.2.5/15](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.5p15)). Can be queried with `CHAR_MIN`, which is either `0` or `SCHAR_MIN` if `char` is unsigned or signed, respectively.
</li>
<li>
**The number, order, and encoding of bytes in the representations of objects**, except where explicitly specified by the standard ([6.2.6.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.6.1p2)).
</li>
<li>
**Which of the three recognized forms of integer representation applies in any given situation, and whether certain bit patterns of integer objects are trap representations** ([6.2.6.2/2](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.6.2p2)).
</li>
<li>
The alignment requirement of each type ([6.2.8/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.8p1)).
</li>
<li>
Whether and in what contexts any extended alignments are supported ([6.2.8/3](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.8p3)).
</li>
<li>
The set of supported extended alignments ([6.2.8/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.2.8p4)).
</li>
<li>
The integer conversion ranks of any extended signed integer types relative to each other ([6.3.1.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.3.1.1p1)).
</li>
<li>
**The effect of assigning an out-of-range value to a signed integer** ([6.3.1.3/3](http://port70.net/%7Ensz/c/c11/n1570.html#6.3.1.3p3)).
</li>
<li>
When an in-range but unrepresentable value is assigned to a floating-point object, how the representable value stored in the object is chosen from between the two nearest representable values ([6.3.1.4/2](http://port70.net/%7Ensz/c/c11/n1570.html#6.3.1.4p2);  [6.3.1.5/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.3.1.5p1); [6.4.4.2/3](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.4.2p3)).
</li>
<li>
**The result of converting an integer to a pointer type**, except for integer constant expressions with value `0` ([6.3.2.3/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.3.2.3p5)).
</li>

### Source form

<li>
The locations within `#pragma` directives where header name tokens are recognized ([6.4/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.4p4)).
</li>
<li>
The characters, including multibyte characters, other than underscore, unaccented Latin letters, universal character names, and decimal digits that may appear in identifiers ([6.4.2.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.2.1p1)).
</li>
<li>
**The number of significant characters in an identifier** ([6.4.2.1/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.2.1p5)).
</li>
<li>
With some exceptions, the manner in which the source characters in an integer character constant are mapped to execution-set characters ([6.4.4.4/2](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.4.4p2); [6.4.4.4/10](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.4.4p10)).
</li>
<li>
The current locale used for computing the value of a wide character constant, and most other aspects of the conversion for many such constants ([6.4.4.4/11](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.4.4p11)).
</li>
<li>
Whether differently-prefixed wide string literal tokens can be concatenated and, if so, the treatment of the resulting multibyte character sequence ([6.4.5/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.5p5))
</li>
<li>
The locale used during translation phase 7 to convert wide string literals to multibyte character sequences, and their value when the result is not representable in the execution character set ([6.4.5/6](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.5p6)).
</li>
<li>
The manner in which header names are mapped to file names ([6.4.7/2](http://port70.net/%7Ensz/c/c11/n1570.html#6.4.7p2)).
</li>

### Evaluation

<li>
Whether and how floating-point expressions are contracted when `FP_CONTRACT` is not used ([6.5/8](http://port70.net/%7Ensz/c/c11/n1570.html#6.5p8)).
</li>
<li>
**The values of the results of the `sizeof` and `_Alignof` operators** ([6.5.3.4/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.5.3.4p5)).
</li>
<li>
The size of the result type of pointer subtraction ([6.5.6/9](http://port70.net/%7Ensz/c/c11/n1570.html#6.5.6p9)).
</li>
<li>
**The result of right-shifting a signed integer with a negative value** ([6.5.7/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.5.7p5)).
</li>

### Runtime behavior

<li>
The extent to which the `register` keyword is effective ([6.7.1/6](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.1p6)).
</li>
<li>
Whether the type of a bitfield declared as `int` is the same type as `unsigned int` or as `signed int` ([6.7.2/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.2p5)).
</li>
<li>
What types bitfields may take, other than optionally-qualified `_Bool`, `signed int`, and `unsigned int`; whether bitfields may have atomic types ([6.7.2.1/5](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.2.1p5)).
</li>
<li>
Aspects of how implementations lay out the storage for bitfields ([6.7.2.1/11](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.2.1p11)).
</li>
<li>
The alignment of non-bitfield members of structures and unions ([6.7.2.1/14](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.2.1p14)).
</li>
<li>
The underlying type for each enumerated type ([6.7.2.2/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.2.2p4)).
</li>
<li>
What constitutes an "access" to an object of `volatile`-qualifed type ([6.7.3/7](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.3p7)).
</li>
<li>
The effectiveness of `inline` function declarations ([6.7.4/6](http://port70.net/%7Ensz/c/c11/n1570.html#6.7.4p6)).
</li>

### Preprocessor

<li>
Whether character constants are converted to integer values the same way in preprocessor conditionals as in ordinary expressions, and whether a single-character constant may have a negative value ([6.10.1/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.1p4)).
</li>
<li>
The locations searched for files designated in an `#include` directive ([6.10.2/2-3](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.2p2)).
</li>
<li>
The manner in which a header name is formed from the tokens of a multi-token `#include` directive ([6.10.2/4](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.2p4)).
</li>
<li>
The limit for `#include` nesting ([6.10.2/6](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.2p6)).
</li>
<li>
Whether a `\` character is inserted before the `\` introducing a universal character name in the result of the preprocessor's `#` operator ([6.10.3.2/2](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.3.2p2)).
</li>
<li>
The behavior of the `#pragma` preprocessing directive for pragmas other than `STDC` ([6.10.6/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.6p1)).
</li>
<li>
The value of the `__DATE__` and `__TIME__` macros if no translation date or time, respectively, is available ([6.10.8.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.8.1p1)).
</li>
<li>
The internal character encoding used for `wchar_t` if macro `__STDC_ISO_10646__` is not defined ([6.10.8.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.8.2p1)).
</li>
<li>
The internal character encoding used for `char32_t` if macro `__STDC_UTF_32__` is not defined ([6.10.8.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#6.10.8.2p1)).
</li>

### Standard Library

### General

- The format of the messages emitted when assertions fail ([7.2.1.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.2.1.1p2)).

### Floating-point environment functions

<li>
Any additional floating-point exceptions beyond those defined by the standard ([7.6/6](http://port70.net/%7Ensz/c/c11/n1570.html#7.6p6)).
</li>
<li>
Any additional floating-point rounding modes beyond those defined by the standard ([7.6/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.6p8)).
</li>
<li>
Any additional floating-point environments beyond those defined by the standard ([7.6/10](http://port70.net/%7Ensz/c/c11/n1570.html#7.6p10)).
</li>
<li>
The default value of the floating-point environment access switch ([7.6.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.6.1p2)).
</li>
<li>
The representation of the floating-point status flags recorded by `fegetexceptflag()` ([7.6.2.2/1](http://port70.net/%7Ensz/c/c11/n1570.html#7.6.2.2p1)).
</li>
<li>
Whether the `feraiseexcept()` function additionally raises the "inexact" floating-point exception whenever it raises the "overflow" or "underflow" floating-point exception ([7.6.2.3/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.6.2.3p2)).
</li>

### Locale-related functions

- The locale strings other than `"C"` supported by `setlocale()` ([7.11.1.1/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.11.1.1p3)).

### Math functions

<li>
The types represented by `float_t` and `double_t` when the `FLT_EVAL_METHOD` macro has a value different from `0`, `1`, and `2` ([7.12/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.12p2)).
</li>
<li>
Any supported floating-point classifications beyond those defined by the standard ([7.12/6](http://port70.net/%7Ensz/c/c11/n1570.html#7.12p6)).
</li>
<li>
The value returned by the `math.h` functions in the event of a domain error ([7.12.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.1p2)).
</li>
<li>
The value returned by the `math.h` functions in the event of a pole error ([7.12.1/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.1p3)).
</li>
<li>
The value returned by the `math.h` functions when the result underflows, and aspects of whether `errno` is set to `ERANGE` and whether a floating-point exception is raised under those circumstances ([7.12.1/6](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.1p6)).
</li>
<li>
The default value of the FP-contraction switch ([7.12.2/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.2p2)).
</li>
<li>
Whether the `fmod()` functions return 0 or raise a domain error when their second argument is 0 ([7.12.10.1/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.10.1p3)).
</li>
<li>
Whether the `remainder()` functions return 0 or raise a domain error when their second argument is 0 ([7.12.10.2/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.10.2p3)).
</li>
<li>
The number of significant bits in the quotient moduli computed by the `remquo()` functions ([7.12.10.3/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.10.3p2)).
</li>
<li>
Whether the `remquo()` functions return 0 or raise a domain error when their second argument is 0 ([7.12.10.3/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.12.10.3p3)).
</li>

### Signals

<li>
The complete set of supported signals, their semantics, and their default handling ([7.14/4](http://port70.net/%7Ensz/c/c11/n1570.html#7.14p4)).
</li>
<li>
When a signal is raised and there is a custom handler associated with that signal, which signals, if any, are blocked for the duration of the execution of the handler ([7.14.1.1/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.14.1.1p3)).
</li>
<li>
Which signals other than `SIGFPE`, `SIGILL`, and `SIGSEGV` cause the behavior upon returning from a custom signal handler to be undefined ([7.14.1.1/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.14.1.1p3)).
</li>
<li>
Which signals are initially configured to be ignored (regardless of their default handling; [7.14.1.1/6](http://port70.net/%7Ensz/c/c11/n1570.html#7.14.1.1p6)).
</li>

### Miscellaneous

- The specific null pointer constant to which macro `NULL` expands ([7.19/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.19p3)).

### File-handling functions

<li>
Whether the last line of a text stream requires a terminating newline ([7.21.2/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.2p2)).
</li>
<li>
The number of null characters automatically appended to a binary stream ([7.21.2/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.2p3)).
</li>
<li>
The initial position of a file opened in append mode ([7.21.3/1](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p1)).
</li>
<li>
Whether a write on a text stream causes the stream to be truncated ([7.21.3/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p2)).
</li>
<li>
Support for stream buffering ([7.21.3/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p3)).
</li>
<li>
Whether zero-length files actually exist ([7.21.3/4](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p4)).
</li>
<li>
The rules for composing valid file names ([7.21.3/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p8)).
</li>
<li>
Whether the same file can simultaneously be open multiple times ([7.21.3/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p8)).
</li>
<li>
The nature and choice of encoding for multibyte characters ([7.21.3/10](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.3p10)).
</li>
<li>
The behavior of the `remove()` function when the target file is open ([7.21.4.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.4.1p2)).
</li>
<li>
The behavior of the `rename()` function when the target file already exists ([7.21.4.2/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.4.2p2)).
</li>
<li>
Whether files created via the `tmpfile()` function are removed in the event that the program terminates abnormally ([7.21.4.3/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.4.3p2)).
</li>
<li>
Which mode changes under which circumstances are permitted via `freopen()` ([7.21.5.4/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.5.4p3)).
</li>

### I/O functions

<li>
Which of the permitted representations of infinite and not-a-number FP values are produced by the printf()-family functions ([7.21.6.1/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.6.1p8)).
</li>
<li>
The manner in which pointers are formatted by the `printf()`-family functions ([7.21.6.1/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.6.1p8)).
</li>
<li>
The behavior of `scanf()`-family functions when the `-` character appears in an internal position of the scanlist of a `[` field ([7.21.6.2/12](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.6.2p12)).
</li>
<li>
Most aspects of the `scanf()`-family functions' handing of `p` fields ([7.21.6.2/12](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.6.2p12)).
</li>
<li>
The `errno` value set by `fgetpos()` on failure ([7.21.9.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.9.1p2)).
</li>
<li>
The `errno` value set by `fsetpos()` on failure ([7.21.9.3/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.9.3p2)).
</li>
<li>
The `errno` value set by `ftell()` on failure ([7.21.9.4/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.21.9.4p3)).
</li>
<li>
The meaning to the `strtod()`-family functions of some supported aspects of a NaN formatting ([7.22.1.3p4](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.1.3p4)).
</li>
<li>
Whether the `strtod()`-family functions set `errno` to `ERANGE` when the result underflows ([7.22.1.3/10](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.1.3p10)).
</li>

### Memory allocation functions

- The behavior of the memory-allocation functions when the number of bytes requested is 0 ([7.22.3/1](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.3p1)).

### System environment functions

<li>
What cleanups, if any, are performed and what status is returned to the host OS when the `abort()` function is called ([7.22.4.1/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.4.1p2)).
</li>
<li>
What status is returned to the host environment when `exit()` is called ([7.22.4.4/5](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.4.4p5)).
</li>
<li>
The handling of open streams and what status is returned to the host environment when `_Exit()` is called ([7.22.4.5/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.4.5p2)).
</li>
<li>
The set of environment names accessible via `getenv()` and the method for altering the environment ([7.22.4.6/2](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.4.6p2)).
</li>
<li>
The return value of the `system()` function ([7.22.4.8/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.22.4.8p3)).
</li>

### Date and time functions

<li>
The local time zone and Daylight Saving time ([7.27.1/1](http://port70.net/%7Ensz/c/c11/n1570.html#7.27.1p1)).
</li>
<li>
The range and precision of times representable via types `clock_t` and `time_t` ([7.27.1/4](http://port70.net/%7Ensz/c/c11/n1570.html#7.27.1p4)).
</li>
<li>
The beginning of the era that serves as the reference for the times returned by the `clock()` function ([7.27.2.1/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.27.2.1p3)).
</li>
<li>
The beginning of the epoch that serves as the reference for the times returned by the `timespec_get()` function (when the time base is `TIME_UTC`; [7.27.2.5/3](http://port70.net/%7Ensz/c/c11/n1570.html#7.27.2.5p3)).
</li>
<li>
The `strftime()` replacement for the `%Z` conversion specifier in the "C" locale ([7.27.3.5/7](http://port70.net/%7Ensz/c/c11/n1570.html#7.27.3.5p7)).
</li>

### Wide-character I/O functions

<li>
Which of the permitted representations of infinite and not-a-number FP values are produced by the `wprintf()`-family functions ([7.29.2.1/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.29.2.1p8)).
</li>
<li>
The manner in which pointers are formatted by the `wprintf()`-family functions ([7.29.2.1/8](http://port70.net/%7Ensz/c/c11/n1570.html#7.29.2.1p8)).
</li>
<li>
The behavior of `wscanf()`-family functions when the `-` character appears in an internal position of the scanlist of a `[` field ([7.29.2.2/12](http://port70.net/%7Ensz/c/c11/n1570.html#7.29.2.2p12)).
</li>
<li>
Most aspects of the `wscanf()`-family functions' handing of `p` fields ([7.29.2.2/12](http://port70.net/%7Ensz/c/c11/n1570.html#7.29.2.2p12)).
</li>
<li>
The meaning to the `wstrtod()`-family functions of some supported aspects of NaN formatting ([7.29.4.1.1/4](http://port70.net/%7Ensz/c/c11/n1570.html#7.29.4.1.1p4)).
</li>
<li>
Whether the `wstrtod()`-family functions set `errno` to `ERANGE` when the result underflows ([7.29.4.1.1/10](http://port70.net/%7Ensz/c/c11/n1570.html#7.29.4.1.1p10)).
</li>

