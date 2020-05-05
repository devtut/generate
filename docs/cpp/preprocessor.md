---
metaTitle: "C++ | Preprocessor"
description: "Include Guards, Conditional logic and cross-platform handling, Macros, X-macros, Preprocessor error messages , Predefined macros, #pragma once, Preprocessor Operators"
---

# Preprocessor


The C preprocessor is a simple text parser/replacer that is run before the actual compilation of the code. Used to extend and ease the use of the C (and later C++) language, it can be used for:

a. **Including other files** using `#include`

b. **Define a text-replacement macro** using `#define`

c. **Conditional Compilation** using`#if` `#ifdef`

d. **Platform/Compiler specific logic** (as an extension of conditional compilation)



## Include Guards


A header file may be included by other header files.   A source file (compilation unit) that includes multiple headers may therefore, indirectly, include some headers more than once.   If such a header file that is included more than once contains definitions, the compiler (after preprocessing) detects a violation of the One Definition Rule (e.g. §3.2 of the 2003 C++ standard) and therefore issues a diagnostic and compilation fails.

Multiple inclusion is prevented using "include guards", which are sometimes also known as header guards or macro guards.    These are implemented using the preprocessor `#define`, `#ifndef`, `#endif` directives.

e.g.

```cpp
// Foo.h
#ifndef FOO_H_INCLUDED 
#define FOO_H_INCLUDED

class Foo    //  a class definition
{
};

#endif

```

The key advantage of using include guards is that they will work with all standard-compliant compilers and preprocessors.

However, include guards also cause some problems for developers, as it is necessary to ensure the macros are unique within all headers used in a project.  Specifically, if two (or more) headers use `FOO_H_INCLUDED` as their include guard, the first of those headers included in a compilation unit will effectively prevent the others from being included.    Particular challenges are introduced if a project uses a number of third-party libraries with header files that happen to use include guards in common.

It is also necessary to ensure that the macros used in include guards do not conflict with any other macros defined in header files.

[Most C++ implementations](https://en.wikipedia.org/wiki/Pragma_once#Portability) also support the `#pragma once` directive which ensures the file is only included once within a single compilation. This is a [**de facto** standard](https://en.wikipedia.org/wiki/De_facto_standard) directive, but it is not part of any ISO C++ standard. For example:

```cpp
// Foo.h
#pragma once

class Foo
{
};

```

While `#pragma once` avoids some problems associated with include guards, a `#pragma` - by definition in the standards - is inherently a compiler-specific hook, and will be silently ignored by compilers that don't support it.   Projects which use `#pragma once` are more difficult to port to compilers that don't support it.

A number of coding guidelines and assurance standards for C++ specifically discourage any use of the preprocessor other than to `#include` header files or for the purposes of placing include guards in headers.



## Conditional logic and cross-platform handling


In a nutshell, conditional pre-processing logic is about making code-logic available or unavailable for compilation using macro definitions.

Three prominent use-cases are:

- different **app profiles** (e.g. debug, release, testing, optimised) that can be candidates of the same app (e.g. with extra logging).
- **cross-platform compiles** - single code-base, multiple compilation platforms.
- utilising a common code-base for multiple **application versions** (e.g. Basic, Premium and Pro versions of a software) - with slightly different features.

**Example a:** A cross-platform approach for removing files (illustrative):

```cpp
#ifdef _WIN32
#include <windows.h> // and other windows system files
#endif
#include <cstdio>

bool remove_file(const std::string &path) 
{
#ifdef _WIN32
  return DeleteFile(path.c_str());
#elif defined(_POSIX_VERSION) || defined(__unix__)
  return (0 == remove(path.c_str()));
#elif defined(__APPLE__)
  //TODO: check if NSAPI has a more specific function with permission dialog
  return (0 == remove(path.c_str()));
#else 
#error "This platform is not supported"
#endif
}

```

Macros like `_WIN32`, `__APPLE__` or `__unix__` are normally predefined by corresponding implementations.

**Example b:** Enabling additional logging for a debug build:

```cpp
void s_PrintAppStateOnUserPrompt()
{
    std::cout << "--------BEGIN-DUMP---------------\n"
              << AppState::Instance()->Settings().ToString() << "\n"
#if ( 1 == TESTING_MODE ) //privacy: we want user details only when testing
              << ListToString(AppState::UndoStack()->GetActionNames())
              << AppState::Instance()->CrntDocument().Name() 
              << AppState::Instance()->CrntDocument().SignatureSHA() << "\n"
#endif
              << "--------END-DUMP---------------\n"
}

```

**Example c:** Enable a premium feature in a separate product build (note: this is illustrative. it is often a better idea to allow a feature to be unlocked without the need to reinstall an application)

```cpp
void MainWindow::OnProcessButtonClick()
{
#ifndef _PREMIUM
    CreatePurchaseDialog("Buy App Premium", "This feature is available for our App Premium users. Click the Buy button to purchase the Premium version at our website");
    return;
#endif
    //...actual feature logic here
}

```

**Some common tricks:**

**Defining symbols at invocation time:**

The preprocessor can be called with predefined symbols (with optional initialisation). For example this command (`gcc -E` runs only the preprocessor)

```cpp
gcc -E -DOPTIMISE_FOR_OS_X -DTESTING_MODE=1 Sample.cpp

```

processes Sample.cpp in the same way as it would if `#define OPTIMISE_FOR_OS_X` and `#define TESTING_MODE 1` were added to the top of Sample.cpp.

**Ensuring a macro is defined:**

If a macro isn't defined and its value is compared or checked, the preprocessor almost always silently assumes the value to be `0`. There are a few ways to work with this. One approach is to assume that the default settings are represented as 0, and any changes (e.g. to the app build profile) needs to be explicitly done (e.g. ENABLE_EXTRA_DEBUGGING=0 by default, set -DENABLE_EXTRA_DEBUGGING=1 to override). Another approach is make all definitions and defaults explicit. This can be achieved using a combination of `#ifndef` and `#error` directives:

```cpp
#ifndef (ENABLE_EXTRA_DEBUGGING)
// please include DefaultDefines.h if not already included.
#    error "ENABLE_EXTRA_DEBUGGING is not defined"
#else
#    if ( 1 == ENABLE_EXTRA_DEBUGGING )
  //code
#    endif
#endif

```



## Macros


Macros are categorized into two main groups: object-like macros and function-like macros. Macros are treated as a token substitution early in the compilation process. This means that large (or repeating) sections of code can be abstracted into a preprocessor macro.

```cpp
// This is an object-like macro
#define    PI         3.14159265358979

// This is a function-like macro.
// Note that we can use previously defined macros
// in other macro definitions (object-like or function-like)
// But watch out, its quite useful if you know what you're doing, but the
// Compiler doesnt know which type to handle, so using inline functions instead
// is quite recommended (But e.g. for Minimum/Maximum functions it is quite useful)
#define    AREA(r)    (PI*(r)*(r))

// They can be used like this:
double pi_macro   = PI;
double area_macro = AREA(4.6);

```

The Qt library makes use of this technique to create a meta-object system by having the user declare the Q_OBJECT macro at the head of the user-defined class extending QObject.

Macro names are usually written in all caps, to make them easier to differentiate from normal code.  This isn't a requirement, but is merely considered good style by many programmers.

When an object-like macro is encountered, it's expanded as a simple copy-paste operation, with the macro's name being replaced with its definition.  When a function-like macro is encountered, both its name and its parameters are expanded.

```cpp
double pi_squared = PI * PI;
// Compiler sees:
double pi_squared = 3.14159265358979 * 3.14159265358979;

double area = AREA(5);
// Compiler sees:
double area = (3.14159265358979*(5)*(5))

```

Due to this, function-like macro parameters are often enclosed within parentheses, as in `AREA()` above.  This is to prevent any bugs that can occur during macro expansion, specifically bugs caused by a single macro parameter being composed of multiple actual values.

```cpp
#define BAD_AREA(r) PI * r * r

double bad_area = BAD_AREA(5 + 1.6);
// Compiler sees:
double bad_area = 3.14159265358979 * 5 + 1.6 * 5 + 1.6;

double good_area = AREA(5 + 1.6);
// Compiler sees:
double good_area = (3.14159265358979*(5 + 1.6)*(5 + 1.6));

```

Also note that due to this simple expansion, care must be taken with the parameters passed to macros, to prevent unexpected side effects.  If the parameter is modified during evaluation, it will be modified each time it is used in the expanded macro, which usually isn't what we want.  This is true even if the macro encloses the parameters in parentheses to prevent expansion from breaking anything.

```cpp
int oops = 5;
double incremental_damage = AREA(oops++);
// Compiler sees:
double incremental_damage = (3.14159265358979*(oops++)*(oops++));

```

Additionally, macros provide no type-safety, leading to hard-to-understand errors about type mismatch.

As programmers normally terminate lines with a semicolon, macros that are intended to be used as standalone lines are often designed to "swallow" a semicolon; this prevents any unintended bugs from being caused by an extra semicolon.

```cpp
#define IF_BREAKER(Func) Func();

if (some_condition)
    // Oops.
    IF_BREAKER(some_func);
else
    std::cout << "I am accidentally an orphan." << std::endl;

```

In this example, the inadvertent double semicolon breaks the `if...else` block, preventing the compiler from matching the `else` to the `if`.  To prevent this, the semicolon is omitted from the macro definition, which will cause it to "swallow" the semicolon immediately following any usage of it.

```cpp
#define IF_FIXER(Func) Func()

if (some_condition)
    IF_FIXER(some_func);
else
    std::cout << "Hooray!  I work again!" << std::endl;

```

Leaving off the trailing semicolon also allows the macro to be used without ending the current statement, which can be beneficial.

```cpp
#define DO_SOMETHING(Func, Param) Func(Param, 2)

// ...

some_function(DO_SOMETHING(some_func, 3), DO_SOMETHING(some_func, 42));

```

Normally, a macro definition ends at the end of the line.  If a macro needs to cover multiple lines, however, a backslash can be used at the end of a line to indicate this.  This backslash must be the last character in the line, which indicates to the preprocessor that the following line should be concatenated onto the current line, treating them as a single line.  This can be used multiple times in a row.

```cpp
#define TEXT "I \
am \
many \
lines."

// ...

std::cout << TEXT << std::endl; // Output:   I am many lines.

```

This is especially useful in complex function-like macros, which may need to cover multiple lines.

```cpp
#define CREATE_OUTPUT_AND_DELETE(Str) \
    std::string* tmp = new std::string(Str); \
    std::cout << *tmp << std::endl; \
    delete tmp;

// ...

CREATE_OUTPUT_AND_DELETE("There's no real need for this to use 'new'.")

```

In the case of more complex function-like macros, it can be useful to give them their own scope to prevent possible name collisions or to cause objects to be destroyed at the end of the macro, similar to an actual function.  A common idiom for this is **do while 0**, where the macro is enclosed in a **do-while** block.  This block is generally **not** followed with a semicolon, allowing it to swallow a semicolon.

```cpp
#define DO_STUFF(Type, Param, ReturnVar) do { \
    Type temp(some_setup_values); \
    ReturnVar = temp.process(Param); \
} while (0)

int x;
DO_STUFF(MyClass, 41153.7, x);

// Compiler sees:

int x;
do {
    MyClass temp(some_setup_values);
    x = temp.process(41153.7);
} while (0);

```

There are also variadic macros; similarly to variadic functions, these take a variable number of arguments, and then expand them all in place of a special "Varargs" parameter, `__VA_ARGS__`.

```cpp
#define VARIADIC(Param, ...) Param(__VA_ARGS__)

VARIADIC(printf, "%d", 8);
// Compiler sees:
printf("%d", 8);

```

Note that during expansion, `__VA_ARGS__` can be placed anywhere in the definition, and will be expanded correctly.

```cpp
#define VARIADIC2(POne, PTwo, PThree, ...) POne(PThree, __VA_ARGS__, PTwo)

VARIADIC2(some_func, 3, 8, 6, 9);
// Compiler sees:
some_func(8, 6, 9, 3);

```

In the case of a zero-argument variadic parameter, different compilers will handle the trailing comma differently.  Some compilers, such as Visual Studio, will silently swallow the comma without any special syntax.  Other compilers, such as GCC, require you to place `##` immediately before `__VA_ARGS__`.  Due to this, it is wise to conditionally define variadic macros when portability is a concern.

```cpp
// In this example, COMPILER is a user-defined macro specifying the compiler being used.

#if       COMPILER == "VS"
    #define VARIADIC3(Name, Param, ...) Name(Param, __VA_ARGS__)
#elif     COMPILER == "GCC"
    #define VARIADIC3(Name, Param, ...) Name(Param, ##__VA_ARGS__)
#endif /* COMPILER */

```



## X-macros


An idiomatic technique for generating repeating code structures at compile time.

An X-macro consists of two parts: the list, and the execution of the list.

Example:

```cpp
#define LIST \
    X(dog)   \
    X(cat)   \
    X(racoon)

// class Animal {
//  public:
//    void say();
// };

#define X(name) Animal name;
LIST
#undef X

int main() {
#define X(name) name.say();
    LIST
#undef X

    return 0;
}

```

which is expanded by the preprocessor into the following:

```cpp
Animal dog;
Animal cat;
Animal racoon;

int main() {
    dog.say();
    cat.say();
    racoon.say();

    return 0;
}    

```

As lists become bigger (let's say, more than 100 elements), this technique helps to avoid excessive copy-pasting.

Source: [https://en.wikipedia.org/wiki/X_Macro](https://en.wikipedia.org/wiki/X_Macro)

See also: [X-macros](http://stackoverflow.com/documentation/c/628/x-macros)

If defining a seamingly irrelevant `X` before using `LIST` is not to your liking, you can pass a macro name as an argument as well:

```cpp
#define LIST(MACRO) \
    MACRO(dog) \
    MACRO(cat) \
    MACRO(racoon)

```

Now, you explicitly specify which macro should be used when expanding the list, e.g.

```cpp
#define FORWARD_DECLARE_ANIMAL(name) Animal name;
LIST(FORWARD_DECLARE_ANIMAL)

```

If each invocation of the `MACRO` should take additional parameters - constant with respect to the list, variadic macros can be used

```cpp
//a walkaround for Visual studio
#define EXPAND(x) x

#define LIST(MACRO, ...) \
    EXPAND(MACRO(dog, __VA_ARGS__)) \
    EXPAND(MACRO(cat, __VA_ARGS__)) \
    EXPAND(MACRO(racoon, __VA_ARGS__))

```

The first argument is supplied by the `LIST`, while the rest is provided by the user in the `LIST` invocation. For example:

```cpp
#define FORWARD_DECLARE(name, type, prefix) type prefix##name;
LIST(FORWARD_DECLARE,Animal,anim_)
LIST(FORWARD_DECLARE,Object,obj_)

```

will expand to

```cpp
Animal anim_dog;
Animal anim_cat;
Animal anim_racoon;
Object obj_dog;
Object obj_cat;
Object obj_racoon;        

```



## Preprocessor error messages 


Compile errors can be generated using the preprocessor. This is useful for a number of reasons some of which include, notifying a user if they are on an unsupported platform or an unsupported compiler.

e.g. Return Error if gcc version is 3.0.0 or earlier.

```cpp
#if __GNUC__ < 3
#error "This code requires gcc > 3.0.0"
#endif

```

e.g. Return Error if compiling on an Apple computer.

```cpp
#ifdef __APPLE__
#error "Apple products are not supported in this release"
#endif

```



## Predefined macros


Predefined macros are those that the compiler defines (in contrast to those user defines in the source file). Those macros must not be re-defined or undefined by user.

The following macros are predefined by the C++ standard:

- `__LINE__` contains the line number of the line this macro is used on, and can be changed by the `#line` directive.
- `__FILE__` contains the filename of the file this macro is used in, and can be changed by the `#line` directive.
- `__DATE__` contains date (in `"Mmm dd yyyy"` format) of the file compilation, where **Mmm** is formatted as if obtained by a call to `std::asctime()`.
- `__TIME__` contains time (in `"hh:mm:ss"` format) of the file compilation.
- `__cplusplus` is defined by (conformant) C++ compilers while compiling C++ files. Its value is the standard version the compiler is **fully** conformant with, i.e. `199711L` for C++98 and C++03, `201103L` for C++11 and `201402L` for C++14 standard.

- `__STDC_HOSTED__` is defined to `1` if the implementation is **hosted**, or `0` if it is **freestanding**.

- `__STDCPP_DEFAULT_NEW_ALIGNMENT__` contains a `size_t` literal, which is the alignment used for a call to alignment-unaware `operator new`.

Additionally, the following macros are allowed to be predefined by implementations, and may or may not be present:

- `__STDC__` has implementation-dependent meaning, and is usually defined only when compiling a file as C, to signify full C standard compliance.  (Or never, if the compiler decides not to support this macro.)

- `__STDC_VERSION__` has implementation-dependent meaning, and its value is usually the C version, similarly to how `__cplusplus` is the C++ version.  (Or is not even defined, if the compiler decides not to support this macro.)
- `__STDC_MB_MIGHT_NEQ_WC__` is defined to `1`, if values of the narrow encoding of the basic character set might not be equal to the values of their wide counterparts (e.g. if `(uintmax_t)'x' != (uintmax_t)L'x'`)
- `__STDC_ISO_10646__` is defined if `wchar_t` is encoded as Unicode, and expands to an integer constant in the form `yyyymmL`, indicating the latest Unicode revision supported.
- `__STDCPP_STRICT_POINTER_SAFETY__` is defined to `1`, if the implementation has **strict pointer safety** (otherwise it has **relaxed pointer safety**)
- `__STDCPP_THREADS__` is defined to `1`, if the program can have more than one thread of execution (applicable to **freestanding implementation** — **hosted implementations** can always have more than one thread)

It is also worth mentioning `__func__`, which is not an macro, but a predefined function-local variable. It contains the name of the function it is used in, as a static character array in an implementation-defined format.

On top of those standard predefined macros, compilers can have their own set of predefined macros. One must refer to the compiler documentation to learn those. E.g.:

- [gcc](https://gcc.gnu.org/onlinedocs/cpp/Predefined-Macros.html)
- [Microsoft Visual C++](https://msdn.microsoft.com/en-us/library/b0084kay.aspx#Anchor_2)
- [clang](http://clang.llvm.org/docs/LanguageExtensions.html#builtin-macros)
- [Intel C++ Compiler](https://software.intel.com/en-us/node/514528)

Some of the macros are just to query support of some feature:

```cpp
#ifdef __cplusplus // if compiled by C++ compiler
extern "C"{ // C code has to be decorated
   // C library header declarations here
}
#endif

```

Others are very useful for debugging:

```cpp
bool success = doSomething( /*some arguments*/ );
if( !success ){
    std::cerr << "ERROR: doSomething() failed on line " << __LINE__ - 2
              << " in function " << __func__ << "()"
              << " in file " << __FILE__
              << std::endl;
}

```

And others for trivial version control:

```cpp
int main( int argc, char *argv[] ){
    if( argc == 2 && std::string( argv[1] ) == "-v" ){
        std::cout << "Hello World program\n"
                  << "v 1.1\n" // I have to remember to update this manually
                  << "compiled: " << __DATE__ << ' ' << __TIME__ // this updates automagically
                  << std::endl;
    }
    else{
        std::cout << "Hello World!\n";
    }
}

```



## #pragma once


[Most, but not all, C++ implementations](https://en.wikipedia.org/wiki/Pragma_once#Portability) support the `#pragma once` directive which ensures the file is only included once within a single compilation. It is not part of any ISO C++ standard. For example:

```cpp
// Foo.h
#pragma once

class Foo
{
};

```

While `#pragma once` avoids some problems associated with [include guards](http://stackoverflow.com/documentation/c%2b%2b/1098/preprocessor/3525/include-guards), a `#pragma` - by definition in the standards - is inherently a compiler-specific hook, and will be silently ignored by compilers that don't support it. Projects which use `#pragma once` must be modified to be standard-compliant.

With some compilers - particularly those that employ [precompiled headers](https://en.wikipedia.org/wiki/Precompiled_header) - `#pragma once` can result in a considerable speedup of the compilation process.  Similarly, some preprocessors achieve speedup of compilation by tracking which headers have employed include guards.  The net benefit, when both `#pragma once` and include guards are employed, depends on the implementation and can be either an increase or decrease of compilation times.

`#pragma once` combined with [include guards](http://stackoverflow.com/documentation/c%2b%2b/1098/preprocessor/3525/include-guards) was the recommended layout for header files when writing MFC based applications on windows, and was generated by Visual Studio’s `add class`, `add dialog`, `add windows` wizards.   Hence it is very common to find them combined in C++ Windows Applicants.



## Preprocessor Operators


`#` operator or stringizing operator is used to convert a Macro parameter to a string literal. It can only be used with the Macros having arguments.

```cpp
// preprocessor will convert the parameter x to the string literal x
#define PRINT(x) printf(#x "\n")

PRINT(This line will be converted to string by preprocessor);
// Compiler sees
printf("This line will be converted to string by preprocessor""\n");

```

Compiler concatenate two strings and the final `printf()` argument will be a string literal with newline character at its end.

Preprocessor will ignore the spaces before or after the macro argument. So below print statement will give us the same result.

```cpp
PRINT(   This line will be converted to string by preprocessor );

```

If the parameter of the string literal requires an escape sequence like before a double quote() it will automatically be inserted by the preprocessor.

```cpp
PRINT(This "line" will be converted to "string" by preprocessor); 
// Compiler sees
printf("This \"line\" will be converted to \"string\" by preprocessor""\n");

```

`##` operator or Token pasting operator is used to concatenate two parameters or tokens of a Macro.

```cpp
// preprocessor will combine the variable and the x
#define PRINT(x) printf("variable" #x " = %d", variable##x)

int variableY = 15;
PRINT(Y);
//compiler sees
printf("variable""Y"" = %d", variableY);

```

and the final output will be

```cpp
variableY = 15

```



#### Remarks


Preprocessor statements are executed before your source files are handed to the compiler. They are capable of very low level conditional logic. Since preprocessor constructs (e.g. object-like macros) aren't typed like normal functions (the preprocessing step happens before compilation) the compiler cannot enforce type checks, they should therefore be carefully used.

