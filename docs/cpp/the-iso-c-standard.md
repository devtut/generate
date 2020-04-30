---
metaTitle: "The ISO C++ Standard"
description: "Current Working Drafts, C++17, C++11, C++14, C++98, C++03, C++20"
---

# The ISO C++ Standard


In 1998, the there was a first publication of the standard making C++ an internally standardized language. From that time, C++ has evolved resulting in different dialects of C++. On this page, you can find an overview of all different standards and their changes compared to the previous version. The details on how to use these features is described on more specialized pages.



## Current Working Drafts


All published ISO standards are available for sale from the ISO store ( [http://www.iso.org](http://www.iso.org) ).
The working drafts of the C++ standards are publicly available for free though.

The different versions of the standard:

- Upcoming (Sometimes referred as C++20 or C++2a): [Current working draft](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2017/n4659.pdf) ([HTML-version](http://eel.is/c++draft/))
- Proposed (Sometimes referred as C++17 or C++1z): [March 2017 working draft N4659](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2017/n4659.pdf).
- C++14 (Sometimes referred as C++1y): [November 2014 working draft N4296](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4296.pdf)
- C++11 (Sometimes referred as C++0x): [February 2011 working draft N3242](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2011/n3242.pdf)
- C++03
- C++98



## C++17


The C++17 standard is feature complete and has been proposed for standardization. In compilers with experimental support for these features, it is usually referred to as C++1z.

### Language Extensions

- [Fold Expressions](http://stackoverflow.com/documentation/c%2b%2b/2676/fold-expressions)
- [declaring non-type template arguments with `auto`](https://stackoverflow.com/documentation/c%2b%2b/460/templates/16205/declaring-non-type-template-arguments-with-auto)
- [Guaranteed copy elision](https://stackoverflow.com/documentation/c%2b%2b/2489/copy-elision/8222/guaranteed-copy-elision)
- [Template parameter deduction for constructors](https://stackoverflow.com/documentation/c%2b%2b/460/templates/16206/template-parameter-deduction-for-constructors)
- [Structured bindings](http://stackoverflow.com/documentation/c%2b%2b/487/functions-with-multiple-return-values/3384/structured-bindings)
- [Compact nested namespaces](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/14269/compact-nested-namespaces)
- [New attributes: `[[fallthrough]]`, `[[nodiscard]]`, `[[maybe_unused]]`](https://stackoverflow.com/documentation/c%2b%2b/5251/attributes)
- [Default message for `static_assert`](https://stackoverflow.com/documentation/c%2b%2b/3822/static-assert/13229/static-assert)
- Initializers in `if` and `switch`
- Inline variables
- [`if constexpr`](https://stackoverflow.com/documentation/c%2b%2b/3899/constexpr/28625/static-if-statement)
- Order of expression evaluation guarantees
- Dynamic memory allocation for over-aligned data

### Library Extensions

- [`std::optional`](http://stackoverflow.com/documentation/c%2b%2b/2423/optionals)
- [`std::variant`](http://stackoverflow.com/documentation/c%2b%2b/5239/stdvariant)
- [`std::string_view`](http://stackoverflow.com/documentation/c%2b%2b/488/stdstring/6571/using-the-stdstring-view-class)
- `merge()` and `extract()` for associative containers
- [A file system library](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3505.html) with [the `<filesystem>` header](http://en.cppreference.com/w/cpp/filesystem).
- [Parallel versions of most of the standard algorithms](https://isocpp.org/files/papers/P0024R2.html) (in [the `<algorithm>` header](http://en.cppreference.com/w/cpp/algorithm)).
- [Addition of mathematical special functions](https://isocpp.org/files/papers/P0226R1.pdf) in [the `<cmath>` header](http://en.cppreference.com/w/cpp/numeric/special_math).
- Moving nodes between map<>, unordered_map<>, set<>, and unordered_set<>



## C++11


The C++11 standard is a major extension to the C++ standard. Below you can find an overview of the changes as they have been grouped on [the isocpp FAQ](https://isocpp.org/faq) with links to more detailed documentation.

### Language Extensions

### [General Features](https://isocpp.org/wiki/faq/cpp11-language)

- [auto](https://stackoverflow.com/documentation/c%2B%2B/2421/auto)
- [decltype](http://stackoverflow.com/documentation/c%2B%2B/7840/variable-declaration-keywords/18513/decltype)
- [Range-for statement](http://stackoverflow.com/documentation/c%2b%2b/589/loops/1926/range-based-for#t=201612030856396632579)
- Initializer lists
- Uniform initialization syntax and semantics
- [Rvalue references](http://stackoverflow.com/documentation/c%2B%2B/763/value-categories/2607/rvalue) and [move semantics](https://stackoverflow.com/documentation/c%2B%2B/2129/move-semantics)
- [Lambdas](https://stackoverflow.com/documentation/c%2B%2B/572/lambdas)
- [noexcept](http://stackoverflow.com/documentation/c%2B%2B/4891/keywords/18664/noexcept) to prevent exception propagation
- [constexpr](https://stackoverflow.com/documentation/c%2B%2B/3899/constexpr)
- [nullptr](http://stackoverflow.com/documentation/c%2B%2B/7836/literals/18669/nullptr) – a null pointer literal
- Copying and rethrowing exceptions
- Inline namespaces
- User-defined literals

### [Classes](https://isocpp.org/wiki/faq/cpp11-language-classes)

- =default and =delete
- Control of default move and copy
- Delegating constructors
- In-class member initializers
- Inherited constructors
- Override controls: override
- Override controls: final
- Explicit conversion operators

### [Other Types](https://isocpp.org/wiki/faq/cpp11-language-types)

- enum class
- long long – a longer integer
- Extended integer types
- Generalized unions
- Generalized PODs

### [Templates](https://isocpp.org/wiki/faq/cpp11-language-templates)

- Extern templates
- Template aliases
- Variadic templates
- Local types as template arguments

### [Concurrency](https://isocpp.org/wiki/faq/cpp11-language-concurrency)

- Concurrency memory model
- Dynamic initialization and destruction with concurrency
- [Thread-local storage](http://stackoverflow.com/documentation/c%2B%2B/699/threading/18759/thread-local-storage)

### [Miscellaneous Language Features](https://isocpp.org/wiki/faq/cpp11-language-misc)

- What is the value of __cplusplus for C++11?
- Suffix return type syntax
- Preventing narrowing
- Right-angle brackets
- [static_assert compile-time assertions](https://stackoverflow.com/documentation/c%2B%2B/3822/static-assert)
- Raw string literals
- Attributes
- Alignment
- C99 features

### Library Extensions

### [General](https://isocpp.org/wiki/faq/cpp11-library)

- unique_ptr
- shared_ptr
- weak_ptr
- Garbage collection ABI
- tuple
- Type traits
- function and bind
- Regular Expressions
- Time utilities
- Random number generation
- Scoped allocators

### [Containers and Algorithms](https://isocpp.org/wiki/faq/cpp11-library-stl)

- Algorithms improvements
- Container improvements
- unordered_* containers
- std::array
- forward_list

### [Concurrency](https://isocpp.org/wiki/faq/cpp11-library-concurrency)

- [Threads](http://stackoverflow.com/documentation/c%2B%2B/699/threading)
- Mutual exclusion
- [Locks](http://stackoverflow.com/documentation/c%2B%2B/9794/thread-synchronization-structures/30170/recursive-lock)
- [Condition variables](http://stackoverflow.com/documentation/c%2B%2B/699/threading/13552/using-condition-variables)
- [Atomics](http://stackoverflow.com/documentation/c%2B%2B/7475/stdatomics)
- [Futures and promises](http://stackoverflow.com/documentation/c%2B%2B/9840/futures-and-promises)
- [async](http://stackoverflow.com/documentation/c%2B%2B/699/threading/4745/using-stdasync-instead-of-stdthread)
- Abandoning a process



## C++14


The C++14 standard is often referred to as a bugfix for C++11. It contains only a limited list of changes of which most are extensions to the new features in C++11. Below you can find an overview of the changes as they have been grouped on [the isocpp FAQ](https://isocpp.org/faq) with links to more detailed documentation.

### [Language Extensions](https://isocpp.org/wiki/faq/cpp14-language)

- Binary literals
- Generalized return type deduction
- decltype(auto)
- [Generalized lambda captures](http://stackoverflow.com/documentation/c%2B%2B/572/lambdas/1950/generalized-capture)
- [Generic lambdas](https://stackoverflow.com/documentation/c%2B%2B/2421/auto/17484/generic-lambda-c14)
- Variable templates
- Extended `constexpr`
- [The `[[deprecated]]` attribute](https://stackoverflow.com/documentation/c%2B%2B/5251/attributes/18756/deprecated-and-deprecatedreason)
- [Digit separators](https://stackoverflow.com/documentation/c%2b%2b/10595/digit-separators#t=201706202222525348956)

### [Library Extensions](https://isocpp.org/wiki/faq/cpp14-library)

- Shared locking
- User-defined literals for `std::` types
- [`std::make_unique`](http://en.cppreference.com/w/cpp/memory/unique_ptr/make_unique)
- Type transformation `_t` aliases
- [Addressing tuples by type](http://stackoverflow.com/q/29719558/3235496) (e.g. `get<string>(t)`)
- [Transparent Operator Functors](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2012/n3421) (e.g. `greater<>(x)`)
- [`std::quoted`](http://en.cppreference.com/w/cpp/io/manip/quoted)

### Deprecated / Removed

- [`std::gets`](http://en.cppreference.com/w/cpp/io/c/gets) was deprecated in C++11 and removed from C++14
- [`std::random_shuffle`](http://en.cppreference.com/w/cpp/algorithm/random_shuffle) is deprecated



## C++98


C++98 is the first standardized version of C++. As it was developed as an extension to C, many of the features which set apart C++ from C are added.

### Language Extensions (in respect to C89/C90)

- Classes, Derived classes, virtual member functions, const member functions
- Function overloading, Operator overloading
- Single line comments (Has been introduced in the C-languague with C99 standard)
- References
- new and delete
- boolean type (Has been introduced in the C-languague with C99 standard)
- templates
- namespaces
- exceptions
- specific casts

### Library Extensions

- The Standard Template Library



## C++03


The C++03 standard mainly addresses defect reports of the C++98 standard. Apart from these defects, it only adds one new feature.

### Language Extensions

- [Value initalization](http://stackoverflow.com/a/620402/2466431)



## C++20


C++20 is the upcoming standard of C++, currently in development, based upon the C++17 standard. It's progress can be tracked on the [official ISO cpp website](https://isocpp.org/std/status).

The following features are simply what has been accepted for the next release of the C++ standard, targeted for 2020.

### Language Extensions

No language extensions have been accepted for now.

### Library Extensions

No library extensions have been accepted for now.



#### Remarks


When C++ is mentioned, often a reference is made to "the Standard". But what is exactly that standard?

C++ has a long history. Started as a small project by Bjarne Stroustrup within Bell Labs, by the early 90's it had become quite popular. Multiple companies were creating C++ compilers so that users could run their C++ compilers on a wide range of computers. But in order to facilitate this, all these competing compilers should share a single definition of the language.

At that point, the C language had successfully been standardized. This means that a formal description of the language was written. This was submitted to the American National Standards Institute (ANSI), which opened up the document for review and subsequently published it in 1989. One year later, the International Organization for Standards (Because it would have different acronyms in different languages they chose one form, ISO, derived from the Greek isos, meaning equal.) adopted the American standard as an International Standard.

For C++, it was clear from the beginning that there was an international interest. A workgroup within ISO was started (known as WG21, within SubCommittee 22). This workgroup drafted a first standard around 1995. But as we programmers know, there's nothing more dangerous to a planned delivery than last minute features, and that happened to C++ as well. In 1995, a cool new library named the STL surfaced, and the people working in WG21 decided to add a slimmed-down version to the C++ draft standard. Naturally, this caused the deadlines to be missed and only 3 years later did the document become final. ISO is a very formal organization, so the C++ Standard was christened with the not very marketable name of ISO/IEC 14882. As standards can be updated, this exact version became known as 14882:1998.

And indeed there was a demand to update the Standard. The Standard is a very thick document, which aims to exactly describe how C++ compilers should work. Even a slight ambiguity can be worth fixing, so by 2003 an update was released as 14882:2003. However, this did not add any feature to C++; the new features were scheduled for the second update.

Informally, this second update was known as C++0x, because it wasn't known whether this would take until 2008 or 2009. Well - that version also got a slight delay, which is why it became 14882:2011.

Luckily, WG21 decided not to let that happen again. C++11 was well-received and let to a renewed interest in C++. So, to keep the momentum, the third update went from planning to publishing in 3 years, to become 14882:2014.

The work didn't stop there, either. The C++17 standard has been proposed and the work for C++20 has been started.

