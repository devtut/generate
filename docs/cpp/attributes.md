---
metaTitle: "C++ | Attributes"
description: "[[fallthrough]], [[deprecated]] and [[deprecated(reason)]], [[nodiscard]], [[noreturn]], [[maybe_unused]]"
---

# Attributes




## [[fallthrough]]


Whenever a `case` is ended in a `switch`, the code of the next case will get executed. This last one can be prevented by using the ´break` statement.
As this so-called fallthrough behavior can introduce bugs when not intended, several compilers and static analyzers give a warning on this.

From C++17 on, a standard attribute was introduced to indicate that the warning is not needed when the code is meant to fall through.
Compilers can safely give warnings when a case is ended without `break` or `[[fallthrough]]` and has at least one statement.

```cpp
switch(input) {
    case 2011:
    case 2014:
    case 2017:
        std::cout << "Using modern C++" << std::endl;
        [[fallthrough]]; // > No warning
    case 1998:
    case 2003:
        standard = input;
}

```

See [the proposal](https://isocpp.org/files/papers/P0188R0.pdf) for more detailed examples on how `[[fallthrough]]` can be used.



## [[deprecated]] and [[deprecated("reason")]]


C++14 introduced a standard way of deprecating functions via attributes. `[[deprecated]]` can be used to indicate that a function is deprecated. `[[deprecated("reason")]]` allows adding a specific reason which can be shown by the compiler.

```cpp
void function(std::unique_ptr<A> &&a);

// Provides specific message which helps other programmers fixing there code
[[deprecated("Use the variant with unique_ptr instead, this function will be removed in the next release")]]
void function(std::auto_ptr<A> a);

// No message, will result in generic warning if called.
[[deprecated]]
void function(A *a);

```

This attribute may be applied to:

- the declaration of a class
- a typedef-name
- a variable
- a non-static data member
- a function
- an enumeration
- a template specialization

(ref. [c++14 standard draft](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3797.pdf): 7.6.5  Deprecated attribute)



## [[nodiscard]]


The `[[nodiscard]]` attribute can be used to indicate that the return value of a function shouldn't be ignored when you do a function call. If the return value is ignored, the compiler should give a warning on this. The attribute can be added to:

- A function definition
- A type

Adding the attribute to a type has the same behaviour as adding the attribute to every single function which returns this type.

```cpp
template<typename Function>
[[nodiscard]] Finally<std::decay_t<Function>> onExit(Function &&f);

void f(int &i) {
    assert(i == 0);                    // Just to make comments clear!
    ++i;                               // i == 1
    auto exit1 = onExit([&i]{ --i; }); // Reduce by 1 on exiting f()
    ++i;                               // i == 2
    onExit([&i]{ --i; });              // BUG: Reducing by 1 directly
                                       //      Compiler warning expected
    std::cout << i << std::end;        // Expected: 2, Real: 1
}

```

See [the proposal](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/p0068r0.pdf) for more detailed examples on how `[[nodiscard]]` can be used.

**Note:** The implementation details of `Finally`/`onExit` are omitted in the example, see [Finally/ScopeExit](http://stackoverflow.com/documentation/c%2b%2b/1320/raii-resource-acquisition-is-initialization/4551/finally-scopeexit).



## [[noreturn]]


C++11 introduced the `[[noreturn]]` attribute.
It can be used for a function to indicate that the function does not return to the caller by either executing a **return** statement, or by reaching the end if it's body (it is important to note that this does not apply to `void` functions, since they do return to the caller, they just do not return any value). Such a function may end by calling `std::terminate` or `std::exit`, or by throwing an exception. It is also worth noting that such a function can return by executing `longjmp`.

For instance, the function below will always either throw an exception or call `std::terminate`, so it is a good candidate for `[[noreturn]]`:

```cpp
[[noreturn]] void ownAssertFailureHandler(std::string message) {
    std::cerr << message << std::endl;
    if (THROW_EXCEPTION_ON_ASSERT)
        throw AssertException(std::move(message));
    std::terminate();
}

```

This kind of functionality allows the compiler to end a function without a return statement if it knows the code will never be executed. Here, because the call to `ownAssertFailureHandler` (defined above) in the code below will never return, the compiler does not need to add code below that call:

```cpp
std::vector<int> createSequence(int end) {
    if (end > 0) {
        std::vector<int> sequence;
        sequence.reserve(end+1);
        for (int i = 0; i <= end; ++i)
            sequence.push_back(i);
        return sequence;
    }
    ownAssertFailureHandler("Negative number passed to createSequence()"s);
    // return std::vector<int>{}; //< Not needed because of [[noreturn]]
}

```

It is undefined behavior if the function will actually return, so the following is not allowed:

```cpp
[[noreturn]] void assertPositive(int number) {
    if (number >= 0)
        return;
    else
        ownAssertFailureHandler("Positive number expected"s); //< [[noreturn]]
}

```

Note that the `[[noreturn]]` is mostly used in void functions. However, this is not a requirement, allowing the functions to be used in generic programming:

```cpp
template<class InconsistencyHandler>
double fortyTwoDivideBy(int i) {
    if (i == 0)
         i = InconsistencyHandler::correct(i);
    return 42. / i;
}

struct InconsistencyThrower {
    static [[noreturn]] int correct(int i) { ownAssertFailureHandler("Unknown inconsistency"s); }
}

struct InconsistencyChangeToOne {
    static int correct(int i) { return 1; }
}

double fortyTwo = fortyTwoDivideBy<InconsistencyChangeToOne>(0);
double unreachable = fortyTwoDivideBy<InconsistencyThrower>(0);

```

The following standard library functions have this attribute:

- std::abort
- std::exit
- std::quick_exit
- std::unexpected
- std::terminate
- std::rethrow_exception
- std::throw_with_nested
- std::nested_exception::rethrow_nested



## [[maybe_unused]]


The `[[maybe_unused]]` attribute is created for indicating in code that certain logic might not be used. This if often linked to preprocessor conditions where this might be used or might not be used. As compilers can give warnings on unused variables, this is a way of suppressing them by indicating intent.

A typical example of variables which are needed in debug builds while unneeded in production are return values indicating success. In the debug builds, the condition should be asserted, though in production these asserts have been removed.

```cpp
[[maybe_unused]] auto mapInsertResult = configuration.emplace("LicenseInfo", stringifiedLicenseInfo);
assert(mapInsertResult.second); // We only get called during startup, so we can't be in the map

```

A more complex example are different kind of helper functions which are in an unnamed namespace. If these functions aren't used during compilation, a compiler might give a warning on them. Ideally you would like to guard them with the same preprocessor tags as the caller, though as this might become complex the `[[maybe_unused]]` attribute is a more maintainable alternative.

```cpp
namespace {
    [[maybe_unused]] std::string createWindowsConfigFilePath(const std::string &relativePath);
    // TODO: Reuse this on BSD, MAC ...
    [[maybe_unused]] std::string createLinuxConfigFilePath(const std::string &relativePath);
}

std::string createConfigFilePath(const std::string &relativePath) {
#if OS == "WINDOWS"
      return createWindowsConfigFilePath(relativePath);
#elif OS == "LINUX"
      return createLinuxConfigFilePath(relativePath);
#else
#error "OS is not yet supported"
#endif
}

```

See [the proposal](https://isocpp.org/files/papers/P0212R0.pdf) for more detailed examples on how `[[maybe_unused]]` can be used.



#### Syntax


<li>
[[details]]: Simple no-argument attribute
</li>
<li>
[[details(arguments)]]: Attribute with arguments
</li>
<li>
__attribute(details): Non-standard GCC/Clang/IBM specific
</li>
<li>
__declspec(details): Non-standard MSVC specific
</li>

