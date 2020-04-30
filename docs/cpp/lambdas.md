---
metaTitle: "Lambdas"
description: "What is a lambda expression?, Specifying the return type, Capture by value, Recursive lambdas, Default capture, Capture by reference, Generic lambdas, Class lambdas and capture of this, Generalized capture, Conversion to function pointer, Using lambdas for inline parameter pack unpacking, Porting lambda functions to C++03 using functors"
---

# Lambdas




## What is a lambda expression?


A **lambda expression** provides a concise way to create simple function objects. A lambda expression is a prvalue whose result object is called [closure object](https://en.wikipedia.org/wiki/Closure_(computer_programming)), which behaves like a function object.

The name 'lambda expression' originates from [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus), which is a mathematical formalism invented in the 1930s by Alonzo Church to investigate questions about logic and computability. Lambda calculus formed the basis of [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)), a functional programming language. Compared to lambda calculus and LISP, C++ lambda expressions share the properties of being unnamed, and to capture variables from the surrounding context, but they lack the ability to operate on and return functions.

A lambda expression is often used as an argument to functions that take a callable object. That can be simpler than creating a named function, which would be only used when passed as the argument. In such cases, lambda expressions are generally preferred because they allow defining the function objects inline.

A lambda consists typically of three parts: a capture list `[]`, an optional parameter list `()` and a body `{}`, all of which can be empty:

```cpp
[](){}                // An empty lambda, which does and returns nothing

```

**Capture list**

`[]` is the **capture list**. By default, variables of the enclosing scope cannot be accessed by a lambda. **Capturing** a variable makes it accessible inside the lambda, either [as a copy](http://stackoverflow.com/documentation/c%2b%2b/572/lambdas/1856/capture-by-value#t=201607271323349545754) or [as a reference](http://stackoverflow.com/documentation/c%2b%2b/572/lambdas/1951/capture-by-reference#t=201607271323349545754). Captured variables become a part of the lambda; in contrast to function arguments, they do not have to be passed when calling the lambda.

```cpp
int a = 0;                       // Define an integer variable
auto f = []()   { return a*9; }; // Error: 'a' cannot be accessed
auto f = [a]()  { return a*9; }; // OK, 'a' is "captured" by value
auto f = [&a]() { return a++; }; // OK, 'a' is "captured" by reference
                                 //      Note: It is the responsibility of the programmer
                                 //      to ensure that a is not destroyed before the
                                 //      lambda is called.
auto b = f();                    // Call the lambda function. a is taken from the capture list and not passed here.

```

**Parameter list**

`()` is the **parameter list**, which is almost the same as in regular functions. If the lambda takes no arguments, these parentheses can be omitted (except if you need to declare the lambda `mutable`). These two lambdas are equivalent:

```cpp
auto call_foo  = [x](){ x.foo(); };
auto call_foo2 = [x]{ x.foo(); };

```

The parameter list can use the placeholder type `auto` instead of actual types. By doing so, this argument behaves like a template parameter of a function template. Following lambdas are equivalent when you want to sort a vector in generic code:

```cpp
auto sort_cpp11 = [](std::vector<T>::const_reference lhs, std::vector<T>::const_reference rhs) { return lhs < rhs; }; 
auto sort_cpp14 = [](const auto &lhs, const auto &rhs) { return lhs < rhs; }; 

```

**Function body**

`{}` is the **body**, which is the same as in regular functions.

**Calling a lambda**

A lambda expression's result object is a [closure](https://en.wikipedia.org/wiki/Closure_(computer_programming)), which can be called using the `operator()` (as with other function objects):

```cpp
int multiplier = 5;
auto timesFive = [multiplier](int a) { return a * multiplier; }; 
std::out << timesFive(2); // Prints 10

multiplier = 15;
std::out << timesFive(2); // Still prints 2*5 == 10

```

**Return Type**

By default, the return type of a lambda expression is deduced.

```cpp
[](){ return true; };

```

In this case the return type is `bool`.

You can also manually specify the return type using the following syntax:

```cpp
[]() -> bool { return true; };

```

**Mutable Lambda**

Objects captured by value in the lambda are by default immutable. This is because the `operator()` of the generated closure object is `const` by default.

```cpp
auto func = [c = 0](){++c; std::cout << c;};  // fails to compile because ++c
                                              // tries to mutate the state of
                                              // the lambda.

```

Modifying can be allowed by using the keyword `mutable`, which make the closer object's `operator()` non-`const`:

```cpp
auto func = [c = 0]() mutable {++c; std::cout << c;};

```

If used together with the return type, `mutable` comes before it.

```cpp
auto func = [c = 0]() mutable -> int {++c; std::cout << c; return c;};

```

**An example to illustrate the usefulness of lambdas**

Before C++11:

```cpp
// Generic functor used for comparison
struct islessthan
{
    islessthan(int threshold) : _threshold(threshold) {}

    bool operator()(int value) const
    {
        return value < _threshold;
    }
private:
    int _threshold;
};

// Declare a vector
const int arr[] = { 1, 2, 3, 4, 5 };
std::vector<int> vec(arr, arr+5);

// Find a number that's less than a given input (assume this would have been function input)
int threshold = 10;
std::vector<int>::iterator it = std::find_if(vec.begin(), vec.end(), islessthan(threshold));

```

Since C++11:

```cpp
// Declare a vector
std::vector<int> vec{ 1, 2, 3, 4, 5 };

// Find a number that's less than a given input (assume this would have been function input)
int threshold = 10;
auto it = std::find_if(vec.begin(), vec.end(), [threshold](int value) { return value < threshold; });

```



## Specifying the return type


For lambdas with a single return statement, or multiple return statements whose expressions are of the same type, the compiler can deduce the return type:

```cpp
// Returns bool, because "value > 10" is a comparison which yields a Boolean result
auto l = [](int value) {
    return value > 10;
}

```

For lambdas with multiple return statements of **different** types, the compiler can't deduce the return type:

```cpp
// error: return types must match if lambda has unspecified return type
auto l = [](int value) {
    if (value < 10) {
        return 1;
    } else {
        return 1.5;
    }
};

```

In this case you have to specify the return type explicitly:

```cpp
// The return type is specified explicitly as 'double'
auto l = [](int value) -> double {
    if (value < 10) {
        return 1;
    } else {
        return 1.5;
    }
};

```

The rules for this match the rules for `auto` type deduction. Lambdas without explicitly specified return types never return references, so if a reference type is desired it must be explicitly specified as well:

```cpp
auto copy = [](X& x) { return x; };       // 'copy' returns an X, so copies its input
auto ref  = [](X& x) -> X& { return x; }; // 'ref' returns an X&, no copy

```



## Capture by value


If you specify the variable's name in the capture list, the lambda will capture it by value. This means that the generated closure type for the lambda stores a copy of the variable. This also requires that the variable's type be **copy-constructible**:

```cpp
int a = 0;

[a]() {
    return a;   // Ok, 'a' is captured by value
};

```

```cpp
auto p = std::unique_ptr<T>(...);

[p]() {         // Compile error; `unique_ptr` is not copy-constructible
    return p->createWidget(); 
};

```

From C++14 on, it is possible to initialize variables on the spot. This allows move only types to be captured in the lambda.

```cpp
auto p = std::make_unique<T>(...);

[p = std::move(p)]() {
    return p->createWidget(); 
};

```

Even though a lambda captures variables by value when they are given by their name, such variables cannot be modified within the lambda body by default. This is because the closure type puts the lambda body in a declaration of `operator() const`.

The `const` applies to accesses to member variables of the closure type, and captured variables that are members of the closure (all appearances to the contrary):

```cpp
int a = 0;

[a]() {
    a = 2;      // Illegal, 'a' is accessed via `const`

    decltype(a) a1 = 1; 
    a1 = 2; // valid: variable 'a1' is not const
};

```

To remove the `const`, you have to specify the keyword `mutable` on the lambda:

```cpp
int a = 0;

[a]() mutable {
    a = 2;      // OK, 'a' can be modified
    return a;
};

```

Because `a` was captured by value, any modifications done by calling the lambda will not affect `a`. The value of `a` was copied into the lambda when it was constructed, so the lambda's copy of `a` is separate from the external `a` variable.

```cpp
int a = 5 ; 
auto plus5Val = [a] (void) { return a + 5 ; } ; 
auto plus5Ref = [&a] (void) {return a + 5 ; } ; 

a = 7 ; 
std::cout << a << ", value " << plus5Val() << ", reference " << plus5Ref() ;
// The result will be "7, value 10, reference 12"

```



## Recursive lambdas


Let's say we wish to write Euclid's `gcd()` as a lambda. As a function, it is:

```cpp
int gcd(int a, int b) {
    return b == 0 ? a : gcd(b, a%b);
}

```

But a lambda cannot be recursive, it has no way to invoke itself. A lambda has no name and using `this` within the body of a lambda refers to a captured `this` (assuming the lambda is created in the body of a member function, otherwise it is an error). So how do we solve this problem?

### Use `std::function`

We can have a lambda capture a reference to a not-yet constructed `std::function`:

```cpp
std::function<int(int, int)> gcd = [&](int a, int b){
    return b == 0 ? a : gcd(b, a%b);
};

```

This works, but should be used sparingly. It's slow (we're using type erasure now instead of a direct function call), it's fragile (copying `gcd` or returning `gcd` will break since the lambda refers to the original object), and it won't work with generic lambdas.

### Using two smart pointers:

```cpp
auto gcd_self = std::make_shared<std::unique_ptr< std::function<int(int, int)> >>();
*gcd_self = std::make_unique<std::function<int(int, int)>>(
  [gcd_self](int a, int b){
    return b == 0 ? a : (**gcd_self)(b, a%b);
  };
};

```

This adds a lot of indirection (which is overhead), but it can be copied/returned, and all copies share state.  It does let you return the lambda, and is otherwise less fragile than the above solution.

### Use a Y-combinator

With the help of a short utility struct, we can solve all of these problems:

```cpp
template <class F>
struct y_combinator {
    F f; // the lambda will be stored here

    // a forwarding operator():
    template <class... Args>
    decltype(auto) operator()(Args&&... args) const {
        // we pass ourselves to f, then the arguments.
        // the lambda should take the first argument as `auto&& recurse` or similar.
        return f(*this, std::forward<Args>(args)...);
    }
};
// helper function that deduces the type of the lambda:
template <class F>
y_combinator<std::decay_t<F>> make_y_combinator(F&& f) {
    return {std::forward<F>(f)};
}
// (Be aware that in C++17 we can do better than a `make_` function)

```

we can implement our `gcd` as:

```cpp
auto gcd = make_y_combinator(
  [](auto&& gcd, int a, int b){
    return b == 0 ? a : gcd(b, a%b);
  }
);

```

The `y_combinator` is a concept from the lambda calculus that lets you have recursion without being able to name yourself until you are defined.  This is exactly the problem lambdas have.

You create a lambda that takes "recurse" as its first argument.  When you want to recurse, you pass the arguments to recurse.

The `y_combinator` then returns a function object that calls that function with its arguments, but with a suitable "recurse" object (namely the `y_combinator` itself) as its first argument.  It forwards the rest of the arguments you call the `y_combinator` with to the lambda as well.

In short:

```cpp
auto foo = make_y_combinator( [&](auto&& recurse, some arguments) {
  // write body that processes some arguments
  // when you want to recurse, call recurse(some other arguments)
});

```

and you have recursion in a lambda with no serious restrictions or significant overhead.



## Default capture


By default, local variables that are not explicitly specified in the capture list, cannot be accessed from within the lambda body. However, it is possible to implicitly capture variables named by the lambda body:

```cpp
int a = 1;
int b = 2;

// Default capture by value
[=]() { return a + b; }; // OK; a and b are captured by value

// Default capture by reference
[&]() { return a + b; }; // OK; a and b are captured by reference

```

Explicit capturing can still be done alongside implicit default capturing. The explicit capture definition will override the default capture:

```cpp
int a = 0;
int b = 1;

[=, &b]() {
    a = 2; // Illegal; 'a' is capture by value, and lambda is not 'mutable'
    b = 2; // OK; 'b' is captured by reference
};

```



## Capture by reference


If you precede a local variable's name with an `&`, then the variable will be captured by reference. Conceptually, this means that the lambda's closure type will have a reference variable, initialized as a reference to the corresponding variable from outside of the lambda's scope. Any use of the variable in the lambda body will refer to the original variable:

```cpp
// Declare variable 'a'
int a = 0;

// Declare a lambda which captures 'a' by reference
auto set = [&a]() {
    a = 1;
};

set();
assert(a == 1);

```

The keyword `mutable` is not needed, because `a` itself is not `const`.

Of course, capturing by reference means that the lambda **must not** escape the scope of the variables it captures. So you could call functions that take a function, but you must not call a function that will **store** the lambda beyond the scope of your references. And you must not return the lambda.



## Generic lambdas


Lambda functions can take arguments of arbitrary types. This allows a lambda to be more generic:

```cpp
auto twice = [](auto x){ return x+x; };

int i = twice(2); // i == 4
std::string s = twice("hello"); // s == "hellohello"

```

This is implemented in C++ by making the closure type's `operator()` overload a template function. The following type has equivalent behavior to the above lambda closure:

```cpp
struct _unique_lambda_type
{
  template<typename T>
  auto operator() (T x) const {return x + x;}
};

```

Not all parameters in a generic lambda need be generic:

```cpp
[](auto x, int y) {return x + y;}

```

Here, `x` is deduced based on the first function argument, while `y` will always be `int`.

Generic lambdas can take arguments by reference as well, using the usual rules for `auto` and `&`. If a generic parameter is taken as `auto&&`, this is a [**forwarding** reference](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2014/n4164.pdf) to the passed in argument and not an [**rvalue** reference](http://en.cppreference.com/w/cpp/language/reference):

```cpp
auto lamb1 = [](int &&x) {return x + 5;};
auto lamb2 = [](auto &&x) {return x + 5;};
int x = 10;
lamb1(x); // Illegal; must use `std::move(x)` for `int&&` parameters.
lamb2(x); // Legal; the type of `x` is deduced as `int&`.

```

Lambda functions can be variadic and perfectly forward their arguments:

```cpp
auto lam = [](auto&&... args){return f(std::forward<decltype(args)>(args)...);};

```

or:

```cpp
auto lam = [](auto&&... args){return f(decltype(args)(args)...);};

```

which only works "properly" with variables of type `auto&&`.

A strong reason to use generic lambdas is for visiting syntax.

```cpp
boost::variant<int, double> value;
apply_visitor(value, [&](auto&& e){
  std::cout << e;
});

```

Here we are visiting in a polymorphic manner; but in other contexts, the names of the type we are passing isn't interesting:

```cpp
mutex_wrapped<std::ostream&> os = std::cout;
os.write([&](auto&& os){
  os << "hello world\n";
});

```

Repeating the type of `std::ostream&` is noise here; it would be like having to mention the type of a variable every time you use it.  Here we are creating a visitor, but no a polymorphic one; `auto` is used for the same reason you might use `auto` in a `for(:)` loop.



## Class lambdas and capture of this


A lambda expression evaluated in a class' member function is implicitly a friend of that class:

```cpp
class Foo
{
private:
    int i;
    
public:
    Foo(int val) : i(val) {}
    
    // definition of a member function
    void Test()
    {
        auto lamb = [](Foo &foo, int val)
        {
            // modification of a private member variable
            foo.i = val;
        };
        
        // lamb is allowed to access a private member, because it is a friend of Foo
        lamb(*this, 30);
    }
};

```

Such a lambda is not only a friend of that class, it has the same access as the class it is declared within has.

Lambdas can capture the `this` pointer which represents the object instance the outer function was called on. This is done by adding `this` to the capture list:

```cpp
class Foo
{
private:
    int i;
    
public:
    Foo(int val) : i(val) {}
    
    void Test()
    {
        // capture the this pointer by value
        auto lamb = [this](int val)
        {
            i = val;
        };
        
        lamb(30);
    }
};

```

When `this` is captured, the lambda can use member names of its containing class as though it were in its containing class. So an implicit `this->` is applied to such members.

Be aware that `this` is captured by value, but not the value of the type. It is captured by the value of `this`, which is a **pointer**. As such, the lambda does not **own** `this`. If the lambda out lives the lifetime of the object that created it, the lambda can become invalid.

This also means that the lambda can modify `this` without being declared `mutable`. It is the pointer which is `const`, not the object being pointed to. That is, unless the outer member function was itself a `const` function.

Also, be aware that the default capture clauses, both `[=]` and `[&]`, will **also** capture `this` implicitly. And they both capture it by the value of the pointer. Indeed, it is an error to specify `this` in the capture list when a default is given.

Lambdas can capture a copy of the `this` object, created at the time the lambda is created. This is done by adding `*this` to the capture list:

```cpp
class Foo
{
private:
    int i;
    
public:
    Foo(int val) : i(val) {}
    
    void Test()
    {
        // capture a copy of the object given by the this pointer
        auto lamb = [*this](int val) mutable
        {
            i = val;
        };
        
        lamb(30); // does not change this->i
    }
};

```



## Generalized capture


Lambdas can capture expressions, rather than just variables. This permits lambdas to store move-only types:

```cpp
auto p = std::make_unique<T>(...);

auto lamb = [p = std::move(p)]() //Overrides capture-by-value of `p`.
{
  p->SomeFunc();
};

```

This moves the outer `p` variable into the lambda capture variable, also called `p`. `lamb` now owns the memory allocated by `make_unique`. Because the closure contains a type that is non-copyable, this means that `lamb` is itself non-copyable. But it can be moved:

```cpp
auto lamb_copy = lamb; //Illegal
auto lamb_move = std::move(lamb); //legal.

```

Now `lamb_move` owns the memory.

Note that `std::function<>` requires that the values stored be copyable. You can write your own [move-only-requiring `std::function`](https://stackoverflow.com/documentation/c%2b%2b/2872/type-erasure/18042/a-move-only-stdfunction#t=201608061624094409548), or you could just stuff the lambda into a `shared_ptr` wrapper:

```cpp
auto shared_lambda = [](auto&& f){
  return [spf = std::make_shared<std::decay_t<decltype(f)>>(decltype(f)(f))]
  (auto&&...args)->decltype(auto) {
    return (*spf)(decltype(args)(args)...);
  };
};
auto lamb_shared = shared_lambda(std::move(lamb_move));

```

takes our move-only lambda and stuffs its state into a shared pointer then returns a lambda that **can** be copied, and then stored in a `std::function` or similar.

Generalized capture uses `auto` type deduction for the variable's type. It will declare these captures as values by default, but they can be references as well:

```cpp
int a = 0;

auto lamb = [&v = a](int add) //Note that `a` and `v` have different names
{
  v += add; //Modifies `a`
};

lamb(20); //`a` becomes 20.

```

Generalize capture does not need to capture an external variable at all. It can capture an arbitrary expression:

```cpp
auto lamb = [p = std::make_unique<T>(...)]()
{
    p->SomeFunc();
}

```

This is useful for giving lambdas arbitrary values that they can hold and potentially modify, without having to declare them externally to the lambda. Of course, that is only useful if you do not intend to access those variables after the lambda has completed its work.



## Conversion to function pointer


If a lambda's capture list is empty, then the lambda has an implicit conversion to a function pointer that takes the same arguments and returns the same return type:

```cpp
auto sorter = [](int lhs, int rhs) -> bool {return lhs < rhs;};

using func_ptr = bool(*)(int, int);
func_ptr sorter_func = sorter; // implicit conversion

```

Such a conversion may also be enforced using unary plus operator:

```cpp
func_ptr sorter_func2 = +sorter; // enforce implicit conversion

```

Calling this function pointer behaves exactly like invoking `operator()` on the lambda. This function pointer is in no way reliant on the source lambda closure's existence. It therefore may outlive the lambda closure.

This feature is mainly useful for using lambdas with APIs that deal in function pointers, rather than C++ function objects.

Conversion to a function pointer is also possible for generic lambdas with an empty capture list. If necessary, template argument deduction will be used to select the correct specialization.

```cpp
auto sorter = [](auto lhs, auto rhs) { return lhs < rhs; };
using func_ptr = bool(*)(int, int);
func_ptr sorter_func = sorter;  // deduces int, int
// note however that the following is ambiguous
// func_ptr sorter_func2 = +sorter;

```



## Using lambdas for inline parameter pack unpacking


Parameter pack unpacking traditionally requires writing a helper function for each time you want to do it.

In this toy example:

```cpp
template<std::size_t...Is>
void print_indexes( std::index_sequence<Is...> ) {
  using discard=int[];
  (void)discard{0,((void)(
    std::cout << Is << '\n' // here Is is a compile-time constant.
  ),0)...};
}
template<std::size_t I>
void print_indexes_upto() {
  return print_indexes( std::make_index_sequence<I>{} );
}

```

The `print_indexes_upto` wants to create and unpack a parameter pack of indexes.  In order to do so, it must call a helper function.  Every time you want to unpack a parameter pack you created, you end up having to create a custom helper function to do it.

This can be avoided with lambdas.

You can unpack parameter packs into a set of invocations of a lambda, like this:

```cpp
template<std::size_t I>
using index_t = std::integral_constant<std::size_t, I>;
template<std::size_t I>
constexpr index_t<I> index{};

template<class=void, std::size_t...Is>
auto index_over( std::index_sequence<Is...> ) {
  return [](auto&& f){
    using discard=int[];
    (void)discard{0,(void(
      f( index<Is> )
    ),0)...};
  };
}

template<std::size_t N>
auto index_over(index_t<N> = {}) {
  return index_over( std::make_index_sequence<N>{} );
}

```

With fold expressions, `index_over()` can be simplified to:

```cpp
template<class=void, std::size_t...Is>
auto index_over( std::index_sequence<Is...> ) {
  return [](auto&& f){
    ((void)(f(index<Is>)), ...);
  };
}

```

Once you have done that, you can use this to replace having to manually unpack parameter packs with a second overload in other code, letting you unpack parameter packs "inline":

```cpp
template<class Tup, class F>
void for_each_tuple_element(Tup&& tup, F&& f) {
  using T = std::remove_reference_t<Tup>;
  using std::tuple_size;
  auto from_zero_to_N = index_over< tuple_size<T>{} >();

  from_zero_to_N(
    [&](auto i){
      using std::get;
      f( get<i>( std::forward<Tup>(tup) ) );
    }
  );
}

```

The `auto i` passed to the lambda by the `index_over` is a `std::integral_constant<std::size_t, ???>`.  This has a `constexpr` conversion to `std::size_t` that does not depend on the state of `this`, so we can use it as a compile-time constant, such as when we pass it to `std::get<i>` above.

To go back to the toy example at the top, rewrite it as:

```cpp
template<std::size_t I>
void print_indexes_upto() {
  index_over(index<I>)([](auto i){
    std::cout << i << '\n'; // here i is a compile-time constant
  });
}

```

which is much shorter, and keeps logic in the code that uses it.

[Live example](http://coliru.stacked-crooked.com/a/32c204301f7163c9) to play with.



## Porting lambda functions to C++03 using functors


Lambda functions in C++ are syntactic sugar that provide a very concise syntax for writing [functors](http://stackoverflow.com/documentation/c%2B%2B/1412/class-functors#t=201607221029133733631). As such, equivalent functionality can be obtained in C++03 (albeit much more verbose) by converting the lambda function into a functor:

```cpp
// Some dummy types:
struct T1 {int dummy;};
struct T2 {int dummy;};
struct R {int dummy;};

// Code using a lambda function (requires C++11)
R use_lambda(T1 val, T2 ref) {
  // Use auto because the type of the lambda is unknown.
  auto lambda = [val, &ref](int arg1, int arg2) -> R {
    /* lambda-body */
    return R();
  };
  return lambda(12, 27);
}

// The functor class (valid C++03)
// Similar to what the compiler generates for the lambda function.
class Functor {
  // Capture list.
  T1 val;
  T2& ref;

public:
  // Constructor
  inline Functor(T1 val, T2& ref) : val(val), ref(ref) {}

  // Functor body
  R operator()(int arg1, int arg2) const {
    /* lambda-body */
    return R();
  }
};

// Equivalent to use_lambda, but uses a functor (valid C++03).
R use_functor(T1 val, T2 ref) {
  Functor functor(val, ref);
  return functor(12, 27);
}

// Make this a self-contained example.
int main() {
  T1 t1;
  T2 t2;
  use_functor(t1,t2);
  use_lambda(t1,t2);
  return 0;
}

```

If the lambda function is `mutable` then make the functor's call-operator non-const, i.e.:

```cpp
R operator()(int arg1, int arg2) /*non-const*/ {
  /* lambda-body */
  return R();
}

```



#### Syntax


- [**default-capture**, **capture-list**] (**argument-list**) mutable **throw-specification** **attributes** -> **return-type** { **lambda-body** } // Order of lambda specifiers and attributes.
- [**capture-list**] (**argument-list**) { **lambda-body** } // Common lambda definition.
- [=] (**argument-list**) { **lambda-body** } // Captures all needed local variables by value.
- [&] (**argument-list**) { **lambda-body** } // Captures all needed local variables by reference.
- [**capture-list**] { **lambda-body** } // Argument list and specifiers can be omitted.



#### Parameters


|Parameter|Details
|------
|**default-capture**|Specifies how all non-listed variables are captured. Can be `=` (capture by value) or `&` (capture by reference). If omitted, non-listed variables are inaccessible within the **lambda-body**. The **default-capture** must precede the **capture-list**.
|**capture-list**|Specifies how local variables are made accessible within the **lambda-body**. Variables without prefix are captured by value. Variables prefixed with `&` are captured by reference. Within a class method, `this` can be used to make all its members accessible by reference. Non-listed variables are inaccessible, unless the list is preceded by a **default-capture**.
|**argument-list**|Specifies the arguments of the lambda function.
|mutable|**(optional)** Normally variables captured by value are `const`. Specifying `mutable` makes them non-const. Changes to those variables are retained between calls.
|**throw-specification**|**(optional)** Specifies the exception throwing behavior of the lambda function. For example: `noexcept` or `throw(std::exception)`.
|**attributes**|**(optional)** Any attributes for the lambda function. For example, if the **lambda-body** always throws an exception then `[[noreturn]]` can be used.
|-> **return-type**|**(optional)** Specifies the return type of the lambda function. Required when the return type cannot be determined by the compiler.
|**lambda-body**|A code block containing the implementation of the lambda function.



#### Remarks


C++17 (the current draft) introduces `constexpr` lambdas, basically lambdas that can be evaluated at compile time. A lambda is automatically `constexpr` if it satisfies `constexpr` requirements, but you can also specify it using the `constexpr` keyword:

```cpp
//Explicitly define this lambdas as constexpr
[]() constexpr {
    //Do stuff
}

```

