---
metaTitle: "C++ | Copy Elision"
description: "Purpose of copy elision, Guaranteed copy elision, Parameter elision, Return value elision, Named return value elision, Copy initialization elision"
---

# Copy Elision



## Purpose of copy elision


There are places in the standard where an object is copied or moved in order to initialize an object. Copy elision (sometimes called return value optimization) is an optimization whereby, under certain specific circumstances, a compiler is permitted to avoid the copy or move even though the standard says that it must happen.

Consider the following function:

```cpp
std::string get_string()
{
  return std::string("I am a string.");
}

```

According to the strict wording of the standard, this function will initialize a temporary `std::string`, then copy/move that into the return value object, then destroy the temporary. The standard is very clear that this is how the code is interpreted.

Copy elision is a rule that permits a C++ compiler to **ignore** the creation of the temporary and its subsequent copy/destruction. That is, the compiler can take the initializing expression for the temporary and initialize the function's return value from it directly. This obviously saves performance.

However, it does have two visible effects on the user:

<li>
The type must have the copy/move constructor that would have been called. Even if the compiler elides the copy/move, the type must still be able to have been copied/moved.
</li>
<li>
Side-effects of copy/move constructors are not guaranteed in circumstances where elision can happen. Consider the following:
</li>

```cpp
struct my_type
{
  my_type() = default;
  my_type(const my_type &) {std::cout <<"Copying\n";}
  my_type(my_type &&) {std::cout <<"Moving\n";}
};

my_type func()
{
  return my_type();
}

```

What will calling `func` do? Well, it will never print "Copying", since the temporary is an rvalue and `my_type` is a moveable type. So will it print "Moving"?

Without the copy elision rule, this would be required to always print "Moving". But because the copy elision rule exists, the move constructor may or may not be called; it is implementation-dependent.

And therefore, you cannot depend on the calling of copy/move constructors in contexts where elision is possible.

Because elision is an optimization, your compiler may not support elision in all cases. And regardless of whether the compiler elides a particular case or not, the type must still support the operation being elided. So if a copy construction is elided, the type must still have a copy constructor, even though it will not be called.



## Guaranteed copy elision


Normally, elision is an optimization. While virtually every compiler support copy elision in the simplest of cases, having elision still places a particular burden on users. Namely, the type who's copy/move is being elided **must** still have the copy/move operation that was elided.

For example:

```cpp
std::mutex a_mutex;
std::lock_guard<std::mutex> get_lock()
{
  return std::lock_guard<std::mutex>(a_mutex);
}

```

This might be useful in cases where `a_mutex` is a mutex that is privately held by some system, yet an external user might want to have a scoped lock to it.

This is also not legal, because `std::lock_guard` cannot be copied or moved. Even though virtually every C++ compiler will elide the copy/move, the standard still **requires** the type to have that operation available.

Until C++17.

C++17 mandates elision by effectively redefining the very meaning of certain expressions so that no copy/moving takes place. Consider the above code.

Under pre-C++17 wording, that code says to create a temporary and then use the temporary to copy/move into the return value, but the temporary copy can be elided. Under C++17 wording, that does not create a temporary at all.

In C++17, any [prvalue expression](http://stackoverflow.com/documentation/c%2B%2B/763/value-categories/2603/prvalue#t=201607301613191630536), when used to initialize an object of the same type as the expression, does not generate a temporary. The expression directly initializes that object. If you return a prvalue of the same type as the return value, then the type need not have a copy/move constructor. And therefore, under C++17 rules, the above code can work.

The C++17 wording works in cases where the prvalue's type matches the type being initialized. So given `get_lock` above, this will also not require a copy/move:

```cpp
std::lock_guard the_lock = get_lock();

```

Since the result of `get_lock` is a prvalue expression being used to initialize an object of the same type, no copying or moving will happen. That expression never creates a temporary; it is used to directly initialize `the_lock`. There is no elision because there is no copy/move to be elided elide.

The term "guaranteed copy elision" is therefore something of a misnomer, but [that is the name of the feature as it is proposed for C++17 standardization](http://wg21.link/P0135). It does not guarantee elision at all; it **eliminates** the copy/move altogether, redefining C++ so that there never was a copy/move to be elided.

This feature only works in cases involving a prvalue expression. As such, this uses the usual elision rules:

```cpp
std::mutex a_mutex;
std::lock_guard<std::mutex> get_lock()
{
  std::lock_guard<std::mutex> my_lock(a_mutex);
  //Do stuff
  return my_lock;
}

```

While this is a valid case for copy elision, C++17 rules do not **eliminate** the copy/move in this case. As such, the type must still have a copy/move constructor to use to initialize the return value. And since `lock_guard` does not, this is still a compile error.
Implementations are allowed to refuse to elide copies when passing or returning an object of trivially-copyable type. This is to allow moving such objects around in registers, which some ABIs might mandate in their calling conventions.

```cpp
struct trivially_copyable {
    int a;  
};

void foo (trivially_copyable a) {}

foo(trivially_copyable{}); //copy elision not mandated

```



## Parameter elision


When you pass an argument to a function, and the argument is a [prvalue expression](http://stackoverflow.com/documentation/c%2B%2B/763/value-categories/2603/prvalue#t=201607301618015777687) of the function's parameter type, and this type is not a reference, then the prvalue's construction can be elided.

```cpp
void func(std::string str) { ... }

func(std::string("foo"));

```

This says to create a temporary `string`, then move it into the function parameter `str`. Copy elision permits this expression to directly create the object in `str`, rather than using a temporary+move.

This is a useful optimization for cases where a constructor is declared `explicit`. For example, we could have written the above as `func("foo")`, but only because `string` has an implicit constructor that converts from a `const char*` to a `string`. If that constructor was `explicit`, we would be forced to use a temporary to call the `explicit` constructor. Copy elision saves us from having to do a needless copy/move.



## Return value elision


If you return a [prvalue expression](http://stackoverflow.com/documentation/c%2B%2B/763/value-categories/2603/prvalue#t=201607301619277418431) from a function, and the prvalue expression has the same type as the function's return type, then the copy from the prvalue temporary can be elided:

```cpp
std::string func()
{
  return std::string("foo");
}

```

Pretty much all compilers will elide the temporary construction in this case.



## Named return value elision


If you return an [lvalue expression](http://stackoverflow.com/documentation/c%2B%2B/763/value-categories/2605/lvalue#t=201607301614328409642) from a function, and this lvalue:

- represents an automatic variable local to that function, which will be destroyed after the `return`
- the automatic variable is not a function parameter
- and the type of the variable is the same type as the function's return type

If all of these are the case, then the copy/move from the lvalue can be elided:

```cpp
std::string func()
{
  std::string str("foo");
  //Do stuff
  return str;
}

```

More complex cases are eligible for elision, but the more complex the case, the less likely the compiler will be to actually elide it:

```cpp
std::string func()
{
  std::string ret("foo");
  if(some_condition)
  {
    return "bar";
  }
  return ret;
}

```

The compiler could still elide `ret`, but the chances of them doing so go down.

As noted earlier, elision is not permitted for value **parameters**.

```cpp
std::string func(std::string str)
{
  str.assign("foo");
  //Do stuff
  return str; //No elision possible
}

```



## Copy initialization elision


If you use a [prvalue expression](http://stackoverflow.com/documentation/c%2B%2B/763/value-categories/2603/prvalue#t=201607301613191630536) to copy initialize a variable, and that variable has the same type as the prvalue expression, then the copying can be elided.

```cpp
std::string str = std::string("foo");

```

Copy initialization effectively transforms this into `std::string str("foo");` (there are minor differences).

This also works with return values:

```cpp
std::string func()
{
  return std::string("foo");
}

std::string str = func();

```

Without copy elision, this would provoke 2 calls to `std::string`'s move constructor. Copy elision permits this to call the move constructor 1 or zero times, and most compilers will opt for the latter.

