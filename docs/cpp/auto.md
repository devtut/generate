---
metaTitle: "auto"
description: "Basic auto sample, auto and Expression Templates, auto, const, and references, Trailing return type, Generic lambda (C++14), auto and proxy objects"
---

# auto



## Basic auto sample


The keyword `auto` provides the auto-deduction of type of a variable.

It is especially convenient when dealing with long type names:

```cpp
std::map< std::string, std::shared_ptr< Widget > > table;
// C++98
std::map< std::string, std::shared_ptr< Widget > >::iterator i = table.find( "42" );
// C++11/14/17
auto j = table.find( "42" );

```

with [range-based for loops](http://stackoverflow.com/documentation/c%2B%2B/589/loops/1926/range-based-for):

```cpp
vector<int> v = {0, 1, 2, 3, 4, 5};
for(auto n: v)
    std::cout << n << ' ';

```

with [lambdas](http://stackoverflow.com/documentation/c%2B%2B/572/lambdas):

```cpp
auto f = [](){ std::cout << "lambda\n"; };
f();        

```

to avoid the repetition of the type:

```cpp
auto w = std::make_shared< Widget >();

```

to avoid surprising and unnecessary copies:

```cpp
auto myMap = std::map<int,float>();
myMap.emplace(1,3.14);

std::pair<int,float> const& firstPair2 = *myMap.begin();  // copy!
auto const& firstPair = *myMap.begin();  // no copy!

```

The reason for the copy is that the returned type is actually `std::pair<const int,float>`!



## auto and Expression Templates


`auto` can also cause problems where expression templates come into play:

```cpp
auto mult(int c) {
    return c * std::valarray<int>{1};
}

auto v = mult(3);
std::cout << v[0]; // some value that could be, but almost certainly is not, 3.

```

The reason is that `operator*` on `valarray` gives you a proxy object that refers to the `valarray` as a means of lazy evaluation. By using `auto`, you're creating a dangling reference. Instead of `mult` had returned a `std::valarray<int>`, then the code would definitely print 3.



## auto, const, and references


The `auto` keyword by itself represents a value type, similar to `int` or `char`. It can be modified with the `const` keyword and the `&` symbol to represent a const type or a reference type, respectively. These modifiers can be combined.

In this example, `s` is a value type (its type will be inferred as `std::string`), so each iteration of the `for` loop **copies** a string from the vector into `s`.

```cpp
std::vector<std::string> strings = { "stuff", "things", "misc" };
for(auto s : strings) {
    std::cout << s << std::endl;
}

```

If the body of the loop modifies `s` (such as by calling `s.append(" and stuff")`), only this copy will be modified, not the original member of `strings`.

On the other hand, if `s` is declared with `auto&` it will be a reference type (inferred to be `std::string&`), so on each iteration of the loop it will be assigned a **reference** to a string in the vector:

```cpp
for(auto& s : strings) {
    std::cout << s << std::endl;
}

```

In the body of this loop, modifications to `s` will directly affect the element of `strings` that it references.

Finally, if `s` is declared `const auto&`, it will be a const reference type, meaning that on each iteration of the loop it will be assigned a **const reference** to a string in the vector:

```cpp
for(const auto& s : strings) {
    std::cout << s << std::endl;
}

```

Within the body of this loop, `s` cannot be modified (i.e. no non-const methods can be called on it).

When using `auto` with range-based `for` loops, it is generally good practice to use `const auto&` if the loop body will not modify the structure being looped over, since this avoids unnecessary copies.



## Trailing return type


`auto` is used in the syntax for trailing return type:

```cpp
auto main() -> int {}

```

which is equivalent to

```cpp
int main() {}

```

Mostly useful combined with `decltype` to use parameters instead of `std::declval<T>`:

```cpp
template <typename T1, typename T2>
auto Add(const T1& lhs, const T2& rhs) -> decltype(lhs + rhs) { return lhs + rhs; }

```



## Generic lambda (C++14)


C++14 allows to use `auto` in lambda argument

```cpp
auto print = [](const auto& arg) { std::cout << arg << std::endl; };

print(42);
print("hello world");

```

That lambda is mostly equivalent to

```cpp
struct lambda {
    template <typename T>
    auto operator ()(const T& arg) const {
        std::cout << arg << std::endl;
    }
};

```

and then

```cpp
lambda print;

print(42);
print("hello world");

```



## auto and proxy objects


Sometimes `auto` may behave not quite as was expected by a programmer.  It type deduces the expression, even when type deduction is not the right thing to do.

As an example, when proxy objects are used in the code:

```cpp
std::vector<bool> flags{true, true, false};
auto flag = flags[0];
flags.push_back(true);

```

Here `flag` would be not `bool`, but `std::vector<bool>::reference`, since for `bool` specialization of template `vector` the `operator []` returns a proxy object with conversion operator `operator bool` defined.

When `flags.push_back(true)` modifies the container, this pseudo-reference could end up dangling, referring to an element that no longer exists.

It also makes the next situation possible:

```cpp
void foo(bool b);

std::vector<bool> getFlags();

auto flag = getFlags()[5];
foo(flag);

```

The `vector` is discarded immediately, so `flag` is a pseudo-reference to an element that has been discarded.  The call to `foo` invokes undefined behavior.

In cases like this you can declare a variable with `auto` and initialize it by casting to the type you want to be deduced:

```cpp
auto flag = static_cast<bool>(getFlags()[5]);

```

but at that point, simply replacing `auto` with `bool` makes more sense.

Another case where proxy objects can cause problems is  [expression templates](https://stackoverflow.com/documentation/c%2b%2b/3404/expression-templates#t=201608151452546656817).  In that case, the templates are sometimes not designed to last beyond the current full-expression for efficiency sake, and using the proxy object on the next causes undefined behavior.



#### Remarks


The keyword `auto` is a typename that represents an automatically-deduced type.

It was already a reserved keyword in C++98, inherited from C. In old versions of C++, it could be used to explicitly state that a variable has automatic storage duration:

```cpp
int main()
{
  auto int i = 5; // removing auto has no effect
}

```

That old meaning is now removed.

