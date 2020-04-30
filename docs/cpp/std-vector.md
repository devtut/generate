---
metaTitle: "std::vector"
description: "Accessing Elements, Initializing a std::vector, Deleting Elements, Iterating Over std::vector, vector<bool>: The Exception To So Many, So Many Rules, Inserting Elements, Using std::vector as a C array, Finding an Element in std::vector, Concatenating Vectors, Reducing the Capacity of a Vector, Using a Sorted Vector for Fast Element Lookup, Matrices Using Vectors, Iterator/Pointer Invalidation, Vector size and capacity, Find max and min Element and Respective Index in a Vector, Converting an array to std::vector, Functions Returning Large Vectors"
---

# std::vector


A vector is a dynamic array with automatically handled storage. The elements in a vector can be accessed just as efficiently as those in an array with the advantage being that vectors can dynamically change in size.

In terms of storage the vector data is (usually) placed in dynamically allocated memory thus requiring some minor overhead; conversely `C-arrays` and `std::array` use automatic storage relative to the declared location and thus do not have any overhead.



## Accessing Elements


There are two primary ways of accessing elements in a [`std::vector`](http://en.cppreference.com/w/cpp/container/vector)

- index-based access
- [iterators](https://stackoverflow.com/documentation/c%2B%2B/473/iterators/1709/overview#t=20160613174720948501)

### Index-based access:

This can be done either with the subscript operator [`[]`](http://en.cppreference.com/w/cpp/container/vector/operator_at), or the member function [`at()`](http://en.cppreference.com/w/cpp/container/vector/at).

Both return a reference to the element at the respective position in the `std::vector` (unless it's a [`vector<bool>`](http://stackoverflow.com/documentation/c%2b%2b/511/stdvector/2561/vectorbool-the-exception-to-many-many-rules#t=20160725190704070024)), so that it can be read as well as modified (if the vector is not `const`).

`[]` and `at()` differ in that `[]` is not guaranteed to perform any bounds checking, while `at()` does. Accessing elements where `index < 0` or `index >= size` is [undefined behavior](http://stackoverflow.com/documentation/c%2b%2b/1812/undefined-behavior#t=201607221823520289181) for `[]`, while `at()` throws a [`std::out_of_range`](http://en.cppreference.com/w/cpp/error/out_of_range) exception.

****Note:**** The examples below use C++11-style initialization for clarity, but the operators can be used with all versions (unless marked C++11).

```cpp
std::vector<int> v{ 1, 2, 3 };

```

```cpp
// using []
int a = v[1];    // a is 2
v[1] = 4;        // v now contains { 1, 4, 3 }

// using at()
int b = v.at(2); // b is 3
v.at(2) = 5;     // v now contains { 1, 4, 5 }
int c = v.at(3); // throws std::out_of_range exception

```

Because the `at()` method performs bounds checking and can throw exceptions, it is slower than `[]`. This makes `[]` preferred code where the semantics of the operation guarantee that the index is in bounds.
In any case, accesses to elements of vectors are done in constant time. That means accessing to the first element of the vector has the same cost (in time) of accessing the second element, the third element and so on.

For example, consider this loop

```cpp
for (std::size_t i = 0; i < v.size(); ++i) {
    v[i] = 1;
}

```

Here we know that the index variable `i` is always in bounds, so it would be a waste of CPU cycles to check that `i` is in bounds for every call to `operator[]`.

The [`front()`](http://en.cppreference.com/w/cpp/container/vector/front) and [`back()`](http://en.cppreference.com/w/cpp/container/vector/back) member functions allow easy reference access to the first and last element of the vector, respectively. These positions are frequently used, and the special accessors can be more readable than their alternatives using `[]`:

```cpp
std::vector<int> v{ 4, 5, 6 }; // In pre-C++11 this is more verbose

int a = v.front();   // a is 4, v.front() is equivalent to v[0]
v.front() = 3;       // v now contains {3, 5, 6}
int b = v.back();    // b is 6, v.back() is equivalent to v[v.size() - 1]
v.back() = 7;        // v now contains {3, 5, 7}

```

****Note****: It is [undefined behavior](http://stackoverflow.com/documentation/c%2b%2b/1812/undefined-behavior#t=201608052302241671564) to invoke `front()` or `back()` on an empty vector. You need to check that the container is not empty using the [`empty()`](http://en.cppreference.com/w/cpp/container/vector/empty) member function (which checks if the container is empty) before calling `front()` or `back()`.  A simple example of the use of 'empty()' to test for an empty vector follows:

```cpp
int main ()
{
  std::vector<int> v;
  int sum (0);

  for (int i=1;i<=10;i++) v.push_back(i);//create and initialize the vector

  while (!v.empty())//loop through until the vector tests to be empty
  {
     sum += v.back();//keep a running total
     v.pop_back();//pop out the element which removes it from the vector
  }

  std::cout << "total: " << sum << '\n';//output the total to the user

  return 0;
}

```

The example above creates a vector with a sequence of numbers from 1 to 10.  Then it pops the elements of the vector out until the vector is empty (using 'empty()') to prevent undefined behavior.  Then the sum of the numbers in the vector is calculated and displayed to the user.

The [`data()`](http://en.cppreference.com/w/cpp/container/vector/data) method returns a pointer to the raw memory used by the `std::vector` to internally store its elements. This is most often used when passing the vector data to legacy code that expects a C-style array.

```cpp
std::vector<int> v{ 1, 2, 3, 4 }; // v contains {1, 2, 3, 4}
int* p = v.data(); // p points to 1
*p = 4;            // v now contains {4, 2, 3, 4}
++p;               // p points to 2
*p = 3;            // v now contains {4, 3, 3, 4}
p[1] = 2;          // v now contains {4, 3, 2, 4}
*(p + 2) = 1;      // v now contains {4, 3, 2, 1}

```

Before C++11, the `data()` method can be simulated by calling `front()` and taking the address of the returned value:

```cpp
std::vector<int> v(4);
int* ptr = &(v.front()); // or &v[0]

```

This works because vectors are always guaranteed to store their elements in contiguous memory locations, assuming the contents of the vector doesn't override unary `operator&`.  If it does, you'll have to re-implement [`std::addressof`](http://en.cppreference.com/w/cpp/memory/addressof) in pre-C++11. It also assumes that the vector isn't empty.

### Iterators:

Iterators are explained in more detail in the example "Iterating over `std::vector`" and the article [Iterators](https://stackoverflow.com/documentation/c%2B%2B/473/iterators/1709/overview#t=20160613174720948501). In short, they act similarly to pointers to the elements of the vector:

```cpp
std::vector<int> v{ 4, 5, 6 };

auto it = v.begin();
int i = *it;        // i is 4
++it; 
i = *it;            // i is 5
*it = 6;            // v contains { 4, 6, 6 }
auto e = v.end();   // e points to the element after the end of v. It can be 
                    // used to check whether an iterator reached the end of the vector:
++it; 
it == v.end();      // false, it points to the element at position 2 (with value 6)
++it;
it == v.end();      // true

```

It is consistent with the standard that a `std::vector<T>`'s iterators actually **be** `T*`s, but most standard libraries do not do this.  Not doing this both improves error messages, catches non-portable code, and can be used to instrument the iterators with debugging checks in non-release builds.  Then, in release builds, the class wrapping around the underlying pointer is optimized away.

You can persist a reference or a pointer to an element of a vector for indirect access.  These references or pointers to elements in the `vector` remain stable and access remains defined unless you add/remove elements at or before the element in the `vector`, or you cause the `vector` capacity to change.  This is the same as the rule for invalidating iterators.

```cpp
std::vector<int> v{ 1, 2, 3 };
int* p = v.data() + 1;     // p points to 2
v.insert(v.begin(), 0);    // p is now invalid, accessing *p is a undefined behavior.
p = v.data() + 1;          // p points to 1
v.reserve(10);             // p is now invalid, accessing *p is a undefined behavior.
p = v.data() + 1;          // p points to 1
v.erase(v.begin());        // p is now invalid, accessing *p is a undefined behavior.

```



## Initializing a std::vector


A [`std::vector`](http://en.cppreference.com/w/cpp/container/vector) can be [initialized](http://en.cppreference.com/w/cpp/container/vector/vector) in several ways while declaring it:

```cpp
std::vector<int> v{ 1, 2, 3 };  // v becomes {1, 2, 3}

// Different from std::vector<int> v(3, 6)
std::vector<int> v{ 3, 6 };     // v becomes {3, 6}

```

```cpp
// Different from std::vector<int> v{3, 6} in C++11
std::vector<int> v(3, 6);  // v becomes {6, 6, 6}

std::vector<int> v(4);     // v becomes {0, 0, 0, 0}

```

A vector can be initialized from another container in several ways:

Copy construction (from another vector only), which copies data from `v2`:

```cpp
std::vector<int> v(v2);
std::vector<int> v = v2;

```

Move construction (from another vector only), which moves data from `v2`:

```cpp
std::vector<int> v(std::move(v2));
std::vector<int> v = std::move(v2);

```

Iterator (range) copy-construction, which copies elements into `v`:

```cpp
// from another vector
std::vector<int> v(v2.begin(), v2.begin() + 3); // v becomes {v2[0], v2[1], v2[2]}

// from an array
int z[] = { 1, 2, 3, 4 };
std::vector<int> v(z, z + 3);                   // v becomes {1, 2, 3}

// from a list
std::list<int> list1{ 1, 2, 3 };
std::vector<int> v(list1.begin(), list1.end()); // v becomes {1, 2, 3}

```

Iterator move-construction, using [`std::make_move_iterator`](http://en.cppreference.com/w/cpp/iterator/make_move_iterator), which moves elements into `v`:

```cpp
// from another vector
std::vector<int> v(std::make_move_iterator(v2.begin()),
                   std::make_move_iterator(v2.end());

// from a list
std::list<int> list1{ 1, 2, 3 };
std::vector<int> v(std::make_move_iterator(list1.begin()),
                   std::make_move_iterator(list1.end()));

```

With the help of the [`assign()`](http://en.cppreference.com/w/cpp/container/vector/assign) member function, a `std::vector` can be reinitialized after its construction:

```cpp
v.assign(4, 100);                      // v becomes {100, 100, 100, 100}

v.assign(v2.begin(), v2.begin() + 3);  // v becomes {v2[0], v2[1], v2[2]}

int z[] = { 1, 2, 3, 4 };
v.assign(z + 1, z + 4);                // v becomes {2, 3, 4}

```



## Deleting Elements


### Deleting the last element:

```cpp
std::vector<int> v{ 1, 2, 3 };
v.pop_back();                           // v becomes {1, 2}

```

### Deleting all elements:

```cpp
std::vector<int> v{ 1, 2, 3 };
v.clear();                              // v becomes an empty vector

```

### Deleting element by index:

```cpp
std::vector<int> v{ 1, 2, 3, 4, 5, 6 };
v.erase(v.begin() + 3);                 // v becomes {1, 2, 3, 5, 6}

```

**Note:** For a `vector` deleting an element which is not the last element, all elements beyond the deleted element have to be copied or moved to fill the gap, see the note below and [std::list](http://stackoverflow.com/documentation/c%2B%2B/783/standard-library-containers/2688/stdlist#t=201607251402013099142).

### Deleting all elements in a range:

```cpp
std::vector<int> v{ 1, 2, 3, 4, 5, 6 };
v.erase(v.begin() + 1, v.begin() + 5);  // v becomes {1, 6}

```

****Note:**** The above methods do not change the capacity of the vector, only the size. See [Vector Size and Capacity](http://stackoverflow.com/documentation/c%2B%2B/511/stdvector/3694/vector-size-and-capacity#t=201608311621108011517).

The [`erase`](http://en.cppreference.com/w/cpp/container/vector/erase) method, which removes a range of elements, is often used as a part of the [**erase-remove**](https://en.wikipedia.org/wiki/Erase%E2%80%93remove_idiom) idiom. That is, first [`std::remove`](http://en.cppreference.com/w/cpp/algorithm/remove) moves some elements to the end of the vector, and then `erase` chops them off. This is a relatively inefficient operation for any indices less than the last index of the vector because all elements after the erased segments must be relocated to new positions. For speed critical applications that require efficient removal of arbitrary elements in a container, see [std::list](http://stackoverflow.com/documentation/c%2B%2B/783/standard-library-containers/2688/stdlist#t=201607251402013099142).

### Deleting elements by value:

```cpp
std::vector<int> v{ 1, 1, 2, 2, 3, 3 };
int value_to_remove = 2;
v.erase(std::remove(v.begin(), v.end(), value_to_remove), v.end()); // v becomes {1, 1, 3, 3}

```

### Deleting elements by condition:

```cpp
// std::remove_if needs a function, that takes a vector element as argument and returns true, 
// if the element shall be removed
bool _predicate(const int& element) {
    return (element > 3); // This will cause all elements to be deleted that are larger than 3
}
...
std::vector<int> v{ 1, 2, 3, 4, 5, 6 };
v.erase(std::remove_if(v.begin(), v.end(), _predicate), v.end()); // v becomes {1, 2, 3}

```

### Deleting elements by lambda, without creating additional predicate function

```cpp
std::vector<int> v{ 1, 2, 3, 4, 5, 6 };
v.erase(std::remove_if(v.begin(), v.end(),
     [](auto& element){return element > 3;} ), v.end()
);

```

### Deleting elements by condition from a loop:

```cpp
std::vector<int> v{ 1, 2, 3, 4, 5, 6 };
std::vector<int>::iterator it = v.begin();
while (it != v.end()) {
    if (condition)
        it = v.erase(it); // after erasing, 'it' will be set to the next element in v
    else
        ++it;             // manually set 'it' to the next element in v
}

```

While it is important **not** to increment `it` in case of a deletion, you should consider using a different method when then erasing repeatedly in a loop. Consider `remove_if` for a more efficient way.

### Deleting elements by condition from a reverse loop:

```cpp
std::vector<int> v{ -1, 0, 1, 2, 3, 4, 5, 6 };
typedef std::vector<int>::reverse_iterator rev_itr;
rev_itr it = v.rbegin();

while (it != v.rend()) { // after the loop only '0' will be in v
    int value = *it;
    if (value) {
        ++it;
        // See explanation below for the following line.
        it = rev_itr(v.erase(it.base()));
    } else
        ++it;
}

```

Note some points for the preceding loop:

<li>
Given a reverse iterator `it` pointing to some element, the method [`base`](http://en.cppreference.com/w/cpp/iterator/reverse_iterator/base) gives the regular (non-reverse) iterator pointing to the same element.
</li>
<li>
`vector::erase(iterator)` erases the element pointed to by an iterator, and returns an iterator to the element that followed the given element.
</li>
<li>
`reverse_iterator::reverse_iterator(iterator)` constructs a reverse iterator from an iterator.
</li>

Put altogether, the line `it = rev_itr(v.erase(it.base()))` says: take the reverse iterator `it`, have `v` erase the element pointed by its regular iterator; take the resulting iterator, construct a reverse iterator from it, and assign it to the reverse iterator `it`.

Deleting all elements using `v.clear()` does not free up memory ([`capacity()`](http://en.cppreference.com/w/cpp/container/vector/capacity) of the vector remains unchanged). To reclaim space, use:

```cpp
std::vector<int>().swap(v);

```

[`shrink_to_fit()`](http://en.cppreference.com/w/cpp/container/vector/shrink_to_fit) frees up unused vector capacity:

```cpp
v.shrink_to_fit();

```

The `shrink_to_fit` does not guarantee to really reclaim space, but most current implementations do.



## Iterating Over std::vector


You can iterate over a [`std::vector`](http://en.cppreference.com/w/cpp/container/vector) in several ways. For each of the following sections, `v` is defined as follows:

```cpp
std::vector<int> v;

```

### Iterating in the Forward Direction

```cpp
// Range based for
for(const auto& value: v) {
    std::cout << value << "\n";
}

// Using a for loop with iterator
for(auto it = std::begin(v); it != std::end(v); ++it) {
    std::cout << *it << "\n";
}

// Using for_each algorithm, using a function or functor:
void fun(int const& value) {
    std::cout << value << "\n";
}

std::for_each(std::begin(v), std::end(v), fun);

// Using for_each algorithm. Using a lambda:
std::for_each(std::begin(v), std::end(v), [](int const& value) {
    std::cout << value << "\n";
});

```

```cpp
// Using a for loop with iterator
for(std::vector<int>::iterator it = std::begin(v); it != std::end(v); ++it) {
    std::cout << *it << "\n";
}

```

```cpp
// Using a for loop with index
for(std::size_t i = 0; i < v.size(); ++i) {
    std::cout << v[i] << "\n";
}

```

### Iterating in the Reverse Direction

```cpp
// There is no standard way to use range based for for this.
// See below for alternatives.

// Using for_each algorithm
// Note: Using a lambda for clarity. But a function or functor will work
std::for_each(std::rbegin(v), std::rend(v), [](auto const& value) {
    std::cout << value << "\n";
});

// Using a for loop with iterator
for(auto rit = std::rbegin(v); rit != std::rend(v); ++rit) {
    std::cout << *rit << "\n";
}

```

```cpp
// Using a for loop with index
for(std::size_t i = 0; i < v.size(); ++i) {
    std::cout << v[v.size() - 1 - i] << "\n";
}

```

Though there is no built-in way to use the range based for to reverse iterate; it is relatively simple to fix this. The range based for uses `begin()` and `end()` to get iterators and thus simulating this with a wrapper object can achieve the results we require.

```cpp
template<class C>
struct ReverseRange {
  C c; // could be a reference or a copy, if the original was a temporary
  ReverseRange(C&& cin): c(std::forward<C>(cin)) {}
  ReverseRange(ReverseRange&&)=default;
  ReverseRange& operator=(ReverseRange&&)=delete;
  auto begin() const {return std::rbegin(c);}
  auto end()   const {return std::rend(c);}
};
// C is meant to be deduced, and perfect forwarded into
template<class C>
ReverseRange<C> make_ReverseRange(C&& c) {return {std::forward<C>(c)};}

int main() {
    std::vector<int> v { 1,2,3,4};
    for(auto const& value: make_ReverseRange(v)) {
        std::cout << value << "\n";
    }
}

```

### Enforcing const elements

Since C++11 the `cbegin()` and `cend()` methods allow you to obtain a **constant iterator** for a vector, even if the vector is non-const.  A constant iterator allows you to read but not modify the contents of the vector which is useful to enforce const correctness:

```cpp
// forward iteration
for (auto pos = v.cbegin(); pos != v.cend(); ++pos) {
   // type of pos is vector<T>::const_iterator
   // *pos = 5; // Compile error - can't write via const iterator
}

// reverse iteration
for (auto pos = v.crbegin(); pos != v.crend(); ++pos) {
   // type of pos is vector<T>::const_iterator
   // *pos = 5; // Compile error - can't write via const iterator
}

// expects Functor::operand()(T&) 
for_each(v.begin(), v.end(), Functor());

// expects Functor::operand()(const T&)
for_each(v.cbegin(), v.cend(), Functor())

```

[as_const](http://en.cppreference.com/w/cpp/utility/as_const) extends this to range iteration:

```cpp
for (auto const& e : std::as_const(v)) {
  std::cout << e << '\n';
}

```

This is easy to implement in earlier versions of C++:

```cpp
template <class T>
constexpr std::add_const_t<T>& as_const(T& t) noexcept {
  return t;
}

```

### A Note on Efficiency

Since the class `std::vector` is basically a class that manages a dynamically allocated contiguous array, the same principle explained [here](http://stackoverflow.com/documentation/c/322/arrays/8977/iterating-through-an-array-efficiently-and-row-major-order) applies to C++ vectors. Accessing the vector's content by index is much more efficient when following the row-major order principle. Of course, each access to the vector also puts its management content into the cache as well, but as has been debated many times (notably [here](http://stackoverflow.com/questions/381621/using-arrays-or-stdvectors-in-c-whats-the-performance-gap) and [here](http://stackoverflow.com/questions/3664272/is-stdvector-so-much-slower-than-plain-arrays)), the difference in performance for iterating over a `std::vector` compared to a raw array is negligible. So the same principle of efficiency for raw arrays in C also applies for C++'s `std::vector`.



## vector<bool>: The Exception To So Many, So Many Rules


The standard (section 23.3.7) specifies that a specialization of `vector<bool>` is provided, which optimizes space by packing the `bool` values, so that each takes up only one bit. Since bits aren't addressable in C++, this means that several requirements on `vector` are not placed on `vector<bool>`:

- The data stored is not required to be contiguous, so a `vector<bool>` can't be passed to a C API which expects a `bool` array.
- `at()`, `operator []`, and dereferencing of iterators do not return a reference to `bool`. Rather they return a proxy object that (imperfectly) simulates a reference to a `bool` by overloading its assignment operators. As an example, the following code may not be valid for `std::vector<bool>`, because dereferencing an iterator does not return a reference:

```cpp
std::vector<bool> v = {true, false};
for (auto &b: v) { } // error

```

Similarly, functions expecting a `bool&` argument cannot be used with the result of `operator []` or `at()` applied to `vector<bool>`, or with the result of dereferencing its iterator:

```

 void f(bool& b);
  f(v[0]);             // error
  f(*v.begin());       // error

```

The implementation of `std::vector<bool>` is dependent on both the compiler and architecture. The specialisation is implemented by packing `n` Booleans into the lowest addressable section of memory. Here, `n` is the size in bits of the lowest addressable memory. In most modern systems this is 1 byte or 8 bits. This means that one byte can store 8 Boolean values. This is an improvement over the traditional implementation where 1 Boolean value is stored in 1 byte of memory.

****Note:**** The below example shows possible bitwise values of individual bytes in a traditional vs. optimized `vector<bool>`. This will not always hold true in all architectures. It is, however, a good way of visualising the optimization. In the below examples a byte is represented as [x, x, x, x, x, x, x, x].

****Traditional** `std::vector<char>` storing 8 Boolean values:**

```cpp
std::vector<char> trad_vect = {true, false, false, false, true, false, true, true};

```

**Bitwise representation:**

```cpp
[0,0,0,0,0,0,0,1], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,0], 
[0,0,0,0,0,0,0,1], [0,0,0,0,0,0,0,0], [0,0,0,0,0,0,0,1], [0,0,0,0,0,0,0,1]

```

****Specialized** `std::vector<bool>` storing 8 Boolean values:**

```cpp
std::vector<bool> optimized_vect = {true, false, false, false, true, false, true, true};

```

**Bitwise representation:**

```cpp
[1,0,0,0,1,0,1,1]

```

Notice in the above example, that in the traditional version of `std::vector<bool>`, 8 Boolean values take up 8 bytes of memory, whereas in the optimized version of `std::vector<bool>`, they only use 1 byte of memory. This is a significant improvement on memory usage. If you need to pass a `vector<bool>` to an C-style API, you may need to copy the values to an array, or find a better way to use the API, if memory and performance are at risk.



## Inserting Elements


Appending an element at the end of a vector (by copying/moving):

```cpp
struct Point {
  double x, y;
  Point(double x, double y) : x(x), y(y) {}
};
std::vector<Point> v;
Point p(10.0, 2.0);
v.push_back(p);  // p is copied into the vector.

```

Appending an element at the end of a vector by constructing the element in place:

```cpp
std::vector<Point> v;
v.emplace_back(10.0, 2.0); // The arguments are passed to the constructor of the
                           // given type (here Point). The object is constructed
                           // in the vector, avoiding a copy.

```

Note that `std::vector` does **not** have a `push_front()` member function due to performance reasons. Adding an element at the beginning causes all existing elements in the vector to be moved. If you want to frequently insert elements at the beginning of your container, then you might want to use `std::list` or `std::deque` instead.

Inserting an element at any position of a vector:

```cpp
std::vector<int> v{ 1, 2, 3 };
v.insert(v.begin(), 9);          // v now contains {9, 1, 2, 3}

```

Inserting an element at any position of a vector by constructing the element in place:

```cpp
std::vector<int> v{ 1, 2, 3 };
v.emplace(v.begin()+1, 9);     // v now contains {1, 9, 2, 3}

```

Inserting another vector at any position of the vector:

```cpp
std::vector<int> v(4);      // contains: 0, 0, 0, 0
std::vector<int> v2(2, 10); // contains: 10, 10
v.insert(v.begin()+2, v2.begin(), v2.end()); // contains: 0, 0, 10, 10, 0, 0

```

Inserting an array at any position of a vector:

```cpp
std::vector<int> v(4); // contains: 0, 0, 0, 0
int a [] = {1, 2, 3}; // contains: 1, 2, 3
v.insert(v.begin()+1, a, a+sizeof(a)/sizeof(a[0])); // contains: 0, 1, 2, 3, 0, 0, 0

```

Use [`reserve()`](http://www.cplusplus.com/reference/vector/vector/reserve/) before inserting multiple elements if resulting vector size is known beforehand to avoid multiple reallocations (see [vector size and capacity](http://stackoverflow.com/documentation/c%2b%2b/511/stdvector/3694/vector-size-and-capacity#t=201607270933320554687)):

```cpp
std::vector<int> v;
v.reserve(100);
for(int i = 0; i < 100; ++i)
    v.emplace_back(i);

```

Be sure to not make the mistake of calling [`resize()`](http://www.cplusplus.com/reference/vector/vector/resize/) in this case, or you will inadvertently create a vector with 200 elements where only the latter one hundred will have the value you intended.



## Using std::vector as a C array


There are several ways to use a `std::vector` as a C array (for example, for compatibility with C libraries). This is possible because the elements in a vector are stored contiguously.

```cpp
std::vector<int> v{ 1, 2, 3 };
int* p = v.data();

```

In contrast to solutions based on previous C++ standards (see below), the member function `.data()` may also be applied to empty vectors, because it doesn't cause undefined behavior in this case.

Before C++11, you would take the address of the vector's first element to get an equivalent pointer, if the vector isn't empty, these both methods are interchangeable:

```cpp
int* p = &v[0];      // combine subscript operator and 0 literal

int* p = &v.front(); // explicitly reference the first element

```

****Note:**** If the vector is empty, `v[0]` and `v.front()` are undefined and cannot be used.

When storing the base address of the vector's data, note that many operations (such as `push_back`, `resize`, etc.) can change the data memory location of the vector, thus [invalidating previous data pointers](http://stackoverflow.com/documentation/c%2B%2B/511/stdvector/1914/iterator-pointer-invalidation). For example:

```cpp
std::vector<int> v;
int* p = v.data();
v.resize(42);      // internal memory location changed; value of p is now invalid

```



## Finding an Element in std::vector


The function [`std::find`](http://en.cppreference.com/w/cpp/algorithm/find), defined in the [`<algorithm>`](http://en.cppreference.com/w/cpp/algorithm) header, can be used to find an element in a `std::vector`.

`std::find` uses the `operator==` to compare elements for equality. It returns an iterator to the first element in the range that compares equal to the value.

If the element in question is not found, `std::find` returns `std::vector::end` (or `std::vector::cend` if the vector is `const`).

```cpp
static const int arr[] = {5, 4, 3, 2, 1};
std::vector<int> v (arr, arr + sizeof(arr) / sizeof(arr[0]) );

std::vector<int>::iterator it = std::find(v.begin(), v.end(), 4);
std::vector<int>::difference_type index = std::distance(v.begin(), it);
// `it` points to the second element of the vector, `index` is 1

std::vector<int>::iterator missing = std::find(v.begin(), v.end(), 10);
std::vector<int>::difference_type index_missing = std::distance(v.begin(), missing);
// `missing` is v.end(), `index_missing` is 5 (ie. size of the vector)

```

```cpp
std::vector<int> v { 5, 4, 3, 2, 1 };

auto it = std::find(v.begin(), v.end(), 4);
auto index = std::distance(v.begin(), it);
// `it` points to the second element of the vector, `index` is 1

auto missing = std::find(v.begin(), v.end(), 10);
auto index_missing = std::distance(v.begin(), missing);
// `missing` is v.end(), `index_missing` is 5 (ie. size of the vector)

```

If you need to perform many searches in a large vector, then you may want to consider sorting the vector first, before using the [`binary_search`](http://en.cppreference.com/w/cpp/algorithm/binary_search) algorithm.

To find the first element in a vector that satisfies a condition, `std::find_if` can be used. In addition to the two parameters given to `std::find`, `std::find_if` accepts a third argument which is a function object or function pointer to a predicate function. The predicate should accept an element from the container as an argument and return a value convertible to `bool`, without modifying the container:

```cpp
bool isEven(int val) {
    return (val % 2 == 0); 
}

struct moreThan {
    moreThan(int limit) : _limit(limit) {}
    
    bool operator()(int val) {
        return val > _limit;
    }
    
    int _limit;
};

static const int arr[] = {1, 3, 7, 8};
std::vector<int> v (arr, arr + sizeof(arr) / sizeof(arr[0]) );
    
std::vector<int>::iterator it = std::find_if(v.begin(), v.end(), isEven);
// `it` points to 8, the first even element

std::vector<int>::iterator missing = std::find_if(v.begin(), v.end(), moreThan(10));
// `missing` is v.end(), as no element is greater than 10

```

```cpp
// find the first value that is even
std::vector<int> v = {1, 3, 7, 8};
auto it = std::find_if(v.begin(), v.end(), [](int val){return val % 2 == 0;});
// `it` points to 8, the first even element

auto missing = std::find_if(v.begin(), v.end(), [](int val){return val > 10;});
// `missing` is v.end(), as no element is greater than 10

```



## Concatenating Vectors


One `std::vector` can be append to another by using the member function [`insert()`](http://en.cppreference.com/w/cpp/container/vector/insert):

```cpp
std::vector<int> a = {0, 1, 2, 3, 4};
std::vector<int> b = {5, 6, 7, 8, 9};

a.insert(a.end(), b.begin(), b.end());

```

However, this solution fails if you try to append a vector to itself, because the standard specifies that iterators given to `insert()` must not be from the same range as the receiver object's elements.

Instead of using the vector's member functions, the functions [`std::begin()`](http://en.cppreference.com/w/cpp/iterator/begin) and [`std::end()`](http://en.cppreference.com/w/cpp/iterator/end) can be used:

```cpp
a.insert(std::end(a), std::begin(b), std::end(b));

```

This is a more general solution, for example, because `b` can also be an array. However, also this solution doesn't allow you to append a vector to itself.

If the order of the elements in the receiving vector doesn't matter, considering the number of elements in each vector can avoid unnecessary copy operations:

```cpp
if (b.size() < a.size())
  a.insert(a.end(), b.begin(), b.end());
else
  b.insert(b.end(), a.begin(), a.end());

```



## Reducing the Capacity of a Vector


A `std::vector` automatically increases its capacity upon insertion as needed, but it never reduces its capacity after element removal.

```cpp
// Initialize a vector with 100 elements
std::vector<int> v(100);

// The vector's capacity is always at least as large as its size
auto const old_capacity = v.capacity();
// old_capacity >= 100

// Remove half of the elements
v.erase(v.begin() + 50, v.end());  // Reduces the size from 100 to 50 (v.size() == 50),
                                   // but not the capacity (v.capacity() == old_capacity)

```

To reduce its capacity, we can copy the contents of a vector to a new temporary vector. The new vector will have the minimum capacity that is needed to store all elements of the original vector. If the size reduction of the original vector was significant, then the capacity reduction for the new vector is likely to be significant. We can then swap the original vector with the temporary one to retain its minimized capacity:

```cpp
std::vector<int>(v).swap(v);

```

In C++11 we can use the `shrink_to_fit()` member function for a similar effect:

```cpp
v.shrink_to_fit();

```

Note: The `shrink_to_fit()` member function is a request and doesn't guarantee to reduce capacity.



## Using a Sorted Vector for Fast Element Lookup


The [`<algorithm>`](http://en.cppreference.com/w/cpp/header/algorithm) header provides a number of useful functions for working with sorted vectors.

An important prerequisite for working with sorted vectors is that the stored values are comparable with `<`.

An unsorted vector can be sorted by using the function [`std::sort()`](http://en.cppreference.com/w/cpp/algorithm/sort):

```cpp
std::vector<int> v;
// add some code here to fill v with some elements
std::sort(v.begin(), v.end());

```

Sorted vectors allow efficient element lookup using the function [`std::lower_bound()`](http://en.cppreference.com/w/cpp/algorithm/lower_bound). Unlike [`std::find()`](http://en.cppreference.com/w/cpp/algorithm/find), this performs an efficient binary search on the vector. The downside is that it only gives valid results for sorted input ranges:

```cpp
// search the vector for the first element with value 42
std::vector<int>::iterator it = std::lower_bound(v.begin(), v.end(), 42);
if (it != v.end() && *it == 42) {
    // we found the element!
}

```

****Note:**** If the requested value is not part of the vector, `std::lower_bound()` will return an iterator to the first element that is **greater** than the requested value. This behavior allows us to insert a new element at its right place in an already sorted vector:

```cpp
int const new_element = 33;
v.insert(std::lower_bound(v.begin(), v.end(), new_element), new_element);

```

If you need to insert a lot of elements at once, it might be more efficient to call `push_back()` for all them first and then call `std::sort()` once all elements have been inserted. In this case, the increased cost of the sorting can pay off against the reduced cost of inserting new elements at the end of the vector and not in the middle.

If your vector contains multiple elements of the same value, `std::lower_bound()` will try to return an iterator to the first element of the searched value. However, if you need to insert a new element **after** the last element of the searched value, you should use the function [`std::upper_bound()`](http://en.cppreference.com/w/cpp/algorithm/upper_bound) as this will cause less shifting around of elements:

```cpp
v.insert(std::upper_bound(v.begin(), v.end(), new_element), new_element);

```

If you need both the upper bound and the lower bound iterators, you can use the function [`std::equal_range()`](http://en.cppreference.com/w/cpp/algorithm/equal_range) to retrieve both of them efficiently with one call:

```cpp
std::pair<std::vector<int>::iterator,
          std::vector<int>::iterator> rg = std::equal_range(v.begin(), v.end(), 42);
std::vector<int>::iterator lower_bound = rg.first;
std::vector<int>::iterator upper_bound = rg.second;

```

In order to test whether an element exists in a sorted vector (although not specific to vectors), you can use the function [`std::binary_search()`](http://en.cppreference.com/w/cpp/algorithm/binary_search):

```cpp
bool exists = std::binary_search(v.begin(), v.end(), value_to_find);

```



## Matrices Using Vectors


Vectors can be used as a 2D matrix by defining them as a vector of vectors.

A matrix with 3 rows and 4 columns with each cell initialised as 0 can be defined as:

```cpp
std::vector<std::vector<int> > matrix(3, std::vector<int>(4));

```

The syntax for initializing them using initialiser lists or otherwise are similar to that of a normal vector.

```

 std::vector<std::vector<int>> matrix = { {0,1,2,3},
                                           {4,5,6,7}, 
                                           {8,9,10,11}
                                         };

```

Values in such a vector can be accessed similar to a 2D array

```cpp
int var = matrix[0][2];

```

Iterating over the entire matrix is similar to that of a normal vector but with an extra dimension.

```cpp
for(int i = 0; i < 3; ++i)
{
    for(int j = 0; j < 4; ++j)
    {
        std::cout << matrix[i][j] << std::endl;
    }
}

```

```cpp
for(auto& row: matrix)
{
    for(auto& col : row)
    { 
        std::cout << col << std::endl;
    }
}

```

A vector of vectors is a convenient way to represent a matrix but it's not the most efficient: individual vectors are scattered around memory and the data structure isn't cache friendly.

Also, in a proper matrix, the length of every row must be the same (this isn't the case for a vector of vectors). The additional flexibility can be a source of errors.



## Iterator/Pointer Invalidation


Iterators and pointers pointing into an `std::vector` can become invalid, but only when performing certain operations. Using invalid iterators/pointers will result in undefined behavior.

Operations which invalidate iterators/pointers include:

<li>
Any insertion operation which changes the `capacity` of the `vector` will invalidate **all** iterators/pointers:

```cpp
vector<int> v(5); // Vector has a size of 5; capacity is unknown.
int *p1 = &v[0];
v.push_back(2);   // p1 may have been invalidated, since the capacity was unknown.

v.reserve(20);    // Capacity is now at least 20.
int *p2 = &v[0];
v.push_back(4);   // p2 is *not* invalidated, since the size of `v` is now 7.
v.insert(v.end(), 30, 9); // Inserts 30 elements at the end. The size exceeds the
                          // requested capacity of 20, so `p2` is (probably) invalidated.
int *p3 = &v[0];
v.reserve(v.capacity() + 20); // Capacity exceeded, thus `p3` is invalid.

```


</li>

```cpp
auto old_cap = v.capacity();
v.shrink_to_fit();
if(old_cap != v.capacity())
    // Iterators were invalidated.

```


<li>
Any insertion operation, which does not increase the capacity, will still invalidate iterators/pointers pointing to elements at the insertion position and past it. This includes the `end` iterator:

```cpp
vector<int> v(5);
v.reserve(20);                 // Capacity is at least 20.
int *p1 = &v[0];
int *p2 = &v[3];
v.insert(v.begin() + 2, 5, 0); // `p2` is invalidated, but since the capacity
                               // did not change, `p1` remains valid.
int *p3 = &v[v.size() - 1];
v.push_back(10); // The capacity did not change, so `p3` and `p1` remain valid.

```


</li>
<li>
Any removal operation will invalidate iterators/pointers pointing to the removed elements and to any elements past the removed elements. This includes the `end` iterator:

```cpp
vector<int> v(10);
int *p1 = &v[0];
int *p2 = &v[5];
v.erase(v.begin() + 3, v.end()); // `p2` is invalid, but `p1` remains valid.

```


</li>
<li>
`operator=` (copy, move, or otherwise) and `clear()` will invalidate all iterators/pointers pointing into the vector.
</li>



## Vector size and capacity


**Vector size** is simply the number of elements in the vector:

<li>
Current vector **size** is queried by `size()` member function. Convenience `empty()` function returns `true` if size is 0:

```cpp
vector<int> v = { 1, 2, 3 }; // size is 3
const vector<int>::size_type size = v.size();
cout << size << endl; // prints 3
cout << boolalpha << v.empty() << endl; // prints false

```


</li>
<li>
Default constructed vector starts with a size of 0:

```cpp
vector<int> v; // size is 0
cout << v.size() << endl; // prints 0

```


</li>
<li>
Adding `N` elements to vector increases **size** by `N` (e.g. by `push_back()`, `insert()` or `resize()` functions).
</li>
<li>
Removing `N` elements from vector decreases **size** by `N` (e.g. by `pop_back()`, `erase()` or `clear()` functions).
</li>
<li>
Vector has an implementation-specific upper limit on its size, but you are likely to run out of RAM before reaching it:

```cpp
vector<int> v;
const vector<int>::size_type max_size = v.max_size();
cout << max_size << endl; // prints some large number
v.resize( max_size ); // probably won't work
v.push_back( 1 ); // definitely won't work

```


</li>

Common mistake: **size** is not necessarily (or even usually) `int`:

```cpp
// !!!bad!!!evil!!!
vector<int> v_bad( N, 1 ); // constructs large N size vector
for( int i = 0; i < v_bad.size(); ++i ) { // size is not supposed to be int!
    do_something( v_bad[i] );
}

```

**Vector capacity** differs from **size**. While **size** is simply how many elements the vector currently has, **capacity** is for how many elements it allocated/reserved memory for. That is useful, because too frequent (re)allocations of too large sizes can be expensive.

<li>
Current vector **capacity** is queried by `capacity()` member function. **Capacity** is always greater or equal to **size**:

```cpp
vector<int> v = { 1, 2, 3 }; // size is 3, capacity is >= 3
const vector<int>::size_type capacity = v.capacity();
cout << capacity << endl; // prints number >= 3

```


</li>
<li>
You can manually reserve capacity by `reserve( N )` function (it changes vector capacity to `N`):

```cpp
// !!!bad!!!evil!!!
vector<int> v_bad;
for( int i = 0; i < 10000; ++i ) {
    v_bad.push_back( i ); // possibly lot of reallocations
}

// good
vector<int> v_good;
v_good.reserve( 10000 ); // good! only one allocation
for( int i = 0; i < 10000; ++i ) {
    v_good.push_back( i ); // no allocations needed anymore
}

```


</li>
<li>
You can request for the excess capacity to be released by `shrink_to_fit()` (but the implementation doesn't have to obey you). This is useful to conserve used memory:

```cpp
vector<int> v = { 1, 2, 3, 4, 5 }; // size is 5, assume capacity is 6
v.shrink_to_fit(); // capacity is 5 (or possibly still 6)
cout << boolalpha << v.capacity() == v.size() << endl; // prints likely true (but possibly false)

```


</li>

Vector partly manages capacity automatically, when you add elements it may decide to grow. Implementers like to use 2 or 1.5 for the grow factor (golden ratio would be the ideal value - but is impractical due to being rational number). On the other hand vector usually do not automatically shrink. For example:

```cpp
vector<int> v; // capacity is possibly (but not guaranteed) to be 0
v.push_back( 1 ); // capacity is some starter value, likely 1
v.clear(); // size is 0 but capacity is still same as before!

v = { 1, 2, 3, 4 }; // size is 4, and lets assume capacity is 4.
v.push_back( 5 ); // capacity grows - let's assume it grows to 6 (1.5 factor)
v.push_back( 6 ); // no change in capacity
v.push_back( 7 ); // capacity grows - let's assume it grows to 9 (1.5 factor)
// and so on
v.pop_back(); v.pop_back(); v.pop_back(); v.pop_back(); // capacity stays the same

```



## Find max and min Element and Respective Index in a Vector


To find the largest or smallest element stored in a vector, you can use the methods [`std::max_element`](http://en.cppreference.com/w/cpp/algorithm/max_element) and [`std::min_element`](http://en.cppreference.com/w/cpp/algorithm/min_element), respectively. These methods are defined in [`<algorithm>`](http://en.cppreference.com/w/cpp/algorithm) header. If several elements are equivalent to the greatest (smallest) element, the methods return the iterator to the first such element. Return `v.end()` for empty vectors.

```cpp
std::vector<int> v = {5, 2, 8, 10, 9}; 
int maxElementIndex = std::max_element(v.begin(),v.end()) - v.begin();
int maxElement = *std::max_element(v.begin(), v.end());

int minElementIndex = std::min_element(v.begin(),v.end()) - v.begin();
int minElement = *std::min_element(v.begin(), v.end());

std::cout << "maxElementIndex:" << maxElementIndex << ", maxElement:" << maxElement << '\n';
std::cout << "minElementIndex:" << minElementIndex << ", minElement:" << minElement << '\n';

```

Output:

> 
<p>maxElementIndex:3, maxElement:10<br />
minElementIndex:1, minElement:2</p>


The minimum and maximum element in a vector can be retrieved at the same time by using the method [`std::minmax_element`](http://en.cppreference.com/w/cpp/minmax_element), which is also defined in [`<algorithm>`](http://en.cppreference.com/w/cpp/algorithm) header:

```cpp
std::vector<int> v = {5, 2, 8, 10, 9}; 
auto minmax = std::minmax_element(v.begin(), v.end());

std::cout << "minimum element: " << *minmax.first << '\n';
std::cout << "maximum element: " << *minmax.second << '\n';

```

Output:

> 
<p>minimum element: 2<br />
maximum element: 10</p>




## Converting an array to std::vector


An array can easily be converted into a `std::vector` by using [`std::begin`](http://en.cppreference.com/w/cpp/iterator/begin) and [`std::end`](http://en.cppreference.com/w/cpp/iterator/end):

```cpp
int values[5] = { 1, 2, 3, 4, 5 }; // source array

std::vector<int> v(std::begin(values), std::end(values)); // copy array to new vector

for(auto &x: v)
    std::cout << x << " ";
std::cout << std::endl;

```

> 
1 2 3 4 5


```cpp
int main(int argc, char* argv[]) {
    // convert main arguments into a vector of strings.
    std::vector<std::string>  args(argv, argv + argc);
}

```

A C++11 initializer_list<> can also be used to initialize the vector at once

```cpp
initializer_list<int> arr = { 1,2,3,4,5 };
vector<int> vec1 {arr};

for (auto & i : vec1)
    cout << i << endl;

```



## Functions Returning Large Vectors


In C++11, compilers are required to implicitly move from a local variable that is being returned. Moreover, most compilers can perform [copy elision](http://stackoverflow.com/documentation/c%2B%2B/2489/copy-elision) in many cases and elide the move altogether. As a result of this, returning large objects that can be moved cheaply no longer requires special handling:

```cpp
#include <vector>
#include <iostream>

// If the compiler is unable to perform named return value optimization (NRVO)
// and elide the move altogether, it is required to move from v into the return value.
std::vector<int> fillVector(int a, int b) {
    std::vector<int> v;
    v.reserve(b-a+1);
    for (int i = a; i <= b; i++) {
        v.push_back(i);
    }
    return v; // implicit move
}

int main() { // declare and fill vector
    std::vector<int> vec = fillVector(1, 10);

    // print vector
    for (auto value : vec)
        std::cout << value << " "; // this will print "1 2 3 4 5 6 7 8 9 10 "

    std::cout << std::endl;

    return 0;
}

```

Before C++11, copy elision was already allowed and implemented by most compilers. However, due to the absence of move semantics, in legacy code or code that has to be compiled with older compiler versions which don't implement this optimization, you can find vectors being passed as output arguments to prevent the unneeded copy:

```cpp
#include <vector>
#include <iostream>

// passing a std::vector by reference
void fillVectorFrom_By_Ref(int a, int b, std::vector<int> &v) {
    assert(v.empty());
    v.reserve(b-a+1);
    for (int i = a; i <= b; i++) {
        v.push_back(i);
    }
}

int main() {// declare vector
    std::vector<int> vec;
    
    // fill vector
    fillVectorFrom_By_Ref(1, 10, vec);
    // print vector
    for (std::vector<int>::const_iterator it = vec.begin(); it != vec.end(); ++it)
        std::cout << *it << " "; // this will print "1 2 3 4 5 6 7 8 9 10 "
    std::cout << std::endl;
    return 0;
}

```



#### Remarks


Use of an `std::vector` requires the inclusion of the `<vector>` header using `#include <vector>`.

Elements in a `std::vector` are stored contiguously on the free store. It should be noted that when vectors are nested as in `std::vector<std::vector<int> >`, the elements of each vector are contiguous, but each vector allocates its own underlying buffer on the free store.

