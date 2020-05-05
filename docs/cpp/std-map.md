---
metaTitle: "C++ | std::map"
description: "Accessing elements, Inserting elements, Searching in std::map or in std::multimap, Initializing a std::map or std::multimap, Checking number of elements, Types of Maps, Deleting elements, Iterating over std::map or std::multimap, Creating std::map with user-defined types as key"
---

# std::map




## Accessing elements


An [`std::map`](http://en.cppreference.com/w/cpp/container/map) takes `(key, value)` pairs as input.

Consider the following example of [`std::map`](http://en.cppreference.com/w/cpp/container/map) initialization:

```cpp
std::map < std::string, int > ranking { std::make_pair("stackoverflow", 2), 
                                        std::make_pair("docs-beta", 1) };

```

In an [`std::map`](http://en.cppreference.com/w/cpp/container/map) , elements can be inserted as follows:

```cpp
ranking["stackoverflow"]=2;
ranking["docs-beta"]=1;

```

In the above example, if the key `stackoverflow` is already present, its value will be updated to 2. If it isn't already present, a new entry will be created.

In an [`std::map`](http://en.cppreference.com/w/cpp/container/map), elements can be accessed directly by giving the key as an index:

```cpp
std::cout << ranking[ "stackoverflow" ] << std::endl;

```

Note that using the `operator[]` on the map will actually **insert a new value** with the queried key into the map. This means that you cannot use it on a `const std::map`, even if the key is already stored in the map. To prevent this insertion, check if the element exists (for example by using `find()`) or use `at()` as described below.

Elements of a `std::map` can be accessed with `at()`:

```cpp
std::cout << ranking.at("stackoverflow") << std::endl;

```

Note that `at()` will throw an `std::out_of_range` exception if the container does not contain the requested element.

In both containers `std::map` and [`std::multimap`](http://en.cppreference.com/w/cpp/container/multimap), elements can be accessed using iterators:

```cpp
// Example using begin()
std::multimap < int, std::string > mmp { std::make_pair(2, "stackoverflow"),
                                         std::make_pair(1, "docs-beta"),
                                         std::make_pair(2, "stackexchange")  };
auto it = mmp.begin();
std::cout << it->first << " : " << it->second << std::endl; // Output: "1 : docs-beta"
it++;
std::cout << it->first << " : " << it->second << std::endl; // Output: "2 : stackoverflow"
it++;
std::cout << it->first << " : " << it->second << std::endl; // Output: "2 : stackexchange"

// Example using rbegin()
std::map < int, std::string > mp {  std::make_pair(2, "stackoverflow"),
                                    std::make_pair(1, "docs-beta"),
                                    std::make_pair(2, "stackexchange")  };
auto it2 = mp.rbegin();
std::cout << it2->first << " : " << it2->second << std::endl; // Output: "2 : stackoverflow"
it2++;
std::cout << it2->first << " : " << it2->second << std::endl; // Output: "1 : docs-beta"

```



## Inserting elements


An element can be inserted into a `std::map` only if its key is not already present in the map. Given for example:

```cpp
std::map< std::string, size_t > fruits_count;

```


<li>
A key-value pair is inserted into a `std::map` through the `insert()` member function. It requires a `pair` as an argument:

```cpp
fruits_count.insert({"grapes", 20});
fruits_count.insert(make_pair("orange", 30));
fruits_count.insert(pair<std::string, size_t>("banana", 40));
fruits_count.insert(map<std::string, size_t>::value_type("cherry", 50));

```


The `insert()` function returns a `pair` consisting of an iterator and a `bool` value:
<ul>
- If the insertion was successful, the iterator points to the newly inserted element, and the `bool` value is `true`.
- If there was already an element with the same `key`, the insertion fails. When that happens, the iterator points to the element causing the conflict, and the `bool` is value is `false`.

The following method can be used to combine insertion and searching operation:

```cpp
auto success = fruits_count.insert({"grapes", 20});
if (!success.second) {           // we already have 'grapes' in the map
    success.first->second += 20; // access the iterator to update the value
}

```

For convenience, the `std::map` container provides the subscript operator to access elements in the map and to insert new ones if they don't exist:

```cpp
fruits_count["apple"] = 10;

```

While simpler, it prevents the user from checking if the element already exists. If an element is missing, `std::map::operator[]` implicitly creates it, initializing it with the default constructor before overwriting it with the supplied value.

<li>
`insert()` can be used to add several elements at once using a braced list of pairs. This version of insert() returns void:

```cpp
fruits_count.insert({{"apricot", 1}, {"jackfruit", 1}, {"lime", 1}, {"mango", 7}}); 

```


</li>
<li>
`insert()` can also be used to add elements by using iterators denoting the begin and end of `value_type` values:

```cpp
std::map< std::string, size_t > fruit_list{ {"lemon", 0}, {"olive", 0}, {"plum", 0}};
fruits_count.insert(fruit_list.begin(), fruit_list.end()); 

```


</li>

Example:

```cpp
std::map<std::string, size_t> fruits_count;
std::string fruit;
while(std::cin >> fruit){
    // insert an element with 'fruit' as key and '1' as value
    // (if the key is already stored in fruits_count, insert does nothing)
    auto ret = fruits_count.insert({fruit, 1});
    if(!ret.second){            // 'fruit' is already in the map 
        ++ret.first->second;    // increment the counter
    }
}

```

Time complexity for an insertion operation is O(log n) because `std::map` are implemented as trees.

A `pair` can be constructed explicitly using `make_pair()` and `emplace()`:

```cpp
std::map< std::string , int > runs;
runs.emplace("Babe Ruth", 714);
runs.insert(make_pair("Barry Bonds", 762));

```

If we know where the new element will be inserted, then we can use `emplace_hint()` to specify an iterator `hint`. If the new element can be inserted just before `hint`, then the insertion can be done in constant time. Otherwise it behaves in the same way as `emplace()`:

```cpp
std::map< std::string , int > runs;
auto it = runs.emplace("Barry Bonds", 762); // get iterator to the inserted element
// the next element will be before "Barry Bonds", so it is inserted before 'it'
runs.emplace_hint(it, "Babe Ruth", 714);

```



## Searching in std::map or in std::multimap


There are several ways to search a key in `std::map` or in `std::multimap`.

<li>
To get the iterator of the first occurrence of a key, the `find()` function can be used. It returns `end()` if the key does not exist.

```cpp
  std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
  auto it = mmp.find(6);
  if(it!=mmp.end())
      std::cout << it->first << ", " << it->second << std::endl; //prints: 6, 5
  else
      std::cout << "Value does not exist!" << std::endl;

  it = mmp.find(66);
  if(it!=mmp.end())
      std::cout << it->first << ", " << it->second << std::endl; 
  else
      std::cout << "Value does not exist!" << std::endl; // This line would be executed.

```


</li>
<li>
Another way to find whether an entry exists in `std::map` or in `std::multimap` is using the `count()` function, which counts how many values are associated with a given key. Since `std::map` associates only one value with each key, its `count()` function can only return 0 (if the key is not present) or 1 (if it is).  For `std::multimap`, `count()` can return values greater than 1 since there can be several values associated with the same key.

```cpp
 std::map< int , int > mp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
 if(mp.count(3) > 0) // 3 exists as a key in map
     std::cout << "The key exists!" << std::endl; // This line would be executed.
 else
     std::cout << "The key does not exist!" << std::endl;

```


If you only care whether some element exists, `find` is strictly better: it documents your intent and, for `multimaps`, it can stop once the first matching element has been found.
</li>
<li>
In the case of `std::multimap`, there could be several elements having the same key. To get this range, the `equal_range()` function is used which returns `std::pair` having iterator lower bound (inclusive) and upper bound (exclusive) respectively. If the key does not exist, both iterators would point to `end()`.

```cpp
  auto eqr = mmp.equal_range(6);
  auto st = eqr.first, en = eqr.second;
  for(auto it = st; it != en; ++it){
      std::cout << it->first << ", " << it->second << std::endl; 
  }
      // prints: 6, 5
      //         6, 7

```


</li>



## Initializing a std::map or std::multimap


`std::map` and `std::multimap` both can be initialized by providing key-value pairs separated by comma. Key-value pairs could be provided by either `{key, value}` or can be explicitly created by `std::make_pair(key, value)`. As `std::map` does not allow duplicate keys and comma operator performs right to left, the pair on right would be overwritten with the pair with same key on the left.

```cpp
std::multimap < int, std::string > mmp { std::make_pair(2, "stackoverflow"),
                                     std::make_pair(1, "docs-beta"),
                                     std::make_pair(2, "stackexchange")  };
// 1 docs-beta
// 2 stackoverflow
// 2 stackexchange

std::map < int, std::string > mp {  std::make_pair(2, "stackoverflow"),
                                std::make_pair(1, "docs-beta"),
                                std::make_pair(2, "stackexchange")  }; 
// 1 docs-beta
// 2 stackoverflow

```

Both could be initialized with iterator.

```cpp
// From std::map or std::multimap iterator
std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {6, 8}, {3, 4}, 
                               {6, 7} };
                       // {1, 2}, {3, 4}, {3, 4}, {6, 5}, {6, 8}, {6, 7}, {8, 9}
auto it = mmp.begin();
std::advance(it,3); //moved cursor on first {6, 5}
std::map< int, int > mp(it, mmp.end()); // {6, 5}, {8, 9}

//From std::pair array
std::pair< int, int > arr[10];
arr[0] = {1, 3};
arr[1] = {1, 5};
arr[2] = {2, 5};
arr[3] = {0, 1};
std::map< int, int > mp(arr,arr+4); //{0 , 1}, {1, 3}, {2, 5}

//From std::vector of std::pair
std::vector< std::pair<int, int> > v{ {1, 5}, {5, 1}, {3, 6}, {3, 2} };
std::multimap< int, int > mp(v.begin(), v.end()); 
                        // {1, 5}, {3, 6}, {3, 2}, {5, 1}

```



## Checking number of elements


The container `std::map` has a member function `empty()`, which returns `true` or `false`, depending on whether the map is empty or not. The member function `size()` returns the number of element stored in a `std::map` container:

```cpp
std::map<std::string , int> rank {{"facebook.com", 1} ,{"google.com", 2}, {"youtube.com", 3}};
if(!rank.empty()){
    std::cout << "Number of elements in the rank map: " << rank.size() << std::endl;
}
else{
    std::cout << "The rank map is empty" << std::endl;
}

```



## Types of Maps


### Regular Map

A map is an associative container, containing key-value pairs.

```cpp
#include <string>
#include <map>
std::map<std::string, size_t> fruits_count;

```

In the above example, `std::string` is the **key** type, and `size_t` is a **value**.

The key acts as an index in the map. Each key must be unique, and must be ordered.

<li>
If you need mutliple elements with the same key, consider using `multimap` (explained below)
</li>
<li>
If your value type does not specify any ordering, or you want to override the default ordering, you may provide one:

```cpp
#include <string>
#include <map>
#include <cstring>
struct StrLess {
    bool operator()(const std::string& a, const std::string& b) {
        return strncmp(a.c_str(), b.c_str(), 8)<0;
               //compare only up to 8 first characters
    }
}
std::map<std::string, size_t, StrLess> fruits_count2;

```


If `StrLess` comparator returns `false` for two keys, they are considered the same even if their actual contents differ.
</li>

### Multi-Map

Multimap allows multiple key-value pairs with the same key to be stored in the map. Otherwise, its interface and creation is very similar to the regular map.

```

#include <string>
 #include <map>
 std::multimap<std::string, size_t> fruits_count;
 std::multimap<std::string, size_t, StrLess> fruits_count2;

```

### Hash-Map (Unordered Map)

A hash map stores key-value pairs similar to a regular map. It does not order the elements with respect to the key though. Instead, a [hash](https://en.wikipedia.org/wiki/Hash_function) value for the key is used to quickly access the needed key-value pairs.

```cpp
#include <string>
#include <unordered_map>
std::unordered_map<std::string, size_t> fruits_count;

```

Unordered maps are usually faster, but the elements are not stored in any predictable order. For example, iterating over all elements in an `unordered_map` gives the elements in a seemingly random order.



## Deleting elements


Removing all elements:

```cpp
std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
mmp.clear(); //empty multimap

```

Removing element from somewhere with the help of iterator:

```cpp
std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
                            // {1, 2}, {3, 4}, {3, 4}, {6, 5}, {6, 7}, {8, 9}
auto it = mmp.begin();
std::advance(it,3); // moved cursor on first {6, 5}
mmp.erase(it); // {1, 2}, {3, 4}, {3, 4}, {6, 7}, {8, 9}

```

Removing all elements in a range:

```cpp
std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
                            // {1, 2}, {3, 4}, {3, 4}, {6, 5}, {6, 7}, {8, 9}
auto it = mmp.begin();
auto it2 = it;
it++; //moved first cursor on first {3, 4}
std::advance(it2,3);  //moved second cursor on first {6, 5}
mmp.erase(it,it2); // {1, 2}, {6, 5}, {6, 7}, {8, 9}

```

Removing all elements having a provided value as key:

```cpp
std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
                            // {1, 2}, {3, 4}, {3, 4}, {6, 5}, {6, 7}, {8, 9}
mmp.erase(6); // {1, 2}, {3, 4}, {3, 4}, {8, 9}

```

Removing elements that satisfy a predicate `pred`:

```cpp
std::map<int,int> m;
auto it = m.begin();
while (it != m.end())
{
   if (pred(*it))
       it = m.erase(it);
   else
       ++it;
}

```



## Iterating over std::map or std::multimap


`std::map` or `std::multimap` could be traversed by the following ways:

```cpp
std::multimap< int , int > mmp{ {1, 2}, {3, 4}, {6, 5}, {8, 9}, {3, 4}, {6, 7} };
                               
//Range based loop - since C++11
for(const auto &x: mmp) 
    std::cout<< x.first <<":"<< x.second << std::endl;

//Forward iterator for loop: it would loop through first element to last element
//it will be a std::map< int, int >::iterator
for (auto it = mmp.begin(); it != mmp.end(); ++it)
std::cout<< it->first <<":"<< it->second << std::endl; //Do something with iterator

//Backward iterator for loop: it would loop through last element to first element
//it will be a std::map< int, int >::reverse_iterator
for (auto it = mmp.rbegin(); it != mmp.rend(); ++it)
std::cout<< it->first <<" "<< it->second << std::endl; //Do something with iterator

```

While iterating over a `std::map` or a `std::multimap`, the use of `auto` is preferred to avoid useless implicit conversions (see [this SO answer](http://stackoverflow.com/questions/32510183/can-the-use-of-c11s-auto-improve-performance) for more details).



## Creating std::map with user-defined types as key


In order to be able to use a class as the key in a map, all that is required of the key is that it be `copiable` and `assignable`.
The ordering within the map is defined by the third argument to the
template (and the argument to the constructor, if used).  This
**defaults** to `std::less<KeyType>`, which defaults to the `<` operator,
but there's no requirement to use the defaults.  Just write a comparison
operator (preferably as a functional object):

```cpp
struct CmpMyType
{
    bool operator()( MyType const& lhs, MyType const& rhs ) const
    {
        //  ...
    }
};

```

In C++, the "compare" predicate must be a [strict weak ordering](http://www.sgi.com/tech/stl/StrictWeakOrdering.html). In particular, `compare(X,X)` must return `false` for any `X`. i.e. if `CmpMyType()(a, b)` returns true, then `CmpMyType()(b, a)` must return false, and if both return false, the elements are considered equal (members of the same equivalence class).

### Strict Weak Ordering

This is a mathematical term to define a relationship between two objects.<br />
Its definition is:

> 
Two objects x and y are equivalent if both f(x, y) and f(y, x) are false. Note that an object is always (by the irreflexivity invariant) equivalent to itself.


In terms of C++ this means if you have two objects of a given type, you should return the following values when compared with the operator <.

```cpp
X    a;
X    b;

Condition:                  Test:     Result
a is equivalent to b:       a < b     false
a is equivalent to b        b < a     false

a is less than b            a < b     true
a is less than b            b < a     false

b is less than a            a < b     false
b is less than a            b < a     true

```

How you define equivalent/less is totally dependent on the type of your object.



#### Remarks


<li>
To use any of `std::map` or `std::multimap` the header file `<map>` should be included.
</li>
<li>
`std::map` and `std::multimap` both keep their elements sorted according to the ascending order of keys. In case of `std::multimap`, no sorting occurs for the values of the same key.
</li>
<li>
The basic difference between `std::map` and `std::multimap` is that the `std::map` one does not allow duplicate values for the same key where `std::multimap` does.
</li>
<li>
Maps are implemented as binary search trees. So `search()`, `insert()`, `erase()` takes Θ(log n) time in average. For constant time operation use `std::unordered_map`.
</li>
<li>
`size()` and `empty()` functions have Θ(1) time complexity, number of nodes is cached to avoid walking through tree each time these functions are called.
</li>

