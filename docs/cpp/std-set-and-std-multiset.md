---
metaTitle: "std::set and std::multiset"
description: "Changing the default sort of a set, Deleting values from a set, Inserting values in a set, Inserting values in a multiset, Searching values in set and multiset"
---

# std::set and std::multiset


`set` is a type of container whose elements are sorted and unique.  `multiset` is similar, but, in the case of `multiset`, multiple elements can have the same value.



## Changing the default sort of a set


`set` and `multiset` have default compare methods, but in some cases you may need to overload them.

Let's imagine we are storing string values in a set, but we know those strings contain only numeric values. By default the sort will be a lexicographical string comparison, so the order won't match the numerical sort. If you want to apply a sort equivalent to what you would have with `int` values, you need a functor to overload the compare method:

```cpp
#include <iostream>
#include <set>
#include <stdlib.h> 

struct custom_compare final
{
    bool operator() (const std::string& left, const std::string& right) const
    {
        int nLeft = atoi(left.c_str());
        int nRight = atoi(right.c_str());
        return nLeft < nRight;
    }
};

int main ()
{
    std::set<std::string> sut({"1", "2", "5", "23", "6", "290"});
  
    std::cout << "### Default sort on std::set<std::string> :" << std::endl;
    for (auto &&data: sut)
        std::cout << data << std::endl;
  
    std::set<std::string, custom_compare> sut_custom({"1", "2", "5", "23", "6", "290"},
                                                     custom_compare{}); //< Compare object optional as its default constructible.
  
    std::cout << std::endl << "### Custom sort on set :" << std::endl;
    for (auto &&data : sut_custom)
        std::cout << data << std::endl;
  
    auto compare_via_lambda = [](auto &&lhs, auto &&rhs){ return lhs > rhs; };
    using set_via_lambda = std::set<std::string, decltype(compare_via_lambda)>;
    set_via_lambda sut_reverse_via_lambda({"1", "2", "5", "23", "6", "290"},
                                          compare_via_lambda);
  
    std::cout << std::endl << "### Lambda sort on set :" << std::endl;
    for (auto &&data : sut_reverse_via_lambda)
        std::cout << data << std::endl;

    return 0;
}

```

Output will be:

```cpp
### Default sort on std::set<std::string> :
1
2
23
290
5
6
### Custom sort on set :
1
2
5
6
23
290  

### Lambda sort on set :
6
5
290
23
2
1

```

In the example above, one can find 3 different ways of adding compare operations to the `std::set`, each of them is useful in its own context.

### Default sort

This will use the compare operator of the key (first template argument).
Often, the key will already provide a good default for the `std::less<T>` function. Unless this function is specialized, it uses the `operator<` of the object.
This is especially useful when other code also tries to use some ordering, as this allows consistency over the whole code base.

Writing the code this way, will reduce the effort to update your code when the key changes is API, like: a class containing 2 members which changes to a class containing 3 members. By updating the `operator<` in the class, all occurrences will get updated.

As you might expect, using the default sort is a reasonable default.

### Custom sort

Adding a custom sort via an object with a compare operator is often used when the default comparison doesn't comply. In the example above this is because the strings are referring to integers. In other cases, it's often used when you want to compare (smart) pointers based upon the object they refer to or because you need different constraints for comparing (example: comparing `std::pair` by the value of `first`).

When creating a compare operator, this should be a stable sorting. If the result of the compare operator changes after insert, you will have undefined behavior. As a good practice, your compare operator should only use the constant data (const members, const functions ...).

As in the example above, you will often encounter classes without members as compare operators. This results in default constructors and copy constructors. The default constructor allows you to omit the instance at construction time and the copy constructor is required as the set takes a copy of the compare operator.

### Lambda sort

[Lambdas](http://stackoverflow.com/documentation/c%2b%2b/572/lambdas) are a shorter way to write function objects. This allows writing the compare operator on less lines, making the overall code more readable.

The disadvantage of the use of lambdas is that each lambda gets a specific type at compile time, so `decltype(lambda)` will be different for each compilation of the same compilation unit (cpp file) as over multiple compilation units (when included via header file). For this reason, its recommended to use function objects as compare operator when used within header files.

This construction is often encountered when a `std::set` is used within the local scope of a function instead, while the function object is preferred when used as function arguments or class members.

### Other sort options

As the compare operator of `std::set` is a template argument, all [callable objects](http://stackoverflow.com/documentation/c%2b%2b/6073/callable-objects) can be used as compare operator and the examples above are only specific cases. The only restrictions these callable objects have are:

- They must be copy constructable
- They must be callable with 2 arguments of the type of the key. (implicit conversions are allowed, though not recommended as it can hurt performance)



## Deleting values from a set


The most obvious method, if you just want to reset your set/multiset to an empty one, is to use `clear`:

```

 std::set<int> sut;
  sut.insert(10);
  sut.insert(15);
  sut.insert(22);
  sut.insert(3); 
  sut.clear(); //size of sut is 0

```

Then the `erase` method can be used.  It offers some possibilities looking somewhat equivalent to the insertion:

```cpp
std::set<int> sut;
std::set<int>::iterator it;
          
sut.insert(10);
sut.insert(15);
sut.insert(22);
sut.insert(3); 
sut.insert(30);
sut.insert(33);
sut.insert(45);
    
// Basic deletion
sut.erase(3);
    
// Using iterator
it = sut.find(22);
sut.erase(it);
          
// Deleting a range of values
it = sut.find(33);
sut.erase(it, sut.end());
    
std::cout << std::endl << "Set under test contains:" << std::endl;
for (it = sut.begin(); it != sut.end(); ++it)
{
  std::cout << *it << std::endl;
}

```

Output will be:

```cpp
Set under test contains:                                                                                                                                                                                                                                                                                                  
10                                                                                                                                                                                                                                                                                                                        
15                                                                                                                                                                                                                                                                                                                        
30 

```

All those methods also apply to `multiset`. Please note that if you ask to delete an element from a `multiset`, and it is present multiple times, **all the equivalent values will be deleted**.



## Inserting values in a set


Three different methods of insertion can used with sets.

- First, a simple insert of the value. This method returns a pair allowing the caller to check whether the insert really occurred.
- Second, an insert by giving a hint of where the value will be inserted. The objective is to optimize the insertion time in such a case, but knowing where a value should be inserted is not the common case. **Be careful in that case; the way to give a hint differs with compiler versions**.
- Finally you can insert a range of values by giving a starting and an ending pointer. The starting one will be included in the insertion, the ending one is excluded.

```cpp
#include <iostream>
#include <set>

int main ()
{
  std::set<int> sut;
  std::set<int>::iterator it;
  std::pair<std::set<int>::iterator,bool> ret;

  // Basic insert
  sut.insert(7);
  sut.insert(5);
  sut.insert(12);
  
  ret = sut.insert(23);
  if (ret.second==true)
    std::cout << "# 23 has been inserted!" << std::endl;
    
  ret = sut.insert(23); // since it's a set and 23 is already present in it, this insert should fail
  if (ret.second==false)
    std::cout << "# 23 already present in set!" << std::endl;
 
  // Insert with hint for optimization
  it = sut.end();
  // This case is optimized for C++11 and above
  // For earlier version, point to the element preceding your insertion
  sut.insert(it, 30); 

  // inserting a range of values
  std::set<int> sut2;
  sut2.insert(20);
  sut2.insert(30);
  sut2.insert(45);
  std::set<int>::iterator itStart = sut2.begin();
  std::set<int>::iterator itEnd = sut2.end();
  
  sut.insert (itStart, itEnd); // second iterator is excluded from insertion

  std::cout << std::endl << "Set under test contains:" << std::endl;
  for (it = sut.begin(); it != sut.end(); ++it)
  {
    std::cout << *it << std::endl;
  }

  return 0;
}

```

Output will be:

```cpp
# 23 has been inserted!                                                                                                                                                                                                                                                                                                   
# 23 already present in set!                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                                          
Set under test contains:                                                                                                                                                                                                                                                                                                  
5                                                                                                                                                                                                                                                                                                                         
7                                                                                                                                                                                                                                                                                                                         
12                                                                                                                                                                                                                                                                                                                        
20                                                                                                                                                                                                                                                                                                                        
23                                                                                                                                                                                                                                                                                                                        
30                                                                                                                                                                                                                                                                                                                        
45   

```



## Inserting values in a multiset


All the insertion methods from sets also apply to multisets. Nevertheless, another possibility exists, which is providing an initializer_list:

```cpp
auto il = { 7, 5, 12 };
std::multiset<int> msut;
msut.insert(il);

```



## Searching values in set and multiset


There are several ways to search a given value in `std::set` or in `std::multiset`:

To get the iterator of the first occurrence of a key, the `find()` function can be used. It returns `end()` if the key does not exist.

```

 std::set<int> sut;
  sut.insert(10);
  sut.insert(15);
  sut.insert(22);
  sut.insert(3); // contains 3, 10, 15, 22    

  auto itS = sut.find(10); // the value is found, so *itS == 10
  itS = sut.find(555); // the value is not found, so itS == sut.end()   

  std::multiset<int> msut;
  sut.insert(10);
  sut.insert(15);
  sut.insert(22);
  sut.insert(15);
  sut.insert(3); // contains 3, 10, 15, 15, 22  

  auto itMS = msut.find(10);

```

Another way is using the `count()` function, which counts how many corresponding values have been found in the `set`/`multiset` (in case of a `set`, the return value can be only 0 or 1). Using the same values as above, we will have:

```cpp
int result = sut.count(10); // result == 1
result = sut.count(555); // result == 0

result = msut.count(10); // result == 1
result = msut.count(15); // result == 2

```

In the case of `std::multiset`, there could be several elements having the same value. To get this range, the `equal_range()` function can be used. It returns `std::pair` having iterator lower bound (inclusive) and upper bound (exclusive) respectively. If the key does not exist, both iterators would point to the nearest superior value (based on compare method used to sort the given `multiset`).

```cpp
auto eqr = msut.equal_range(15);
auto st = eqr.first; // point to first element '15'
auto en = eqr.second; // point to element '22'

eqr = msut.equal_range(9); // both eqr.first and eqr.second point to element '10'

```



#### Remarks


Different styles of C++ have been used in those examples.  Be careful that if you are using a C++98 compiler; some of this code may not be usable.

