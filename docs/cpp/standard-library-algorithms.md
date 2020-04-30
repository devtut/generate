---
metaTitle: "Standard Library Algorithms"
description: "std::next_permutation, std::for_each, std::accumulate, std::find, std::count, std::count_if, std::find_if, std::min_element, Using std::nth_element To Find The Median (Or Other Quantiles)"
---

# Standard Library Algorithms



## std::next_permutation


```cpp
template< class Iterator >
bool next_permutation( Iterator first, Iterator last );
template< class Iterator, class Compare >
bool next_permutation( Iterator first, Iterator last, Compare cmpFun );

```

**Effects:**<br />
Sift the data sequence of the range [first, last) into the next lexicographically higher permutation. If `cmpFun` is provided, the permutation rule is customized.

**Parameters:**<br />
`first`- the beginning of the range to be permutated, inclusive<br />
`last` - the end of the range to be permutated, exclusive

**Return Value:**<br />
Returns true if such permutation exists.<br />
Otherwise the range is swaped to the lexicographically smallest permutation and return false.

**Complexity:**<br />
O(n), n is the distance from `first` to `last`.

**Example**:

```cpp
std::vector< int > v { 1, 2, 3 };
do
{
   for( int i = 0; i < v.size(); i += 1 )
   {
       std::cout << v[i];
   }
   std::cout << std::endl;
}while( std::next_permutation( v.begin(), v.end() ) );

```

print all the permutation cases of 1,2,3 in lexicographically-increasing order.<br />
output:

```cpp
123  
132
213
231
312
321

```



## std::for_each


```cpp
template<class InputIterator, class Function>
    Function for_each(InputIterator first, InputIterator last, Function f);

```

**Effects:**

Applies `f` to the result of dereferencing every iterator in the range `[first, last)` starting from `first` and proceeding to `last - 1`.

**Parameters:**

`first, last` - the range to apply `f` to.

`f` - callable object which is applied to the result of dereferencing every iterator in the range `[first, last)`.

**Return value:**

`f` (until C++11) and `std::move(f)` (since C++11).

**Complexity:**

Applies `f` exactly `last - first` times.

**Example:**

```cpp
std::vector<int> v { 1, 2, 4, 8, 16 };
std::for_each(v.begin(), v.end(), [](int elem) { std::cout << elem << " "; });

```

Applies the given function for every element of the vector `v` printing this element to `stdout`.



## std::accumulate


Defined in header `<numeric>`

```cpp
template<class InputIterator, class T>
T accumulate(InputIterator first, InputIterator last, T init); // (1)

template<class InputIterator, class T, class BinaryOperation>
T accumulate(InputIterator first, InputIterator last, T init, BinaryOperation f); // (2)

```

**Effects:**

[std::accumulate](http://en.cppreference.com/w/cpp/algorithm/accumulate) performs [fold](https://en.wikipedia.org/wiki/Fold_(higher-order_function)) operation using `f` function on range `[first, last)` starting with `init` as accumulator value.

Effectively it's equivalent of:

```cpp
T acc = init;
for (auto it = first; first != last; ++it)
    acc = f(acc, *it);
return acc;

```

In version (1) `operator+` is used in place of `f`, so accumulate over container is equivalent of sum of container elements.

**Parameters:**

`first, last` - the range to apply `f` to.<br/>
`init` - initial value of accumulator.<br/>
`f` - binary folding function.

**Return value:**

Accumulated value of `f` applications.

**Complexity:**

**O(n×k)**, where **n** is the distance from `first` to `last`, **O(k)** is complexity of `f` function.

**Example:**

Simple sum example:

```cpp
std::vector<int> v { 2, 3, 4 };
auto sum = std::accumulate(v.begin(), v.end(), 1);
std::cout << sum << std::endl;

```

Output:

```cpp
10

```

Convert digits to number:

```cpp
class Converter {
public:
    int operator()(int a, int d) const { return a * 10 + d; }
};

```

and later

```cpp
const int ds[3] = {1, 2, 3};
int n = std::accumulate(ds, ds + 3, 0, Converter());
std::cout << n << std::endl;

```

```cpp
const std::vector<int> ds = {1, 2, 3};
int n = std::accumulate(ds.begin(), ds.end(),
                        0,
                        [](int a, int d) { return a * 10 + d; });
std::cout << n << std::endl;

```

Output:

```cpp
123

```



## std::find


```cpp
template <class InputIterator, class T>
InputIterator find (InputIterator first, InputIterator last, const T& val);

```

**Effects**

Finds the first occurrence of val within the range [first, last)

**Parameters**

`first` => iterator pointing to the beginning of the range
`last`  => iterator pointing to the end of the range
`val`   => The value to find within the range

**Return**

An iterator that points to the first element within the range that is equal(==) to val, the iterator points to last if val is not found.

**Example**

```cpp
#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

int main(int argc, const char * argv[]) {

  //create a vector
  vector<int> intVec {4, 6, 8, 9, 10, 30, 55,100, 45, 2, 4, 7, 9, 43, 48};

  //define iterators
  vector<int>::iterator  itr_9; 
  vector<int>::iterator  itr_43; 
  vector<int>::iterator  itr_50; 

  //calling find
  itr_9 = find(intVec.begin(), intVec.end(), 9); //occurs twice
  itr_43 = find(intVec.begin(), intVec.end(), 43); //occurs once

  //a value not in the vector
  itr_50 = find(intVec.begin(), intVec.end(), 50); //does not occur

  cout << "first occurence of: " << *itr_9 << endl;
  cout << "only occurence of: " << *itr_43 << Lendl;


  /*
    let's prove that itr_9 is pointing to the first occurence
    of 9 by looking at the element after 9, which should be 10 
    not 43
  */
  cout << "element after first 9: " << *(itr_9 + 1) << ends;

  /*
    to avoid dereferencing intVec.end(), lets look at the 
    element right before the end
  */
  cout << "last element: " << *(itr_50 - 1) << endl;

  return 0;
}

```

**Output**

```cpp
first occurence of: 9
only occurence of: 43
element after first 9: 10
last element: 48

```



## std::count


```cpp
template <class InputIterator, class T>
typename iterator_traits<InputIterator>::difference_type
count (InputIterator first, InputIterator last, const T& val);

```

**Effects**

Counts the number of elements that are equal to val

**Parameters**

`first` => iterator pointing to the beginning of the range<br />
`last`  => iterator pointing to the end of the range<br />
`val`   => The occurrence of this value in the range will be counted

**Return**

The number of elements in the range that are equal(==) to val.

**Example**

```cpp
#include <vector>
#include <algorithm>
#include <iostream>

using namespace std;

int main(int argc, const char * argv[]) {
  
  //create vector
  vector<int> intVec{4,6,8,9,10,30,55,100,45,2,4,7,9,43,48};
  
  //count occurences of 9, 55, and 101
  size_t count_9 = count(intVec.begin(), intVec.end(), 9); //occurs twice
  size_t count_55 = count(intVec.begin(), intVec.end(), 55); //occurs once
  size_t count_101 = count(intVec.begin(), intVec.end(), 101); //occurs once
  
  //print result
  cout << "There are " << count_9  << " 9s"<< endl;
  cout << "There is " << count_55  << " 55"<< endl;
  cout << "There is " << count_101  << " 101"<< ends;

  //find the first element == 4 in the vector
  vector<int>::iterator itr_4 = find(intVec.begin(), intVec.end(), 4);

  //count its occurences in the vector starting from the first one
  size_t count_4 = count(itr_4, intVec.end(), *itr_4); // should be 2

  cout << "There are " << count_4  << " " << *itr_4 << endl;

  return 0;
}

```

**Output**

```cpp
There are 2 9s
There is 1 55
There is 0 101
There are 2 4

```



## std::count_if


```cpp
template <class InputIterator, class UnaryPredicate>
typename iterator_traits<InputIterator>::difference_type
count_if (InputIterator first, InputIterator last, UnaryPredicate red);

```

**Effects**

Counts the number of elements in a range for which a specified predicate function is true

**Parameters**

`first` => iterator pointing to the beginning of the range
`last`  => iterator pointing to the end of the range
`red` => predicate function(returns true or false)

**Return**

The number of elements within the specified range for which the predicate function returned true.

**Example**

```cpp
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

/*
    Define a few functions to use as predicates
*/

//return true if number is odd
bool isOdd(int i){
  return i%2 == 1;
}

//functor that returns true if number is greater than the value of the constructor parameter provided
class Greater {
  int _than;
public:
  Greater(int th): _than(th){}
  bool operator()(int i){
    return i > _than;
  }
};

int main(int argc, const char * argv[]) {
  
  //create a vector
  vector<int> myvec = {1,5,8,0,7,6,4,5,2,1,5,0,6,9,7};

  //using a lambda function to count even numbers
  size_t evenCount = count_if(myvec.begin(), myvec.end(), [](int i){return i % 2 == 0;}); // >= C++11
  
  //using function pointer to count odd number in the first half of the vector
  size_t oddCount = count_if(myvec.begin(), myvec.end()- myvec.size()/2, isOdd);
  
  //using a functor to count numbers greater than 5
  size_t greaterCount = count_if(myvec.begin(), myvec.end(), Greater(5));

  cout << "vector size: " << myvec.size() << endl;
  cout << "even numbers: " << evenCount << " found" << endl;
  cout << "odd numbers: " << oddCount << " found" << endl;
  cout << "numbers > 5: " << greaterCount << " found"<< endl;
  
  return 0;
}

```

**Output**

```cpp
vector size: 15
even numbers: 7 found
odd numbers: 4 found
numbers > 5: 6 found

```



## std::find_if


```cpp
template <class InputIterator, class UnaryPredicate>
InputIterator find_if (InputIterator first, InputIterator last, UnaryPredicate pred);

```

**Effects**

Finds the first element in a range for which the predicate function `pred` returns true.

**Parameters**

`first` => iterator pointing to the beginning of the range
`last`  => iterator pointing to the end of the range
`pred`  => predicate function(returns true or false)

**Return**

An iterator that points to the first element within the range the predicate function pred returns true for. The iterator points to last if val is not found

**Example**

```cpp
#include <iostream>
#include <vector>
#include <algorithm>


using namespace std;

/*
    define some functions to use as predicates
*/

//Returns true if x is multiple of 10
bool multOf10(int x) {
  return x % 10 == 0;
}

//returns true if item greater than passed in parameter
class Greater {
  int _than;

public:
  Greater(int th):_than(th){
    
  }
  bool operator()(int data) const 
  {
    return data > _than;
  }
};


int main()
{

  vector<int> myvec {2, 5, 6, 10, 56, 7, 48, 89, 850, 7, 456};
  
  //with a lambda function
  vector<int>::iterator gt10 = find_if(myvec.begin(), myvec.end(), [](int x){return x>10;}); // >= C++11
  
  //with a function pointer
  vector<int>::iterator pow10 = find_if(myvec.begin(), myvec.end(), multOf10);

  //with functor
  vector<int>::iterator gt5 = find_if(myvec.begin(), myvec.end(), Greater(5));

  //not Found
  vector<int>::iterator nf = find_if(myvec.begin(), myvec.end(), Greater(1000)); // nf points to myvec.end()


  //check if pointer points to myvec.end()
  if(nf != myvec.end()) {
    cout << "nf points to: " << *nf << endl;
  }
  else {
    cout << "item not found" << endl;
  }

  
  
  cout << "First item >   10: " << *gt10  << endl;
  cout << "First Item n * 10: " << *pow10 << endl;
  cout << "First Item >    5: " << *gt5   << endl;
  
  return 0;
}

```

**Output**

```cpp
item not found
First item >   10: 56
First Item n * 10: 10
First Item >    5: 6

```



## std::min_element


```cpp
template <class ForwardIterator>
ForwardIterator min_element (ForwardIterator first, ForwardIterator last);

template <class ForwardIterator, class Compare>
ForwardIterator min_element (ForwardIterator first, ForwardIterator last,Compare comp);

```

**Effects**

Finds the minimum element in a range

**Parameters**

`first` - iterator pointing to the beginning of the range<br />
`last` - iterator pointing to the end of the range
`comp` - a function pointer or function object that takes two arguments and returns true or false indicating whether argument is less than argument 2. This function should not modify inputs

**Return**

Iterator to the minimum element in the range

**Complexity**

Linear in one less than the number of elements compared.

**Example**

```cpp
#include <iostream>
#include <algorithm>
#include <vector>
#include <utility>  //to use make_pair

using namespace std;

//function compare two pairs
bool pairLessThanFunction(const pair<string, int> &p1, const pair<string, int> &p2)
{
  return p1.second < p2.second;
}

int main(int argc, const char * argv[]) {
  
  vector<int> intVec {30,200,167,56,75,94,10,73,52,6,39,43};
  
  vector<pair<string, int>> pairVector = {make_pair("y", 25), make_pair("b", 2), make_pair("z", 26), make_pair("e", 5) };
  
  
  // default using < operator
  auto minInt = min_element(intVec.begin(), intVec.end());
  
  //Using pairLessThanFunction
  auto minPairFunction = min_element(pairVector.begin(), pairVector.end(), pairLessThanFunction);
  
  
  //print minimum of intVector
  cout << "min int from default: " << *minInt << endl;
  
  //print minimum of pairVector
  cout << "min pair from PairLessThanFunction: " << (*minPairFunction).second << endl;
  
  return 0;
}

```

**Output**

```cpp
min int from default: 6
min pair from PairLessThanFunction: 2

```



## Using std::nth_element To Find The Median (Or Other Quantiles)


The [`std::nth_element`](http://en.cppreference.com/w/cpp/algorithm/nth_element) algorithm takes three iterators: an iterator to the beginning, **n**th position, and end. Once the function returns, the **n**th element (by order) will be the **n**th smallest element. (The function has more elaborate overloads, e.g., some taking comparison functors; see the above link for all the variations.)

**Note** This function is very efficient - it has linear complexity.

For the sake of this example, let's define the median of a sequence of length **n** as the element that would be in position ⌈n / 2⌉. For example, the median of a sequence of length 5 is the 3rd smallest element, and so is the median of a sequence of length 6.

To use this function to find the median, we can use the following. Say we start with

```cpp
std::vector<int> v{5, 1, 2, 3, 4};    

std::vector<int>::iterator b = v.begin();
std::vector<int>::iterator e = v.end();

std::vector<int>::iterator med = b;
std::advance(med, v.size() / 2); 

// This makes the 2nd position hold the median.
std::nth_element(b, med, e);    

// The median is now at v[2].

```

To find the **p**th [quantile](https://en.wikipedia.org/wiki/Quantile), we would change some of the lines above:

```cpp
const std::size_t pos = p * std::distance(b, e);

std::advance(nth, pos);

```

and look for the quantile at position `pos`.

