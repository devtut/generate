---
metaTitle: "std::forward_list"
description: "Example, Methods"
---

# std::forward_list


`std::forward_list` is a container that supports fast insertion and removal of elements from anywhere in the container. Fast random access is not supported. It is implemented as a singly-linked list and essentially does not have any overhead compared to its implementation in C. Compared to `std::list` this container provides more space efficient storage when bidirectional iteration is not needed.



## Example


```cpp
#include <forward_list>
#include <string>
#include <iostream>

template<typename T>
std::ostream& operator<<(std::ostream& s, const std::forward_list<T>& v) {
    s.put('[');
    char comma[3] = {'\0', ' ', '\0'};
    for (const auto& e : v) {
        s << comma << e;
        comma[0] = ',';
    }
    return s << ']';
}

int main() 
{
    // c++11 initializer list syntax:
    std::forward_list<std::string> words1 {"the", "frogurt", "is", "also", "cursed"};
    std::cout << "words1: " << words1 << '\n';
 
    // words2 == words1
    std::forward_list<std::string> words2(words1.begin(), words1.end());
    std::cout << "words2: " << words2 << '\n';
 
    // words3 == words1
    std::forward_list<std::string> words3(words1);
    std::cout << "words3: " << words3 << '\n';
 
    // words4 is {"Mo", "Mo", "Mo", "Mo", "Mo"}
    std::forward_list<std::string> words4(5, "Mo");
    std::cout << "words4: " << words4 << '\n';
}

```

Output:

```cpp
words1: [the, frogurt, is, also, cursed]
words2: [the, frogurt, is, also, cursed]
words3: [the, frogurt, is, also, cursed]
words4: [Mo, Mo, Mo, Mo, Mo]

```



## Methods


|Method name|Definition
|---|---|---|---|---|---|---|---|---|---
|`operator=`|assigns values to the container
|`assign`|assigns values to the container
|`get_allocator`|returns the associated allocator
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---
|**Element access**|
|`front`|access the first element
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---
|**Iterators**|
|`before_begi`n|returns an iterator to the element before beginning
|`cbefore_begin`|returns a constant iterator to the element before beginning
|`begin`|returns an iterator to the beginning
|`cbegin`|returns a const iterator to the beginning
|`end`|returns an iterator to the end
|`cend`|returns a iterator to the end
|**Capacity**|
|`empty`|checks whether the container is empty
|`max_size`|returns the maximum possible number of elements
|**Modifiers**|
|`clear`|clears the contents
|`insert_after`|inserts elements after an element
|`emplace_after`|constructs elements in-place after an element
|`erase_after`|erases an element after an element
|`push_front`|inserts an element to the beginning
|`emplace_front`|constructs an element in-place at the beginning
|`pop_front`|removes the first element
|`resize`|changes the number of elements stored
|`swap`|swaps the contents
|**Operations**|
|`merge`|merges two sorted lists
|`splice_after`|moves elements from another forward_list
|`remove`|removes elements satisfying specific criteria
|`remove_if`|removes elements satisfying specific criteria
|`reverse`|reverses the order of the elements
|`unique`|removes consecutive duplicate elements
|`sort`|sorts the elements



#### Remarks


Adding, removing and moving the elements within the list, or across several lists, does not invalidate the iterators currently referring to other elements in the list. However, an iterator or reference referring to an element is invalidated when the corresponding element is removed (via erase_after) from the list.
std::forward_list meets the requirements of Container (except for the size member function and that operator=='s complexity is always linear), AllocatorAwareContainer and SequenceContainer.

