---
metaTitle: "std::pair"
description: "Creating a Pair and accessing the elements, Compare operators"
---

# std::pair



## Creating a Pair and accessing the elements


Pair allows us to treat two objects as one object. Pairs can be easily constructed with the help of template function `std::make_pair`.

Alternative way is to create pair and assign its elements (`first` and `second`) later.

```cpp
#include <iostream>
#include <utility>

int main()
{
    std::pair<int,int> p = std::make_pair(1,2); //Creating the pair
    std::cout << p.first << " " << p.second << std::endl; //Accessing the elements




    //We can also create a pair and assign the elements later
    std::pair<int,int> p1;
    p1.first = 3;
    p1.second = 4;
    std::cout << p1.first << " " << p1.second << std::endl;

    //We can also create a pair using a constructor
    std::pair<int,int> p2 = std::pair<int,int>(5, 6);
    std::cout << p2.first << " " << p2.second << std::endl;

    return 0;
}

```



## Compare operators


Parameters of these operators are `lhs` and `rhs`

- `operator==` tests if both elements on `lhs` and `rhs` pair are equal. The return value is `true` if both `lhs.first == rhs.first` AND  `lhs.second == rhs.second`, otherwise `false`

```cpp
std::pair<int, int> p1 = std::make_pair(1, 2);
std::pair<int, int> p2 = std::make_pair(2, 2);

if (p1 == p2)
    std::cout << "equals";
else
    std::cout << "not equal"//statement will show this, because they are not identical

```


<li>
`operator!=`  tests if any elements on `lhs` and `rhs` pair are not equal. The return value is `true` if either `lhs.first != rhs.first` OR `lhs.second != rhs.second`, otherwise return `false`.
</li>
<li>
`operator<` tests if `lhs.first<rhs.first`, returns `true`. Otherwise, if `rhs.first<lhs.first` returns `false`. Otherwise, if `lhs.second<rhs.second` returns `true`, otherwise, returns `false`.
</li>
<li>
`operator<=` returns `!(rhs<lhs)`
</li>
<li>
`operator>` returns `rhs<lhs`
</li>
<li>
`operator>=` returns `!(lhs<rhs)`
Another example with containers of pairs. It uses `operator<` because it needs to sort container.
</li>

```cpp
#include <iostream>
#include <utility>
#include <vector>
#include <algorithm>
#include <string>
 
int main()
{
    std::vector<std::pair<int, std::string>> v = { {2, "baz"},
                                                   {2, "bar"},
                                                   {1, "foo"} };
    std::sort(v.begin(), v.end());
 
    for(const auto& p: v) {
        std::cout << "(" << p.first << "," << p.second << ") ";
        //output: (1,foo) (2,bar) (2,baz)
    }
}

```

