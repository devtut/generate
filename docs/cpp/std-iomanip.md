---
metaTitle: "C++ | std::iomanip"
description: "std::setprecision, std::setfill, std::setiosflags, std::setw"
---

# std::iomanip



## std::setprecision


When used in an expression `out << setprecision(n)` or `in >> setprecision(n)`, sets the precision parameter of the stream out or in to exactly n. Parameter of this function is integer, which is new value for precision.

Example:

```cpp
#include <iostream>
#include <iomanip>
#include <cmath>
#include <limits>
int main()
{
    const long double pi = std::acos(-1.L);
    std::cout << "default precision (6): " << pi << '\n'    
              << "std::precision(10):    " << std::setprecision(10) << pi << '\n'
              << "max precision:         "
              << std::setprecision(std::numeric_limits<long double>::digits10 + 1)
              << pi << '\n';
}
//Output
//default precision (6): 3.14159
//std::precision(10):    3.141592654
//max precision:         3.141592653589793239

```



## std::setfill


When used in an expression `out << setfill(c)` sets the fill character of the stream out to `c`.

Note: The current fill character may be obtained with `std::ostream::fill`.

Example:

```cpp
#include <iostream>
#include <iomanip>
int main()
{
    std::cout << "default fill: " << std::setw(10) << 42 << '\n'
          << "setfill('*'): " << std::setfill('*')
                              << std::setw(10) << 42 << '\n';
}
//output::
//default fill:         42
//setfill('*'): ********42

```



## std::setiosflags


When used in an expression `out << setiosflags(mask)` or `in >> setiosflags(mask)`, sets all format flags of the stream out or in as specified by the mask.

List of all `std::ios_base::fmtflags` :

- `dec` - use decimal base for integer I/O
- `oct` - use octal base for integer I/O
- `hex` - use hexadecimal base for integer I/O
- `basefield` - `dec|oct|hex|0` useful for masking operations
- `left` - left adjustment(add fill characters to the right)
- `right` - right adjustment (adds fill characters to the left)
- `internal` - internal adjustment (adds fill characters to the internal designated point)
- `adjustfield` - `left|right|internal`. Useful for masking operations
- `scientific` - generate floating point types using scientific notation, or hex notation if combined with fixed
- `fixed` - generate floating point types using fixed notation, or hex notation if combined with scientific
- `floatfield` - `scientific|fixed|(scientific|fixed)|0`. Useful for masking operations
- `boolalpha` - insert and extract `bool` type in alphanumeric format
- `showbase` - generate a prefix indicating the numeric base for integer output, require the currency indicator in monetary I/O
- `showpoint` - generate a decimal-point character unconditionally for floating-point number output
- `showpos` - generate a `+` character for non-negative numeric output
- `skipws` - skip leading whitespace before certain input operations
- `unitbuf`    flush the output after each output operation
<li>`uppercase` - replace certain lowercase letters with their uppercase
equivalents in certain output output operations</li>

Example of manipulators:

```

   #include <iostream>
    #include <string>
    #include<iomanip>
    int main()
    {
      int l_iTemp = 47;
      std::cout<<  std::resetiosflags(std::ios_base::basefield);
      std::cout<<std::setiosflags( std::ios_base::oct)<<l_iTemp<<std::endl;
      //output: 57
      std::cout<<  std::resetiosflags(std::ios_base::basefield);
      std::cout<<std::setiosflags( std::ios_base::hex)<<l_iTemp<<std::endl;
      //output: 2f
      std::cout<<std::setiosflags( std::ios_base::uppercase)<<l_iTemp<<std::endl;
      //output 2F
      std::cout<<std::setfill('0')<<std::setw(12);
      std::cout<<std::resetiosflags(std::ios_base::uppercase);
      std::cout<<std::setiosflags( std::ios_base::right)<<l_iTemp<<std::endl;
      //output: 00000000002f
      
      std::cout<<std::resetiosflags(std::ios_base::basefield|std::ios_base::adjustfield);
      std::cout<<std::setfill('.')<<std::setw(10);
      std::cout<<std::setiosflags( std::ios_base::left)<<l_iTemp<<std::endl;
      //output: 47........
      
      std::cout<<std::resetiosflags(std::ios_base::adjustfield)<<std::setfill('#');
      std::cout<<std::setiosflags(std::ios_base::internal|std::ios_base::showpos);
      std::cout<<std::setw(10)<<l_iTemp<<std::endl;
      //output +#######47
      
      double l_dTemp = -1.2;
      double pi = 3.14159265359;
      std::cout<<pi<<"    "<<l_dTemp<<std::endl;
      //output +3.14159   -1.2
      std::cout<<std::setiosflags(std::ios_base::showpoint)<<l_dTemp<<std::endl;
      //output -1.20000
      std::cout<<setiosflags(std::ios_base::scientific)<<pi<<std::endl;
      //output: +3.141593e+00
      std::cout<<std::resetiosflags(std::ios_base::floatfield);
      std::cout<<setiosflags(std::ios_base::fixed)<<pi<<std::endl;
      //output: +3.141593
      bool b = true;
      std::cout<<std::setiosflags(std::ios_base::unitbuf|std::ios_base::boolalpha)<<b;
      //output: true
      return 0;
    }

```



## std::setw


```cpp
int val = 10;
// val will be printed to the extreme left end of the  output console:
std::cout << val << std::endl;
 // val will be printed in an output field of length 10 starting from right end of the field:
std::cout << std::setw(10) << val << std::endl;

```

This outputs:

```cpp
10
        10
1234567890

```

(where the last line is there to aid in seeing the character offsets).

Sometimes we need to set the width of the output field, usually when we need to get the output in some structured and proper layout. That can be done using **`std::setw`** of **std::iomanip**.

The syntax for `std::setw` is:

```cpp
std::setw(int n)

```

where n is the length of the output field to be set

