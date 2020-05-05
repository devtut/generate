---
metaTitle: "C++ | Stream manipulators"
description: "Stream manipulators, Output stream manipulators, Input stream manipulators"
---

# Stream manipulators


Manipulators are special helper functions that help controlling input and output streams using `operator >>` or `operator <<`.

They all can be included by `#include <iomanip>`.



## Stream manipulators


[`std::boolalpha`](http://en.cppreference.com/w/cpp/io/manip/booalpha) and [`std::noboolalpha`](http://en.cppreference.com/w/cpp/io/manip/booalpha) - switch between textual and numeric representation of booleans.

```cpp
std::cout << std::boolalpha << 1;
// Output: true

std::cout << std::noboolalpha << false;
// Output: 0

bool boolValue;
std::cin >> std::boolalpha >> boolValue;
std::cout << "Value \"" << std::boolalpha << boolValue
          << "\" was parsed as " << std::noboolalpha << boolValue;
// Input: true
// Output: Value "true" was parsed as 0

```

[`std::showbase`](http://en.cppreference.com/w/cpp/io/manip/showbase) and [`std::noshowbase`](http://en.cppreference.com/w/cpp/io/manip/showbase) - control whether prefix indicating numeric base is used.

[`std::dec`](http://en.cppreference.com/w/cpp/io/manip/hex) (decimal), [`std::hex`](http://en.cppreference.com/w/cpp/io/manip/hex) (hexadecimal) and [`std::oct`](http://en.cppreference.com/w/cpp/io/manip/hex) (octal) - are used for changing base for integers.

```cpp
#include <sstream>

std::cout << std::dec << 29 << ' - '
          << std::hex << 29 << ' - '
          << std::showbase << std::oct << 29 << ' - '
          << std::noshowbase << 29  '\n';
int number;
std::istringstream("3B") >> std::hex >> number;
std::cout << std::dec << 10;
// Output: 22 - 1D - 35 - 035
// 59

```

Default values are [`std::ios_base::noshowbase`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags) and [`std::ios_base::dec`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags).
<br>

If you want to see more about `std::istringstream` check out the <[sstream](http://en.cppreference.com/w/cpp/header/sstream)> header.

[`std::uppercase`](http://en.cppreference.com/w/cpp/io/manip/uppercase) and [`std::nouppercase`](http://en.cppreference.com/w/cpp/io/manip/uppercase) - control whether uppercase characters are used in floating-point and hexadecimal integer output. Have no effect on input streams.

```cpp
std::cout << std::hex << std::showbase
              << "0x2a with nouppercase: " << std::nouppercase << 0x2a << '\n'
              << "1e-10 with uppercase: " << std::uppercase << 1e-10 << '\n'
}
// Output: 0x2a with nouppercase: 0x2a
// 1e-10 with uppercase: 1E-10

```

Default is `std::nouppercase`.

[`std::setw(n)`](http://en.cppreference.com/w/cpp/io/manip/setw) - changes the width of the next input/output field to exactly `n`.

The width property `n` is resetting to `0` when some functions are called (full list is [here](http://en.cppreference.com/w/cpp/io/manip/setw)).

```cpp
std::cout << "no setw:" << 51 << '\n'
          << "setw(7): " << std::setw(7) << 51 << '\n'
          << "setw(7), more output: " << 13
          << std::setw(7) << std::setfill('*') << 67 << ' ' << 94 << '\n';

char* input = "Hello, world!";
char arr[10];
std::cin >> std::setw(6) >> arr;
std::cout << "Input from \"Hello, world!\" with setw(6) gave \"" << arr << "\"\n";

// Output: 51
// setw(7):      51
// setw(7), more output: 13*****67 94

// Input: Hello, world!
// Output: Input from "Hello, world!" with setw(6) gave "Hello"

```

Default is `std::setw(0)`.

[`std::left`](http://en.cppreference.com/w/cpp/io/manip/left), [`std::right`](http://en.cppreference.com/w/cpp/io/manip/left) and [`std::internal`](http://en.cppreference.com/w/cpp/io/manip/left) - modify the default position of the fill characters by setting [`std::ios_base::adjustfield`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags) to [`std::ios_base::left`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags), [`std::ios_base::right`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags) and [`std::ios_base::internal`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags) correspondingly. `std::left` and `std::right` apply to any output, `std::internal` - for integer, floating-point and monetary output. Have no effect on input streams.

```cpp
#include <locale>
...

std::cout.imbue(std::locale("en_US.utf8"));

std::cout << std::left << std::showbase << std::setfill('*')
          << "flt: " << std::setw(15) << -9.87  << '\n'
          << "hex: " << std::setw(15) << 41 << '\n'
          << "  $: " << std::setw(15) << std::put_money(367, false) << '\n'
          << "usd: " << std::setw(15) << std::put_money(367, true) << '\n'
          << "usd: " << std::setw(15)
          << std::setfill(' ') << std::put_money(367, false) << '\n';
// Output:
// flt: -9.87**********
// hex: 41*************
//   $: $3.67**********
// usd: USD *3.67******
// usd: $3.67          

std::cout << std::internal << std::showbase << std::setfill('*')
          << "flt: " << std::setw(15) << -9.87  << '\n'
          << "hex: " << std::setw(15) << 41 << '\n'
          << "  $: " << std::setw(15) << std::put_money(367, false) << '\n'
          << "usd: " << std::setw(15) << std::put_money(367, true) << '\n'
          << "usd: " << std::setw(15)
          << std::setfill(' ') << std::put_money(367, true) << '\n';
// Output:
// flt: -**********9.87
// hex: *************41
//   $: $3.67**********
// usd: USD *******3.67
// usd: USD        3.67

std::cout << std::right << std::showbase << std::setfill('*')
          << "flt: " << std::setw(15) << -9.87  << '\n'
          << "hex: " << std::setw(15) << 41 << '\n'
          << "  $: " << std::setw(15) << std::put_money(367, false) << '\n'
          << "usd: " << std::setw(15) << std::put_money(367, true) << '\n'
          << "usd: " << std::setw(15)
          << std::setfill(' ') << std::put_money(367, true) << '\n';
// Output:
// flt: **********-9.87
// hex: *************41
//   $: **********$3.67
// usd: ******USD *3.67
// usd:       USD  3.67

```

Default is `std::left`.

[`std::fixed`](http://en.cppreference.com/w/cpp/io/manip/fixed), [`std::scientific`](http://en.cppreference.com/w/cpp/io/manip/fixed), [`std::hexfloat`](http://en.cppreference.com/w/cpp/io/manip/fixed) [C++11] and [`std::defaultfloat`](http://en.cppreference.com/w/cpp/io/manip/fixed) [C++11] - change formatting for floating-point input/output.

`std::fixed` sets the [`std::ios_base::floatfield`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags) to [`std::ios_base::fixed`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags),
<br>
`std::scientific` - to [`std::ios_base::scientific`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags),
<br>
`std::hexfloat` - to `std::ios_base::fixed | std::ios_base::scientific` and
<br>
`std::defaultfloat` - to `std::ios_base::fmtflags(0)`.
<br><br>
[`fmtflags`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags)

```cpp
#include <sstream>
...

std::cout << '\n'
          << "The number 0.07 in fixed:      " << std::fixed << 0.01 << '\n'
          << "The number 0.07 in scientific: " << std::scientific << 0.01 << '\n'
          << "The number 0.07 in hexfloat:   " << std::hexfloat << 0.01 << '\n'
          << "The number 0.07 in default:    " << std::defaultfloat << 0.01 << '\n';

double f;
std::istringstream is("0x1P-1022");
double f = std::strtod(is.str().c_str(), NULL);
std::cout << "Parsing 0x1P-1022 as hex gives " << f << '\n';

// Output:
// The number 0.01 in fixed:      0.070000
// The number 0.01 in scientific: 7.000000e-02
// The number 0.01 in hexfloat:   0x1.1eb851eb851ecp-4
// The number 0.01 in default:    0.07
// Parsing 0x1P-1022 as hex gives 2.22507e-308

```

Default is `std::ios_base::fmtflags(0)`.

There is a **bug** on some compilers which causes

```cpp
double f;
std::istringstream("0x1P-1022") >> std::hexfloat >> f;
std::cout << "Parsing 0x1P-1022 as hex gives " << f << '\n';
// Output: Parsing 0x1P-1022 as hex gives 0

```

[`std::showpoint`](http://en.cppreference.com/w/cpp/io/manip/showpoint) and [`std::noshowpoint`](http://en.cppreference.com/w/cpp/io/manip/showpoint) - control whether decimal point is always included in floating-point representation. Have no effect on input streams.

```cpp
std::cout << "7.0 with showpoint: " << std::showpoint << 7.0 << '\n'
          << "7.0 with noshowpoint: " << std::noshowpoint << 7.0 << '\n';
// Output: 1.0 with showpoint: 7.00000
// 1.0 with noshowpoint: 7

```

Default is `std::showpoint`.

[`std::showpos`](http://en.cppreference.com/w/cpp/io/manip/showpos) and [`std::noshowpos`](http://en.cppreference.com/w/cpp/io/manip/showpos) - control displaying of the `+` sign in **non-negative** output. Have no effect on input streams.

```cpp
std::cout << "With showpos: " << std::showpos
          << 0 << ' ' << -2.718 << ' ' << 17 << '\n'
          << "Without showpos: " << std::noshowpos
          << 0 << ' ' << -2.718 << ' ' << 17 << '\n';
// Output: With showpos: +0 -2.718 +17
// Without showpos: 0 -2.718 17

```

Default if `std::noshowpos`.

[`std::unitbuf`](http://en.cppreference.com/w/cpp/io/manip/unitbuf), [`std::nounitbuf`](http://en.cppreference.com/w/cpp/io/manip/unitbuf) - control flushing output stream after every operation. Have no effect on input stream. `std::unitbuf` causes flushing.

[`std::setbase(base)`](http://en.cppreference.com/w/cpp/io/manip/setbase) - sets the numeric base of the stream.
<br><br>
`std::setbase(8)` equals to setting `std::ios_base::basefield` to `std::ios_base::oct`,<br> `std::setbase(16)` - to `std::ios_base::hex`,<br> `std::setbase(10)` - to `std::ios_base::dec`.

If `base` is other then `8`, `10` or `16` then `std::ios_base::basefield` is setting to `std::ios_base::fmtflags(0)`. It means decimal output and prefix-dependent input.

As default `std::ios_base::basefield` is `std::ios_base::dec` then by default `std::setbase(10)`.

[`std::setprecision(n)`](http://en.cppreference.com/w/cpp/io/manip/setprecision) - changes floating-point precision.

```cpp
#include <cmath>
#include <limits>
...

typedef std::numeric_limits<long double> ld;
const long double pi = std::acos(-1.L);

std::cout << '\n'
          << "default precision (6):   pi: " << pi << '\n'
          << "                       10pi: " << 10 * pi << '\n'
          << "std::setprecision(4):  10pi: " << std::setprecision(4) << 10 * pi << '\n'
          << "                    10000pi: " << 10000 * pi << '\n'
          << "std::fixed:         10000pi: " << std::fixed << 10000 * pi << std::defaultfloat << '\n'
          << "std::setprecision(10):   pi: " << std::setprecision(10) << pi << '\n'
          << "max-1 radix precicion:   pi: " << std::setprecision(ld::digits - 1) << pi << '\n'
          << "max+1 radix precision:   pi: " << std::setprecision(ld::digits + 1) << pi << '\n'
          << "significant digits prec: pi: " << std::setprecision(ld::digits10) << pi << '\n';

// Output:
// default precision (6):   pi: 3.14159
//                        10pi: 31.4159
// std::setprecision(4):  10pi: 31.42
//                     10000pi: 3.142e+04
// std::fixed:         10000pi: 31415.9265
// std::setprecision(10):   pi: 3.141592654
// max-1 radix precicion:   pi: 3.14159265358979323851280895940618620443274267017841339111328125
// max+1 radix precision:   pi: 3.14159265358979323851280895940618620443274267017841339111328125
// significant digits prec: pi: 3.14159265358979324

```

Default is `std::setprecision(6)`.

[`std::setiosflags(mask)`](http://en.cppreference.com/w/cpp/io/manip/setiosflags) and [`std::resetiosflags(mask)`](http://en.cppreference.com/w/cpp/io/manip/resetiosflags) - set and clear flags specified in `mask` of `std::ios_base::fmtflags` type.

```cpp
#include <sstream>
...

std::istringstream in("10 010 10 010 10 010");
int num1, num2;

in >> std::oct >> num1 >> num2;
std::cout << "Parsing \"10 010\" with std::oct gives:   " << num1 << ' ' << num2 << '\n';
// Output: Parsing "10 010" with std::oct gives:   8 8

in >> std::dec >> num1 >> num2;
std::cout << "Parsing \"10 010\" with std::dec gives:   " << num1 << ' ' << num2 << '\n';
// Output: Parsing "10 010" with std::oct gives:   10 10

in >> std::resetiosflags(std::ios_base::basefield) >> num1 >> num2;
std::cout << "Parsing \"10 010\" with autodetect gives: " << num1 << ' ' << num2 << '\n';
// Parsing "10 010" with autodetect gives: 10 8

std::cout << std::setiosflags(std::ios_base::hex |
                              std::ios_base::uppercase |
                              std::ios_base::showbase) << 42 << '\n';
// Output: OX2A

```

[`std::skipws`](http://en.cppreference.com/w/cpp/io/manip/skipws) and [`std::noskipws`](http://en.cppreference.com/w/cpp/io/manip/skipws) - control skipping of leading whitespace by the formatted input functions. Have no effect on output streams.

```cpp
#include <sstream>
...

char c1, c2, c3;
std::istringstream("a b c") >> c1 >> c2 >> c3;
std::cout << "Default  behavior:  c1 = " << c1 << "  c2 = " << c2 << "  c3 = " << c3 << '\n';

std::istringstream("a b c") >> std::noskipws >> c1 >> c2 >> c3;
std::cout << "noskipws behavior:  c1 = " << c1 << "  c2 = " << c2 << "  c3 = " << c3 << '\n';
// Output: Default  behavior:  c1 = a  c2 = b  c3 = c
// noskipws behavior:  c1 = a  c2 =    c3 = b

```

Default is [`std::ios_base::skipws`](http://en.cppreference.com/w/cpp/io/ios_base/fmtflags).

[`std::quoted(s[, delim[, escape]])`](http://en.cppreference.com/w/cpp/io/manip/quoted) [C++14] - inserts or extracts quoted strings with embedded spaces.<br><br>
`s` - the string to insert or extract.<br>
`delim` - the character to use as the delimiter, `"` by default.<br>
`escape` - the character to use as the escape character, `\` by default.

```cpp
#include <sstream>
...
 
std::stringstream ss;
std::string in = "String with spaces, and embedded \"quotes\" too";
std::string out;
 
ss << std::quoted(in);
std::cout << "read in     [" << in << "]\n"
          << "stored as   [" << ss.str() << "]\n";
 
ss >> std::quoted(out);
std::cout << "written out [" << out << "]\n";
// Output:
// read in     [String with spaces, and embedded "quotes" too]
// stored as   ["String with spaces, and embedded \"quotes\" too"]
// written out [String with spaces, and embedded "quotes" too]

```

For more information see the link above.



## Output stream manipulators


[`std::ends`](http://en.cppreference.com/w/cpp/io/manip/ends) - inserts a null character `'\0'` to output stream.
More formally this manipulator's declaration looks like

```cpp
template <class charT, class traits>
std::basic_ostream<charT, traits>& ends(std::basic_ostream<charT, traits>& os);

```

and this manipulator places character by calling `os.put(charT())` when used in an expression<br>`os << std::ends;`

[`std::endl`](http://en.cppreference.com/w/cpp/io/manip/endl) and [`std::flush`](http://en.cppreference.com/w/cpp/io/manip/flush) both flush output stream `out` by calling `out.flush()`. It causes immediately producing output. But `std::endl` inserts end of line `'\n'` symbol before flushing.

```cpp
std::cout << "First line." << std::endl << "Second line. " << std::flush
          << "Still second line.";
// Output: First line.
// Second line. Still second line.

```

[`std::setfill(c)`](http://en.cppreference.com/w/cpp/io/manip/setfill) - changes the fill character to `c`. Often used with `std::setw`.

```cpp
std::cout << "\nDefault fill: " << std::setw(10) << 79 << '\n'
          << "setfill('#'): " << std::setfill('#')
          << std::setw(10) << 42 << '\n';
// Output:
// Default fill:         79
// setfill('#'): ########79

```

[`std::put_money(mon[, intl])`](http://en.cppreference.com/w/cpp/io/manip/put_money) [C++11]. In an expression `out << std::put_money(mon, intl)`, converts the monetary value `mon` (of `long double` or `std::basic_string` type) to its character representation as specified by the [`std::money_put`](http://en.cppreference.com/w/cpp/locale/money_put) facet of the locale currently imbued in `out`. Use international currency strings if `intl` is `true`, use currency symbols otherwise.

```cpp
long double money = 123.45;
// or std::string money = "123.45";
 
std::cout.imbue(std::locale("en_US.utf8"));
std::cout << std::showbase << "en_US: " << std::put_money(money)
          << " or " << std::put_money(money, true) << '\n';
// Output: en_US: $1.23 or USD  1.23
 
std::cout.imbue(std::locale("ru_RU.utf8"));
std::cout << "ru_RU: " << std::put_money(money)
          << " or " << std::put_money(money, true) << '\n';
// Output: ru_RU: 1.23 руб or 1.23 RUB 
 
std::cout.imbue(std::locale("ja_JP.utf8"));
std::cout << "ja_JP: " << std::put_money(money)
          << " or " << std::put_money(money, true) << '\n';
// Output: ja_JP: ￥123 or JPY  123

```

[`std::put_time(tmb, fmt)`](http://en.cppreference.com/w/cpp/io/manip/put_time) [C++11] - formats and outputs a date/time value to `std::tm` according to the specified format `fmt`.<br>

`tmb` - pointer to the calendar time structure `const std::tm*` as obtained from `localtime()` or `gmtime()`.<br>
`fmt` - pointer to a null-terminated string `const CharT*` specifying the format of conversion.

```cpp
#include <ctime>
...

std::time_t t = std::time(nullptr);
std::tm tm = *std::localtime(&t);

std::cout.imbue(std::locale("ru_RU.utf8"));
std::cout << "\nru_RU: " << std::put_time(&tm, "%c %Z") << '\n';
// Possible output:
// ru_RU: Вт 04 июл 2017 15:08:35 UTC

```

For more information see the link above.



## Input stream manipulators


[`std::ws`](http://en.cppreference.com/w/cpp/io/manip/ws) - consumes leading whitespaces in input stream. It different from `std::skipws`.

```cpp
#include <sstream>
...

std::string str;
std::istringstream("  \v\n\r\t    Wow!There   is no whitespaces!") >> std::ws >> str;
std::cout << str;
// Output: Wow!There   is no whitespaces!

```

[`std::get_money(mon[, intl])`](http://en.cppreference.com/w/cpp/io/manip/get_money) [C++11]. In an expression `in >> std::get_money(mon, intl)` parses the character input as a monetary value, as specified by the [`std::money_get`](http://en.cppreference.com/w/cpp/locale/money_get) facet of the locale currently imbued in `in`, and stores the value in `mon` (of `long double` or `std::basic_string` type). Manipulator expects **required** international currency strings if `intl` is `true`, expects **optional** currency symbols otherwise.

```cpp
#include <sstream>
#include <locale>
...
 
std::istringstream in("$1,234.56 2.22 USD  3.33");
long double v1, v2;
std::string v3;

in.imbue(std::locale("en_US.UTF-8"));
in >> std::get_money(v1) >> std::get_money(v2) >> std::get_money(v3, true);
if (in) {
    std::cout << std::quoted(in.str()) << " parsed as: "
              << v1 << ", " << v2 << ", " << v3 << '\n';
}
// Output:
// "$1,234.56 2.22 USD  3.33" parsed as: 123456, 222, 333

```

[`std::get_time(tmb, fmt)`](http://en.cppreference.com/w/cpp/io/manip/get_time) [C++11] - parses a date/time value stored in `tmb` of specified format `fmt`.<br><br>
`tmb` - valid pointer to the `const std::tm*` object where the result will be stored.<br>
`fmt` - pointer to a null-terminated string `const CharT*` specifying the conversion format.

```cpp
#include <sstream>
#include <locale>
...

std::tm t = {};
std::istringstream ss("2011-Februar-18 23:12:34");

ss.imbue(std::locale("de_DE.utf-8"));
ss >> std::get_time(&t, "%Y-%b-%d %H:%M:%S");
if (ss.fail()) {
    std::cout << "Parse failed\n";
}
else {
    std::cout << std::put_time(&t, "%c") << '\n';
}
// Possible output:
// Sun Feb 18 23:12:34 2011

```

For more information see the link above.



#### Remarks


Manipulators can be used in other way. For example:
<br>

<li>`os.width(n);` equals to `os << std::setw(n);`<br>
`is.width(n);` equals to `is >> std::setw(n);`</li>

<li>`os.precision(n);` equals to `os << std::setprecision(n);`<br>
`is.precision(n);` equals to `is >> std::setprecision(n);`</li>

1. `os.setfill(c);` equals to `os << std::setfill(c);`

<li>`str >> std::setbase(base);` or `str << std::setbase(base);`
equals to</li>

```cpp
str.setf(base ==  8 ? std::ios_base::oct :
            base == 10 ? std::ios_base::dec :
                base == 16 ? std::ios_base::hex :
                     std::ios_base::fmtflags(0),
         std::ios_base::basefield);

```


<li>`os.setf(std::ios_base::flag);` equals to `os << std::flag;`<br>
`is.setf(std::ios_base::flag);` equals to `is >> std::flag;`
<br><br>
`os.unsetf(std::ios_base::flag);` equals to `os << std::no ## flag;`<br>
`is.unsetf(std::ios_base::flag);` equals to `is >> std::no ## flag;`
<br><br>
(where **##** - is **concatenation operator**)
<br><br>
for next `flag`s: **`boolalpha`**, **`showbase`**, **`showpoint`**, **`showpos`**, **`skipws`**, **`uppercase`**.</li>

<li>**`std::ios_base::basefield`**.<br>
For `flag`s: **`dec`**, **`hex`** and **`oct`**:</li>

<li>`os.setf(std::ios_base::flag, std::ios_base::basefield);` equals to `os << std::flag;`<br>
`is.setf(std::ios_base::flag, std::ios_base::basefield);` equals to `is >> std::flag;`<br>
**( 1 )**
<br><br></li>
<li>`str.unsetf(std::ios_base::flag, std::ios_base::basefield);` equals to `str.setf(std::ios_base::fmtflags(0), std::ios_base::basefield);`<br>
**( 2 )**</li>

<li>**`std::ios_base::adjustfield`**.<br>
For `flag`s: **`left`**, **`right`** and **`internal`**:</li>

<li>`os.setf(std::ios_base::flag, std::ios_base::adjustfield);` equals to `os << std::flag;`<br>
`is.setf(std::ios_base::flag, std::ios_base::adjustfield);` equals to `is >> std::flag;`<br>
**( 1 )**
<br><br></li>
<li>`str.unsetf(std::ios_base::flag, std::ios_base::adjustfield);` equals to `str.setf(std::ios_base::fmtflags(0), std::ios_base::adjustfield);`<br>
**( 2 )**</li>

**( 1 )** If flag of corresponding field previously set have already unset by `unsetf`.<br>
**( 2 )** If `flag` is set.

1. **`std::ios_base::floatfield`**.<br>

<li>`os.setf(std::ios_base::flag, std::ios_base::floatfield);` equals to `os << std::flag;`<br>
`is.setf(std::ios_base::flag, std::ios_base::floatfield);` equals to `is >> std::flag;`
<br><br>
for `flag`s: **`fixed`** and **`scientific`**.<br><br></li>
<li>`os.setf(std::ios_base::fmtflags(0), std::ios_base::floatfield);` equals to `os << std::defaultfloat;`<br>
`is.setf(std::ios_base::fmtflags(0), std::ios_base::floatfield);` equals to `is >> std::defaultfloat;`<br></li>

<li>`str.setf(std::ios_base::fmtflags(0), std::ios_base::flag);` equals to `str.unsetf(std::ios_base::flag)`<br><br>
for `flag`s: **`basefield`**, **`adjustfield`**, **`floatfield`**.</li>

<li>`os.setf(mask)` equals to `os << setiosflags(mask);`<br>
`is.setf(mask)` equals to `is >> setiosflags(mask);`
<br><br>
`os.unsetf(mask)` equals to `os << resetiosflags(mask);`<br>
`is.unsetf(mask)` equals to `is >> resetiosflags(mask);`
<br><br>
For almost all `mask` of `std::ios_base::fmtflags` type.</li>

