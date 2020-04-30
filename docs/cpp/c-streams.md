---
metaTitle: "C++ Streams"
description: "String streams, Printing collections with iostream, Reading a file till the end, Parsing files"
---

# C++ Streams



## String streams


[`std::ostringstream`](http://www.cplusplus.com/reference/sstream/ostringstream/) is a class whose objects look like an output stream (that is, you can write to them via `operator<<`), but actually store the writing results, and provide them in the form of a stream.

Consider the following short code:

```cpp
#include <sstream>
#include <string>                                                                                                                          

using namespace std;

int main()
{
    ostringstream ss;
    ss << "the answer to everything is " << 42;
    const string result = ss.str(); 
}   

```

The line

```cpp
ostringstream ss;

```

creates such an object. This object is first manipulated like a regular stream:

```cpp
ss << "the answer to everything is " << 42;

```

Following that, though, the resulting stream can be obtained like this:

```cpp
const string result = ss.str();

```

(the string `result` will be equal to `"the answer to everything is 42"`).

This is mainly useful when we have a class for which stream serialization has been defined, and for which we want a string form. For example, suppose we have some class

```cpp
class foo 
{   
    // All sort of stuff here.
};  

ostream &operator<<(ostream &os, const foo &f);

```

To get the string representation of a `foo` object,

```cpp
foo f;

```

we could use

```cpp
ostringstream ss; 
ss << f;
const string result = ss.str();        

```

Then `result` contains the string representation of the `foo` object.



## Printing collections with iostream


### Basic printing

`std::ostream_iterator` allows to print contents of an STL container to any output stream without explicit loops. The second argument of `std::ostream_iterator` constructor sets the delimiter. For example, the following code:

```cpp
std::vector<int> v = {1,2,3,4};
std::copy(v.begin(), v.end(), std::ostream_iterator<int>(std::cout, " ! "));

```

will print

```cpp
1 ! 2 ! 3 ! 4 !

```

### Implicit type cast

`std::ostream_iterator` allows to cast container's content type implicitly. For example, let's tune `std::cout` to print floating-point values with 3 digits after decimal point:

```cpp
std::cout << std::setprecision(3);
std::fixed(std::cout);

```

and instantiate `std::ostream_iterator` with `float`, while the contained values remain `int`:

```cpp
std::vector<int> v = {1,2,3,4};
std::copy(v.begin(), v.end(), std::ostream_iterator<float>(std::cout, " ! "));

```

so the code above yields

```cpp
1.000 ! 2.000 ! 3.000 ! 4.000 !

```

despite `std::vector` holds `int`s.

### Generation and transformation

`std::generate`, `std::generate_n` and `std::transform` functions provide a very powerful tool for on-the-fly data manipulation. For example, having a vector:

```cpp
std::vector<int> v = {1,2,3,4,8,16};

```

we can easily print boolean value of "x is even" statement for each element:

```cpp
std::boolalpha(std::cout); // print booleans alphabetically
std::transform(v.begin(), v.end(), std::ostream_iterator<bool>(std::cout, " "),
[](int val) {
    return (val % 2) == 0;
});

```

or print the squared element:

```cpp
std::transform(v.begin(), v.end(), std::ostream_iterator<int>(std::cout, " "),
[](int val) {
    return val * val;
});

```

Printing N space-delimited random numbers:

```cpp
const int N = 10;
std::generate_n(std::ostream_iterator<int>(std::cout, " "), N, std::rand);

```

### Arrays

As in the section about reading text files, almost all these considerations may be applied to native arrays. For example, let's print squared values from a native array:

```cpp
int v[] = {1,2,3,4,8,16};
std::transform(v, std::end(v), std::ostream_iterator<int>(std::cout, " "),
[](int val) {
    return val * val;
});

```



## Reading a file till the end


### Reading a text file line-by-line

A proper way to read a text file line-by-line till the end is usually not clear from `ifstream` documentation. Let's consider some common mistakes done by beginner C++ programmers, and a proper way to read the file.

### Lines without whitespace characters

For the sake of simplicity, let's assume that each line in the file contains no whitespace symbols.

`ifstream` has `operator bool()`, which returns true when a stream has no errors and is ready to read. Moreover, `ifstream::operator >>` returns a reference to the stream itself, so we can read and check for EOF (as well as for errors) in one line with very elegant syntax:

```cpp
std::ifstream ifs("1.txt");
std::string s;
while(ifs >> s) {
    std::cout << s << std::endl;
}

```

### Lines with whitespace characters

`ifstream::operator >>` reads the stream until any whitespace character occurs, so the above code will print the words from a line on separate lines. To read everything till the end of line, use `std::getline` instead of `ifstream::operator >>`. `getline` returns reference to the thread it worked with, so the same syntax is available:

```cpp
while(std::getline(ifs, s)) {
    std::cout << s << std::endl;
}

```

Obviously, `std::getline` should also be used for reading a single-line file till the end.

### Reading a file into a buffer at once

Finally, let's read the file from the beginning till the end without stopping at any character, including whitespaces and newlines. If we know the exact file size or upper bound of the length is acceptable, we can resize the string and then read:

```cpp
s.resize(100);
std::copy(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>(),
    s.begin());

```

Otherwise, we need to insert each character to the end of the string, so `std::back_inserter` is what we need:

```cpp
std::copy(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>(),
    std::back_inserter(s));

```

Alternatively, it is possible to initialize a collection with stream data, using a constructor with iterator range arguments:

```cpp
std::vector v(std::istreambuf_iterator<char>(ifs),
    std::istreambuf_iterator<char>());

```

Note that these examples are also applicable if `ifs` is opened as binary file:

```cpp
std::ifstream ifs("1.txt", std::ios::binary);

```

### Copying streams

A file may be copied to another file with streams and iterators:

```cpp
std::ofstream ofs("out.file");
std::copy(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>(),
    std::ostream_iterator<char>(ofs));
ofs.close();

```

or redirected to any other type of stream with a compatible interface. For example Boost.Asio network stream:

```cpp
boost::asio::ip::tcp::iostream stream;
stream.connect("example.com", "http");
std::copy(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>(),
    std::ostream_iterator<char>(stream));
stream.close();

```

### Arrays

As iterators might be thought of as a generalization of pointers, STL containers in the examples above may be replaced with native arrays. Here is how to parse numbers into array:

```cpp
int arr[100];
std::copy(std::istream_iterator<char>(ifs), std::istream_iterator<char>(), arr);

```

Beware of buffer overflow, as arrays cannot be resized on-the-fly after they were allocated. For example, if the code above will be fed with a file that contains more than 100 integer numbers, it will attempt to write outside the array and run into undefined behavior.



## Parsing files


### Parsing files into STL containers

`istream_iterator`s are very useful for reading sequences of numbers or other parsable data into STL containers without explicit loops in the code.

Using explicit container size:

```cpp
std::vector<int> v(100);
std::copy(std::istream_iterator<int>(ifs), std::istream_iterator<int>(),
    v.begin());

```

or with inserting iterator:

```cpp
std::vector<int> v;
std::copy(std::istream_iterator<int>(ifs), std::istream_iterator<int>(),
    std::back_inserter(v));

```

Note that the numbers in the input file may be divided by any number of any whitespace characters and newlines.

### Parsing heterogeneous text tables

As `istream::operator>>` reads text until a whitespace symbol, it may be used in `while` condition to parse complex data tables. For example, if we have a file with two real numbers followed by a string (without spaces) on each line:

```cpp
1.12 3.14 foo
2.1 2.2 barr

```

it may be parsed like this:

```cpp
std::string s;
double a, b;
while(ifs >> a >> b >> s) {
    std::cout << a << " " << b << " " << s << std::endl;
}

```

### Transformation

Any range-manipulating function may be used with `std::istream_iterator` ranges. One of them is `std::transform`, which allows to process data on-the-fly. For example, let's read integer values, multiply them by 3.14 and store the result into floating-point container:

```cpp
std::vector<double> v(100);
std::transform(std::istream_iterator<int>(ifs), std::istream_iterator<int>(),
v.begin(),
[](int val) {
    return val * 3.14;
});

```



#### Remarks


Default constructor of `std::istream_iterator` constructs an iterator which represents the end of the stream. Thus, `std::copy(std::istream_iterator<int>(ifs), std::istream_iterator<int>(), ....` means to copy from the current position in `ifs` to the end.

