---
metaTitle: "Basic input/output in c++"
description: "user input and standard output"
---

# Basic input/output in c++




## user input and standard output


```cpp
#include <iostream>

int main()
{
    int value;
    std::cout << "Enter a value: " << std::endl;
    std::cin >> value;
    std::cout << "The square of entered value is: " << value * value << std::endl;
    return 0;
}

```



#### Remarks


The standard library `<iostream>` defines few streams for input and output:

```cpp
|stream | description                      |
|-------|----------------------------------|
|cin    | standard input stream            |
|cout   | standard output stream           |
|cerr   | standard error (output) stream   |
|clog   | standard logging (output) stream |

```

Out of four streams mentioned above `cin` is basically used for user input and other three are used for outputting the data. In general or in most coding environments `cin` (**console input** or standard input) is keyboard and `cout` (**console output** or standard output) is monitor.

```cpp
cin >> value

cin   - input stream
'>>'  - extraction operator
value - variable (destination)

```

`cin` here extracts the input entered by the user and feeds in variable value. The value is extracted only after user presses ENTER key.

```cpp
cout << "Enter a value: "

cout              - output stream
'<<'              - insertion operator
"Enter a value: " - string to be displayed

```

`cout` here takes the string to be displayed and inserts it to standard output or monitor

All four streams are located in standard namespace `std` so we need to print `std::stream` for stream `stream` to use it.

There is also a manipulator `std::endl` in code. It can be used only with output streams. It inserts end of line `'\n'` character in the stream and flushes it. It causes immediately producing output.

