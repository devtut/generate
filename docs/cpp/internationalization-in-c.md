---
metaTitle: "Internationalization in C++"
description: "Understanding C++ string characteristics"
---

# Internationalization in C++



## Understanding C++ string characteristics


```cpp
#include <iostream>
#include <string>

int main()
{
    const char * C_String = "This is a line of text w";
    const char * C_Problem_String = "This is a line of text ኚ";
    std::string Std_String("This is a second line of text w");
    std::string Std_Problem_String("This is a second line of ϯϵxϯ ኚ");

    std::cout << "String Length: " << Std_String.length() << '\n';
    std::cout << "String Length: " << Std_Problem_String.length() << '\n';

    std::cout << "CString Length: " << strlen(C_String) << '\n';
    std::cout << "CString Length: " << strlen(C_Problem_String) << '\n';
    return 0;
}

```

Depending on platform (windows, OSX, etc) and compiler (GCC, MSVC, etc), this program **may fail to compile, display different values, or display the same values**.

Example output under the Microsoft MSVC compiler:

> 
<p>String Length: 31<br />
String Length: 31<br />
CString Length: 24<br />
CString Length: 24</p>


This shows that under MSVC each of the extended-characters used is considered a single "character", and this platform fully supports internationalised languages.<br />
**It should be noted however that this behaviour is unusual, these international characters are stored internally as Unicode and thus are actually several bytes long. **This may cause unexpected errors****

Under the GNC/GCC compiler the program output is:

> 
<p>String Length: 31<br />
String Length: 36<br />
CString Length: 24<br />
CString Length: 26</p>


This example demonstrates that while the GCC compiler used on this (Linux) platform does support these extended-characters, it also uses (**correctly)** several bytes to store an individual character.<br />
In this case the use of Unicode characters is possible, but the programmer must take great care in remembering that the length of a "string" in this scenario is the ****length in bytes****, not the ****length in readable characters****.

These differences are due to how international languages are handled on a per-platform basis - and more importantly, that the C and C++ strings used in this example can be considered ****an array of bytes****, such that (for this usage) ****the C++ language considers a character (char) to be a single byte****.



#### Remarks


The C++ language does not dictate any character-set, some compilers may ****support**** the use of UTF-8, or even UTF-16. However there is no certainty that anything beyond simple ANSI/ASCII characters will be provided.

Thus all international language support is implementation defined, reliant on what platform, operating system, and compiler you may be using.

Several third party libraries (such as the International Unicode Committee Library) that can be used to extend the international support of the platform.

