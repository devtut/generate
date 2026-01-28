---
metaTitle: "C++ | Type Inference"
description: "Data Type: Auto, Lambda auto, Loops and auto"
---

# Type Inference




## Data Type: Auto


This example shows the basic type inferences the compiler can perform.

```cpp
auto a = 1;        //    a = int
auto b = 2u;       //    b = unsigned int
auto c = &a;       //    c = int*
const auto  d = c; //    d = const int*
const auto& e = b; //    e = const unsigned int& 

auto x = a + b     //    x = int, #compiler warning unsigned and signed

auto v = std::vector<int>;    //    v = std::vector<int>

```

However, the auto keyword does not always perform the expected type inference without additional hints for `&` or `const` or `constexpr`

```cpp
//    y = unsigned int, 
//    note that y does not infer as const unsigned int&
//    The compiler would have generated a copy instead of a reference value to e or b
auto y = e;    

```



## Lambda auto


The data type auto keyword is a convenient way for programmers to declare lambda functions. It helps by shortening the amount of text programmers need to type to declare a function pointer.

```cpp
auto DoThis = [](int a, int b) { return a + b; };
//    Do this is of type (int)(*DoThis)(int, int)
//    else we would have to write this long
int(*pDoThis)(int, int)= [](int a, int b) { return a + b; };

auto c = Dothis(1, 2);    //    c = int
auto d = pDothis(1, 2);   //    d = int

//    using 'auto' shortens the definition for lambda functions

```

By default, if the return type of lambda functions is not defined, it will be automatically inferred from the return expression types.

These 3 is basically the same thing

```cpp
[](int a, int b) -> int  { return a + b; };
[](int a, int b) -> auto { return a + b; };
[](int a, int b) { return a + b; };

```



## Loops and auto


This example shows how auto can be used to shorten type declaration for for loops

```cpp
std::map<int, std::string> Map;
for (auto pair : Map)            //    pair = std::pair<int, std::string>
for (const auto pair : Map)      //    pair = const std::pair<int, std::string>    
for (const auto& pair : Map)     //    pair = const std::pair<int, std::string>&
for (auto i = 0; i < 1000; ++i)        //    i = int
for (auto i = 0; i < Map.size(); ++i)  //    Note that i = int and not size_t
for (auto i = Map.size(); i > 0; --i)  //    i = size_t

```



#### Remarks


It is usually better to declare `const`, `&` and `constexpr` whenever you use `auto` if it is ever required to prevent unwanted behaviors such as copying or mutations. Those additional hints ensures that the compiler does not generate any other forms of inference. It is also not advisible to over use `auto` and should be used only when the actual declaration is very long, especially with STL templates.

