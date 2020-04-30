---
metaTitle: "Inline functions"
description: "Non-member inline function declaration, Non-member inline function definition, Member inline functions, What is function inlining?"
---

# Inline functions


A function defined with the `inline` specifier is an inline function. An inline function can be multiply defined without violating the [One Definition Rule](http://stackoverflow.com/documentation/c%2b%2b/4907/one-definition-rule-odr), and can therefore be defined in a header with external linkage. Declaring a function inline hints to the compiler that the function should be inlined during code generation, but does not provide a guarantee.



## Non-member inline function declaration


```cpp
inline int add(int x, int y);

```



## Non-member inline function definition


```cpp
inline int add(int x, int y)
{
    return x + y;
}

```



## Member inline functions


```cpp
// header (.hpp)    
struct A
{
    void i_am_inlined()
    {
    }
};

struct B
{
    void i_am_NOT_inlined();
};

// source (.cpp)    
void B::i_am_NOT_inlined()
{
}

```



## What is function inlining?


```cpp
inline int add(int x, int y)
{
    return x + y;
}

int main()
{
    int a = 1, b = 2;
    int c = add(a, b);
}

```

In the above code, when `add` is inlined, the resulting code would become something like this

```cpp
int main()
{
    int a = 1, b = 2;
    int c = a + b;
}

```

The inline function is nowhere to be seen, its body gets **inlined** into the caller's body. Had `add` not been inlined, a function would be called. The overhead of calling a function -- such as creating a new [stack frame](http://stackoverflow.com/q/10057443/183120), copying arguments, making local variables, jump (losing locality of reference and there by cache misses), etc. -- has to be incurred.



#### Syntax


- **`inline`** **function_declaration**
- **`inline`** **function_definition**
- **class { function_definition };**



#### Remarks


Usually if code generated for a function is **sufficiently** small then it's a good candidate to be inlined. Why so? If a function is large and is inlined in a loop, for all the calls made, the large function's code would be duplicated leading to the generated binary size bloat. But, how small is sufficient?

While inline functions seem to be great way to avoid function calling overhead, it's to be noted that not all functions that are marked `inline` are inlined. In other words, when you say `inline`, it is only a hint to the compiler, not an order: the compiler isn't obliged to inline the function, it's free to ignore it - most of them do. Modern compilers are better at making such optimisations that this keyword is now a vestige of the past, when this suggestion of function inlining by the programmer
was taken seriously by the compilers. Even functions not marked `inline` are inlined by the compiler when it sees benefit in doing so.

### Inline as a linkage directive

The more practical use of `inline` in modern C++ comes from using it as a linkage directive. When **defining**, not declaring, a function in a header which is going to be included in multiple sources, then each translation unit will have its own copy of this function leading to a [ODR](http://stackoverflow.com/q/4192170/183120) (One Definition Rule) violation; this rule roughly says that there can be only one definition of a function, variable, etc. To circumvent this violation, marking the function definition `inline` implicitly makes the function linkage internal.

### FAQs

**When should I write the keyword 'inline' for a function/method in C++?**

Only when you want the function to be defined in a header. More exactly only when the function's definition can show up in multiple compilation units. It's a good idea to define small (as in one liner) functions in the header file as it gives the compiler more information to work with while optimizing your code. It also increases compilation time.

**When should I not write the keyword 'inline' for a function/method in C++?**

Don't add `inline` when you think your code will run faster if the compiler inlines it.

**When will the the compiler not know when to make a function/method inline?**

Generally, the compiler will be able to do this better than you. However, the compiler doesn't have the option to inline code if it doesn't have the function definition. In maximally optimized code usually all private methods are inlined whether you ask for it or not.

### See Also

<li>
[When should I write the keyword &#39;inline&#39; for a function/method?](http://stackoverflow.com/q/1759300/183120)
</li>
<li>
[Is there still a use for inline?](http://stackoverflow.com/q/29796264/183120)
</li>

