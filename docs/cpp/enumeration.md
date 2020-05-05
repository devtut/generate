---
metaTitle: "C++ | Enumeration"
description: "Iteration over an enum, Basic Enumeration Declaration, Enumeration in switch statements, Scoped enums, Enum forward declaration in C++11"
---

# Enumeration



## Iteration over an enum


There is no built-in to iterate over enumeration.

But there are several ways

<li>
for `enum` with only consecutive values:

```cpp
enum E {
    Begin,
    E1 = Begin,
    E2,
    // ..
    En,
    End
};

for (E e = E::Begin; e != E::End; ++e) {
    // Do job with e
}

```


</li>

with `enum class`, `operator ++` has to be implemented:

```cpp
E& operator ++ (E& e)
{
    if (e == E::End) {
        throw std::out_of_range("for E& operator ++ (E&)");
    }
    e = E(static_cast<std::underlying_type<E>::type>(e) + 1);
    return e;
}

```


<li>
using a container as `std::vector`

```cpp
enum E {
    E1 = 4,
    E2 = 8,
    // ..
    En
};

std::vector<E> build_all_E()
{
    const E all[] = {E1, E2, /*..*/ En};
    
    return std::vector<E>(all, all + sizeof(all) / sizeof(E));
}

std::vector<E> all_E = build_all_E();

```


and then

```cpp
for (std::vector<E>::const_iterator it = all_E.begin(); it != all_E.end(); ++it) {
    E e = *it;
    // Do job with e;
}

```


</li>

<li>
or `std::initializer_list` and a simpler syntax:

```cpp
enum E {
    E1 = 4,
    E2 = 8,
    // ..
    En
};

constexpr std::initializer_list<E> all_E = {E1, E2, /*..*/ En};

```


and then

```cpp
for (auto e : all_E) {
    // Do job with e
}

```


</li>



## Basic Enumeration Declaration


Standard enumerations allow users to declare a useful name for a set of integers. The names are collectively referred to as enumerators. An enumeration and its associated enumerators are defined as follows:

```cpp
enum myEnum
{
    enumName1,
    enumName2,
};

```

An enumeration is a **type**, one which is distinct from all other types. In this case, the name of this type is `myEnum`. Objects of this type are expected to assume the value of an enumerator within the enumeration.

The enumerators declared within the enumeration are constant values of the type of the enumeration. Though the enumerators are declared within the type, the scope operator `::` is not needed to access the name. So the name of the first enumerator is `enumName1`.

The scope operator can be optionally used to access an enumerator within an enumeration. So `enumName1` can also be spelled `myEnum::enumName1`.

Enumerators are assigned integer values starting from 0 and increasing by 1 for each enumerator in an enumeration. So in the above case, `enumName1` has the value 0, while `enumName2` has the value 1.

Enumerators can also be assigned a specific value by the user; this value must be an integral constant expression. Enumerators who's values are not explicitly provided will have their value set to the value of the previous enumerator + 1.

```cpp
enum myEnum
{
    enumName1 = 1, // value will be 1
    enumName2 = 2, // value will be 2
    enumName3,     // value will be 3, previous value + 1
    enumName4 = 7, // value will be 7
    enumName5,     // value will be 8
    enumName6 = 5, // value will be 5, legal to go backwards
    enumName7 = 3, // value will be 3, legal to reuse numbers
    enumName8 = enumName4 + 2, // value will be 9, legal to take prior enums and adjust them
};

```



## Enumeration in switch statements


A common use for enumerators is for switch statements and so they commonly appear in state machines. In fact a useful feature of switch statements with enumerations is that if no default statement is included for the switch, and not all values of the enum have been utilized, the compiler will issue a warning.

```cpp
enum State {
    start,
    middle,
    end
};

...

switch(myState) {
    case start:
       ...
    case middle:
       ...
} // warning: enumeration value 'end' not handled in switch [-Wswitch]

```



## Scoped enums


C++11 introduces what are known as **scoped enums**. These are enumerations whose members must be qualified with `enumname::membername`. Scoped enums are declared using the `enum class` syntax. For example, to store the colors in a rainbow:

```cpp
enum class rainbow {
    RED,
    ORANGE,
    YELLOW,
    GREEN,
    BLUE,
    INDIGO,
    VIOLET
};

```

To access a specific color:

```cpp
rainbow r = rainbow::INDIGO;

```

`enum class`es cannot be implicitly converted to `int`s without a cast. So `int x = rainbow::RED` is invalid.

Scoped enums also allow you to specify the **underlying type**, which is the type used to represent a member. By default it is `int`. In a Tic-Tac-Toe game, you may store the piece as

```cpp
enum class piece : char {
    EMPTY = '\0',
    X = 'X',
    O = 'O',
};

```

As you may notice, `enum`s can have a trailing comma after the last member.



## Enum forward declaration in C++11


Scoped enumerations:

```cpp
...
enum class Status; // Forward declaration 
Status doWork(); // Use the forward declaration
...
enum class Status { Invalid, Success, Fail };
Status doWork() // Full declaration required for implementation
{
    return Status::Success;
}    

```

Unscoped enumerations:

```cpp
...
enum Status: int; // Forward declaration, explicit type required
Status doWork(); // Use the forward declaration
...
enum Status: int{ Invalid=0, Success, Fail }; // Must match forward declare type
static_assert( Success == 1 );

```

An in-depth multi-file example can be found here: [Blind fruit merchant example](http://stackoverflow.com/a/19074269/1873507)

