---
metaTitle: "C++ | operator precedence"
description: "Logical && and || operators: short-circuit, Arithmetic operators, Logical AND and OR operators, Unary Operators"
---

# operator precedence



## Logical && and || operators: short-circuit


&& has precedence over ||, this means that parentheses are placed to evaluate what would be evaluated together.

c++ uses short-circuit evaluation in && and || to not do unnecessary executions.<br />
If the left hand side of || returns true the right hand side does not need to be evaluated anymore.

```cpp
#include <iostream>
#include <string>

using namespace std;

bool True(string id){
    cout << "True" << id << endl;
    return true;
}

bool False(string id){
    cout << "False" << id << endl;
    return false;
}


int main(){
    bool result;
    //let's evaluate 3 booleans with || and && to illustrate operator precedence
    //precedence does not mean that && will be evaluated first but rather where    
    //parentheses would be added
    //example 1
    result =
        False("A") || False("B") && False("C"); 
                // eq. False("A") || (False("B") && False("C"))
    //FalseA
    //FalseB
    //"Short-circuit evaluation skip of C"
    //A is false so we have to evaluate the right of ||,
    //B being false we do not have to evaluate C to know that the result is false
    

    
    result =
        True("A") || False("B") && False("C"); 
                // eq. True("A") || (False("B") && False("C"))
    cout << result << " :=====================" << endl;
    //TrueA
    //"Short-circuit evaluation skip of B"
    //"Short-circuit evaluation skip of C"
    //A is true so we do not have to evaluate 
    //        the right of || to know that the result is true
    //If || had precedence over && the equivalent evaluation would be:
    // (True("A") || False("B")) && False("C")
    //What would print
    //TrueA
    //"Short-circuit evaluation skip of B"
    //FalseC
    //Because the parentheses are placed differently 
    //the parts that get evaluated are differently
    //which makes that the end result in this case would be False because C is false
}

```



## Arithmetic operators


Arithmetic operators in C++ have the same precedence as they do in mathematics:

Multiplication and division have left associativity(meaning that they will be evaluated from left to right) and they have higher precedence than addition and subtraction, which also have left associativity.

We can also force the precedence of expression using parentheses `(` `)`. Just the same way as you would do that in normal mathematics.

```cpp
// volume of a spherical shell = 4 pi R^3 - 4 pi r^3
double vol = 4.0*pi*R*R*R/3.0 - 4.0*pi*r*r*r/3.0;

//Addition:

int a = 2+4/2;          // equal to: 2+(4/2)         result: 4
int b = (3+3)/2;        // equal to: (3+3)/2         result: 3

//With Multiplication

int c = 3+4/2*6;        // equal to: 3+((4/2)*6)     result: 15
int d = 3*(3+6)/9;      // equal to: (3*(3+6))/9     result: 3

//Division and Modulo

int g = 3-3%1;          // equal to: 3 % 1 = 0  3 - 0 = 3
int h = 3-(3%1);        // equal to: 3 % 1 = 0  3 - 0 = 3
int i = 3-3/1%3;        // equal to: 3 / 1 = 3  3 % 3 = 0  3 - 0 = 3
int l = 3-(3/1)%3;      // equal to: 3 / 1 = 3  3 % 3 = 0  3 - 0 = 3
int m = 3-(3/(1%3));    // equal to: 1 % 3 = 1  3 / 1 = 3  3 - 3 = 0

```



## Logical AND and OR operators


These operators have the usual precedence in C++: AND before OR.

```cpp
// You can drive with a foreign license for up to 60 days
bool can_drive = has_domestic_license || has_foreign_license && num_days <= 60;

```

This code is equivalent to the following:

```cpp
// You can drive with a foreign license for up to 60 days
bool can_drive = has_domestic_license || (has_foreign_license && num_days <= 60);

```

Adding the parenthesis does not change the behavior, though, it does make it easier to read. By adding these parentheses, no confusion exist about the intent of the writer.



## Unary Operators


Unary operators act on the object upon which they are called and have high precedence. (See Remarks)

When used postfix, the action occurs only after the entire operation is evaluated, leading to some interesting arithmetics:

```cpp
int a = 1;
++a;            // result: 2
a--;            // result: 1
int minusa=-a;  // result: -1

bool b = true;
!b; // result: true

a=4;
int c = a++/2;      // equal to: (a==4) 4 / 2   result: 2 ('a' incremented postfix)
cout << a << endl;  // prints 5!
int d = ++a/2;      // equal to: (a+1) == 6 / 2 result: 3

int arr[4] =  {1,2,3,4};

int *ptr1 = &arr[0];    // points to arr[0] which is 1
int *ptr2 = ptr1++;     // ptr2 points to arr[0] which is still 1; ptr1 incremented
std::cout << *ptr1++ << std::endl;  // prints  2

int e = arr[0]++;       // receives the value of arr[0] before it is incremented
std::cout << e << std::endl;      // prints 1
std::cout << *ptr2 << std::endl;  // prints arr[0] which is now 2

```



#### Remarks


Operators are listed top to bottom, in descending precedence. Operators with the same number have equal precedence and the same associativity.

1. `::`
1. The postfix operators: `[]` `()` `T(...)` `.` `->` `++` `--`  `dynamic_cast` `static_cast` `reinterpret_cast` `const_cast` `typeid`
1. The unary prefix operators: `++` `--` `*` `&` `+` `-` `!` `~` `sizeof` `new` `delete` `delete[]`; the C-style cast notation, `(T)...`;  (C++11 and above) `sizeof...` `alignof` `noexcept`
1. `.*` and `->*`
1. `*`, `/`, and `%`, binary arithmetic operators
1. `+` and `-`, binary arithmetic operators
1. `<<` and `>>`
1. `<`, `>`, `<=`, `>=`
1. `==` and `!=`
1. `&`, the bitwise AND operator
1. `^`
1. `|`
1. `&&`
1. `||`
1. `?:` (ternary conditional operator)
1. `=`, `*=`, `/=`, `%=`, `+=`, `-=`, `>>=`, `<<=`, `&=`, `^=`, `|=`
1. `throw`
1. `,` (the comma operator)

The assignment, compound assignment, and ternary conditional operators are right-associative. All other binary operators are left-associative.

The rules for the ternary conditional operator are a bit more complicated than simple precedence rules can express.

- An operand binds less tightly to a `?` on its left or a `:` on its right than to any other operator. Effectively, the second operand of the conditional operator is parsed as though it is parenthesized. This allows an expression such as `a ? b , c : d` to be syntactically valid.
- An operand binds more tightly to a `?` on its right than to an assignment operator or `throw` on its left, so `a = b ? c : d` is equivalent to `a = (b ? c : d)` and `throw a ? b : c` is equivalent to `throw (a ? b : c)`.
- An operand binds more tightly to an assignment operator on its right than to `:` on its left, so `a ? b : c = d` is equivalent to `a ? b : (c = d)`.

