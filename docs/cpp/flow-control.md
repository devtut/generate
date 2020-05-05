---
metaTitle: "C++ | Flow Control"
description: "case, switch, catch, default, if, else, goto, return, throw, try, Conditional Structures: if, if..else, Jump statements : break, continue, goto, exit."
---

# Flow Control



## case


Introduces a case label of a switch statement. The operand must be a constant expression and match the switch condition in type. When the switch statement is executed, it will jump to the case label with operand equal to the condition, if any.

```cpp
char c = getchar();
bool confirmed;
switch (c) {
  case 'y':
    confirmed = true;
    break;
  case 'n':
    confirmed = false;
    break;
  default:
    std::cout << "invalid response!\n";
    abort();
}

```



## switch


According to the C++ standard,

> 
<p>The `switch` statement causes control to be transferred to one of several statements depending on the value
of a condition.</p>


The keyword `switch` is followed by a parenthesized condition and a block, which may contain `case` labels and an optional `default` label. When the switch statement is executed, control will be transferred either to a `case` label with a value matching that of the condition, if any, or to the `default` label, if any.

The condition must be an expression or a declaration, which has either integer or enumeration type, or a class type with a conversion function to integer or enumeration type.

```cpp
char c = getchar();
bool confirmed;
switch (c) {
  case 'y':
    confirmed = true;
    break;
  case 'n':
    confirmed = false;
    break;
  default:
    std::cout << "invalid response!\n";
    abort();
}

```



## catch


The `catch` keyword introduces an exception handler, that is, a block into which control will be transferred when an exception of compatible type is thrown. The `catch` keyword is followed by a parenthesized **exception declaration**, which is similar in form to a function parameter declaration: the parameter name may be omitted, and the ellipsis `...` is allowed, which matches any type. The exception handler will only handle the exception if its declaration is compatible with the type of the exception. For more details, see [catching exceptions](http://stackoverflow.com/documentation/c%2b%2b/1354/exceptions/4414/catching-exceptions#t=201608050644277417742).

```cpp
try {
    std::vector<int> v(N);
    // do something
} catch (const std::bad_alloc&) {
    std::cout << "failed to allocate memory for vector!" << std::endl;
} catch (const std::runtime_error& e) {
    std::cout << "runtime error: " << e.what() << std::endl;
} catch (...) {
    std::cout << "unexpected exception!" << std::endl;
    throw;
}

```



## default


In a switch statement, introduces a label that will be jumped to if the condition's value is not equal to any of the case labels' values.

```cpp
char c = getchar();
bool confirmed;
switch (c) {
  case 'y':
    confirmed = true;
    break;
  case 'n':
    confirmed = false;
    break;
  default:
    std::cout << "invalid response!\n";
    abort();
}

```

Defines a default constructor, copy constructor, move constructor, destructor, copy assignment operator, or move assignment operator to have its default behaviour.

```cpp
class Base {
    // ...
    // we want to be able to delete derived classes through Base*,
    // but have the usual behaviour for Base's destructor.
    virtual ~Base() = default;
};

```



## if


Introduces an if statement. The keyword `if` must be followed by a parenthesized condition, which can be either an expression or a declaration. If the condition is truthy, the substatement after the condition will be executed.

```cpp
int x;
std::cout << "Please enter a positive number." << std::endl;
std::cin >> x;
if (x <= 0) {
    std::cout << "You didn't enter a positive number!" << std::endl;
    abort();
}

```



## else


The first substatement of an if statement may be followed by the keyword `else`. The substatement after the `else` keyword will be executed when the condition is falsey (that is, when the first substatement is not executed).

```cpp
int x;
std::cin >> x;
if (x%2 == 0) {
    std::cout << "The number is even\n";
} else {
    std::cout << "The number is odd\n";
}

```



## goto


Jumps to a labelled statement, which must be located in the current function.

```cpp
bool f(int arg) {
    bool result = false;
    hWidget widget = get_widget(arg);
    if (!g()) {
        // we can't continue, but must do cleanup still
        goto end;
    }
    // ...
    result = true;
  end:
    release_widget(widget);
    return result;
}

```



## return


Returns control from a function to its caller.

If `return` has an operand, the operand is converted to the function's return type, and the converted value is returned to the caller.

```cpp
int f() {
    return 42;
}
int x = f(); // x is 42
int g() {
    return 3.14;
}
int y = g(); // y is 3

```

If `return` does not have an operand, the function must have `void` return type. As a special case, a `void`-returning function can also return an expression if the expression has type `void`.

```cpp
void f(int x) {
    if (x < 0) return;
    std::cout << sqrt(x);
}
int g() { return 42; }
void h() {
    return f(); // calls f, then returns
    return g(); // ill-formed
}

```

When `main` returns, `std::exit` is implicitly called with the return value, and the value is thus returned to the execution environment. (However, returning from `main`  destroys automatic local variables, while calling `std::exit` directly does not.)

```cpp
int main(int argc, char** argv) {
    if (argc < 2) {
        std::cout << "Missing argument\n";
        return EXIT_FAILURE; // equivalent to: exit(EXIT_FAILURE);
    }
}

```



## throw


<li>
When `throw` occurs in an expression with an operand, its effect is to throw an [exception](http://stackoverflow.com/documentation/c%2b%2b/1354/exceptions), which is a copy of the operand.

```cpp
void print_asterisks(int count) {
    if (count < 0) {
        throw std::invalid_argument("count cannot be negative!");
    }
    while (count--) { putchar('*'); }
}

```


</li>
<li>
When `throw` occurs in an expression without an operand, its effect is to [rethrow the current exception](http://stackoverflow.com/documentation/c%2b%2b/1354/exceptions/5574/rethrow-propagate-exception). If there is no current exception, `std::terminate` is called.

```cpp
try {
    // something risky
} catch (const std::bad_alloc&) {
    std::cerr << "out of memory" << std::endl;
} catch (...) {
    std::cerr << "unexpected exception" << std::endl;
    // hope the caller knows how to handle this exception
    throw;
}

```


</li>
<li>
When `throw` occurs in a function declarator, it introduces a dynamic exception specification, which lists the types of exceptions that the function is allowed to propagate.

```cpp
// this function might propagate a std::runtime_error,
// but not, say, a std::logic_error
void risky() throw(std::runtime_error);
// this function can't propagate any exceptions
void safe() throw();

```


Dynamic exception specifications are deprecated as of C++11.
</li>

Note that the first two uses of `throw` listed above constitute expressions rather than statements. (The type of a throw expression is `void`.) This makes it possible to nest them within expressions, like so:

```cpp
unsigned int predecessor(unsigned int x) {
    return (x > 0) ? (x - 1) : (throw std::invalid_argument("0 has no predecessor"));
}

```



## try


The keyword `try` is followed by a block, or by a constructor initializer list and then a block (see [here](http://stackoverflow.com/documentation/c%2b%2b/1354/exceptions/6692/function-try-blocks-in-constructor)). The try block is followed by one or more [catch blocks](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18492/catch). If an [exception](http://stackoverflow.com/documentation/c%2b%2b/1354/exceptions) propagates out of the try block, each of the corresponding catch blocks after the try block has the opportunity to handle the exception, if the types match.

```cpp
std::vector<int> v(N);     // if an exception is thrown here,
                           // it will not be caught by the following catch block
try {
    std::vector<int> v(N); // if an exception is thrown here,
                           // it will be caught by the following catch block
    // do something with v
} catch (const std::bad_alloc&) {
    // handle bad_alloc exceptions from the try block
}    

```



## Conditional Structures: if, if..else


**if and else:**

it used to check whether the given expression returns true or false and acts as such:

```cpp
if (condition) statement

```

the condition can be any valid C++ expression that returns something that be checked against truth/falsehood for example:

```cpp
if (true) { /* code here */ }  // evaluate that true is true and execute the code in the brackets
if (false) { /* code here */ } // always skip the code since false is always false

```

the condition can be anything, a function, a variable, or a comparison for example

```cpp
if(istrue()) { } // evaluate the function, if it returns true, the if will execute the code
if(isTrue(var)) { } //evalute the return of the function after passing the argument var
if(a == b) { } // this will evaluate the return of the experssion (a==b) which will be true if equal and false if unequal
if(a) { } //if a is a boolean type, it will evaluate for its value, if it's an integer, any non zero value will be true, 

```

if we want to check for a multiple expressions we can do it in two ways :

**using binary operators** :

```cpp
if (a && b) { } // will be true only if both a and b are true (binary operators are outside the scope here
if (a || b ) { } //true if a or b is true 

```

**using if/ifelse/else**:

for a simple switch either if or else

```cpp
if (a== "test") {
    //will execute if a is a string "test" 
} else {
    // only if the first failed, will execute 
}

```

for multiple choices :

```cpp
if (a=='a') { 
// if a is a char valued 'a'  
} else if (a=='b') {
// if a is a char valued 'b' 
} else if (a=='c') {
// if a is a char valued 'c'
} else { 
//if a is none of the above
}

```

however it must be noted that you should use '**switch**' instead if your code checks for the same variable's value



## Jump statements : break, continue, goto, exit.


**The break instruction:**

Using break we can leave a loop even if the condition for its end is not fulfilled. It can be used to end an infinite loop, or to force it to end before its natural end

The syntax is

```cpp
break;

```

**Example**:
we often use `break` in `switch` cases,ie once a case i switch is satisfied then the code block of that condition is executed .

```cpp
switch(conditon){
case 1: block1;
case 2: block2;
case 3: block3;
default: blockdefault;
} 

```

in this case if case 1 is satisfied then block 1 is executed , what we really want is only the block1 to be processed but instead once the block1 is processed remaining blocks,block2,block3 and blockdefault are also processed even though only case 1 was satified.To avoid this we use break at the end of each block like :

```cpp
switch(condition){
case 1: block1;
        break;
case 2: block2;
        break;
case 3: block3;
        break;
default: blockdefault;
        break;
}

```

so  only one block is processed and the control moves out of the switch loop.

break can also be used in other conditional and non conditional loops like `if`,`while`,`for` etc;

example:

```cpp
if(condition1){
   ....
   if(condition2){
    .......
    break;
    }
 ...
}

```

**The continue instruction:**

The continue instruction causes the program to skip the rest of the loop in the present iteration as if the end of the statement block would have been reached, causing it to jump to the following iteration.

The syntax is

```cpp
continue;

```

**Example**
consider the following :

```cpp
for(int i=0;i<10;i++){
if(i%2==0)
continue;
cout<<"\n @"<<i;
}

```

which produces the output:

```

@1
 @3
 @5
 @7
 @9

```

i this code whenever the condition `i%2==0` is satisfied `continue` is processed,this causes the compiler to skip all the remaining code( printing @ and i) and increment/decrement statement of the loop gets executed.

[<img src="https://i.stack.imgur.com/Fbuep.jpg" alt="in for loop" />](https://i.stack.imgur.com/Fbuep.jpg)

**The goto instruction:**

It allows making an absolute jump to another point in the program. You should use this feature carefully since its execution ignores any type of nesting limitation.
The destination point is identified by a label, which is then used as an argument for the goto instruction. A label is made of a valid identifier followed by a colon (:)

The syntax is

```cpp
goto label;
..
.
label: statement;

```

**Note:** **Use of goto statement is highly discouraged because it makes difficult to trace the control flow of a program, making the program hard to understand and hard to modify.**

[<img src="https://i.stack.imgur.com/Li8ZR.jpg" alt="types of goto" />](https://i.stack.imgur.com/Li8ZR.jpg)

**Example :**

```cpp
int num = 1;
STEP:
do{
 
    if( num%2==0 )
    {
        num = num + 1;
        goto STEP;
     }

   cout << "value of num : " << num << endl;
   num = num + 1;
 }while( num < 10 );

```

output :

```cpp
value of num : 1
value of num : 3
value of num : 5
value of num : 7
value of num : 9

```

whenever the condition `num%2==0` is satisfied the goto sends the execution control to the beginning of the `do-while` loop.

**The exit function:**

`exit` is a function defined in `cstdlib`.
The purpose of `exit` is to terminate the running program with an specific exit code. Its prototype is:

```cpp
void exit (int exit code);

```

`cstdlib` defines the standard exit codes `EXIT_SUCCESS` and `EXIT_FAILURE`.



#### Remarks


Check out the [loops topic](https://stackoverflow.com/documentation/c%2B%2B/589/loops) for the different kind of loops.

