---
metaTitle: "Keywords"
description: "as, goto, volatile, checked, unchecked, virtual, override, new, stackalloc, break, const, for, async, await, abstract, fixed, default, is, sealed, readonly, typeof, this, foreach, dynamic, try, catch, finally, throw, namespace, void, ref, out, base, params, float, double, decimal, char, operator, continue, while, return, null, string, uint, using, static, int, ulong, lock, internal, where, extern, when, if, if...else, if... else if, struct, switch, unsafe, true, false, var, enum, in, sizeof, long, bool, unchecked, do, interface, implicit, ushort, delegate, partial, sbyte, event"
---

# Keywords


[Keywords](https://msdn.microsoft.com/en-us/library/x53a06bb(v=vs.71).aspx) are predefined, reserved identifiers with special meaning to the compiler. They cannot be used as identifiers in your program without the `@` prefix. For example `@if` is a legal identifier but not the keyword `if`.



## as


The `as` keyword is an operator similar to a **cast**. If a cast is not possible, using `as` produces `null` rather than resulting in an `InvalidCastException`.

`expression as type` is equivalent to `expression is type ? (type)expression : (type)null` with the caveat that `as` is only valid on reference conversions, nullable conversions, and boxing conversions. User-defined conversions are **not** supported; a regular cast must be used instead.

For the expansion above, the compiler generates code such that `expression` will only be evaluated once and use single dynamic type check (unlike the two in the sample above).

`as` can be useful when expecting an argument to facilitate several types. Specifically it grants the user multiple options - rather than checking every possibility with `is` before casting, or just casting and catching exceptions. It is best practice to use 'as' when casting/checking an object which will cause only one unboxing penalty.  Using `is` to check, then casting will cause two unboxing penalties.

If an argument is expected to be an instance of a specific type, a regular cast is preferred as its purpose is more clear to the reader.

Because a call to `as` may produce `null`, always check the result to avoid a `NullReferenceException`.

**Example usage**

```cs
object something = "Hello";
Console.WriteLine(something as string);        //Hello
Console.Writeline(something as Nullable<int>); //null
Console.WriteLine(something as int?);          //null

//This does NOT compile:
//destination type must be a reference type (or a nullable value type)
Console.WriteLine(something as int);

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/b26q6N)

Equivalent example without using `as`:

```cs
Console.WriteLine(something is string ? (string)something : (string)null);

```

This is useful when overriding the `Equals` function in custom classes.

```cs
class MyCustomClass
{

    public override bool Equals(object obj)
    {
        MyCustomClass customObject = obj as MyCustomClass;

        // if it is null it may be really null
        // or it may be of a different type
        if (Object.ReferenceEquals(null, customObject))
        {
            // If it is null then it is not equal to this instance.
            return false;
        }

        // Other equality controls specific to class
    }

}

```



## goto


`goto` can be used to jump to a specific line inside the code, specified by a label.

### `goto` as a:

### Label:

```cs
void InfiniteHello()
{
    sayHello:
    Console.WriteLine("Hello!");
    goto sayHello;
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/Tpm3LV)

### Case statement:

```cs
enum Permissions { Read, Write };

switch (GetRequestedPermission())
{
    case Permissions.Read:
        GrantReadAccess();
        break;

    case Permissions.Write:
        GrantWriteAccess();
        goto case Permissions.Read; //People with write access also get read
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/2IV2wC)

This is particularly useful in executing multiple behaviors in a switch statement, as C# does not support [fall-through case blocks](http://stackoverflow.com/a/174223/365102).

### Exception Retry

```cs
var exCount = 0;
retry:
try
{
    //Do work
}
catch (IOException)
{
    exCount++;
    if (exCount < 3)
    {
        Thread.Sleep(100);
        goto retry;
    }
    throw;
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/kc6oiT)

Similar to many languages, use of goto keyword is discouraged except the cases below.

[Valid usages of `goto`](https://en.wikipedia.org/wiki/Goto#Common_usage_patterns_of_Goto) which apply to C#:

<li>
Fall-through case in switch statement.
</li>
<li>
Multi-level break. LINQ can often be used instead, but it usually has worse performance.
</li>
<li>
Resource deallocation when working with unwrapped low-level objects. In C#, low-level objects should usually be wrapped in separate classes.
</li>
<li>
Finite state machines, for example, parsers; used internally by compiler generated async/await state machines.
</li>



## volatile


Adding the `volatile` keyword to a field indicates to the compiler that the field's value may be changed by multiple separate threads. The primary purpose of the `volatile` keyword is to prevent compiler optimizations that assume only single-threaded access. Using `volatile` ensures that the value of the field is the most recent value that is available, and the value is not subject to the caching that non-volatile values are.

It is good practice to mark **every variable** that may be used by multiple threads as `volatile` to prevent unexpected behavior due to behind-the-scenes optimizations. Consider the following code block:

```cs
public class Example
{
    public int x;

    public void DoStuff()
    {
        x = 5;

        // the compiler will optimize this to y = 15
        var y = x + 10;

        /* the value of x will always be the current value, but y will always be "15" */
        Debug.WriteLine("x = " + x + ", y = " + y);
    }    
}

```

In the above code-block, the compiler reads the statements `x = 5` and `y = x + 10` and determines that the value of `y` will always end up as 15. Thus, it will optimize the last statement as `y = 15`. However, the variable `x` is in fact a `public` field and the value of `x` may be modified at runtime through a different thread acting on this field separately. Now consider this modified code-block. Do note that the field `x` is now declared as `volatile`.

```cs
public class Example
{
    public volatile int x;

    public void DoStuff()
    {
        x = 5;

        // the compiler no longer optimizes this statement
        var y = x + 10;

        /* the value of x and y will always be the correct values */
        Debug.WriteLine("x = " + x + ", y = " + y);
    }    
}

```

Now, the compiler looks for **read** usages of the field `x` and ensures that the current value of the field is always retrieved. This ensures that even if multiple threads are reading and writing to this field, the current value of `x` is always retrieved.

`volatile` can only be used on fields within `class`es or `struct`s. The following [is **not** valid](https://msdn.microsoft.com/en-us/library/x13ttww7.aspx):

`volatile` can only be applied to fields of following types:

- reference types or generic type parameters known to be reference types
- primitive types such as `sbyte`, `byte`, `short`, `ushort`, `int`, `uint`, `char`, `float`, and `bool`
- enums types based on `byte`, `sbyte`, `short`, `ushort`, `int` or `uint`
- `IntPtr` and `UIntPtr`

**Remarks:**

- The [`volatile`](http://stackoverflow.com/questions/72275/when-should-the-volatile-keyword-be-used-in-c) modifier is usually used for a field that is accessed by multiple threads without using the lock statement to serialize access.
- The `volatile` keyword can be applied to fields of reference types
- The `volatile` keyword will not make operating on 64-bit primitives on a 32-bit platform atomic.  Interlocked operations such as [`Interlocked.Read`](https://msdn.microsoft.com/en-us/library/system.threading.interlocked.read(v=vs.110).aspx) and [`Interlocked.Exchange`](https://msdn.microsoft.com/en-us/library/dk0121zy(v=vs.110).aspx) must still be used for safe multi-threaded access on these platforms.



## checked, unchecked


The `checked` and `unchecked` keywords define how operations handle mathematical overflow. "Overflow" in the context of the `checked` and `unchecked` keywords is when an integer arithmetic operation results in a value which is greater in magnitude than the target data type can represent.

When overflow occurs within a `checked` block (or when the compiler is set to globally use checked arithmetic), an exception is thrown to warn of undesired behavior. Meanwhile, in an `unchecked` block, overflow is silent: no exceptions are thrown, and the value will simply wrap around to the opposite boundary. This can lead to subtle, hard to find bugs.

Since most arithmetic operations are done on values that are not large or small enough to overflow, most of the time, there is no need to explicitly define a block as `checked`. Care needs to be taken when doing arithmetic on unbounded input that may cause overflow, for example when doing arithmetic in recursive functions or while taking user input.

**Neither `checked` nor `unchecked` affect floating point arithmetic operations.**

When a block or expression is declared as `unchecked`, any arithmetic operations inside it are allowed to overflow without causing an error. An example where this behavior is **desired** would be the calculation of a checksum, where the value is allowed to "wrap around" during calculation:

```cs
byte Checksum(byte[] data) {
    byte result = 0;
    for (int i = 0; i < data.Length; i++) {
        result = unchecked(result + data[i]); // unchecked expression
    }
    return result;
}

```

One of the most common uses for `unchecked` is implementing a custom override for `object.GetHashCode()`, a type of checksum. You can see the keyword's use in the answers to this question: [What is the best algorithm for an overridden System.Object.GetHashCode?](http://stackoverflow.com/questions/263400/what-is-the-best-algorithm-for-an-overridden-system-object-gethashcode).

When a block or expression is declared as `checked`, any arithmetic operation that causes an overflow results in an `OverflowException` being thrown.

```cs
int SafeSum(int x, int y) {
    checked { // checked block
        return x + y; 
    }
}

```

Both checked and unchecked may be in block and expression form.

Checked and unchecked blocks do not affect called methods, only operators called directly in the current method. For example, `Enum.ToObject()`, `Convert.ToInt32()`, and user-defined operators are not affected by custom checked/unchecked contexts.

****Note**: The default overflow default behavior (checked vs. unchecked) may be changed in the **Project Properties** or through the **/checked[+|-]**  command line switch. It is common to default to checked operations for debug builds and unchecked for release builds. The `checked` and `unchecked` keywords would then be used only where a <em>default** approach does not apply and you need an explicit behavior to ensure correctness.</em>



## virtual, override, new


### virtual and override

The `virtual` keyword allows a method, property, indexer or event to be overridden by derived classes and present polymorphic behavior. (Members are non-virtual by default in C#)

```cs
public class BaseClass
{
    public virtual void Foo()
    {
        Console.WriteLine("Foo from BaseClass");
    }
}

```

In order to override a member, the `override` keyword is used in the derived classes. (Note the signature of the members must be identical)

```cs
public class DerivedClass: BaseClass
{
    public override void Foo()
    {
        Console.WriteLine("Foo from DerivedClass");
    }
}

```

The polymorphic behavior of virtual members means that when invoked, the actual member being executed is determined at runtime instead of at compile time. The overriding member in the most derived class the particular object is an instance of will be the one executed.

In short, object can be declared  of type `BaseClass` at compile time but if at runtime it is an instance of `DerivedClass` then the overridden member will be executed:

```cs
BaseClass obj1 = new BaseClass();
obj1.Foo(); //Outputs "Foo from BaseClass"

obj1 = new DerivedClass();
obj1.Foo(); //Outputs "Foo from DerivedClass"    

```

Overriding a method is optional:

```cs
public class SecondDerivedClass: DerivedClass {}

var obj1 = new SecondDerivedClass();
obj1.Foo(); //Outputs "Foo from DerivedClass"    

```

### new

Since only members defined as `virtual` are overridable and polymorphic, a derived class redefining a non virtual member might lead to unexpected results.

```cs
public class BaseClass
{
    public void Foo()
    {
        Console.WriteLine("Foo from BaseClass");
    }
}

public class DerivedClass: BaseClass
{
    public void Foo()
    {
        Console.WriteLine("Foo from DerivedClass");
    }
}

BaseClass obj1 = new BaseClass();
obj1.Foo(); //Outputs "Foo from BaseClass"

obj1 = new DerivedClass();
obj1.Foo(); //Outputs "Foo from BaseClass" too!    

```

When this happens, the member executed is always determined at compile time based on the type of the object.

- If the object is declared of type `BaseClass` (even if at runtime is of a derived class) then the method of `BaseClass` is executed
- If the object is declared of type `DerivedClass` then the method of `DerivedClass` is executed.

This is usually an accident (When a member is added to the base type after an identical one was added to the derived type) and a compiler warning **CS0108** is generated in those scenarios.

If it was intentional, then the `new` keyword is used to suppress the compiler warning (And inform other developers of your intentions!). the behavior remains the same, the `new` keyword just suppresses the compiler warning.

```cs
public class BaseClass
{
    public void Foo()
    {
        Console.WriteLine("Foo from BaseClass");
    }
}

public class DerivedClass: BaseClass
{
    public new void Foo()
    {
        Console.WriteLine("Foo from DerivedClass");
    }
}

BaseClass obj1 = new BaseClass();
obj1.Foo(); //Outputs "Foo from BaseClass"

obj1 = new DerivedClass();
obj1.Foo(); //Outputs "Foo from BaseClass" too! 

```

### The usage of override is **not** optional

Unlike in C++, the usage of the `override` keyword is **not** optional:

```cs
public class A
{
    public virtual void Foo()
    {
    }
}

public class B : A
{
    public void Foo() // Generates CS0108
    {
    }
}

```

The above example also causes warning **CS0108**, because `B.Foo()` is not automatically overriding `A.Foo()`. Add `override` when the intention is to override the base class and cause polymorphic behavior, add `new` when you want non-polymorphic behavior and resolve the call using the static type. The latter should be used with caution, as it may cause severe confusion.

The following code even results in an error:

```cs
public class A
{
    public void Foo()
    {
    }
}

public class B : A
{
    public override void Foo() // Error: Nothing to override
    {
    }
}

```

### Derived classes can introduce polymorphism

The following code is perfectly valid (although rare):

```cs

   public class A
    {
        public void Foo()
        {
            Console.WriteLine("A");
        }
    }

    public class B : A
    {
        public new virtual void Foo() 
        {
            Console.WriteLine("B");
        }
    }

```

Now all objects with a static reference of B (and its derivatives) use polymorphism to resolve `Foo()`, while references of A use `A.Foo()`.

```cs
A a = new A();
a.Foo(); // Prints "A";
a = new B();
a.Foo(); // Prints "A";
B b = new B();
b.Foo(); // Prints "B";

```

### Virtual methods cannot be private

The C# compiler is strict in preventing senseless constructs. Methods marked as `virtual` cannot be private. Because a private method cannot be seen from a derived type, it couldn't be overwritten either. This fails to compile:

```cs
public class A
{
    private virtual void Foo() // Error: virtual methods cannot be private
    {
    }
}

```



## stackalloc


The `stackalloc` keyword creates a region of memory on the stack and returns a pointer to the start of that memory. Stack allocated memory is automatically removed when the scope it was created in is exited.

```cs
//Allocate 1024 bytes. This returns a pointer to the first byte.
byte* ptr = stackalloc byte[1024];

//Assign some values...
ptr[0] = 109;
ptr[1] = 13;
ptr[2] = 232;
...

```

<sup>**Used in an unsafe context.**</sup>

As with all pointers in C# there is no bounds checking on reads and assignments. Reading beyond the bounds of the allocated memory will have unpredictable results - it may access some arbitrary location within memory or it may cause an access violation exception.

```cs
//Allocate 1 byte
byte* ptr = stackalloc byte[1];

//Unpredictable results...
ptr[10] = 1;
ptr[-1] = 2;

```

Stack allocated memory is automatically removed when the scope it was created in is exited. This means that you should never return the memory created with stackalloc or store it beyond the lifetime of the scope.

```cs
unsafe IntPtr Leak() {
    //Allocate some memory on the stack
    var ptr = stackalloc byte[1024];

    //Return a pointer to that memory (this exits the scope of "Leak")
    return new IntPtr(ptr);
}

unsafe void Bad() {
     //ptr is now an invalid pointer, using it in any way will have
     //unpredictable results. This is exactly the same as accessing beyond
     //the bounds of the pointer.
     var ptr = Leak();
}

```

`stackalloc` can only be used when declaring **and** initialising variables. The following is **not** valid:

```cs
byte* ptr;
...
ptr = stackalloc byte[1024];

```

**Remarks:**

`stackalloc` should only be used for performance optimizations (either for computation or interop). This is due to the fact that:

- The garbage collector is not required as the memory is allocated on the stack rather than the heap - the memory is released as soon as the variable goes out of scope
- It is faster to allocate memory on the stack rather than the heap
- Increase the chance of cache hits on the CPU due to the locality of data



## break


In a loop (for, foreach, do, while) the `break` statement aborts the execution of the innermost loop and returns to the code after it. Also it can be used with `yield` in which it specifies that an iterator has come to an end.

```cs
for (var i = 0; i < 10; i++)
{
    if (i == 5)
    {
        break;
    }
    Console.WriteLine("This will appear only 5 times, as the break will stop the loop.");
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/QtpNyk)

```cs
foreach (var stuff in stuffCollection)
{
    if (stuff.SomeStringProp == null)
        break;
    // If stuff.SomeStringProp for any "stuff" is null, the loop is aborted.
    Console.WriteLine(stuff.SomeStringProp);
}

```

The break-statement is also used in switch-case constructs to break out of a case or default segment.

```cs
switch(a)
{
    case 5:
        Console.WriteLine("a was 5!");
        break;

    default:
        Console.WriteLine("a was something else!");
        break;
}

```

In switch statements, the 'break' keyword is required at the end of each case statement.  This is contrary to some languages that allow for 'falling through' to the next case statement in the series.  Workarounds for this would include 'goto' statements or stacking the 'case' statements sequentially.

Following code will give numbers `0, 1, 2, ..., 9` and the last line will not be executed. `yield break` signifies the end of the function (not just a loop).

```cs
public static IEnumerable<int> GetNumbers()
{
    int i = 0;
    while (true) {
        if (i < 10) {
            yield return i++;
        } else {
            yield break;
        }
    }
    Console.WriteLine("This line will not be executed");
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/IjSyVJ)

Note that unlike some other languages, there is no way to label a particular break in C#. This means that in the case of nested loops, only the innermost loop will be stopped:

```cs
foreach (var outerItem in outerList)
{
    foreach (var innerItem in innerList)
    {
        if (innerItem.ShoudBreakForWhateverReason)
            // This will only break out of the inner loop, the outer will continue:
            break; 
    }
}

```

If you want to break out of the **outer** loop here, you can use one of several different strategies, such as:

- A **goto** statement to jump out of the whole looping structure.
- A specific flag variable (`shouldBreak` in the following example) that can be checked at the end of each iteration of the outer loop.
- Refactoring the code to use a `return` statement in the innermost loop body, or avoid the whole nested loop structure altogether.

```cs
bool shouldBreak = false;
while(comeCondition)
{
    while(otherCondition)
    {
        if (conditionToBreak)
        {
            // Either tranfer control flow to the label below...
            goto endAllLooping;

            // OR use a flag, which can be checked in the outer loop:
            shouldBreak = true;
        }
    }

    if(shouldBreakNow)
    {
        break; // Break out of outer loop if flag was set to true
    }
}

endAllLooping: // label from where control flow will continue

```



## const


`const` is used to represent values that **will never change** throughout the lifetime of the program.  Its value is constant from **compile-time**, as opposed to the [`readonly`](http://stackoverflow.com/documentation/c%23/26/keywords/110/readonly) keyword, whose value is constant from run-time.

For example, since the speed of light will never change, we can store it in a constant.

```cs
const double c = 299792458;  // Speed of light

double CalculateEnergy(double mass)
{
    return mass * c * c;
}

```

This is essentially the same as having `return mass * 299792458 * 299792458`, as the compiler will directly substitute `c` with its constant value.

As a result, `c` cannot be changed once declared.  The following will produce a compile-time error:

```cs
const double c = 299792458;  // Speed of light 

c = 500;  //compile-time error

```

A constant can be prefixed with the same access modifiers as methods:

```cs
private const double c = 299792458;
public const double c = 299792458;
internal const double c = 299792458;

```

`const` members are `static` by nature. However using `static` explicitly is not permitted.

You can also define method-local constants:

```cs
double CalculateEnergy(double mass)
{
    const c = 299792458;
    return mass * c * c;
}

```

These can not be prefixed with a `private` or `public` keyword, since they are implicitly local to the method they are defined in.

Not all types can be used in a `const` declaration. The value types that are allowed, are the pre-defined types `sbyte`, `byte`, `short`, `ushort`, `int`, `uint`, `long`, `ulong`, `char`, `float`, `double`, `decimal`, `bool`, and all `enum` types. Trying to declare `const` members with other value types (such as `TimeSpan` or `Guid`) will fail at compile-time.

For the special pre-defined reference type `string`, constants can be declared with any value. For all other reference types, constants can be declared but must always have the value `null`.

Because `const` values are known at compile-time, they are allowed as `case` labels in a `switch` statement, as standard arguments for optional parameters, as arguments to attribute specifications, and so on.

If `const` values are used across different assemblies, care must be taken with versioning. For example, if assembly A defines a `public const int MaxRetries = 3;`, and assembly B uses that constant, then if the value of `MaxRetries` is later changed to `5` in assembly A (which is then re-compiled), that change will not be effective in assembly B **unless** assembly B is also re-compiled (with a reference to the new version of A).

For that reason, if a value might change in future revisions of the program, and if the value needs to be publicly visible, do not declare that value `const` unless you know that all dependent assemblies will be re-compiled whenever something is changed. The alternative is using `static readonly` instead of `const`, which is resolved at runtime.



## for


Syntax: `for (initializer; condition; iterator)`

- The `for` loop is commonly used when the number of iterations is known.
- The statements in the `initializer` section run only once, before you enter the loop.
- The `condition` section contains a boolean expression that's evaluated at the end of every loop iteration to determine whether the loop should exit or should run again.
- The `iterator` section defines what happens after each iteration of the body of the loop.

This example shows how `for` can be used to iterate over the characters of a string:

```cs
string str = "Hello";
for (int i = 0; i < str.Length; i++)
{
    Console.WriteLine(str[i]);                
}

```

Output:

> 
<p>H<br />
e<br />
l<br />
l<br />
o</p>


[<kbd>Live Demo on .NET Fiddle</kbd>](https://dotnetfiddle.net/Ybg356)

All of the expressions that define a `for` statement are optional; for example, the following statement is used to create an infinite loop:

```cs
for( ; ; )
{
    // Your code here
}

```

The `initializer` section can contain multiple variables, so long as they are of the same type. The `condition` section can consist of any expression which can be evaluated to a `bool`. And the `iterator` section can perform multiple actions separated by comma:

```cs
string hello = "hello";
for (int i = 0, j = 1, k = 9; i < 3 && k > 0; i++, hello += i) {
    Console.WriteLine(hello);
}

```

Output:

> 
<p>hello<br />
hello1<br />
hello12</p>


[<kbd>Live Demo on .NET Fiddle</kbd>](https://dotnetfiddle.net/LQcqCv)



## async, await


The `await` keyword was added as part of C# 5.0 release which is supported from Visual Studio 2012 onwards. It leverages Task Parallel Library (TPL) which made the multi-threading relatively easier. The `async` and `await` keywords are used in pair in the same function as shown below. The `await` keyword is used to pause the current asynchronous method's execution until the awaited asynchronous task is completed and/or its results returned. In order to use the `await` keyword, the method that uses it must be marked with the `async` keyword.

Using `async` with `void` is strongly discouraged. For more info you can look [here](https://msdn.microsoft.com/en-us/magazine/jj991977.aspx).

Example:

```cs
public async Task DoSomethingAsync()
{    
    Console.WriteLine("Starting a useless process...");
    Stopwatch stopwatch = Stopwatch.StartNew();
    int delay = await UselessProcessAsync(1000);
    stopwatch.Stop();
    Console.WriteLine("A useless process took {0} milliseconds to execute.", stopwatch.ElapsedMilliseconds);
}

public async Task<int> UselessProcessAsync(int x)
{
    await Task.Delay(x);
    return x;
}

```

Output:

> 
<p>"Starting a useless process..."<br />
<br />
**... 1 second delay... **</p>
"A useless process took 1000 milliseconds to execute."


The keyword pairs `async` and `await` can be omitted if a `Task` or `Task<T>` returning method only returns a single asynchronous operation.

**Rather than this:**

```cs
public async Task PrintAndDelayAsync(string message, int delay)
{
    Debug.WriteLine(message);
    await Task.Delay(x);
}

```

**It is preferred to do this:**

```cs
public Task PrintAndDelayAsync(string message, int delay)
{
    Debug.WriteLine(message);
    return Task.Delay(x);
}

```

In C# 5.0 `await` cannot be used in `catch` and `finally`.

[With C# 6.0](https://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/50/await-in-catch-and-finally#t=201607281146527675886) `await` can be used in `catch` and `finally`.



## abstract


A class marked with the keyword `abstract` cannot be instantiated.

A class **must** be marked as abstract if it contains abstract members or if it inherits abstract members that it doesn't implement. A class **may** be marked as abstract even if no abstract members are involved.

Abstract classes are usually used as base classes when some part of the implementation needs to be specified by another component.

```cs
abstract class Animal 
{
    string Name { get; set; }
    public abstract void MakeSound();
}

public class Cat : Animal 
{
    public override void MakeSound()
    {
        Console.WriteLine("Meov meov");
    }
}

public class Dog : Animal 
{   
    public override void MakeSound()
    {
        Console.WriteLine("Bark bark");
    }
}

Animal cat = new Cat();       // Allowed due to Cat deriving from Animal
cat.MakeSound();              // will print out "Meov meov"    

Animal dog = new Dog();       // Allowed due to Dog deriving from Animal
dog.MakeSound();              // will print out "Bark bark"

Animal animal = new Animal(); // Not allowed due to being an abstract class

```

A method, property, or event marked with the keyword `abstract` indicates that the implementation for that member is expected to be provided in a subclass. As mentioned above, abstract members can only appear in abstract classes.

```cs
abstract class Animal 
{
   public abstract string Name { get; set; }
}

public class Cat : Animal 
{
    public override string Name { get; set; }
}

public class Dog : Animal 
{
    public override string Name { get; set; }
}

```



## fixed


The fixed statement fixes memory in one location. Objects in memory are usually moving arround, this makes garbage collection possible. But when we use unsafe pointers to memory addresses, that memory must not be moved.

- We use the fixed statement to ensure that the garbage collector does not relocate the string data.

<h3>Fixed Variables</h3>

```cs
var myStr = "Hello world!";

fixed (char* ptr = myStr)
{
    // myStr is now fixed (won't be [re]moved by the Garbage Collector).
    // We can now do something with ptr.
}

```

<sup>**Used in an unsafe context.**</sup>

<h3>Fixed Array Size</h3>

```cs
unsafe struct Example
{
    public fixed byte SomeField[8];
    public fixed char AnotherField[64];
}

```

`fixed` can only be used on fields in a `struct` (must also be used in an unsafe context).



## default


For classes, interfaces, delegate, array, nullable (such as int?) and pointer types, `default(TheType)` returns `null`:

```cs
class MyClass {}
Debug.Assert(default(MyClass) == null);
Debug.Assert(default(string) == null);

```

For structs and enums, `default(TheType)` returns the same as `new TheType()`:

```cs
struct Coordinates
{
    public int X { get; set; }
    public int Y { get; set; }
}

struct MyStruct
{
    public string Name { get; set; }
    public Coordinates Location { get; set; }
    public Coordinates? SecondLocation { get; set; }
    public TimeSpan Duration { get; set; }
}

var defaultStruct = default(MyStruct);
Debug.Assert(defaultStruct.Equals(new MyStruct()));
Debug.Assert(defaultStruct.Location.Equals(new Coordinates()));
Debug.Assert(defaultStruct.Location.X == 0);
Debug.Assert(defaultStruct.Location.Y == 0);
Debug.Assert(defaultStruct.SecondLocation == null);
Debug.Assert(defaultStruct.Name == null);
Debug.Assert(defaultStruct.Duration == TimeSpan.Zero);

```

`default(T)` can be particularly useful when `T` is a generic parameter for which no constraint is present to decide whether `T` is a reference type or a value type, for example:

```cs
public T GetResourceOrDefault<T>(string resourceName)
{
   if (ResourceExists(resourceName))
   {
      return (T)GetResource(resourceName);
   }
   else
   {
      return default(T);
   }
}

```



## is


Checks if an object is compatible with a given type, i.e. if an object is an instance of the `BaseInterface` type, or a type that derives from `BaseInterface`:

```cs
interface BaseInterface {}
class BaseClass : BaseInterface {}
class DerivedClass : BaseClass {}

var d = new DerivedClass();
Console.WriteLine(d is DerivedClass);  // True
Console.WriteLine(d is BaseClass);     // True
Console.WriteLine(d is BaseInterface); // True
Console.WriteLine(d is object);        // True
Console.WriteLine(d is string);        // False

var b = new BaseClass();
Console.WriteLine(b is DerivedClass);  // False
Console.WriteLine(b is BaseClass);     // True
Console.WriteLine(b is BaseInterface); // True
Console.WriteLine(b is object);        // True
Console.WriteLine(b is string);        // False

```

If the intent of the cast is to use the object, it is best practice to use the [`as`](http://stackoverflow.com/documentation/c%23/26/keywords/138/as) keyword'

```cs
interface BaseInterface {}
class BaseClass : BaseInterface {}
class DerivedClass : BaseClass {}

var d = new DerivedClass();
Console.WriteLine(d is DerivedClass);  // True - valid use of 'is'
Console.WriteLine(d is BaseClass);     // True - valid use of 'is'

if(d is BaseClass){
    var castedD = (BaseClass)d;
    castedD.Method(); // valid, but not best practice
}

var asD = d as BaseClass;

if(asD!=null){
    asD.Method(); //prefered method since you incur only one unboxing penalty
}

```

But, from C# 7 [`pattern matching`](https://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/13323/pattern-matching#t=201707130940065371224) feature extends the is operator to check for a type and declare a new variable at the same time. Same code part with C# 7 :

```cs
if(d is BaseClass asD ){
    asD.Method();
}

```



## sealed


When applied to a class, the `sealed` modifier prevents other classes from inheriting from it.

```cs
class A { }
sealed class B : A { }
class C : B { } //error : Cannot derive from the sealed class

```

When applied to a `virtual` method (or virtual property), the `sealed` modifier prevents this method (property) from being **overriden** in derived classes.

```cs
public class A 
{
    public sealed override string ToString() // Virtual method inherited from class Object
    {
        return "Do not override me!";
    }
}

public class B: A 
{
    public override string ToString() // Compile time error
    { 
        return "An attempt to override"; 
    }
}

```



## readonly


The `readonly` keyword is a field modifier. When a field declaration includes a `readonly` modifier, assignments to that field can only occur as part of the declaration or in a constructor in the same class.

The `readonly` keyword is different from the `const` keyword. A `const` field can only be initialized at the declaration of the field. A `readonly` field can be initialized either at the declaration or in a constructor. Therefore, `readonly` fields can have different values depending on the constructor used.

The `readonly` keyword is often used when injecting dependencies.

```cs
class Person
{
    readonly string _name;
    readonly string _surname = "Surname";

    Person(string name)
    {
        _name = name;
    }
    void ChangeName()
    {
        _name = "another name"; // Compile error
        _surname = "another surname"; // Compile error
    }
}

```

> 
Note: Declaring a field **readonly** does not imply **immutability**. If the field is a **reference type** then the **content** of the object can be changed. **Readonly** is typically used to prevent having the object being **overwritten** and assigned only during **instantiation** of that object.


> 
Note: Inside the constructor a readonly field can be reassigned


```cs
public class Car
{
    public double Speed {get; set;}
}

//In code

private readonly Car car = new Car();

private void SomeMethod()
{
    car.Speed = 100;
}

```



## typeof


Returns the `Type` of an object, without the need to instantiate it.

```cs
Type type = typeof(string);
Console.WriteLine(type.FullName); //System.String
Console.WriteLine("Hello".GetType() == type); //True
Console.WriteLine("Hello".GetType() == typeof(string)); //True

```



## this


The `this` keyword refers to the current instance of class(object). That way two variables with the same name, one at the class-level (a field) and one being a parameter (or local variable) of a method, can be distinguished.

```cs
public MyClass {
    int a;

    void set_a(int a)
    {
        //this.a refers to the variable defined outside of the method,
        //while a refers to the passed parameter.
        this.a = a;
    }
}

```

Other usages of the keyword are [chaining non-static constructor overloads](http://stackoverflow.com/documentation/c%23/25/constructors-destructors/56/calling-a-constructor-from-another-constructor#t=201607231911138150753):

```cs
public MyClass(int arg) : this(arg, null)
{
}

```

and writing [indexers](http://stackoverflow.com/documentation/c%23/1660/indexer#t=201607252101420586035):

```cs
public string this[int idx1, string idx2]
{
    get { /* ... */ }
    set { /* ... */ }
}

```

and declaring [extension methods](http://stackoverflow.com/documentation/c%23/20/extension-methods#t=20160723191025670507):

```cs
public static int Count<TItem>(this IEnumerable<TItem> source)
{
    // ...
}

```

If there is no conflict with a local variable or parameter, it is a matter of style whether to use `this` or not, so `this.MemberOfType` and `MemberOfType` would be equivalent in that case. Also see [`base`](http://stackoverflow.com/documentation/c%23/26/keywords/1840/base#t=201607261455204441752) keyword.

Note that if an extension method is to be called on the current instance, `this` is required. For example if your are inside a non-static method of a class which implements `IEnumerable<>` and you want to call the extension `Count` from before, you must use:

```cs
this.Count()  // works like StaticClassForExtensionMethod.Count(this)

```

and `this` cannot be omitted there.



## foreach


`foreach` is used to iterate over the elements of an array or the items within a collection which implements [`IEnumerable`](https://msdn.microsoft.com/en-us/library/system.collections.ienumerable(v=vs.110).aspx)✝.

```cs
var lines = new string[] { 
    "Hello world!", 
    "How are you doing today?", 
    "Goodbye"
};

foreach (string line in lines)
{
    Console.WriteLine(line);
}

```

This will output

> 
<p>"Hello world!"<br />
"How are you doing today?"<br />
"Goodbye"</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/0jy78m)

You can exit the `foreach` loop at any point by using the [break](http://stackoverflow.com/documentation/c%23/26/keywords/2858/break) keyword or move on to the next iteration using the [continue](http://stackoverflow.com/documentation/c%23/26/keywords/154/continue) keyword.

```cs
var numbers = new int[] {1, 2, 3, 4, 5, 6};

foreach (var number in numbers)
{
    // Skip if 2
    if (number == 2)
        continue;

    // Stop iteration if 5
    if (number == 5)
        break;

    Console.Write(number + ", ");
}

// Prints: 1, 3, 4, 

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/dfSAbF)

Notice that the order of iteration is guaranteed **only** for certain collections such as arrays and `List`, but **not** guaranteed for many other collections.

✝ While `IEnumerable` is typically used to indicate enumerable collections, `foreach` only requires that the collection expose publicly the `object GetEnumerator()` method, which should return an object that exposes the `bool MoveNext()` method and the `object Current { get; }` property.



## dynamic


The `dynamic` keyword is used with [dynamically typed objects](http://stackoverflow.com/documentation/c%23/762/dynamic-type#t=201607212330428041437). Objects declared as `dynamic` forego compile-time static checks, and are instead evaluated at runtime.

```cs
using System;
using System.Dynamic;

dynamic info = new ExpandoObject();
info.Id = 123;
info.Another = 456;

Console.WriteLine(info.Another);
// 456

Console.WriteLine(info.DoesntExist);
// Throws RuntimeBinderException

```

The following example uses `dynamic` with Newtonsoft's library Json.NET, in order to easily read data from a deserialized JSON file.

```cs
try
{
    string json = @"{ x : 10, y : ""ho""}";
    dynamic deserializedJson = JsonConvert.DeserializeObject(json);
    int x = deserializedJson.x;
    string y = deserializedJson.y;
    // int z = deserializedJson.z; // throws RuntimeBinderException
}
catch (RuntimeBinderException e)
{
    // This exception is thrown when a property
    // that wasn't assigned to a dynamic variable is used
}

```

There are some limitations associated with the dynamic keyword. One of them is the use of extension methods. The following example adds an extension method for string: `SayHello`.

```cs
static class StringExtensions
{
    public static string SayHello(this string s) => $"Hello {s}!";
}

```

The first approach will be to call it as usual (as for a string):

```cs
var person = "Person";
Console.WriteLine(person.SayHello());

dynamic manager = "Manager";
Console.WriteLine(manager.SayHello()); // RuntimeBinderException

```

No compilation error, but at runtime you get a `RuntimeBinderException`. The workaround for this will be to call the extension method via the static class:

```cs
var helloManager = StringExtensions.SayHello(manager);
Console.WriteLine(helloManager);

```



## try, catch, finally, throw


`try`, `catch`, `finally`, and `throw` allow you to handle exceptions in your code.

```cs
var processor = new InputProcessor();

// The code within the try block will be executed. If an exception occurs during execution of
// this code, execution will pass to the catch block corresponding to the exception type.
try 
{
    processor.Process(input);
}
// If a FormatException is thrown during the try block, then this catch block
// will be executed.
catch (FormatException ex)
{
    // Throw is a keyword that will manually throw an exception, triggering any catch block that is
    // waiting for that exception type. 
    throw new InvalidOperationException("Invalid input", ex);
}
// catch can be used to catch all or any specific exceptions. This catch block,
// with no type specified, catches any exception that hasn't already been caught
// in a prior catch block.
catch
{
    LogUnexpectedException(); 
    throw; // Re-throws the original exception.
}
// The finally block is executed after all try-catch blocks have been; either after the try has
// succeeded in running all commands or after all exceptions have been caught. 
finally
{
    processor.Dispose();
}

```

**Note:** The `return` keyword can be used in `try` block, and the `finally` block will still be executed (just before returning). For example:

```cs
try 
{
    connection.Open();
    return connection.Get(query);
} 
finally 
{
    connection.Close();
}

```

The statement `connection.Close()` will execute before the result of `connection.Get(query)` is returned.



## namespace


The `namespace` keyword is an organization construct that helps us understand how a codebase is arranged. Namespaces in C# are virtual spaces rather than being in a physical folder.

```cs
namespace StackOverflow
{
    namespace Documentation
    {
        namespace CSharp.Keywords
        {
            public class Program
            {
                public static void Main()
                {
                    Console.WriteLine(typeof(Program).Namespace);
                    //StackOverflow.Documentation.CSharp.Keywords
                }
            }
        }
    }
}

```

Namespaces in C# can also be written in chained syntax. The following is equivalent to above:

```cs
namespace StackOverflow.Documentation.CSharp.Keywords
{
    public class Program
    {
        public static void Main()
        {
            Console.WriteLine(typeof(Program).Namespace);
            //StackOverflow.Documentation.CSharp.Keywords
        }
    }
}

```



## void


The reserved word `"void"` is an alias of `System.Void` type, and has two uses:

1. Declare a method that doesn't have a return value:

```cs
public void DoSomething()
{
    // Do some work, don't return any value to the caller.
}

```

A method with a return type of void can still have the `return` keyword in its body. This is useful when you want to exit the method's execution and return the flow to the caller:

```cs
public void DoSomething()
{
    // Do some work...

    if (condition)
        return;

    // Do some more work if the condition evaluated to false.
}

```


1. Declare a pointer to an unknown type in an unsafe context.

In an unsafe context, a type may be a pointer type, a value type, or a reference type. A pointer type declaration is usually `type* identifier`, where the type is a known type - i.e `int* myInt`, but can also be `void* identifier`, where the type is unknown.

Note that declaring a void pointer type is [discouraged by Microsoft.](https://msdn.microsoft.com/en-us/library/y31yhkeb.aspx)



## ref, out


The `ref` and `out` keywords cause an argument to be passed by reference, not by value. For value types, this means that the value of the variable can be changed by the callee.

```cs
int x = 5;
ChangeX(ref x);
// The value of x could be different now

```

For reference types, the instance in the variable can not only be modified (as is the case without `ref`), but it can also be replaced altogether:

```cs
Address a = new Address();
ChangeFieldInAddress(a);
// a will be the same instance as before, even if it is modified
CreateANewInstance(ref a);
// a could be an entirely new instance now

```

The main difference between the `out` and `ref` keyword is that `ref` requires the variable to be initialized by the caller, while `out` passes that responsibility to the callee.

To use an `out` parameter, both the method definition and the calling method must explicitly use the `out` keyword.

```cs
int number = 1;
Console.WriteLine("Before AddByRef: " + number); // number = 1
AddOneByRef(ref number);
Console.WriteLine("After AddByRef: " + number);  // number = 2
SetByOut(out number);
Console.WriteLine("After SetByOut: " + number);  // number = 34

void AddOneByRef(ref int value)
{
    value++;
}

void SetByOut(out int value)
{
    value = 34;
}

```

[<kbd>Live Demo on .NET Fiddle</kbd>](https://dotnetfiddle.net/ma2ikc)

The following does **not** compile, because `out` parameters must have a value assigned before the method returns (it would compile using `ref` instead):

```cs
void PrintByOut(out int value)
{
    Console.WriteLine("Hello!");
}

```

**using out keyword as Generic Modifier**

`out` keyword can also be used in generic type parameters when defining generic interfaces and delegates. In this case, the `out` keyword specifies that the type parameter is covariant.

> 
Covariance enables you to use a more derived type than that specified by the generic parameter. This allows for implicit conversion of classes that implement variant interfaces and implicit conversion of delegate types. Covariance and contravariance are supported for reference types, but they are not supported for value types. - MSDN


```cs
//if we have an interface like this
interface ICovariant<out R> { }

//and two variables like
ICovariant<Object> iobj = new Sample<Object>();
ICovariant<String> istr = new Sample<String>();

// then the following statement is valid
// without the out keyword this would have thrown error
iobj = istr; // implicit conversion occurs here

```



## base


The **`base`** keyword is used to access members from a base class.  It is commonly used to call base implementations of virtual methods, or to specify which base constructor should be called.

**Choosing a constructor**

```cs
public class Child : SomeBaseClass {
    public Child() : base("some string for the base class")
    {
    }
}

public class SomeBaseClass {
    public SomeBaseClass()
    {
        // new Child() will not call this constructor, as it does not have a parameter
    }
    public SomeBaseClass(string message)
    {
        // new Child() will use this base constructor because of the specified parameter in Child's constructor
        Console.WriteLine(message);
    }
}

```

**Calling base implementation of virtual method**

```cs
public override void SomeVirtualMethod() {
    // Do something, then call base implementation
    base.SomeVirtualMethod();
}

```

It is possible to use the base keyword to call a base implementation from any method.  This ties the method call directly to the base implementation, which means that even if new child classes override a virtual method, the base implementation will still be called so this needs to be used with caution.

```cs
public class Parent
{
    public virtual int VirtualMethod()
    {
        return 1;
    }
}

public class Child : Parent
{
    public override int VirtualMethod() {
        return 11;
    }

    public int NormalMethod()
    {
        return base.VirtualMethod();
    }

    public void CallMethods()
    {
        Assert.AreEqual(11, VirtualMethod());

        Assert.AreEqual(1, NormalMethod());
        Assert.AreEqual(1, base.VirtualMethod());
    }
}

public class GrandChild : Child
{
    public override int VirtualMethod()
    {
        return 21;
    }

    public void CallAgain()
    {
        Assert.AreEqual(21, VirtualMethod());
        Assert.AreEqual(11, base.VirtualMethod());

        // Notice that the call to NormalMethod below still returns the value
        // from the extreme base class even though the method has been overridden
        // in the child class.
        Assert.AreEqual(1, NormalMethod());
    }
}

```



## params


`params` allows a method parameter to receive a variable number of arguments, i.e. zero, one or multiple arguments are allowed for that parameter.

```cs
static int AddAll(params int[] numbers)
{
    int total = 0;
    foreach (int number in numbers)
    {
        total += number;
    }
    
    return total;
}

```

This method can now be called with a typical list of `int` arguments, or an array of ints.

```cs
AddAll(5, 10, 15, 20);                // 50
AddAll(new int[] { 5, 10, 15, 20 });  // 50

```

`params` must appear at most once and if used, it must be **last** in the argument list, even if the succeeding type is different to that of the array.

Be careful when overloading functions when using the `params` keyword.  C# prefers matching more specific overloads before resorting to trying to use overloads with `params`.  For example if you have two methods:

```cs
static double Add(params double[] numbers)
{
    Console.WriteLine("Add with array of doubles");
    double total = 0.0;
    foreach (double number in numbers)
    {
        total += number;
    }
    
    return total;
}

static int Add(int a, int b)
{
    Console.WriteLine("Add with 2 ints");
    return a + b;
}

```

Then the specific 2 argument overload will take precedence before trying the `params` overload.

```cs
Add(2, 3);      //prints "Add with 2 ints"
Add(2, 3.0);    //prints "Add with array of doubles" (doubles are not ints)
Add(2, 3, 4);   //prints "Add with array of doubles" (no 3 argument overload)

```



## float, double, decimal


### float

`float` is an alias to the .NET datatype `System.Single`. It allows IEEE 754 single-precision floating point numbers to be stored. This data type is present in `mscorlib.dll` which is implicitly referenced by every C# project when you create them.

Approximate range: -3.4 × 10<sup>38</sup> to 3.4 × 10<sup>38</sup>

Decimal precision: 6-9 significant digits

**Notation**:

```cs
float f = 0.1259;
var f1 = 0.7895f; // f is literal suffix to represent float values 

```

> 
<p>It should be noted that the `float` type often results in significant
rounding errors. In applications where precision is important, other
data types should be considered.</p>


### double

`double` is an alias to the .NET datatype `System.Double`. It represents a double-precision 64-bit floating-point number. This datatype is present in `mscorlib.dll` which is implicitly referenced in any C# project.

Range: ±5.0 × 10<sup>−324</sup> to ±1.7 × 10<sup>308</sup>

Decimal precision: 15-16 significant digits

**Notation**:

```cs
double distance = 200.34; // a double value
double salary = 245; // an integer implicitly type-casted to double value
var marks = 123.764D; // D is literal suffix to represent double values

```

### decimal

`decimal` is an alias to the .NET datatype `System.Decimal`. It represents a keyword indicates a 128-bit data type. Compared to floating-point types, the decimal type has more precision and a smaller range, which makes it appropriate for financial and monetary calculations. This datatype is present in `mscorlib.dll` which is implicitly referenced in any C# project.

Range: -7.9 × 10<sup>28</sup> to 7.9 × 10<sup>28</sup>

Decimal precision: 28-29 significant digits

**Notation**:

```cs
decimal payable = 152.25m; // a decimal value
var marks = 754.24m; // m is literal suffix to represent decimal values

```



## char


A char is single letter stored inside a variable. It is built-in value type which takes two bytes of memory space. It represents `System.Char` data type found in `mscorlib.dll` which is implicitly referenced by every C# project when you create them.

There are multiple ways to do this.

1. `char c = 'c';`
1. `char c = '\u0063'; //Unicode`
1. `char c = '\x0063'; //Hex`
1. `char c = (char)99;//Integral`

A char can be implicitly converted to `ushort, int, uint, long, ulong, float, double,` or `decimal` and it will return the integer value of that char.

```cs
ushort u = c;

```

returns 99 etc.

However, there are no implicit conversions from other types to char. Instead you must cast them.

```cs
ushort u = 99;
 char c = (char)u;

```



## operator


Most of the [built-in operators](https://msdn.microsoft.com/en-us/library/6a71f45d.aspx) (including conversion operators) can be overloaded by using the `operator` keyword along with the `public` and `static` modifiers.

The operators comes in three forms: unary operators, binary operators and conversion operators.

Unary and binary operators requires at least one parameter of same type as the containing type, and some requires a complementary matching operator.

Conversion operators must convert to or from the enclosing type.

```cs
public struct Vector32
{
    
    public Vector32(int x, int y)
    {
        X = x;
        Y = y;
    }
    
    public int X { get; }
    public int Y { get; }

    public static bool operator ==(Vector32 left, Vector32 right)
        => left.X == right.X && left.Y == right.Y;

    public static bool operator !=(Vector32 left, Vector32 right)
        => !(left == right);

    public static Vector32 operator +(Vector32 left, Vector32 right)
        => new Vector32(left.X + right.X, left.Y + right.Y);

    public static Vector32 operator +(Vector32 left, int right)
        => new Vector32(left.X + right, left.Y + right);

    public static Vector32 operator +(int left, Vector32 right)
        => right + left;

    public static Vector32 operator -(Vector32 left, Vector32 right)
        => new Vector32(left.X - right.X, left.Y - right.Y);

    public static Vector32 operator -(Vector32 left, int right)
        => new Vector32(left.X - right, left.Y - right);

    public static Vector32 operator -(int left, Vector32 right)
        => right - left;

    public static implicit operator Vector64(Vector32 vector)
        => new Vector64(vector.X, vector.Y);

    public override string ToString() => $"{{{X}, {Y}}}";

}

public struct Vector64
{

    public Vector64(long x, long y)
    {
        X = x;
        Y = y;
    }

    public long X { get; }
    public long Y { get; }

    public override string ToString() => $"{{{X}, {Y}}}";

}

```

**Example**

```cs
var vector1 = new Vector32(15, 39);
var vector2 = new Vector32(87, 64);
        
Console.WriteLine(vector1 == vector2); // false
Console.WriteLine(vector1 != vector2); // true
Console.WriteLine(vector1 + vector2);  // {102, 103}
Console.WriteLine(vector1 - vector2);  // {-72, -25}

```



## continue


Immediately pass control to the next iteration of the enclosing loop construct (for, foreach, do, while):

```cs
for (var i = 0; i < 10; i++)
{
    if (i < 5)
    {
        continue;
    }
    Console.WriteLine(i);
}

```

Output:

> 
<p>5<br />
6<br />
7<br />
8<br />
9</p>


[<kbd>Live Demo on .NET Fiddle</kbd>](https://dotnetfiddle.net/H2NB0V)

```cs
var stuff = new [] {"a", "b", null, "c", "d"};

foreach (var s in stuff)
{
    if (s == null)
    {
        continue;
    }           
    Console.WriteLine(s);
}

```

Output:

> 
<p>a<br />
b<br />
c<br />
d</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/l1JPiI)



## while


The `while` operator iterates over a block of code until the conditional query equals false or the code is interrupted with a [`goto`](http://stackoverflow.com/documentation/c%23/26/keywords/193/goto), [`return`](http://stackoverflow.com/documentation/c%23/26/keywords/4600/return), [`break`](http://stackoverflow.com/documentation/c%23/26/keywords/2858/break) or `throw` statement.

Syntax for `while` keyword:

> 
<p>while( **condition** )
{ **code block;**  }</p>


Example:

```cs
int i = 0;
while (i++ < 5)
{
    Console.WriteLine("While is on loop number {0}.", i);
}

```

Output:

> 
<p>"While is on loop number 1." <br />
"While is on loop number 2."<br />
"While is on loop number 3."<br />
"While is on loop number 4."<br />
"While is on loop number 5."</p>


[<kbd>Live Demo on .NET Fiddle</kbd>](https://dotnetfiddle.net/KRQjV0)

A while loop is **Entry Controlled**, as the condition is checked **before** the execution of the enclosed code block. This means that the while loop wouldn't execute its statements if the condition is false.

```cs
bool a = false;

while (a == true)
{
    Console.WriteLine("This will never be printed.");
}

```

Giving a `while` condition without provisioning it to become false at some point will result in an infinite or endless loop. As far as possible, this should be avoided, however, there may be some exceptional circumstances when you need this.

You can create such a loop as follows:

```cs
while (true)
{
//...
}

```

Note that the C# compiler will transform loops such as

```cs
while (true)
{
// ...
}

```

or

```cs
for(;;)
{
// ...
}

```

into

```cs
{
:label
// ...
goto label;
}

```

Note that a while loop may have any condition, no matter how complex, as long as it evaluates to (or returns) a boolean value (bool). It may also contain a function that returns a boolean value (as such a function evaluates to the same type as an expression such as `a==x'). For example,

```cs
while (AgriculturalService.MoreCornToPick(myFarm.GetAddress()))
{
    myFarm.PickCorn();
}

```



## return


> 
<p>MSDN: The return statement terminates execution of the method in which
it appears and returns control to the calling method. It can also
return an optional value. If the method is a void type, the return
statement can be omitted.</p>


```cs
public int Sum(int valueA, int valueB)
{
    return valueA + valueB;
}


public void Terminate(bool terminateEarly)
{
    if (terminateEarly) return; // method returns to caller if true was passed in
    else Console.WriteLine("Not early"); // prints only if terminateEarly was false
}

```



## null


A variable of a reference type can hold either a valid reference to an instance or a null reference. The null reference is the default value of reference type variables, as well as nullable value types.

`null` is the keyword that represents a null reference.

As an expression, it can be used to assign the null reference to variables of the aforementioned types:

```cs
object a = null;
string b = null;
int? c = null;
List<int> d  = null;

```

Non-nullable value types cannot be assigned a null reference. All the following assignments are invalid:

```cs
int a = null; 
float b = null;
decimal c = null;

```

The null reference should **not** be confused with valid instances of various types such as:

- an empty list (`new List<int>()`)
- an empty string (`""`)
- the number zero (`0`, `0f`, `0m`)
- the null character ( `'\0'` )

Sometimes, it is meaningful to check if something is either null or an empty/default object. The System.String.IsNullOrEmpty(String) method may be used to check this, or you may implement your own equivalent method.

```cs
private void GreetUser(string userName)
{
    if (String.IsNullOrEmpty(userName))
    {
        //The method that called us either sent in an empty string, or they sent us a null reference. Either way, we need to report the problem.
        throw new InvalidOperationException("userName may not be null or empty.");
    }
    else
    {
        //userName is acceptable.
        Console.WriteLine("Hello, " + userName + "!");
    }
}

```



## string


`string` is an alias to the .NET datatype `System.String`, which allows text (sequences of characters) to be stored.

Notation:

```cs
string a = "Hello";
var b = "world";
var f = new string(new []{ 'h', 'i', '!' }); // hi!

```

Each character in the string is encoded in UTF-16, which means that each character will require a minimum 2 bytes of storage space.



## uint


An **unsigned integer**, or **uint**, is a numeric datatype that only can hold positive integers. Like it's name suggests, it represents an unsigned 32-bit integer. The **uint** keyword itself is an alias for the Common Type System type `System.UInt32`. This datatype is present in `mscorlib.dll`, which is implicitly referenced by every C# project when you create them. It occupies four bytes of memory space.

Unsigned integers can hold any value from 0 to 4,294,967,295.

**Examples on how and now not to declare unsigned integers**

```cs
uint i = 425697; // Valid expression, explicitly stated to compiler
var i1 = 789247U; // Valid expression, suffix allows compiler to determine datatype
uint x = 3.0; // Error, there is no implicit conversion

```

**Please note:** According to [Microsoft](https://msdn.microsoft.com/en-us/library/x0sksh43.aspx), it is recommended to use the **[int](http://stackoverflow.com/documentation/c%23/26/keywords#t=201608192209189084166)** datatype wherever possible as the **uint** datatype is not CLS-compliant.



## using


There are two types of `using` keyword usage, `using statement` and `using directive`:

<li>
**using statement**:
The `using` keyword ensures that objects that implement the `IDisposable` interface are properly disposed after usage. There is a separate topic for the [using statement](http://stackoverflow.com/documentation/c%23/38/using-statement#t=201607311905386691069)
</li>
<li>
**using directive**
The `using` directive has three usages, see the [msdn page for the using directive](https://msdn.microsoft.com/en-us/library/sf0df423.aspx). There is a separate topic for the [using directive](http://stackoverflow.com/documentation/c%23/52/using-directive#t=201607311908368095223).
</li>



## static


The `static` modifier is used to declare a static member, which does not need to be instantiated in order to be accessed, but instead is accessed simply through its name, i.e. `DateTime.Now`.

`static` can be used with classes, fields, methods, properties, operators, events, and constructors.

While an instance of a class contains a separate copy of all instance fields of the class, there is only one copy of each static field.

```cs
class A
{
    static public int count = 0;

    public A()
    {
        count++;
    }
}

class Program
{
    static void Main(string[] args)
    {
        A a = new A();
        A b = new A();
        A c = new A();

        Console.WriteLine(A.count); // 3 
    }
}

```

`count` equals to the total number of instances of `A` class.

The static modifier can also be used to declare a static constructor for a class, to initialize static data or run code that only needs to be called once. Static constructors are called before the class is referenced for the first time.

```cs
class A
{
    static public DateTime InitializationTime;

    // Static constructor
    static A()
    {
        InitializationTime = DateTime.Now;
        // Guaranteed to only run once
        Console.WriteLine(InitializationTime.ToString());
    }
}

```

A `static class` is marked with the `static` keyword, and can be used as a beneficial container for a set of methods that work on parameters, but don't necessarily require being tied to an instance. Because of the `static` nature of the class, it cannot be instantiated, but it can contain a `static constructor`. Some features of a `static class` include:

- Can't be inherited
- Can't inherit from anything other than `Object`
- Can contain a static constructor but not an instance constructor
- Can only contain static members
- Is sealed

The compiler is also friendly and will let the developer know if any instance members exist within the class. An example would be a static class that converts between US and Canadian metrics:

```cs
static class ConversionHelper {
    private static double oneGallonPerLitreRate = 0.264172;

    public static double litreToGallonConversion(int litres) {
        return litres * oneGallonPerLitreRate;
    }
}

```

When classes are declared static:

```cs
public static class Functions
{
  public static int Double(int value)
  {
    return value + value;
  }
}

```

all function, properties or members within the class also need to be declared static. No instance of the class can be created.
In essence a static class allows you to create bundles of functions that are grouped together logically.

Since C#6 `static` can also be used alongside `using` to import static members and methods. They can be used then without class name.

Old way, without `using static`:

```cs
using System;

public class ConsoleApplication
{
    public static void Main()
    {
         Console.WriteLine("Hello World!"); //Writeline is method belonging to static class Console
    }

}

```

Example with `using static`

```cs
using static System.Console;

public class ConsoleApplication
{
    public static void Main()
    {
         WriteLine("Hello World!"); //Writeline is method belonging to static class Console
    }

}

```

### **Drawbacks**

While static classes can be incredibly useful, they do come with their own caveats:

<li>
Once the static class has been called, the class is loaded into memory and cannot be run through the garbage collector until the AppDomain housing the static class is unloaded.
</li>
<li>
A static class cannot implement an interface.
</li>



## int


`int` is an alias for `System.Int32`, which is a data type for signed 32-bit integers. This data type can be found in `mscorlib.dll` which is implicitly referenced by every C# project when you create them.

Range: -2,147,483,648 to 2,147,483,647

```cs
int int1 = -10007;
var int2 = 2132012521;     

```



## ulong


Keyword used for unsigned 64-bit integers. It represents `System.UInt64` data type found in `mscorlib.dll` which is implicitly referenced by every C# project when you create them.

Range: 0 to 18,446,744,073,709,551,615

```cs
ulong veryLargeInt = 18446744073609451315;
var anotherVeryLargeInt = 15446744063609451315UL;

```



## lock


`lock` provides thread-safety for a block of code, so that it can be accessed by only one thread within the same process. Example:

```cs
private static object _lockObj = new object();
static void Main(string[] args)
{
    Task.Run(() => TaskWork());
    Task.Run(() => TaskWork());
    Task.Run(() => TaskWork());

    Console.ReadKey();
}

private static void TaskWork()
{
    lock(_lockObj)
    {
        Console.WriteLine("Entered");

        Task.Delay(3000);
        Console.WriteLine("Done Delaying");

        // Access shared resources safely

        Console.WriteLine("Leaving");
    }   
}

Output:

Entered
Done Delaying
Leaving
Entered
Done Delaying
Leaving
Entered
Done Delaying
Leaving

```

**Use cases:**

Whenever you have a block of code that might produce side-effects if executed by multiple threads at the same time. The lock keyword along with a **shared synchronization object**  (`_objLock` in the example) can be used to prevent that.

Note that `_objLock`  can't be `null` and multiple threads executing the code must use the same object instance (either by making it a `static` field, or by using the same class instance for both threads)

From the compiler side, the lock keyword is a syntactic sugar that is replaced by `Monitor.Enter(_lockObj);` and `Monitor.Exit(_lockObj);`. So if you replace the lock by surrounding the block of code with these two methods, you would get the same results. You can see actual code in [Syntactic sugar in C# - lock example](http://stackoverflow.com/documentation/c%23/2994/syntactic-sugar-in-c-sharp/10166/lock#t=20160723121800624366)



## internal


The [`internal`](https://msdn.microsoft.com/en-us/library/7c5ka91b.aspx) keyword is an access modifier for types and type members. Internal types or members are **accessible only within files in the same assembly**

**usage:**

```cs
public class BaseClass 
{
    // Only accessible within the same assembly
    internal static int x = 0;
}

```

The difference between different access modifiers is clarified [here](http://stackoverflow.com/a/614844/266562)

****Access modifiers****

> 
**public**


> 
<blockquote>
The type or member can be accessed by any other code in the same assembly or another assembly that references it.


> 
**private**


> 
<blockquote>
The type or member can only be accessed by code in the same class or struct.


> 
**protected**


> 
<blockquote>
The type or member can only be accessed by code in the same class or struct, or in a derived class.


> 
**internal**


> 
<blockquote>
The type or member can be accessed by any code in the same assembly, but not from another assembly.


> 
**protected internal**


> 
<blockquote>
The type or member can be accessed by any code in the same assembly, or by any derived class in another assembly.


When **no access modifier** is set, a default access modifier is used. So there is always some form of access modifier even if it's not set.



## where


`where` can serve two purposes in C#: type constraining in a generic argument, and filtering LINQ queries.

In a generic class, let's consider

```cs
public class Cup<T>
{
    // ...
}

```

T is called a type parameter. The class definition can impose constraints on the actual types that can be supplied for T.

The following kinds of constraints can be applied:

- value type
- reference type
- default constructor
- inheritance and implementation

**value type**

In this case only `struct`s (this includes 'primitive' data types such as `int`, `boolean` etc) can be supplied

```cs
public class Cup<T> where T : struct
{
    // ...
}

```

**reference type**

In this case only class types can be supplied

```cs
public class Cup<T> where T : class
{
    // ...
}

```

**hybrid value/reference type**

Occasionally it is desired to restrict type arguments to those available in a database, and these will usually map to value types and strings. As all type restrictions must be met, it is not possible to specify `where T : struct or string` (this is not valid syntax). A workaround is to restrict type arguments to [`IConvertible`](https://msdn.microsoft.com/en-us/library/system.iconvertible(v=vs.110).aspx) which has built in types of "... Boolean, SByte, Byte, Int16, UInt16, Int32, UInt32, Int64, UInt64, Single, Double, Decimal, DateTime, Char, and String." It is possible other objects will implement IConvertible, though this is rare in practice.

```cs
public class Cup<T> where T : IConvertible
{
    // ...
}

```

**default constructor**

Only types that contain a default constructor will be allowed. This includes value types and classes that contain a default (parameterless) constructor

```cs
public class Cup<T> where T : new
{
    // ...
}

```

**inheritance and implementation**

Only types that inherit from a certain base class or implement a certain interface can be supplied.

```cs
public class Cup<T> where T : Beverage
{
    // ...
}


public class Cup<T> where T : IBeer
{
    // ...
}

```

The constraint can even reference another type parameter:

```cs
public class Cup<T, U> where U : T
{
    // ...
}

```

Multiple constraints can be specified for a type argument:

```cs
public class Cup<T> where T : class, new()
{
    // ...
}

```

### The previous examples show generic constraints on a class definition, but constraints can be used anywhere a type argument is supplied: classes, structs, interfaces, methods, etc.

`where` can also be a LINQ clause. In this case it is analogous to `WHERE` in SQL:

```cs
int[] nums = { 5, 2, 1, 3, 9, 8, 6, 7, 2, 0 };

var query =
    from num in nums 
    where num < 5
    select num;

    foreach (var n in query)
    {
        Console.Write(n + " ");
    }
    // prints 2 1 3 2 0

```



## extern


The `extern` keyword is used to declare methods that are implemented externally. This can be used in conjunction with the DllImport attribute to call into unmanaged code using Interop services. which in this case it will come with `static` modifier

For Example:

```cs
using System.Runtime.InteropServices;
public class MyClass
{
    [DllImport("User32.dll")]
    private static extern int SetForegroundWindow(IntPtr point);

    public void ActivateProcessWindow(Process p)
    {
        SetForegroundWindow(p.MainWindowHandle);
    }
}

```

This uses the SetForegroundWindow method imported from the User32.dll library

This can also be used to define an external assembly alias. which let us to reference different versions of same components from single assembly.

To reference two assemblies with the same fully-qualified type names, an alias must be specified at a command prompt, as follows:

```cs
/r:GridV1=grid.dll
/r:GridV2=grid20.dll

```

This creates the external aliases GridV1 and GridV2. To use these aliases from within a program, reference them by using the extern keyword. For example:

```cs
extern alias GridV1;
extern alias GridV2;

```



## when


The `when` is a keyword added in **C# 6**, and it is used for exception filtering.

Before the introduction of the `when` keyword, you could have had one catch clause for each type of exception; with the addition of the keyword, a more fine-grained control is now possible.

A `when` expression is attached to a `catch` branch, and only if the `when` condition is `true`, the `catch` clause will be executed. It is possible to have several `catch` clauses with the same exception class types, and different `when` conditions.

```cs
private void CatchException(Action action)
{
    try
    {
        action.Invoke();
    }
    
    // exception filter
    catch (Exception ex) when (ex.Message.Contains("when"))
    {
        Console.WriteLine("Caught an exception with when");
    }

    catch (Exception ex)
    {
        Console.WriteLine("Caught an exception without when");
    }
}

private void Method1() { throw new Exception("message for exception with when"); }
private void Method2() { throw new Exception("message for general exception"); }


CatchException(Method1);
CatchException(Method2);

```



## if, if...else, if... else if


The `if` statement is used to control the flow of the program. An `if` statement identifies which statement to run based on the value of a `Boolean` expression.

For a single statement, the `braces`{} are optional but recommended.

```cs
int a = 4;
if(a % 2 == 0) 
{
     Console.WriteLine("a contains an even number");
}
// output: "a contains an even number"

```

The `if` can also have an `else` clause, that will be executed in case the condition evaluates to false:

```cs
int a = 5;
if(a % 2 == 0) 
{
     Console.WriteLine("a contains an even number");
}
else
{
     Console.WriteLine("a contains an odd number");
}
// output: "a contains an odd number"

```

The `if`...`else if` construct lets you specify multiple conditions:

```cs
int a = 9;
if(a % 2 == 0) 
{
     Console.WriteLine("a contains an even number");
}
else if(a % 3 == 0) 
{
     Console.WriteLine("a contains an odd number that is a multiple of 3"); 
}
else
{
     Console.WriteLine("a contains an odd number");
}
// output: "a contains an odd number that is a multiple of 3"

```

### **Important to note** that if a condition is met in the above example , the control skips other tests and jumps to the end of that particular if else construct.So, the **order** of tests is important if you are using if .. else if construct

C# Boolean expressions use [short-circuit evaluation](https://en.wikipedia.org/wiki/Short-circuit_evaluation). This is important in cases where evaluating conditions may have side effects:

```cs
if (someBooleanMethodWithSideEffects() && someOtherBooleanMethodWithSideEffects()) {
  //...
}

```

There's no guarantee that `someOtherBooleanMethodWithSideEffects` will actually run.

It's also important in cases where earlier conditions ensure that it's "safe" to evaluate later ones. For example:

```cs
if (someCollection != null && someCollection.Count > 0) {
   // ..
}

```

The order is very important in this case because, if we reverse the order:

```cs
if (someCollection.Count > 0 && someCollection != null) {

```

it will throw a `NullReferenceException` if `someCollection` is `null`.



## struct


A `struct` type is a value type that is typically used to encapsulate small groups of related variables, such as the coordinates of a rectangle or the characteristics of an item in an inventory.

> 
[Classes](http://stackoverflow.com/a/3924092/266562) are reference types, structs are value types.


```cs
using static System.Console;

namespace ConsoleApplication1
{
    struct Point
    {
        public int X;
        public int Y;

        public override string ToString()
        {
            return $"X = {X}, Y = {Y}";
        }

        public void Display(string name)
        {
            WriteLine(name + ": " + ToString());
        }
    }

    class Program
    {
        static void Main()
        {
            var point1 = new Point {X = 10, Y = 20};
            // it's not a reference but value type
            var point2 = point1;
            point2.X = 777;
            point2.Y = 888;
            point1.Display(nameof(point1)); // point1: X = 10, Y = 20
            point2.Display(nameof(point2)); // point2: X = 777, Y = 888

            ReadKey();
        }
    }
}

```

Structs can also contain constructors, constants, fields, methods, properties, indexers, operators, events, and nested types, although if several such members are required, you should consider making your type a class instead.

**Some [suggestions](https://msdn.microsoft.com/en-us/library/ms229017.aspx) from MS on when to use struct and when to use class:**

**CONSIDER**

defining a struct instead of a class if instances of the type are small and commonly short-lived or are commonly embedded in other objects.

**AVOID**

defining a struct unless the type has all of the following characteristics:

<li>It logically represents a single value, similar to primitive types
(int, double, etc.)</li>
- It has an instance size under 16 bytes.
- It is immutable.
- It will not have to be boxed frequently.



## switch


The `switch` statement is a control statement that selects a switch section to execute from a list of candidates. A switch statement includes one or more switch sections. Each switch section contains one or more `case` labels followed by one or more statements. If no case label contains a matching value, control is transferred to the `default` section, if there is one. Case fall-through is not supported in C#, strictly speaking. However, if 1 or more `case` labels are empty, execution will follow the code of the next `case` block which contains code. This allows grouping of multiple `case` labels with the same implementation. In the following example, if `month` equals 12, the code in `case 2` will be executed since the `case` labels `12` `1` and `2` are grouped. If a `case` block is not empty, a `break` must be present before the next `case` label, otherwise the compiler will flag an error.

```cs
int month = DateTime.Now.Month; // this is expected to be 1-12 for Jan-Dec

switch (month)
{
    case 12: 
    case 1: 
    case 2:
        Console.WriteLine("Winter");
        break;
    case 3: 
    case 4: 
    case 5:
        Console.WriteLine("Spring");
        break;
    case 6: 
    case 7: 
    case 8:
        Console.WriteLine("Summer");
        break;
    case 9:     
    case 10: 
    case 11:
        Console.WriteLine("Autumn");
        break;
    default:
        Console.WriteLine("Incorrect month index");
        break;
}

```

A `case` can only be labeled by a value known at **compile time** (e.g. `1`, `"str"`, `Enum.A`), so a `variable` isn't a valid `case` label, but a `const` or an `Enum` value is (as well as any literal value).



## unsafe


The `unsafe` keyword can be used in type or method declarations or to declare an inline block.

The purpose of this keyword is to enable the use of the **unsafe subset** of C# for the block in question. The unsafe subset includes features like pointers, stack allocation, C-like arrays, and so on.

Unsafe code is not verifiable and that's why its usage is discouraged. Compilation of unsafe code requires passing a switch to the C# compiler. Additionally, the CLR requires that the running assembly has full trust.

Despite these limitations, unsafe code has valid usages in making some operations more performant (e.g. array indexing) or easier (e.g. interop with some unmanaged libraries).

As a very simple example

```cs
// compile with /unsafe
class UnsafeTest
{
   unsafe static void SquarePtrParam(int* p)
   {
      *p *= *p; // the '*' dereferences the pointer.
      //Since we passed in "the address of i", this becomes "i *= i"
   }

   unsafe static void Main()
   {
      int i = 5;
      // Unsafe method: uses address-of operator (&):
      SquarePtrParam(&i); // "&i" means "the address of i". The behavior is similar to "ref i"
      Console.WriteLine(i); // Output: 25
   }
}

```

While working with pointers, we can change the values of memory locations directly, rather than having to address them by name.  Note that this often requires the use of the [fixed](http://stackoverflow.com/documentation/c%23/26/keywords/59/fixed#t=20160802171014149858) keyword to prevent possible memory corruption as the garbage collector moves things around (otherwise, you may get [error CS0212](https://msdn.microsoft.com/en-us/library/29ak9b70(v=vs.140).aspx)).  Since a variable that has been "fixed" cannot be written to, we also often have to have a second pointer that starts out pointing to the same location as the first.

```cs
void Main()
{
    int[] intArray = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    UnsafeSquareArray(intArray);
    foreach(int i in intArray)
        Console.WriteLine(i);
}

unsafe static void UnsafeSquareArray(int[] pArr)
{
    int len = pArr.Length;

    //in C or C++, we could say
    // int* a = &(pArr[0])
    // however, C# requires you to "fix" the variable first 
    fixed(int* fixedPointer = &(pArr[0]))
    {
        //Declare a new int pointer because "fixedPointer" cannot be written to.
        // "p" points to the same address space, but we can modify it
        int* p = fixedPointer;

        for (int i = 0; i < len; i++)
        {
            *p *= *p; //square the value, just like we did in SquarePtrParam, above
            p++;      //move the pointer to the next memory space.
                      // NOTE that the pointer will move 4 bytes since "p" is an
                      // int pointer and an int takes 4 bytes

            //the above 2 lines could be written as one, like this:
            // "*p *= *p++;"
        }
    }
}

```

Output:

```cs
1
4
9
16
25
36
49
64
81
100

```

`unsafe` also allows the use of [stackalloc](http://stackoverflow.com/documentation/c%23/26/keywords/57/stackalloc#t=20160802171014149858) which will allocate memory on the stack  like _alloca in the C run-time library.  We can modify the above example to use `stackalloc` as follows:

```cs
unsafe void Main()
{
    const int len=10;
    int* seedArray = stackalloc int[len];
    
    //We can no longer use the initializer "{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}" as before.
    // We have at least 2 options to populate the array. The end result of either
    // option will be the same (doing both will also be the same here).

    //FIRST OPTION:
    int* p = seedArray; // we don't want to lose where the array starts, so we
                        // create a shadow copy of the pointer
    for(int i=1; i<=len; i++)
        *p++ = i;
    //end of first option

    //SECOND OPTION:
    for(int i=0; i<len; i++)
        seedArray[i] = i+1;
    //end of second option

    UnsafeSquareArray(seedArray, len);
    for(int i=0; i< len; i++)
        Console.WriteLine(seedArray[i]);
}

//Now that we are dealing directly in pointers, we don't need to mess around with
// "fixed", which dramatically simplifies the code
unsafe static void UnsafeSquareArray(int* p, int len)
{
    for (int i = 0; i < len; i++)
        *p *= *p++;
}

```

(Output is the same as above)



## true, false


The `true` and `false` keywords have two uses:

1. As literal Boolean values

```cs
var myTrueBool = true;
var myFalseBool = false;

```


1. As operators that can be overloaded

```cs
public static bool operator true(MyClass x)
{
    return x.value >= 0;
}

public static bool operator false(MyClass x)
{
    return x.value < 0;
}

```

Overloading the false operator was useful prior to C# 2.0, before the introduction of `Nullable` types.<br />
A type that overloads the `true` operator, must also overload the `false` operator.



## var


An implicitly-typed local variable that is strongly typed just as if the user had declared the type. Unlike other variable declarations, the compiler determines the type of variable that this represents based on the value that is assigned to it.

```cs
var i = 10; // implicitly typed, the compiler must determine what type of variable this is
int i = 10; // explicitly typed, the type of variable is explicitly stated to the compiler

// Note that these both represent the same type of variable (int) with the same value (10).

```

Unlike other types of variables, variable definitions with this keyword need to be initialized when declared. This is due to the **var** keyword representing an implicitly-typed variable.

```cs
var i;
i = 10;

// This code will not run as it is not initialized upon declaration.

```

The **var** keyword can also be used to create new datatypes on the fly. These new datatypes are known as **anonymous types**. They are quite useful, as they allow a user to define a set of properties without having to explicitly declare any kind of object type first.

**Plain anonymous type**

```cs
var a = new { number = 1, text = "hi" };

```

**LINQ query that returns an anonymous type**

```cs
public class Dog
{
    public string Name { get; set; }
    public int Age { get; set; }
}

public class DogWithBreed
{
    public Dog Dog { get; set; }
    public string BreedName  { get; set; }
}

public void GetDogsWithBreedNames()
{
    var db = new DogDataContext(ConnectString);
    var result = from d in db.Dogs
             join b in db.Breeds on d.BreedId equals b.BreedId
             select new 
                    {
                        DogName = d.Name,
                        BreedName = b.BreedName
                    };

    DoStuff(result);
}

```

You can use var keyword in foreach statement

```cs
public bool hasItemInList(List<String> list, string stringToSearch)
{
    foreach(var item in list)
    {
        if( ( (string)item ).equals(stringToSearch) )
            return true;
    }

    return false;
}

```



## enum


The `enum` keyword tells the compiler that this class inherits from the abstract class `Enum`, without the programmer having to explicitly inherit it. `Enum` is a descendant of `ValueType`, which is intended for use with distinct set of named constants.

```cs
public enum DaysOfWeek
{
    Monday,
    Tuesday,
}

```

You can optionally specify a specific value for each one (or some of them):

```cs
public enum NotableYear
{
   EndOfWwI = 1918;
   EnfOfWwII = 1945,
}

```

In this example I omitted a value for 0, this is usually a bad practice. An `enum` will always have a default value produced by explicit conversion `(YourEnumType) 0`, where `YourEnumType` is your declared `enume` type. Without a value of 0 defined, an `enum` will not have a defined value at initiation.

The default underlying type of `enum` is `int`, you can change the underlying type to any integral type including `byte`, `sbyte`, `short`, `ushort`, `int`, `uint`, `long` and `ulong`. Below is an enum with underlying type `byte`:

```cs
enum Days : byte
{
    Sunday = 0,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday
};

```

Also note that you can convert to/from underlying type simply with a cast:

```cs
int value = (int)NotableYear.EndOfWwI;

```

For these reasons you'd better always check if an `enum` is valid when you're exposing library functions:

```cs
void PrintNotes(NotableYear year)
{
    if (!Enum.IsDefined(typeof(NotableYear), year))
        throw InvalidEnumArgumentException("year", (int)year, typeof(NotableYear));

    // ...
}

```



## in


The `in` keyword has three uses:

a) As part of the syntax in a `foreach` statement  or as part of the syntax in a LINQ query

```cs
foreach (var member in sequence)
{
    // ...
}

```

b) In the context of generic interfaces and generic delegate types signifies **contravariance** for the type parameter in question:

```cs
public interface IComparer<in T>
{
    // ...
}

```

c) In the context of LINQ query refers to the collection that is being queried

```cs
var query = from x in source select new { x.Name, x.ID, };

```



## sizeof


Used to obtain the size in bytes for an unmanaged type

```cs
int byteSize = sizeof(byte) // 1
int sbyteSize = sizeof(sbyte) // 1
int shortSize = sizeof(short) // 2
int ushortSize = sizeof(ushort) // 2
int intSize = sizeof(int) // 4
int uintSize = sizeof(uint) // 4
int longSize = sizeof(long) // 8
int ulongSize = sizeof(ulong) // 8
int charSize = sizeof(char) // 2(Unicode)
int floatSize = sizeof(float) // 4
int doubleSize = sizeof(double) // 8
int decimalSize = sizeof(decimal) // 16
int boolSize = sizeof(bool) // 1

```



## long


The **long** keyword is used to represent signed 64-bit integers. It is an alias for the `System.Int64` datatype present in `mscorlib.dll`, which is implicitly referenced by every C# project when you create them.

**Any **long** variable can be declared both explicitly and implicitly:**

```cs
long long1 = 9223372036854775806;  // explicit declaration, long keyword used
var long2 = -9223372036854775806L; // implicit declaration, 'L' suffix used

```

A **long** variable can hold any value from –9,223,372,036,854,775,808 to 9,223,372,036,854,775,807, and can be useful in situations which a variable must hold a value that exceeds the bounds of what other variables (such as the [**int**](http://stackoverflow.com/documentation/c%23/26/keywords#t=201608201800043158849) variable) can hold.



## bool


Keyword for storing the Boolean values `true` and `false`. bool is an alias of System.Boolean.

The default value of a bool is false.

```cs
bool b; // default value is false
b = true; // true
b = ((5 + 2) == 6); // false

```

For a bool to allow null values it must be initialized as a bool?.

The default value of a bool? is null.

```cs
bool? a // default value is null

```



## unchecked


The `unchecked` keyword prevents the compiler from checking for overflows/underflows.

For example:

```cs
const int ConstantMax = int.MaxValue;
unchecked
{
    int1 = 2147483647 + 10;
}
int1 = unchecked(ConstantMax + 10);

```

Without the `unchecked` keyword, neither of the two addition operations will compile.

### When is this useful?

This is useful as it may help speed up calculations that definitely will not overflow since checking for overflow takes time, or when an overflow/underflow is desired behavior (for instance, when generating a hash code).



## do


The do operator iterates over a block of code until a conditional query equals false. The do-while loop can also be interrupted by a [`goto`](http://stackoverflow.com/documentation/c%23/26/keywords/193/goto), [`return`](http://stackoverflow.com/documentation/c%23/26/keywords/4600/return), [`break`](http://stackoverflow.com/documentation/c%23/26/keywords/2858/break) or `throw` statement.

The syntax for the `do` keyword is:

> 
<p>do
{ **code block;**  }
while( **condition** );</p>


Example:

```cs
int i = 0;

do
{
    Console.WriteLine("Do is on loop number {0}.", i);
} while (i++ < 5);

```

Output:

> 
<p>"Do is on loop number 1." <br />
"Do is on loop number 2."<br />
"Do is on loop number 3."<br />
"Do is on loop number 4."<br />
"Do is on loop number 5."</p>


Unlike the [`while`](http://stackoverflow.com/documentation/c%23/26/keywords/4396/while) loop, the do-while loop is **Exit Controlled**. This means that the do-while loop would execute its statements at least once, even if the condition fails the first time.

```cs
bool a = false;

do
{
    Console.WriteLine("This will be printed once, even if a is false.");
} while (a == true);

```



## interface


An [`interface`](http://stackoverflow.com/questions/tagged/interface+c%23) contains the [signatures](http://stackoverflow.com/questions/tagged/signature+c%23) of methods, properties and events. The derived classes defines the members as the interface contains only the declaration of the members.

An interface is declared using the `interface` keyword.

```cs
interface IProduct
{
    decimal Price { get; }
}

class Product : IProduct
{
    const decimal vat = 0.2M;
    
    public Product(decimal price)
    {
        _price = price;
    }
    
    private decimal _price;
    public decimal Price { get { return _price * (1 + vat); } }
}

```



## implicit


The `implicit` keyword is used to overload a conversion operator. For example, you may declare a `Fraction` class that should automatically be converted to a `double` when needed, and that can be automatically converted from `int`:

```cs
class Fraction(int numerator, int denominator)
{
    public int Numerator { get; } = numerator;
    public int Denominator { get; } = denominator;
    // ...
    public static implicit operator double(Fraction f)
    {
        return f.Numerator / (double) f.Denominator;
    }
    public static implicit operator Fraction(int i)
    {
        return new Fraction(i, 1);
    }
}

```



## ushort


A numeric type used to store 16-bit positive integers.  `ushort` is an alias for `System.UInt16`, and takes up 2 bytes of memory.

Valid range is `0` to `65535`.

```cs
ushort a = 50; // 50
ushort b = 65536; // Error, cannot be converted
ushort c = unchecked((ushort)65536); // Overflows (wraps around to 0)

```



## delegate


Delegates are types that represent a reference to a method. They are used for passing methods as arguments to other methods.

Delegates can hold static methods, instance methods, anonymous methods, or lambda expressions.

```cs
class DelegateExample
{
    public void Run()
    {
        //using class method
        InvokeDelegate( WriteToConsole ); 
        
        //using anonymous method
        DelegateInvoker di = delegate ( string input ) 
        { 
            Console.WriteLine( string.Format( "di: {0} ", input ) );
            return true; 
        };
        InvokeDelegate( di ); 
        
        //using lambda expression
        InvokeDelegate( input => false ); 
    }

    public delegate bool DelegateInvoker( string input );

    public void InvokeDelegate(DelegateInvoker func)
    {
        var ret = func( "hello world" );
        Console.WriteLine( string.Format( " > delegate returned {0}", ret ) );
    }

    public bool WriteToConsole( string input )
    {
        Console.WriteLine( string.Format( "WriteToConsole: '{0}'", input ) );
        return true;
    }
}

```

When assigning a method to a delegate it is important to note that the method must have the same return type as well as parameters. This differs from 'normal' method overloading, where only the parameters define the signature of the method.

Events are built on top of delegates.



## partial


The keyword `partial` can be used during type definition of class, struct, or interface to allow the type definition to be split into several files. This is useful to incorporate new features in auto generated code.

**File1.cs**

```cs
namespace A
{
    public partial class Test
    {
        public string Var1 {get;set;}
    }
}

```

**File2.cs**

```cs
namespace A
{
    public partial class Test
    {
        public string Var2 {get;set;}
    }
}

```

**Note:** A class can be split into any number of files. However, all declaration must be under same namespace and the same assembly.

Methods can also be declared partial using the `partial` keyword. In this case one file will contain only the method definition and another file will contain the implementation.

> 
A partial method has its signature defined in one part of a partial type, and its implementation defined in another part of the type. Partial methods enable class designers to provide method hooks, similar to event handlers, that developers may decide to implement or not. If the developer does not supply an implementation, the compiler removes the signature at compile time. The following conditions apply to partial methods:
<ul>
- Signatures in both parts of the partial type must match.
- The method must return void.
- No access modifiers are allowed. Partial methods are implicitly private.
</ul>
-- MSDN


**File1.cs**

```cs
namespace A
{
    public partial class Test
    {
        public string Var1 {get;set;}
        public partial Method1(string str);
    }
}

```

**File2.cs**

```cs
namespace A
{
    public partial class Test
    {
        public string Var2 {get;set;}
        public partial Method1(string str)
        {
            Console.WriteLine(str);
        }
    }
}

```

**Note:** The type containing the partial method must also be declared partial.



## sbyte


A numeric type used to store 8-bit **signed** integers.  `sbyte` is an alias for `System.SByte`, and takes up 1 byte of memory.  For the unsigned equivalent, use `byte`.

Valid range is `-127` to `127` (the leftover is used to store the sign).

```cs
sbyte a = 127; // 127
sbyte b = -127; // -127
sbyte c = 200; // Error, cannot be converted
sbyte d = unchecked((sbyte)129); // -127 (overflows)

```



## event


An `event` allows the developer to implement a notification pattern.

**Simple example**

```cs
public class Server
{
    // defines the event
    public event EventHandler DataChangeEvent;

    void RaiseEvent()
    {
        var ev = DataChangeEvent;
        if(ev != null)
        {
            ev(this, EventArgs.Empty);
        }
    }
}

public class Client
{
    public void Client(Server server)
    {
        // client subscribes to the server's DataChangeEvent
        server.DataChangeEvent += server_DataChanged;
    }

    private void server_DataChanged(object sender, EventArgs args)
    {
        // notified when the server raises the DataChangeEvent
    }
}

```

[MSDN reference](https://msdn.microsoft.com/en-CA/library/awbftdfh.aspx)



#### Remarks


C# has a predefined collection of "keywords" (or reserved words) which each have a special function. These words can not be used as identifiers (names for variables, methods, classes, etc.) unless prefixed with `@`.

- [`abstract`](http://stackoverflow.com/documentation/c%23/26/keywords/2872/abstract)
- [`as`](http://stackoverflow.com/documentation/c%23/26/keywords/138/as)
- [`base`](http://stackoverflow.com/documentation/c%23/26/keywords/1840/base)
- [`bool`](http://stackoverflow.com/documentation/c%23/26/keywords/8712/bool)
- [`break`](http://stackoverflow.com/documentation/c%23/26/keywords/2858/break)
- `byte`
- `case`
- [`catch`](http://stackoverflow.com/documentation/c%23/26/keywords/148/try-catch-finally-throw)
- [`char`](http://stackoverflow.com/documentation/c%23/26/keywords/6009/char)
- [`checked`](http://stackoverflow.com/documentation/c%23/26/keywords/192/checked-unchecked)
- `class`
- [`const`](http://stackoverflow.com/documentation/c%23/26/keywords/141/const)
- [`continue`](http://stackoverflow.com/documentation/c%23/26/keywords/154/continue)
- [`decimal`](http://stackoverflow.com/documentation/c%23/26/keywords/2873/float-double-decimal)
- [`default`](http://stackoverflow.com/documentation/c%23/26/keywords/109/default)
- [`delegate`](http://stackoverflow.com/documentation/c%23/26/keywords/18720/delegate)
- [`do`](http://stackoverflow.com/documentation/c%23/26/keywords/12229/do)
- [`double`](http://stackoverflow.com/documentation/c%23/26/keywords/2873/float-double-decimal)
- [`else`](http://stackoverflow.com/documentation/c%23/26/keywords/11359/if-if-else-if-else-if)
- [`enum`](http://stackoverflow.com/documentation/c%23/26/keywords/245/enum)
- [`event`](http://stackoverflow.com/documentation/c%23/26/keywords/18722/event)
- `explicit`
- [`extern`](http://stackoverflow.com/documentation/c%23/26/keywords/8191/extern)
- [`false`](http://stackoverflow.com/documentation/c%23/26/keywords/17113/true-false)
- [`finally`](http://stackoverflow.com/documentation/c%23/26/keywords/148/try-catch-finally-throw)
- [`fixed`](http://stackoverflow.com/documentation/c%23/26/keywords/59/fixed)
- [`float`](http://stackoverflow.com/documentation/c%23/26/keywords/2873/float-double-decimal)
- [`for`](http://stackoverflow.com/documentation/c%23/26/keywords/3722/for)
- [`foreach`](http://stackoverflow.com/documentation/c%23/26/keywords/1928/foreach)
- [`goto`](http://stackoverflow.com/documentation/c%23/26/keywords/193/goto)
- [`if`](http://stackoverflow.com/documentation/c%23/26/keywords/11359/if-if-else-if-else-if)
- [`implicit`](http://stackoverflow.com/documentation/c%23/26/keywords/16557/implicit)
- [`in`](http://stackoverflow.com/documentation/c%23/26/keywords/4992/in)
- [`int`](http://stackoverflow.com/documentation/c%23/26/keywords/5328/int)
- [`interface`](http://stackoverflow.com/documentation/c%23/26/keywords/14354/interface)
- [`internal`](http://stackoverflow.com/documentation/c%23/26/keywords/8102/internal#t=201607221603473329189)
- [`is`](http://stackoverflow.com/documentation/c%23/26/keywords/139/is)
- [`lock`](http://stackoverflow.com/documentation/c%23/26/keywords/6452/lock#t=201607261640175640513)
- [`long`](http://stackoverflow.com/documentation/c%23/26/keywords/5329/long)
- [`namespace`](http://stackoverflow.com/documentation/c%23/26/keywords/142/namespace)
- [`new`](http://stackoverflow.com/documentation/c%23/26/keywords/5805/virtual-override-new)
- [`null`](http://stackoverflow.com/documentation/c%23/26/keywords/6750/null)
- `object`
- [`operator`](http://stackoverflow.com/documentation/c%23/26/keywords/12604/operator)
- [`out`](http://stackoverflow.com/documentation/c%23/26/keywords/184/ref-out)
- [`override`](http://stackoverflow.com/documentation/c%23/26/keywords/5805/virtual-override-new)
- [`params`](http://stackoverflow.com/documentation/c%23/26/keywords/2513/params)
- `private`
- `protected`
- `public`
- [`readonly`](http://stackoverflow.com/documentation/c%23/26/keywords/110/readonly)
- [`ref`](http://stackoverflow.com/documentation/c%23/26/keywords/184/ref-out)
- [`return`](http://stackoverflow.com/documentation/c%23/26/keywords/4600/return)
- [`sbyte`](http://stackoverflow.com/documentation/c%23/26/keywords/18290/sbyte)
- [`sealed`](http://stackoverflow.com/documentation/c%23/26/keywords/5245/sealed)
- `short`
- [`sizeof`](http://stackoverflow.com/documentation/c%23/26/keywords/5246/sizeof)
- [`stackalloc`](http://stackoverflow.com/documentation/c%23/26/keywords/57/stackalloc)
- [`static`](http://stackoverflow.com/documentation/c%23/26/keywords/5248/static)
- [`string`](http://stackoverflow.com/documentation/c%23/26/keywords/17143/string)
- [`struct`](http://stackoverflow.com/documentation/c%23/26/keywords/13023/struct#t=201607251950535084892)
- [`switch`](http://stackoverflow.com/documentation/c%23/26/keywords/14353/switch)
- [`this`](http://stackoverflow.com/documentation/c%23/26/keywords/2914/this)
- [`throw`](http://stackoverflow.com/documentation/c%23/26/keywords/148/try-catch-finally-throw)
- [`true`](http://stackoverflow.com/documentation/c%23/26/keywords/17113/true-false)
- [`try`](http://stackoverflow.com/documentation/c%23/26/keywords/148/try-catch-finally-throw)
- [`typeof`](http://stackoverflow.com/documentation/c%23/26/keywords/140/typeof)
- [`uint`](http://stackoverflow.com/documentation/c%23/26/keywords/2874/uint)
- [`ulong`](http://stackoverflow.com/documentation/c%23/26/keywords/5330/ulong)
- [`unchecked`](http://stackoverflow.com/documentation/c%23/26/keywords/192/checked-unchecked)
- [`unsafe`](https://stackoverflow.com/documentation/c%23/26/keywords/15630/unsafe)
- [`ushort`](http://stackoverflow.com/documentation/c%23/26/keywords/18289/ushort)
- [`using` (directive)](http://stackoverflow.com/documentation/c%23/52/using-directive#t=201605012059492751326)
- [`using` (statement)](http://stackoverflow.com/documentation/c%23/38/using-statement#t=20160501205709291959)
- [`virtual`](http://stackoverflow.com/documentation/c%23/26/keywords/5805/virtual-override-new)
- [`void`](http://stackoverflow.com/documentation/c%23/26/keywords/2980/void)
- [`volatile`](http://stackoverflow.com/documentation/c%23/26/keywords/58/volatile)
- [`when`](http://stackoverflow.com/documentation/c%23/26/keywords/9258/when)
- [`while`](http://stackoverflow.com/documentation/c%23/26/keywords/4396/while)

Apart from these, C# also uses some keywords to provide specific meaning in code. They are called contextual keywords. Contextual keywords can be used as identifiers and doesn't need to be prefixed with `@` when used as identifiers.

- `add`
- `alias`
- `ascending`
- [`async`](http://stackoverflow.com/documentation/c%23/26/keywords/5993/async-await)
- [`await`](http://stackoverflow.com/documentation/c%23/26/keywords/5993/async-await)
- `descending`
- `dynamic`
- `from`
- `get`
- `global`
- `group`
- `into`
- `join`
- `let`
- [`nameof`](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/43/operator-nameof)
- `orderby`
- [`partial`](http://stackoverflow.com/documentation/c%23/26/keywords/19199/partial#t=201608110434471554387)
- `remove`
- `select`
- `set`
- `value`
- [`var`](http://stackoverflow.com/documentation/c%23/26/keywords/4503/var)
- [`where`](http://stackoverflow.com/documentation/c%23/26/keywords/8137/where-type-constraints)
- [`yield`](http://stackoverflow.com/documentation/c%23/61/yield-keyword-in-c#t=201605012045372009603)

