---
metaTitle: ".NET Framework - Custom Types"
description: "Struct Definition, Class Definition, Enum Definition"
---

# Custom Types



## Struct Definition


### Structs inherit from System.ValueType, are value types, and live on the stack. When value types are passed as a parameter, they are passed by value.

```dotnet
Struct MyStruct
{
    public int x;
    public int y;
}

```

**Passed by value** means that the value of the parameter is **copied** for the method, and any changes made to the parameter in the method are not reflected outside of the method. For instance, consider the following code, which calls a method named `AddNumbers`, passing in the variables `a` and `b`, which are of type `int`, which is a Value type.

```dotnet
int a = 5;
int b = 6;

AddNumbers(a,b);

public AddNumbers(int x, int y)
{
    int z = x + y; // z becomes 11
    x = x + 5; // now we changed x to be 10
    z = x + y; // now z becomes 16
} 

```

Even though we added 5 to `x` inside the method, the value of `a` remains unchanged, because it's a Value type, and that means `x` was a **copy** of `a`'s value, but not actually `a`.

Remember, Value types live on the stack, and are passed by value.



## Class Definition


### Classes inherit from System.Object, are reference types, and live on the heap. When reference types are passed as a parameter, they are passed by reference.

```dotnet
public Class MyClass
{
    public int a;
    public int b;
}

```

**Passed by reference** means that a **reference** to the parameter is passed to the method, and any changes to the parameter will be reflected outside of the method when it returns, because the reference is **to the exact same object in memory**. Let's use the same example as before, but we'll "wrap" the `int`s in a class first.

```dotnet
MyClass instanceOfMyClass = new MyClass();
instanceOfMyClass.a = 5;
instanceOfMyClass.b = 6;

AddNumbers(instanceOfMyClass);

public AddNumbers(MyClass sample)
{
    int z = sample.a + sample.b; // z becomes 11
    sample.a = sample.a + 5; // now we changed a to be 10
    z = sample.a + sample.b; // now z becomes 16
} 

```

This time, when we changed `sample.a` to `10`, the value of `instanceOfMyClass.a` **also** changes, because it was **passed by reference**. Passed by reference means that a **reference** (also sometimes called a **pointer**) to the object was passed into the method, instead of a copy of the object itself.

Remember, Reference types live on the heap, and are passed by reference.



## Enum Definition


### An enum is a special type of class. The `enum` keyword tells the compiler that this class inherits from the abstract System.Enum class. Enums are used for distinct lists of items.

```dotnet
public enum MyEnum
{
    Monday = 1,
    Tuesday,
    Wednesday,
    //...
}

```

You can think of an enum as a convenient way of mapping constants to some underlying value. The enum defined above declares values for each day of the week, and starts with `1`. `Tuesday` would then automatically become mapped to `2`, `Wednesday` to `3`, etc.

By default, enums use `int` as the underlying type and start at 0, but you can use any of the following **integral types**: `byte, sbyte, short, ushort, int, uint, long, or ulong`, and can specify explicit values for any item. If some items are explicitly specified, but some are not, each item after the last defined one will be incremented by 1.

We would use this example by **casting** some other value to a **MyEnum** like so:

```dotnet
MyEnum instance = (MyEnum)3; // the variable named 'instance' gets a 
                             //value of MyEnum.Wednesday, which maps to 3.

int x = 2;
instance = (MyEnum)x; // now 'instance' has a value of MyEnum.Tuesday

```

Another useful, although more complex, type of enum is called `Flags`. By **decorating** an enum with the `Flags` attribute, you can assign a variable more than one value at a time. Note that when doing this you **must** define values explicitly in base 2 representation.

```dotnet
[Flags]
public enum MyEnum
{
    Monday = 1,
    Tuesday = 2,
    Wednesday = 4,
    Thursday = 8,
    Friday = 16,
    Saturday = 32, 
    Sunday = 64
}

```

Now you can compare more than one value at a time, either using **bitwise comparisons** or, if you are using .NET 4.0 or later, the built-in `Enum.HasFlag` method.

```dotnet
MyEnum instance = MyEnum.Monday | MyEnum.Thursday; // instance now has a value of
                                                   // *both* Monday and Thursday,
                                                   // represented by (in binary) 0100. 

if (instance.HasFlag(MyEnum.Wednesday))
{
    // it doesn't, so this block is skipped
}
else if (instance.HasFlag(MyEnum.Thursday))
{
    // it does, so this block is executed
}

```

Since the Enum class is subclassed from `System.ValueType`, it is treated as a value type and passed by value, not by reference. The base object is created on the heap, but when you pass an enum value into a function call, a copy of the value using the underlying value type of the Enum (typically System.Int32) is pushed onto the stack. The compiler tracks the association between this value and the base object that was created on the stack. See [ValueType Class (System) (MSDN)](https://msdn.microsoft.com/en-us/library/system.valuetype(v=vs.110).aspx) for more information.



#### Remarks


Typically a `struct` is used only when performance is very important. Since value types live on the stack, they can be accessed much quicker than classes. However, the stack has much less room than the heap, so structs should be kept small (Microsoft recommends `struct`s take up no more than 16 bytes).

A `class` is the most-used type (of these three) in C#, and is generally what you should go with first.

An `enum` is used whenever you can have a clearly defined, distinct list of items that only need to be defined once (at compile time). Enums are helpful to programmers as a lightweight reference to some value: instead of defining a list of `constant` variables to compare to, you can use an enum, and get Intellisense support to make sure you don't accidentally use a wrong value.

