---
metaTitle: "Overflow"
description: "Integer overflow, Overflow during operation, Ordering matters"
---

# Overflow




## Integer overflow


There is a maximum capacity an integer can store. And when you go over that limit, it will loop back to the negative side. For `int`, it is `2147483647`

```cs
int x = int.MaxValue;                //MaxValue is 2147483647
x = unchecked(x + 1);                //make operation explicitly unchecked so that the example also works when the check for arithmetic overflow/underflow is enabled in the project settings 
Console.WriteLine(x);                //Will print -2147483648
Console.WriteLine(int.MinValue);     //Same as Min value

```

For any integers out of this range use namespace System.Numerics which has datatype
BigInteger. Check below link for more information [https://msdn.microsoft.com/en-us/library/system.numerics.biginteger(v=vs.110).aspx](https://msdn.microsoft.com/en-us/library/system.numerics.biginteger(v=vs.110).aspx)



## Overflow during operation


Overflow also happens during the operation. In the following example, x is an `int`, 1 is an `int` by default. Therefore addition is an `int` addition. And the result will be an `int`. And it will overflow.

```cs
int x = int.MaxValue;               //MaxValue is 2147483647
long y = x + 1;                     //It will be overflown
Console.WriteLine(y);               //Will print -2147483648
Console.WriteLine(int.MinValue);    //Same as Min value

```

You can prevent that by using 1L. Now 1 will be a `long` and addition will be a `long` addition

```cs
int x = int.MaxValue;               //MaxValue is 2147483647
long y = x + 1L;                    //It will be OK
Console.WriteLine(y);               //Will print 2147483648

```



## Ordering matters


There is overflow in the following code

```cs
int x = int.MaxValue;
Console.WriteLine(x + x + 1L);  //prints -1

```

Whereas in the following code there is no overflow

```cs
int x = int.MaxValue;
Console.WriteLine(x + 1L + x);  //prints 4294967295

```

This is due to the left-to-right ordering of the operations. In the first code fragment `x + x` overflows and after that it becomes a `long`. On the other hand `x + 1L` becomes `long` and after that `x` is added to this value.

