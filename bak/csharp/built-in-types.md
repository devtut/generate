---
metaTitle: "C# | Built-in Types"
description: "Conversion of boxed value types, Immutable reference type - string, Value type - char, Value type - short, int, long (signed 16 bit, 32 bit, 64 bit integers), Value type - ushort, uint, ulong (unsigned 16 bit, 32 bit, 64 bit integers), Value type - bool, Comparisons with boxed value types"
---

# Built-in Types



## Conversion of boxed value types


[Boxed](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) value types can only be unboxed into their original `Type`, even if a conversion of the two `Type`s is valid, e.g.:

```cs
object boxedInt = (int)1; // int boxed in an object

long unboxedInt1 = (long)boxedInt; // invalid cast

```

This can be avoided by first unboxing into the original `Type`, e.g.:

```cs
long unboxedInt2 = (long)(int)boxedInt; // valid

```



## Immutable reference type - string


```cs
// assign string from a string literal
string s = "hello";

// assign string from an array of characters
char[] chars = new char[] { 'h', 'e', 'l', 'l', 'o' };
string s = new string(chars, 0, chars.Length);

// assign string from a char pointer, derived from a string
string s;
unsafe
{
    fixed (char* charPointer = "hello")
    {
        s = new string(charPointer);
    }
}

```



## Value type - char


```cs
// single character s
char c = 's';

// character s: casted from integer value
char c = (char)115;

// unicode character: single character s
char c = '\u0073';

// unicode character: smiley face
char c = '\u263a';

```



## Value type - short, int, long (signed 16 bit, 32 bit, 64 bit integers)


```cs
// assigning a signed short to its minimum value
short s = -32768;

// assigning a signed short to its maximum value
short s = 32767;

// assigning a signed int to its minimum value
int i = -2147483648;

// assigning a signed int to its maximum value
int i = 2147483647;

// assigning a signed long to its minimum value (note the long postfix)
long l = -9223372036854775808L;

// assigning a signed long to its maximum value (note the long postfix)
long l = 9223372036854775807L;

```

It is also possible to make these types nullable, meaning that additionally to the usual values, null can be assigned, too. If a variable of a nullable type is not initialized, it will be null instead of 0. Nullable types are marked by adding a question mark (?) after the type.

```cs
int a; //This is now 0.
int? b; //This is now null.

```



## Value type - ushort, uint, ulong (unsigned 16 bit, 32 bit, 64 bit integers)


```cs
// assigning an unsigned short to its minimum value
ushort s = 0;

// assigning an unsigned short to its maximum value
ushort s = 65535;

// assigning an unsigned int to its minimum value
uint i = 0;

// assigning an unsigned int to its maximum value
uint i = 4294967295;

// assigning an unsigned long to its minimum value (note the unsigned long postfix)
ulong l = 0UL;

// assigning an unsigned long to its maximum value (note the unsigned long postfix)
ulong l = 18446744073709551615UL;

```

It is also possible to make these types nullable, meaning that additionally to the usual values, null can be assigned, too. If a variable of a nullable type is not initialized, it will be null instead of 0. Nullable types are marked by adding a question mark (?) after the type.

```cs
uint a; //This is now 0.
uint? b; //This is now null.

```



## Value type - bool


```cs
// default value of boolean is false
bool b;
//default value of nullable boolean is null
bool? z;
b = true;
if(b) {
    Console.WriteLine("Boolean has true value");
}

```

The bool keyword is an alias of System.Boolean. It is used to declare variables to store the Boolean values, `true` and `false`.



## Comparisons with boxed value types


If value types are assigned to variables of type `object` they are [**boxed**](https://msdn.microsoft.com/en-GB/library/yz2be5wk.aspx) - the value is stored in an instance of a `System.Object`. This can lead to unintended consequences when comparing values with `==`, e.g.:

```cs
object left = (int)1;  // int in an object box
object right = (int)1; // int in an object box

var comparison1 = left == right;      // false

```

This can be avoided by using the overloaded `Equals` method, which will give the expected result.

```cs
var comparison2 = left.Equals(right); // true

```

Alternatively, the same could be done by unboxing the `left` and `right` variables so that the `int` values are compared:

```cs
var comparison3 = (int)left == (int)right; // true

```

