---
metaTitle: "C# | Literals"
description: "uint literals, int literals, sbyte literals, decimal literals, double literals, float literals, long literals, ulong literal, string literals, char literals, byte literals, short literal, ushort literal, bool literals"
---

# Literals



## uint literals


`uint` literals are defined by using the suffix `U` or `u`, or by using an integral values within the range of `uint`:

```cs
uint ui = 5U;

```



## int literals


`int` literals are defined by simply using integral values within the range of `int`:

```cs
int i = 5;

```



## sbyte literals


`sbyte` type has no literal suffix. Integer literals are implicitly converted from `int`:

```cs
sbyte sb = 127;

```



## decimal literals


`decimal` literals are defined by using the suffix M or m on a real number:

```cs
decimal m = 30.5M;

```



## double literals


`double` literals are defined by using the suffix D or d, or by using a real number:

```cs
double d = 30.5D;

```



## float literals


`float` literals are defined by using the suffix F or f, or by using a real number:

```cs
float f = 30.5F;

```



## long literals


`long` literals are defined by using the suffix `L` or `l`, or by using an integral values within the range of `long`:

```cs
long l = 5L;

```



## ulong literal


`ulong` literals are defined by using the suffix `UL`, `ul`, `Ul`, `uL`, `LU`, `lu`, `Lu`, or `lU`, or by using an integral values within the range of `ulong`:

```cs
ulong ul = 5UL;

```



## string literals


`string` literals are defined by wrapping the value with double-quotes `"`:

```cs
string s = "hello, this is a string literal";

```

String literals may contain escape sequences. See [String Escape Sequences](http://stackoverflow.com/documentation/c%23/39/string-escape-sequences)

Additionally, C# supports verbatim string literals (See [Verbatim Strings](http://stackoverflow.com/documentation/c%23/16/verbatim-strings)). These are defined by wrapping the value with double-quotes `"`, and prepending it with `@`. Escape sequences are ignored in verbatim string literals, and all whitespace characters are included:

```cs
string s = @"The path is:
C:\Windows\System32";
//The backslashes and newline are included in the string

```



## char literals


`char` literals are defined by wrapping the value with single-quotes `'`:

```cs
char c = 'h';

```

Character literals may contain escape sequences. See [String Escape Sequences](http://stackoverflow.com/documentation/c%23/39/string-escape-sequences)

A character literal must be exactly one character long (after all escape sequences have been evaluated). Empty character literals are not valid. The default character (returned by `default(char)` or `new char()`) is `'\0'`, or the NULL character (not to be confused with the `null` literal and null references).



## byte literals


`byte` type has no literal suffix. Integer literals are implicitly converted from `int`:

```cs
byte b = 127;

```



## short literal


`short` type has no literal. Integer literals are implicitly converted from `int`:

```cs
short s = 127;

```



## ushort literal


`ushort` type has no literal suffix. Integer literals are implicitly converted from `int`:

```cs
ushort us = 127;

```



## bool literals


`bool` literals are either `true` or `false`;

```cs
bool b = true;

```



#### Syntax


- **bool:** true or false
- **byte:** None, integer literal implicitly converted from int
- **sbyte:** None, integer literal implicitly converted from int
- **char:** Wrap the value with single-quotes
- **decimal:** M or m
- **double:** D, d, or a real number
- **float:** F or f
- **int:** None, default for integral values within the range of int
- **uint:** U, u, or integral values within the range of uint
- **long:** L, l, or integral values within the range of long
- **ulong:** UL, ul, Ul, uL, LU, lu, Lu, lU, or integral values within the range of ulong
- **short:** None, integer literal implicitly converted from int
- **ushort:** None, integer literal implicitly converted from int
- **string:** Wrap the value with double-quotes, optionally prepended with `@`
- **null**: The literal `null`

