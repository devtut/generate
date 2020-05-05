---
metaTitle: "C# | Checked and Unchecked"
description: "Checked and Unchecked, Checked and Unchecked as a scope"
---

# Checked and Unchecked



## Checked and Unchecked


C# statements executes in either checked or unchecked context. In a checked context, arithmetic overflow raises an exception. In an unchecked context, arithmetic overflow is ignored and the result is truncated.

```cs
short m = 32767;   
short n = 32767;
int result1 =  checked((short)(m + n));   //will throw an OverflowException
int result2 =  unchecked((short)(m + n)); // will return -2

```

If neither of these are specified then the default context will rely on other factors, such as compiler options.



## Checked and Unchecked as a scope


The keywords can also create scopes in order to (un)check multiple operations.

```cs
short m = 32767;
short n = 32767;
checked
{
    int result1 = (short)(m + n); //will throw an OverflowException
}
unchecked
{
    int result2 = (short)(m + n); // will return -2
}

```



#### Syntax


- checked(a + b)                   // checked expression
- unchecked(a + b)                 // unchecked expression
- checked { c = a + b; c += 5; }   // checked block
- unchecked { c = a + b; c += 5; } // unchecked block

