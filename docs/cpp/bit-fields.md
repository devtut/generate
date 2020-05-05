---
metaTitle: "C++ | Bit fields"
description: "Declaration and Usage"
---

# Bit fields


Bit fields tightly pack C and C++ structures to reduce size.  This appears painless: specify the number of bits for members, and compiler does the work of co-mingling bits.  The restriction is inability to take the address of a bit field member, since it is stored co-mingled.  `sizeof()` is also disallowed.

The cost of bit fields is slower access, as memory must be retrieved and bitwise operations applied to extract or modify member values.  These operations also add to executable size.



## Declaration and Usage


```cpp
struct FileAttributes
{
    unsigned int ReadOnly: 1;    
    unsigned int Hidden: 1;
};

```

Here, each of these two fields will occupy 1 bit in memory. It is specified by **`: 1`** expression after the variable names. Base type of bit field could be any integral type (8-bit int to 64-bit int). Using `unsigned` type is recommended, otherwise surprises may come.

If more bits are required, replace "1" with number of bits required. For example:

```cpp
struct Date
{
    unsigned int Year : 13; // 2^13 = 8192, enough for "year" representation for long time
    unsigned int Month: 4;  // 2^4 = 16, enough to represent 1-12 month values.
    unsigned int Day:   5;  // 32
};

```

The whole structure is using just 22 bits, and with normal compiler settings, `sizeof` this structure would be 4 bytes.

Usage is pretty simple. Just declare the variable, and use it like ordinary structure.

```cpp
Date d;

d.Year = 2016;
d.Month = 7;
d.Day =  22;

std::cout << "Year:" << d.Year << std::endl <<
        "Month:" << d.Month << std::endl <<
        "Day:" << d.Day << std::endl;

```



#### Remarks


How expensive are the bitwise operations?  Suppose a simple non-bit field structure:

```cpp
struct foo {
    unsigned x;
    unsigned y;
}
static struct foo my_var;

```

In some later code:

```cpp
my_var.y = 5;

```

If `sizeof (unsigned) == 4`, then x is stored at the start of the structure, and y is stored 4 bytes in.  Assembly code generated may resemble:

```cpp
loada register1,#myvar     ; get the address of the structure
storei register1[4],#0x05  ; put the value '5' at offset 4, e.g., set y=5

```

This is straightforward because x is not co-mingled with y.  But imagine redefining the structure with bit fields:

```cpp
struct foo {
    unsigned x : 4; /* Range 0-0x0f, or 0 through 15 */
    unsigned y : 4;
}

```

Both `x` and `y` will be allocated 4 bits, sharing a single byte.  The structure thus takes up 1 byte, instead of 8.  Consider the assembly to set `y` now, assuming it ends up in the upper nibble:

```cpp
loada  register1,#myvar        ; get the address of the structure
loadb  register2,register1[0]  ; get the byte from memory
andb   register2,#0x0f         ; zero out y in the byte, leaving x alone
orb    register2,#0x50         ; put the 5 into the 'y' portion of the byte
stb    register1[0],register2  ; put the modified byte back into memory

```

This may be a good trade-off if we have thousands or millions of these structures, and it helps keeps memory in cache or prevents swapping—or could bloat the executable to worsen these problems and slow processing.  As with all things, use good judgement.

**Device driver use:** Avoid bit fields as a clever implementation strategy for device drivers.  Bit field storage layouts are not necessarily consistent between compilers, making such implementations non-portable.  The read-modify-write to set values may not do what devices expect, causing unexpected behaviors.

