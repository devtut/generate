---
metaTitle: "Bit Manipulation"
description: "Checking, setting, clearing, and toggling individual bits. Using long as bit mask, java.util.BitSet class, Checking if a number is a power of 2, Packing / unpacking values as bit fragments, Expressing the power of 2, Signed vs unsigned shift"
---

# Bit Manipulation




## Checking, setting, clearing, and toggling individual bits. Using long as bit mask


Assuming we want to modify bit `n` of an integer primitive, `i` (byte, short, char, int, or long):

```java
(i & 1 << n) != 0 // checks bit 'n'
i |= 1 << n;      // sets bit 'n' to 1
i &= ~(1 << n);   // sets bit 'n' to 0
i ^= 1 << n;      // toggles the value of bit 'n'

```

Using long/int/short/byte as a bit mask:

```java
public class BitMaskExample {
    private static final long FIRST_BIT = 1L << 0;
    private static final long SECOND_BIT = 1L << 1;
    private static final long THIRD_BIT = 1L << 2;
    private static final long FOURTH_BIT = 1L << 3;
    private static final long FIFTH_BIT = 1L << 4;
    private static final long BIT_55 = 1L << 54;

    public static void main(String[] args) {
        checkBitMask(FIRST_BIT | THIRD_BIT | FIFTH_BIT | BIT_55);
    }

    private static void checkBitMask(long bitmask) {
        System.out.println("FIRST_BIT: " + ((bitmask & FIRST_BIT) != 0));
        System.out.println("SECOND_BIT: " + ((bitmask & SECOND_BIT) != 0));
        System.out.println("THIRD_BIT: " + ((bitmask & THIRD_BIT) != 0));
        System.out.println("FOURTh_BIT: " + ((bitmask & FOURTH_BIT) != 0));
        System.out.println("FIFTH_BIT: " + ((bitmask & FIFTH_BIT) != 0));
        System.out.println("BIT_55: " + ((bitmask & BIT_55) != 0));
    }
}

```

Prints

```java
FIRST_BIT: true
SECOND_BIT: false
THIRD_BIT: true
FOURTh_BIT: false
FIFTH_BIT: true
BIT_55: true

```

which matches that mask we passed as `checkBitMask` parameter: `FIRST_BIT | THIRD_BIT | FIFTH_BIT | BIT_55`.



## java.util.BitSet class


Since 1.7 there's a [java.util.BitSet](http://docs.oracle.com/javase/8/docs/api/java/util/BitSet.html) class that provides simple and user-friendly bit storage and manipulation interface:

```java
final BitSet bitSet = new BitSet(8); // by default all bits are unset

IntStream.range(0, 8).filter(i -> i % 2 == 0).forEach(bitSet::set); // {0, 2, 4, 6}

bitSet.set(3); // {0, 2, 3, 4, 6}

bitSet.set(3, false); // {0, 2, 4, 6}

final boolean b = bitSet.get(3); // b = false

bitSet.flip(6); // {0, 2, 4}

bitSet.set(100); // {0, 2, 4, 100} - expands automatically

```

`BitSet` implements `Clonable` and `Serializable`, and under the hood all bit values are stored in `long[] words` field, that expands automatically.

It also supports whole-set logical operations `and`, `or`, `xor`, `andNot`:

```java
bitSet.and(new BitSet(8));
bitSet.or(new BitSet(8));
bitSet.xor(new BitSet(8));
bitSet.andNot(new BitSet(8));

```



## Checking if a number is a power of 2


If an integer `x` is a power of 2, only one bit is set, whereas `x-1` has all bits set after that. For example: `4` is `100` and `3` is `011` as binary number, which satisfies the aforementioned condition. Zero is not a power of 2 and has to be checked explicitly.

```java
boolean isPowerOfTwo(int x)
{
    return (x != 0) && ((x & (x - 1)) == 0);
}

```

**Usage for Left and Right Shift**

Let’s suppose, we have three kind of permissions, **READ**, **WRITE** and **EXECUTE**. Each permission can range from 0 to 7. (Let’s assume 4 bit number system)

> 
RESOURCE = READ  WRITE  EXECUTE (12 bit number)
RESOURCE = 0100 0110 0101 = 4 6 5 (12 bit number)


How can we get the (12 bit number) permissions, set on above (12 bit number)?

> 
0100 0110 0101
0000 0000 0111 (&)
0000 0000 0101 = 5


So, this is how we can get the **EXECUTE** permissions of the **RESOURCE**. Now, what if we want to get **READ** permissions of the **RESOURCE**?

> 
0100 0110 0101
0111 0000 0000 (&)
0100 0000 0000 = 1024


Right? You are probably assuming this? But, permissions are resulted in 1024. We want to get only READ permissions for the resource. Don’t worry, that’s why we had the shift operators. If we see, READ permissions are 8 bits behind the actual result, so if apply some shift operator, which will bring READ permissions to the very right of the result? What if we do:

> 
<p>0100 0000 0000 >> 8 => 0000 0000 0100 (Because it’s a positive number
so replaced with 0’s, if you don’t care about sign, just use unsigned
right shift operator)</p>


We now actually have the **READ** permissions which is 4.

Now, for example, we are given **READ**, **WRITE**, **EXECUTE** permissions for a **RESOURCE**, what can we do to make permissions for this **RESOURCE**?

Let’s first take the example of binary permissions. (Still assuming 4 bit number system)

> 
READ = 0001
WRITE = 0100
EXECUTE = 0110


If you are thinking that we will simply do:

`READ | WRITE | EXECUTE`, you are somewhat right but not exactly. See, what will happen if we will perform READ | WRITE | EXECUTE

0001 | 0100 | 0110 => 0111

But permissions are actually being represented (in our example) as 0001 0100 0110

So, in order to do this, we know that **READ** is placed 8 bits behind, **WRITE** is placed 4 bits behind and **PERMISSIONS** is placed at the last. The number system being used for **RESOURCE** permissions is actually 12 bit (in our example). It can(will) be different in different systems.

> 
(READ << 8) | (WRITE << 4) | (EXECUTE)
0000 0000 0001 << 8 (READ)
0001 0000 0000 (Left shift by 8 bits)
0000 0000 0100 << 4 (WRITE)
0000 0100 0000 (Left shift by 4 bits)
0000 0000 0001 (EXECUTE)


Now if we add the results of above shifting, it will be something like;

> 
0001 0000 0000 (READ)
0000 0100 0000 (WRITE)
0000 0000 0001 (EXECUTE)
0001 0100 0001 (PERMISSIONS)




## Packing / unpacking values as bit fragments


It is common for memory performance to compress multiple values into a single primitive value.
This may be useful to pass various information into a single variable.

For example, one can pack 3 bytes - such as color code in [RGB](https://en.wikipedia.org/wiki/RGB_color_model) - into an single int.

**Packing the values**

```java
// Raw bytes as input
byte[] b = {(byte)0x65, (byte)0xFF, (byte)0x31};

// Packed in big endian: x == 0x65FF31
int x = (b[0] & 0xFF) << 16  // Red
      | (b[1] & 0xFF) <<  8  // Green
      | (b[2] & 0xFF) <<  0; // Blue

// Packed in little endian: y == 0x31FF65
int y = (b[0] & 0xFF) <<  0
      | (b[1] & 0xFF) <<  8
      | (b[2] & 0xFF) << 16;

```

**Unpacking the values**

```java
// Raw int32 as input
int x = 0x31FF65;

// Unpacked in big endian: {0x65, 0xFF, 0x31}
byte[] c = {
    (byte)(x >> 16),
    (byte)(x >>  8),
    (byte)(x & 0xFF)
};

// Unpacked in little endian: {0x31, 0xFF, 0x65}
byte[] d = {
    (byte)(x & 0xFF),
    (byte)(x >>  8),
    (byte)(x >> 16)
};

```



## Expressing the power of 2


For expressing the power of 2 (2^n) of integers, one may use a bitshift operation that allows to explicitly specify the `n`.

The syntax is basically:

```java
int pow2 = 1<<n;

```

Examples:

```java
int twoExp4 = 1<<4; //2^4
int twoExp5 = 1<<5; //2^5
int twoExp6 = 1<<6; //2^6
...
int twoExp31 = 1<<31; //2^31

```

This is especially useful when defining constant values that should make it apparent, that a power of 2 is used, instead of using hexadecimal or decimal values.

```java
int twoExp4 = 0x10; //hexadecimal
int twoExp5 = 0x20; //hexadecimal
int twoExp6 = 64; //decimal
...
int twoExp31 = -2147483648; //is that a power of 2?

```

A simple method to calculate the int power of 2 would be

```java
int pow2(int exp){
    return 1<<exp;
}

```



## Signed vs unsigned shift


In Java, all number primitives are signed. For example, an int always represent values from [-2^31 - 1, 2^31], keeping the first bit to sign the value - 1 for negative value, 0 for positive.

Basic shift operators `>>` and `<<` are signed operators. They will conserve the sign of the value.

But it is common for programmers to use numbers to store **unsigned values**. For an int, it means shifting the range to [0, 2^32 - 1], to have twice as much value as with a signed int.

For those power users, the bit for sign as no meaning. That's why Java added `>>>`, a left-shift operator, disregarding that sign bit.

```

               initial value:               4 (                                100)
     signed left-shift: 4 << 1               8 (                               1000)
    signed right-shift: 4 >> 1               2 (                                 10)
 unsigned right-shift: 4 >>> 1               2 (                                 10)
                initial value:              -4 (   11111111111111111111111111111100)
    signed left-shift: -4 << 1              -8 (   11111111111111111111111111111000)
   signed right-shift: -4 >> 1              -2 (   11111111111111111111111111111110)
unsigned right-shift: -4 >>> 1      2147483646 (    1111111111111111111111111111110)

```

****Why is there no `<<<` ?****

This comes from the intended definition of right-shift. As it fills the emptied places on the left, there are no decision to take regarding the bit of sign. As a consequence, there is no need for 2 different operators.

See this [question](https://www.quora.com/Why-is-there-no-unsigned-left-shift-operator-in-Java) for a more detailled answer.



#### Remarks


<li>
Unlike C/C++, Java is completely endian-neutral with respect to the underlying machine hardware. You do not get big or little endian behavior by default; you have to explicitly specify which behavior you want.
</li>
<li>
The `byte` type is signed, with the range -128 to +127. To convert a byte value to its unsigned equivalent, mask it with 0xFF like this: `(b & 0xFF)`.
</li>

