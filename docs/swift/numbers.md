---
metaTitle: "Swift - Numbers"
description: "Convert numbers to/from strings, Number types and literals, Rounding, Convert one numeric type to another, Random number generation, Exponentiation"
---

# Numbers



## Convert numbers to/from strings


Use String initializers for converting numbers into strings:

```swift
String(1635999)                              // returns "1635999"
String(1635999, radix: 10)                   // returns "1635999"
String(1635999, radix: 2)                    // returns "110001111011010011111"
String(1635999, radix: 16)                   // returns "18f69f"
String(1635999, radix: 16, uppercase: true)  // returns "18F69F"
String(1635999, radix: 17)                   // returns "129gf4"
String(1635999, radix: 36)                   // returns "z2cf"

```

Or use [string interpolation](http://stackoverflow.com/documentation/swift/320/strings/1129/string-literals#t=201604140705404838053) for simple cases:

```swift
let x = 42, y = 9001
"Between \(x) and \(y)"  // equivalent to "Between 42 and 9001"

```

Use initializers of numeric types to convert strings into numbers:

```swift
if let num = Int("42") { /* ... */ }                // num is 42
if let num = Int("Z2cF") { /* ... */ }              // returns nil (not a number)
if let num = Int("z2cf", radix: 36) { /* ... */ }   // num is 1635999
if let num = Int("Z2cF", radix: 36) { /* ... */ }   // num is 1635999
if let num = Int8("Z2cF", radix: 36) { /* ... */ }  // returns nil (too large for Int8)

```



## Number types and literals


Swift's built-in numeric types are:

- Word-sized (architecture-dependent) signed [Int](https://developer.apple.com/reference/swift/int) and unsigned [UInt](https://developer.apple.com/reference/swift/uint).
- Fixed-size signed integers [Int8](https://developer.apple.com/reference/swift/int8), [Int16](https://developer.apple.com/reference/swift/int16), [Int32](https://developer.apple.com/reference/swift/int32), [Int64](https://developer.apple.com/reference/swift/int64), and unsigned integers [UInt8](https://developer.apple.com/reference/swift/uint8), [UInt16](https://developer.apple.com/reference/swift/uint16), [UInt32](https://developer.apple.com/reference/swift/uint32), [UInt64](https://developer.apple.com/reference/swift/uint64).
- Floating-point types [Float32/Float](https://developer.apple.com/reference/swift/float), [Float64/Double](https://developer.apple.com/reference/swift/double), and [Float80](https://developer.apple.com/reference/swift/float80) (x86-only).

### Literals

A numeric literal's type is inferred from context:

```swift
let x = 42    // x is Int by default
let y = 42.0  // y is Double by default

let z: UInt = 42     // z is UInt
let w: Float = -1    // w is Float
let q = 100 as Int8  // q is Int8

```

Underscores (`_`) may be used to separate digits in numeric literals. Leading zeros are ignored.

Floating point literals may be specified using [significand](https://en.wikipedia.org/wiki/Significand) and exponent parts (`*«significand»* **e** *«exponent»*` for decimal; `**0x** *«significand»* **p** *«exponent»*` for hexadecimal).

### Integer literal syntax

```swift
let decimal = 10               // ten
let decimal = -1000            // negative one thousand
let decimal = -1_000           // equivalent to -1000
let decimal = 42_42_42         // equivalent to 424242
let decimal = 0755             // equivalent to 755, NOT 493 as in some other languages
let decimal = 0123456789

let hexadecimal = 0x10         // equivalent to 16
let hexadecimal = 0x7FFFFFFF
let hexadecimal = 0xBadFace
let hexadecimal = 0x0123_4567_89ab_cdef

let octal = 0o10               // equivalent to 8
let octal = 0o755              // equivalent to 493
let octal = -0o0123_4567             

let binary = -0b101010         // equivalent to -42
let binary = 0b111_101_101     // equivalent to 0o755
let binary = 0b1011_1010_1101  // equivalent to 0xB_A_D

```

### Floating-point literal syntax

```swift
let decimal = 0.0
let decimal = -42.0123456789
let decimal = 1_000.234_567_89

let decimal = 4.567e5               // equivalent to 4.567×10⁵, or 456_700.0
let decimal = -2E-4                 // equivalent to -2×10⁻⁴, or -0.0002
let decimal = 1e+0                  // equivalent to 1×10⁰, or 1.0

let hexadecimal = 0x1p0             // equivalent to 1×2⁰, or 1.0
let hexadecimal = 0x1p-2            // equivalent to 1×2⁻², or 0.25
let hexadecimal = 0xFEEDp+3         // equivalent to 65261×2³, or 522088.0
let hexadecimal = 0x1234.5P4        // equivalent to 0x12345, or 74565.0
let hexadecimal = 0x123.45P8        // equivalent to 0x12345, or 74565.0
let hexadecimal = 0x12.345P12       // equivalent to 0x12345, or 74565.0
let hexadecimal = 0x1.2345P16       // equivalent to 0x12345, or 74565.0
let hexadecimal = 0x0.12345P20      // equivalent to 0x12345, or 74565.0

```



## Rounding


### round

Rounds the value to the nearest whole number with x.5 rounding up (but note that -x.5 rounds down).

```swift
round(3.000) // 3
round(3.001) // 3
round(3.499) // 3
round(3.500) // 4
round(3.999) // 4

round(-3.000) // -3
round(-3.001) // -3
round(-3.499) // -3
round(-3.500) // -4  *** careful here ***
round(-3.999) // -4

```

### ceil

Rounds any number with a decimal value up to the next larger whole number.

```swift
ceil(3.000) // 3
ceil(3.001) // 4
ceil(3.999) // 4

ceil(-3.000) // -3
ceil(-3.001) // -3
ceil(-3.999) // -3

```

### floor

Rounds any number with a decimal value down to the next smaller whole number.

```swift
floor(3.000) // 3
floor(3.001) // 3
floor(3.999) // 3

floor(-3.000) // -3
floor(-3.001) // -4
floor(-3.999) // -4

```

### Int

Converts a `Double` to an `Int`, dropping any decimal value.

```swift
Int(3.000) // 3
Int(3.001) // 3
Int(3.999) // 3

Int(-3.000) // -3
Int(-3.001) // -3
Int(-3.999) // -3

```

### Notes

- `round`, `ceil` and `floor` handle both 64 and 32 bit architecture.



## Convert one numeric type to another


```swift
func doSomething1(value: Double) { /* ... */ }
func doSomething2(value: UInt) { /* ... */ }

let x = 42               // x is an Int
doSomething1(Double(x))  // convert x to a Double
doSomething2(UInt(x))    // convert x to a UInt

```

Integer initializers produce a **runtime error** if the value overflows or underflows:

```swift
Int8(-129.0) // fatal error: floating point value cannot be converted to Int8 because it is less than Int8.min
Int8(-129)   // crash: EXC_BAD_INSTRUCTION / SIGILL
Int8(-128)   // ok
Int8(-2)     // ok
Int8(17)     // ok
Int8(127)    // ok
Int8(128)    // crash: EXC_BAD_INSTRUCTION / SIGILL
Int8(128.0)  // fatal error: floating point value cannot be converted to Int8 because it is greater than Int8.max

```

Float-to-integer conversion **rounds values towards zero**:

```swift
Int(-2.2)  // -2
Int(-1.9)  // -1
Int(-0.1)  //  0
Int(1.0)   //  1
Int(1.2)   //  1
Int(1.9)   //  1
Int(2.0)   //  2

```

Integer-to-float conversion may be **lossy**:

```swift
Int(Float(1_000_000_000_000_000_000))  // 999999984306749440

```



## Random number generation


`arc4random_uniform(someNumber: UInt32) -> UInt32`

This gives you random integers in the range `0` to `someNumber - 1`.

The maximum value for `UInt32` is 4,294,967,295 (that is, `2^32 - 1`).

**Examples:**

<li>
Coin flip

```swift
  let flip = arc4random_uniform(2) // 0 or 1

```


</li>
<li>
Dice roll

```swift
  let roll = arc4random_uniform(6) + 1 // 1...6

```


</li>
<li>
Random day in October

```swift
  let day = arc4random_uniform(31) + 1 // 1...31

```


</li>
<li>
Random year in the 1990s

```swift
  let year = 1990 + arc4random_uniform(10)

```


</li>

**General form:**

```swift
let number = min + arc4random_uniform(max - min + 1)

```

where `number`, `max`, and `min` are `UInt32`.

### Notes

- There is a slight modulo bias with `arc4random` so `arc4random_uniform` is preferred.
- You can cast a `UInt32` value to an `Int` but just beware of going out of range.



## Exponentiation


In Swift, we can **exponentiate** `Double`s with the built-in `pow()` method:

```swift
pow(BASE, EXPONENT)

```

In the code below, the base (5) is set to the power of the exponent (2) :

```swift
let number = pow(5.0, 2.0) // Equals 25

```

