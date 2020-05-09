---
metaTitle: "Objective-C - Format-Specifiers"
description: "Integer Example - %i"
---

# Format-Specifiers


Format-Specifiers are used in Objective-c to implant object-values into a string.



## Integer Example - %i


```objc
int highScore = 57;
NSString *scoreBoard = [NSString stringWithFormat:@"HighScore: %i", (int)highScore];

NSLog(scoreBoard);//logs "HighScore: 57"

```



#### Syntax


- %@ //String
- %d //Signed 32-bit integer
- %D //Signed 32-bit integer
- %u //Unsigned 32-bit integer
- %U //Unsigned 32-bit integer
- %x //Unsigned 32-bit integer in lowercase hexadecimal format
- %X //Unsigned 32-bit integer in UPPERCASE hexadecimal format
- %o //Unsigned 32-bit integer in octal format
- %O //Unsigned 32-bit integer in octal format
- %f //64-bit floating-point number
- %F //64-bit floating-point number printed in decimal notation
- %e //64-bit floating-point number in lowercase scientific notation format
- %E //64-bit floating-point number in UPPERCASE scientific notation format
- %g //special case %e which uses %f when less than 4 sig-figs are available, else %e
- %G //special case %E which uses %f when less than 4 sig-figs are available, else %E
- %c //8-bit unsigned character
- %C //16-bit UTF-16 code unit
- %s //UTF8 String
- %S //16-bit variant of %s
- %p //Void Pointer in lowercase hexidecmial format with leading '0x'
- %zx //special case %p which removes leading '0x' (For use with no-type cast)
- %a //64-bit floating-point number in scientific notation with leading '0x' and one hexadecimal digit before the decimal point using a 'p' to notate the exponent.
- %A //64-bit floating-point number in scientific notation with leading '0x' and one hexadecimal digit before the decimal point using an 'P' to notate the exponent.



#### Remarks


Due to the nature of format-specifiers, if you wish to include the percentage symbol (%) in your string, you must escape it using a second percentage symbol.

Example:

```objc
int progress = 45;//percent
NSString *progressString = [NSString stringWithFormat:@"Progress: %i%%", (int)progress];

NSLog(progressString);//logs "Progress: 45%"

```

No Format Specifier for BOOL-type exists.

Common-use solutions include:

```objc
BOOL myBool = YES;
NSString *boolState = [NSString stringWithFormat:@"BOOL state: %@", myBool?@"true":@"false"];

NSLog(boolState);//logs "true"

```

Which utilizes a ternary operator for casting a string-equivalent.

```objc
BOOL myBool = YES;
NSString *boolState = [NSString stringWithFormat:@"BOOL state: %i", myBool];

NSLog(boolState);//logs "1" (binary)

```

Which utilizes an (int) cast for implanting a binary-equivalent.

