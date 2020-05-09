---
metaTitle: "VBA - Converting other types to strings"
description: "Use CStr to convert a numeric type to a string, Use Format to convert and format a numeric type as a string, Use StrConv to convert a byte-array of single-byte characters to a string, Implicitly convert a byte array of multi-byte-characters to a string"
---

# Converting other types to strings



## Use CStr to convert a numeric type to a string


```vb
Const zipCode As Long = 10012
Dim zipCodeText As String
'Convert the zipCode number to a string of digit characters
zipCodeText = CStr(zipCode)
'zipCodeText = "10012"

```



## Use Format to convert and format a numeric type as a string


```vb
Const zipCode As long = 10012
Dim zeroPaddedNumber As String
zeroPaddedZipCode = Format(zipCode, "00000000")
'zeroPaddedNumber = "00010012"

```



## Use StrConv to convert a byte-array of single-byte characters to a string


```vb
'Declare an array of bytes, assign single-byte character codes, and convert to a string
Dim singleByteChars(4) As Byte
singleByteChars(0) = 72
singleByteChars(1) = 101
singleByteChars(2) = 108
singleByteChars(3) = 108
singleByteChars(4) = 111
Dim stringFromSingleByteChars As String
stringFromSingleByteChars = StrConv(singleByteChars, vbUnicode)
'stringFromSingleByteChars = "Hello"

```



## Implicitly convert a byte array of multi-byte-characters to a string


```vb
'Declare an array of bytes, assign multi-byte character codes, and convert to a string
Dim multiByteChars(9) As Byte
multiByteChars(0) = 87
multiByteChars(1) = 0
multiByteChars(2) = 111
multiByteChars(3) = 0
multiByteChars(4) = 114
multiByteChars(5) = 0
multiByteChars(6) = 108
multiByteChars(7) = 0
multiByteChars(8) = 100
multiByteChars(9) = 0
            
Dim stringFromMultiByteChars As String
stringFromMultiByteChars = multiByteChars
'stringFromMultiByteChars = "World"

```



#### Remarks


VBA will implicitly convert some types to string as necessary and without any extra work on the part of the programmer, but VBA also provides a number of explicit string conversion functions, and you can also write your own.

Three of the most frequently used functions are `CStr`, `Format` and `StrConv`.

