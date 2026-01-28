---
metaTitle: "Visual Basic .NET - Type conversion"
description: "Converting Text of The Textbox to an Integer"
---

# Type conversion



## Converting Text of The Textbox to an Integer


From [MSDN](https://msdn.microsoft.com/en-us/library/s2dy91zy.aspx)

> 
<p>Use the CInt function to provide conversions from any other data type
to an Integer subtype. For example, CInt forces integer arithmetic
when currency, single-precision, or double-precision arithmetic would
normally occur.</p>


Assuming that you have 1 button and 2 textbox. If you type on textbox1.text `5.5` and on textbox2.text `10`.

If you have this code:

```vb
Dim result = textbox1.text + textbox2.text
MsgBox("Result: " & result)
'It will output
5.510

```

In order to add the values of the 2 textboxes you need to convert their values to `Int` by using the `CInt(expression)`.

```vb
Dim result = CInt(textbox1.text) + CInt(textbox2.text)
MsgBox("Result: " & result)
'It will output
16

```

> 
<p>Note: When the fractional part of a value is exactly 0.5, the CInt
function rounds to the closest even number. For example, <strong>0.5 rounds to
0</strong>, while **1.5 rounds to 2, and 3.5 rounds to 4**. The purpose of rounding to
the closest even number is to compensate for a bias that could
accumulate when many numbers are added together.</p>




#### Syntax


- CBool(expression)
- CByte(expression)
- CChar(expression)
- CDate(expression)
- CDbl(expression)
- CDec(expression)
- CInt(expression)
- CLng(expression)
- CObj(expression)
- CSByte(expression)
- CShort(expression)
- CSng(expression)
- CStr(expression)
- CUInt(expression)
- CULng(expression)
- CUShort(expression)



#### Parameters


|Function name|Range for Expression argument
|---|---|---|---|---|---|---|---|---|---
|CBool|Any valid Char or String or numeric expression
|CByte|0 through 255 (unsigned); fractional parts are rounded.
|CChar|Any valid Char or String expression; only first character of a String is converted; value can be 0 through 65535 (unsigned).

