---
metaTitle: "VBA - Declaring and assigning strings"
description: "Declare a string constant, Declare a variable-width string variable, Declare and assign a fixed-width string, Declare and assign a string array, Assign specific characters within a string using Mid statement, Assignment to and from a byte array"
---

# Declaring and assigning strings



## Declare a string constant


```vb
Const appName As String = "The App For That"

```



## Declare a variable-width string variable


```vb
Dim surname As String 'surname can accept strings of variable length
surname = "Smith"
surname = "Johnson"

```



## Declare and assign a fixed-width string


```vb
'Declare and assign a 1-character fixed-width string
Dim middleInitial As String * 1 'middleInitial must be 1 character in length
middleInitial = "M"

'Declare and assign a 2-character fixed-width string `stateCode`,
'must be 2 characters in length
Dim stateCode As String * 2
stateCode = "TX"

```



## Declare and assign a string array


```vb
'Declare, dimension and assign a string array with 3 elements
Dim departments(2) As String
departments(0) = "Engineering"
departments(1) = "Finance"
departments(2) = "Marketing"

'Declare an undimensioned string array and then dynamically assign with
'the results of a function that returns a string array
Dim stateNames() As String
stateNames = VBA.Strings.Split("Texas;California;New York", ";")

'Declare, dimension and assign a fixed-width string array
Dim stateCodes(2) As String * 2
stateCodes(0) = "TX"
stateCodes(1) = "CA"
stateCodes(2) = "NY"

```



## Assign specific characters within a string using Mid statement


VBA offers a Mid function for **returning** substrings within a string, but it also offers the Mid **Statement** which can be used to assign substrings or individual characters withing a string.

The `Mid` function will typically appear on the right-hand-side of an assignment statement or in a condition, but the `Mid` Statement typically appears on the left hand side of an assignment statement.

```vb
Dim surname As String
surname = "Smith"

'Use the Mid statement to change the 3rd character in a string
Mid(surname, 3, 1) = "y"
Debug.Print surname

'Output:
'Smyth

```

Note: If you need to assign to individual **bytes** in a string instead of individual **characters** within a string (see the Remarks below regarding the Multi-Byte Character Set), the `MidB` statement can be used. In this instance, the second argument for the `MidB` statement is the 1-based position of the byte where the replacement will start so the equivalent line to the example above would be `MidB(surname, 5, 2) = "y"`.



## Assignment to and from a byte array


Strings can be assigned directly to byte arrays and visa-versa. Remember that Strings are stored in a Multi-Byte Character Set (see Remarks below) so only every other index of the resulting array will be the portion of the character that falls within the ASCII range.

```vb
Dim bytes() As Byte
Dim example As String

example = "Testing."
bytes = example             'Direct assignment.

'Loop through the characters. Step 2 is used due to wide encoding.
Dim i As Long
For i = LBound(bytes) To UBound(bytes) Step 2
    Debug.Print Chr$(bytes(i))  'Prints T, e, s, t, i, n, g, .
Next

Dim reverted As String
reverted = bytes            'Direct assignment.
Debug.Print reverted        'Prints "Testing."

```



#### Remarks


Strings are a [Reference type](https://msdn.microsoft.com/en-us/library/t63sy5hs.aspx)  and are central to most programming tasks. Strings are assigned text, even if the text happens to be numeric. Strings can be zero-length, or any length up to 2GB. Modern versions of VBA store Strings internally using a Byte array of Multi-Byte Character Set bytes (an alternative to Unicode).

