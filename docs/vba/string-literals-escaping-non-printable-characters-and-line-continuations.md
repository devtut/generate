---
metaTitle: "VBA - String Literals - Escaping, non-printable characters and line-continuations"
description: "Escaping the  character, Assigning long string literals, Using VBA string constants"
---

# String Literals - Escaping, non-printable characters and line-continuations



## Escaping the " character


VBA syntax requires that a string-literal appear within `"` marks, so when your string needs to **contain** quotation marks, you'll need to escape/prepend the `"` character with an extra `"` so that VBA understands that you intend the `""` to be interpreted as a `"` string.

```vb
'The following 2 lines produce the same output
Debug.Print "The man said, ""Never use air-quotes"""
Debug.Print "The man said, " & """" & "Never use air-quotes" & """"

'Output:
'The man said, "Never use air-quotes"
'The man said, "Never use air-quotes"

```



## Assigning long string literals


The VBA editor only allows 1023 characters per line, but typically only the first 100-150 characters are visible without scrolling. If you need to assign long string literals, but you want to keep your code readable, you'll need to use line-continuations and concatenation to assign your string.

```vb
Debug.Print "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " & _
            "Integer hendrerit maximus arcu, ut elementum odio varius " & _
            "nec. Integer ipsum enim, iaculis et egestas ac, condiment" & _
            "um ut tellus."
'Output:
'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer hendrerit maximus arcu, ut elementum odio varius nec. Integer ipsum enim, iaculis et egestas ac, condimentum ut tellus.

```

VBA will let you use a limited number of line-continuations (the actual number varies by the length of each line within the continued-block), so if you have very long strings, you'll need to assign and re-assign with concatenation.

```vb
Dim loremIpsum As String

'Assign the first part of the string
loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. " & _
              "Integer hendrerit maximus arcu, ut elementum odio varius "
'Re-assign with the previous value AND the next section of the string
loremIpsum = loremIpsum & _
            "nec. Integer ipsum enim, iaculis et egestas ac, condiment" & _
            "um ut tellus."

Debug.Print loremIpsum

'Output:
'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer hendrerit maximus arcu, ut elementum odio varius nec. Integer ipsum enim, iaculis et egestas ac, condimentum ut tellus.

```



## Using VBA string constants


VBA defines a number of string constants for special characters like:

- vbCr : Carriage-Return 'Same as "\r" in C style languages.
- vbLf : Line-Feed 'Same as "\n" in C style languages.
- vbCrLf : Carriage-Return & Line-Feed (a new-line in Windows)
- vbTab: Tab Character
- vbNullString: an empty string, like ""

You can use these constants with concatenation and other string functions to build string-literals with special-characters.

```vb
Debug.Print "Hello " & vbCrLf & "World"
'Output:
'Hello
'World

Debug.Print vbTab & "Hello" & vbTab & "World"
'Output:
'    Hello    World

Dim EmptyString As String
EmptyString = vbNullString
Debug.Print EmptyString = ""
'Output:
'True

```

Using `vbNullString` is considered better practice than the equivalent value of `""` due to differences in how the code is compiled. Strings are accessed via a pointer to an allocated area of memory, and the VBA compiler is smart enough to use a null pointer to represent `vbNullString`. The literal `""` is allocated memory as if it were a String typed Variant, making the use of the constant much more efficient:

```vb
Debug.Print StrPtr(vbNullString)    'Prints 0.
Debug.Print StrPtr("")              'Prints a memory address.

```



#### Remarks


The assignment of string-literals in VBA is confined by the limitations of the IDE and the codepage of the current user's language settings. The examples above demonstrate the special-cases of escaped strings, special, non-printable strings and long string-literals.

When assigning string-literals that contain characters that are specific to a certain codepage, you may need to consider internationalization concerns by assigning a string from a separate unicode resource file.

