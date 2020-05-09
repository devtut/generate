---
metaTitle: "VBA - Assigning strings with repeated characters"
description: "Use the String function to assign a string with n repeated characters, Use the String and Space functions to assign an n-character string"
---

# Assigning strings with repeated characters



## Use the String function to assign a string with n repeated characters


```vb
Dim lineOfHyphens As String
'Assign a string with 80 repeated hyphens
lineOfHyphens = String$(80, "-")

```



## Use the String and Space functions to assign an n-character string


```vb
Dim stringOfSpaces As String

'Assign a string with 255 repeated spaces using Space$
stringOfSpaces = Space$(255)

'Assign a string with 255 repeated spaces using String$
stringOfSpaces = String$(255, " ")

```



#### Remarks


There are times you need to assign a string variable with a specific character repeated a specific number of times. VBA provides two main functions for this purpose:

- `String`/`String$`
- `Space`/`Space$`.

