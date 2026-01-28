---
metaTitle: "VBA - Comments"
description: "Apostrophe Comments, REM Comments"
---

# Comments



## Apostrophe Comments


A comment is marked by an apostrophe (`'`), and ignored when the code executes. Comments help explain your code to future readers, including yourself.

Since all lines starting with a comment are ignored, they can also be used to prevent code from executing (while you debug or refactor). Placing an apostrophe `'` before your code turns it into a comment.  (This is called **commenting out** the line.)

```vb
Sub InlineDocumentation()
  'Comments start with an "'"

  'They can be place before a line of code, which prevents the line from executing
  'Debug.Print "Hello World"

  'They can also be placed after a statement
  'The statement still executes, until the compiler arrives at the comment
  Debug.Print "Hello World"  'Prints a welcome message

'Comments can have 0 indention....
     '... or as much as needed

  '''' Comments can contain multiple apostrophes ''''

  'Comments can span lines (using line continuations) _
    but this can make for hard to read code

  'If you need to have mult-line comments, it is often easier to 
  'use an apostrophe on each line

  'The continued statement syntax (:) is treated as part of the comment, so 
  'it is not possible to place an executable statement after a comment
  'This won't run : Debug.Print "Hello World"
End Sub

'Comments can appear inside or outside a procedure

```



## REM Comments


```vb
Sub RemComments()
  Rem Comments start with "Rem" (VBA will change any alternate casing to "Rem")
  Rem is an abbreviation of Remark, and similar to DOS syntax
  Rem Is a legacy approach to adding comments, and apostrophes should be preferred

  Rem Comments CANNOT appear after a statement, use the apostrophe syntax instead
  Rem Unless they are preceded by the instruction separator token
  Debug.Print "Hello World": Rem prints a welcome message
  Debug.Print "Hello World" 'Prints a welcome message

  'Rem cannot be immediately followed by the following characters "!,@,#,$,%,&"
  'Whereas the apostrophe syntax can be followed by any printable character.

End Sub

Rem Comments can appear inside or outside a procedure

```



#### Remarks


**Comment Blocks**

If you need to comment or uncomment several lines at once, you can use the IDE's **Edit Toolbar** buttons:

> 
**Comment Block** - Adds a single apostrophe to the start of all selected lines


[<img src="http://i.stack.imgur.com/1fTtY.png" alt="Comment Block" />](http://i.stack.imgur.com/1fTtY.png)

> 
**Uncomment Block** - Removes the first apostrophe from the start of all selected lines


[<img src="http://i.stack.imgur.com/gbE0b.png" alt="Uncomment Block" />](http://i.stack.imgur.com/gbE0b.png)

**Multi-line Comments**
Many other languages support multi-line block comments, but VBA only allows single-line comments.

