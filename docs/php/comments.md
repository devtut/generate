---
metaTitle: "Comments"
description: "Single Line Comments, Multi Line Comments"
---

# Comments



## Single Line Comments


The single line comment begins with "//" or "#".  When encountered, all text to the right will be ignored by the PHP interpreter.

```
// This is a comment

# This is also a comment

echo "Hello World!"; // This is also a comment, beginning where we see "//"

```



## Multi Line Comments


The multi-line comment can be used to comment out large blocks of code.  It begins with `/*` and ends with `*/`.

```
/* This is a multi-line comment.
   It spans multiple lines.
   This is still part of the comment. 
*/

```



#### Remarks


Keep the following tips in mind when deciding how to comment your code:

- You should always write your code as if comments didn't exist, using well chosen variable and function names.
- Comments are meant to communicate to other human beings, not to repeat what is written in the code.
- Various php commenting style guides exist (e.g. [pear](https://pear.php.net/manual/en/standards.sample.php), [zend](https://framework.zend.com/manual/1.12/en/coding-standard.coding-style.html#coding-standards.inline-documentation), etc).  Find out which one your company uses and use it consistently!

