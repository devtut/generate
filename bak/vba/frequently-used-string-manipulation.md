---
metaTitle: "VBA - Frequently used string manipulation"
description: "String manipulation frequently used examples"
---

# Frequently used string manipulation


Quick examples for MID LEFT and RIGHT string functions using INSTR FIND and LEN.

How do you find the text between two search terms (Say: after a colon and before a comma)?
How do you get the remainder of a word (using MID or using RIGHT)? Which of these functions use Zero-based params and return codes vs One-based? What happens when things go wrong? How do they handle empty strings, unfound results and negative numbers?



## String manipulation frequently used examples


Better MID() and other string extraction examples, currently lacking from the web. Please help me make a good example, or complete this one here. Something like this:

```vb
DIM strEmpty as String, strNull as String, theText as String
DIM idx as Integer
DIM letterCount as Integer
DIM result as String

strNull = NOTHING
strEmpty = ""
theText = "1234, 78910"  

' -----------------
' Extract the word after the comma ", "  and before "910"  result: "78" ***
' -----------------

' Get index (place) of comma using INSTR
idx = ...   ' some explanation here
if idx < ... ' check if no comma found in text

' or get index of comma using FIND
idx = ...   ' some explanation here... Note: The difference is...
if idx < ...  ' check if no comma found in text

result = MID(theText, ..., LEN(...

' Retrieve remaining word after the comma
result = MID(theText, idx+1, LEN(theText) - idx+1)

' Get word until the comma using LEFT
result = LEFT(theText, idx - 1)

' Get remaining text after the comma-and-space using RIGHT
result = ...

' What happens when things go wrong
result = MID(strNothing, 1, 2)    ' this causes ...
result = MID(strEmpty, 1, 2) ' which causes...
result = MID(theText, 30, 2) ' and now...
result = MID(theText, 2, 999) ' no worries...
result = MID(theText, 0, 2)
result = MID(theText, 2, 0)
result = MID(theText -1, 2)
result = MID(theText 2, -1)
idx = INSTR(strNothing, "123")
idx = INSTR(theText, strNothing)
idx = INSTR(theText, strEmpty) 
i = LEN(strEmpty) 
i = LEN(strNothing) '...

```

Please feel free to edit this example and make it better. As long as it remains clear, and has in it common usage practices.

