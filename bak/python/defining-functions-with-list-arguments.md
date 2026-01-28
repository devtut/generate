---
metaTitle: "Python - Defining functions with list arguments"
description: "Function and Call"
---

# Defining functions with list arguments



## Function and Call


Lists as arguments are just another variable:

```py
def func(myList):
    for item in myList:
        print(item)

```

and can be passed in the function call itself:

```py
func([1,2,3,5,7])

1
2
3
5
7

```

Or as a variable:

```py
aList = ['a','b','c','d']
func(aList)

a
b
c
d

```

