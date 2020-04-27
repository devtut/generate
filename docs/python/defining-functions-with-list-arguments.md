---
metaTitle: Defining functions with list arguments
description: Function and Call
---

# Defining functions with list arguments



## Function and Call


Lists as arguments are just another variable:

```
def func(myList):
    for item in myList:
        print(item)

```

and can be passed in the function call itself:

```
func([1,2,3,5,7])

1
2
3
5
7

```

Or as a variable:

```
aList = ['a','b','c','d']
func(aList)

a
b
c
d

```

