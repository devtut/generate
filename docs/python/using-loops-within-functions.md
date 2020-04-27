---
metaTitle: "Using loops within functions"
description: "Return statement inside loop in a function"
---

# Using loops within functions


In Python function will be returned as soon as execution hits "return" statement.



## Return statement inside loop in a function


In this example, function will return as soon as value var has 1

```
def func(params):
    for value in params:
        print ('Got value {}'.format(value))

        if value == 1:
            # Returns from function as soon as value is 1
            print (">>>> Got 1")
            return

        print ("Still looping")

    return "Couldn't find 1"

func([5, 3, 1, 2, 8, 9])

```

output

```
Got value 5
Still looping
Got value 3
Still looping
Got value 1
>>>> Got 1

```

