---
metaTitle: "Boolean Operators"
description: "and, or, not, Short-circuit evaluation, `and` and `or` are not guaranteed to return a boolean, A simple example"
---

# Boolean Operators



## and


Evaluates to the second argument if and only if both of the arguments are truthy. Otherwise evaluates to the first falsey argument.

```
x = True
y = True
z = x and y # z = True

x = True
y = False
z = x and y # z = False

x = False
y = True
z = x and y # z = False

x = False
y = False
z = x and y # z = False

x = 1
y = 1
z = x and y # z = y, so z = 1, see `and` and `or` are not guaranteed to be a boolean

x = 0
y = 1
z = x and y # z = x, so z = 0 (see above)

x = 1
y = 0
z = x and y # z = y, so z = 0 (see above)

x = 0
y = 0
z = x and y # z = x, so z = 0 (see above)

```

The `1`'s in the above example can be changed to any truthy value, and the `0`'s can be changed to any falsey value.



## or


Evaluates to the first truthy argument if either one of the arguments is truthy. If both arguments are falsey, evaluates to the second argument.

```
x = True
y = True
z = x or y # z = True

x = True
y = False
z = x or y # z = True

x = False
y = True
z = x or y # z = True

x = False
y = False
z = x or y # z = False

x = 1
y = 1
z = x or y # z = x, so z = 1, see `and` and `or` are not guaranteed to be a boolean

x = 1
y = 0
z = x or y # z = x, so z = 1 (see above)

x = 0
y = 1
z = x or y # z = y, so z = 1 (see above)

x = 0
y = 0
z = x or y # z = y, so z = 0 (see above)

```

The `1`'s in the above example can be changed to any truthy value, and the `0`'s can be changed to any falsey value.



## not


It returns the opposite of the following statement:

```
x = True
y = not x # y = False

x = False
y = not x # y = True

```



## Short-circuit evaluation


Python [minimally evaluates](https://en.wikipedia.org/wiki/Short-circuit_evaluation) Boolean expressions.

```
>>> def true_func():
...     print("true_func()")
...     return True
... 
>>> def false_func():
...     print("false_func()")
...     return False
... 
>>> true_func() or false_func()
true_func()
True
>>> false_func() or true_func()
false_func()
true_func()
True
>>> true_func() and false_func()
true_func()
false_func()
False
>>> false_func() and false_func()
false_func()
False

```



## `and` and `or` are not guaranteed to return a boolean


When you use `or`, it will either return the first value in the expression if it's true, else it will blindly return the second value. I.e. `or` is equivalent to:

```
def or_(a, b):
    if a:
        return a
    else:
        return b

```

For `and`, it will return its first value if it's false, else it returns the last value:

```
def and_(a, b):
    if not a:
        return a
    else:
        return b

```



## A simple example


In Python you can compare a single element using two binary operators--one on either side:

```
if 3.14 < x < 3.142:
    print("x is near pi")

```

In many (most?) programming languages, this would be evaluated in a way contrary to regular math: `(3.14 < x) < 3.142`, but in Python it is treated like `3.14 < x and x < 3.142`, just like most non-programmers would expect.

