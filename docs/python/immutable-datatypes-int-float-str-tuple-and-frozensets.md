---
metaTitle: "Immutable datatypes(int, float, str, tuple and frozensets)"
description: "Individual characters of strings are not assignable, Tuple's individual members aren't assignable, Frozenset's are immutable and not assignable"
---

# Immutable datatypes(int, float, str, tuple and frozensets)




## Individual characters of strings are not assignable


```py
foo = "bar"
foo[0] = "c" # Error 

```

Immutable variable value can not be changed once they are created.



## Tuple's individual members aren't assignable


```py
foo = ("bar", 1, "Hello!",)
foo[1] = 2 # ERROR!! 

```

Second line would return an error since tuple members once created aren't assignable.
Because of tuple's immutability.<br>



## Frozenset's are immutable and not assignable


```py
foo = frozenset(["bar", 1, "Hello!"])
foo[2] = 7 # ERROR
foo.add(3) # ERROR

```

Second line would return an error since frozenset members once created aren't assignable.
Third line would return error as frozensets do not support functions that can manipulate members.

