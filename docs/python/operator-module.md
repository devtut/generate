---
metaTitle: "Operator module"
description: "Itemgetter, Operators as alternative to an infix operator, Methodcaller"
---

# Operator module



## Itemgetter


Grouping the key-value pairs of a dictionary by the value with `itemgetter`:

```py
from itertools import groupby
from operator import itemgetter
adict = {'a': 1, 'b': 5, 'c': 1}

dict((i, dict(v)) for i, v in groupby(adict.items(), itemgetter(1)))
# Output: {1: {'a': 1, 'c': 1}, 5: {'b': 5}}

```

which is equivalent (but faster) to a `lambda` function like this:

```py
dict((i, dict(v)) for i, v in groupby(adict.items(), lambda x: x[1]))

```

Or sorting a list of tuples by the second element first the first element as secondary:

```py
alist_of_tuples = [(5,2), (1,3), (2,2)]
sorted(alist_of_tuples, key=itemgetter(1,0))
# Output: [(2, 2), (5, 2), (1, 3)]

```



## Operators as alternative to an infix operator


For every infix operator, e.g. `+` there is a `operator`-function (`operator.add` for `+`):

```py
1 + 1
# Output: 2
from operator import add
add(1, 1)
# Output: 2

```

even though the main documentation states that for the arithmetic operators only numerical input is allowed it **is** possible:

```py
from operator import mul
mul('a', 10)
# Output: 'aaaaaaaaaa'
mul([3], 3)
# Output: [3, 3, 3]

```

See also: [mapping from operation to operator function in the official Python documentation](https://docs.python.org/3/library/operator.html#mapping-operators-to-functions).



## Methodcaller


Instead of this `lambda`-function that calls the method explicitly:

```py
alist = ['wolf', 'sheep', 'duck']
list(filter(lambda x: x.startswith('d'), alist))     # Keep only elements that start with 'd'
# Output: ['duck']

```

one could use a operator-function that does the same:

```py
from operator import methodcaller
list(filter(methodcaller('startswith', 'd'), alist)) # Does the same but is faster.
# Output: ['duck']

```

