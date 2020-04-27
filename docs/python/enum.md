---
metaTitle: Enum
description: Creating an enum (Python 2.4 through 3.3), Iteration
---

# Enum



## Creating an enum (Python 2.4 through 3.3)


Enums have been backported from Python 3.4 to Python 2.4 through Python 3.3. You can get this the [enum34](https://pypi.python.org/pypi/enum34) backport from PyPI.

```
pip install enum34

```

Creation of an enum is identical to how it works in Python 3.4+

```
from enum import Enum

class Color(Enum):
    red = 1
    green = 2
    blue = 3

print(Color.red)  # Color.red    
print(Color(1))  # Color.red    
print(Color['red'])  # Color.red  

```



## Iteration


Enums are iterable:

```
class Color(Enum):
    red = 1
    green = 2
    blue = 3

[c for c in Color]  # [<Color.red: 1>, <Color.green: 2>, <Color.blue: 3>]

```



#### Remarks


Enums were added to Python in version 3.4 by [PEP 435](https://www.python.org/dev/peps/pep-0435/).

