---
metaTitle: "Dictionary"
description: "Introduction to Dictionary, Avoiding KeyError Exceptions, Iterating Over a  Dictionary, Dictionary with default values, Merging dictionaries, Accessing values of a dictionary, Accessing keys and values, Creating  an ordered dictionary, Unpacking dictionaries using the ** operator, Creating a dictionary, The dict() constructor, The trailing comma, All combinations of dictionary values, Dictionaries Example"
---

# Dictionary




## Introduction to Dictionary


A dictionary is an example of a **key value store** also known as **Mapping** in Python. It allows you to store and retrieve elements by referencing a key.  As dictionaries are referenced by key, they have very fast lookups. As they are primarily used for referencing items by key, they are not sorted.

### creating a dict

Dictionaries can be initiated in many ways:

### literal syntax

```py
d = {}                        # empty dict
d = {'key': 'value'}          # dict with initial values

```

```py
# Also unpacking one or multiple dictionaries with the literal syntax is possible

# makes a shallow copy of otherdict
d = {**otherdict}
# also updates the shallow copy with the contents of the yetanotherdict.
d = {**otherdict, **yetanotherdict}

```

### dict comprehension

```py
d = {k:v for k,v in [('key', 'value',)]}

```

see also: [Comprehensions](http://stackoverflow.com/documentation/python/196/comprehensions#t=201604151707279475687)

### built-in class: `dict()`

```py
d = dict()                    # emtpy dict
d = dict(key='value')         # explicit keyword arguments
d = dict([('key', 'value')])  # passing in a list of key/value pairs
# make a shallow copy of another dict (only possible if keys are only strings!)
d = dict(**otherdict)         

```

### modifying a dict

To add items to a dictionary, simply create a new key with a value:

```py
d['newkey'] = 42

```

It also possible to add `list` and `dictionary` as value:

```py
d['new_list'] = [1, 2, 3]
d['new_dict'] = {'nested_dict': 1}

```

To delete an item, delete the key from the dictionary:

```py
del d['newkey']

```



## Avoiding KeyError Exceptions


One common pitfall when using dictionaries is to access a non-existent key. This typically results in a `KeyError` exception

```py
mydict = {}
mydict['not there']

```

```py
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'not there'

```

One way to avoid key errors is to use the `dict.get` method, which allows you to specify a default value to return in the case of an absent key.

```py
value = mydict.get(key, default_value)

```

Which returns `mydict[key]` if it exists, but otherwise returns `default_value`. Note that this doesn't add `key` to `mydict`. So if you want to retain that key value pair, you should use `mydict.setdefault(key, default_value)`, which **does** store the key value pair.

```py
mydict = {}
print(mydict)
# {}
print(mydict.get("foo", "bar"))
# bar
print(mydict)
# {}
print(mydict.setdefault("foo", "bar"))
# bar
print(mydict)
# {'foo': 'bar'}

```

An alternative way to deal with the problem is catching the exception

```py
try:
    value = mydict[key]
except KeyError:
    value = default_value

```

You could also check if the key is `in` the dictionary.

```py
if key in mydict:
    value = mydict[key]
else:
    value = default_value

```

Do note, however, that in multi-threaded environments it is possible for the key to be removed from the dictionary after you check, creating a race condition where the exception can still be thrown.

Another option is to use a subclass of dict, collections.defaultdict, that has a default_factory to create new entries in the dict when given a new_key.



## Iterating Over a  Dictionary


If you use a dictionary as an iterator (e.g. in a `for` statement), it traverses the **keys** of the dictionary. For example:

```py
d = {'a': 1, 'b': 2, 'c':3}
for key in d:
    print(key, d[key])
# c 3
# b 2
# a 1

```

The same is true when used in a comprehension

```py
print([key for key in d])
# ['c', 'b', 'a']

```

The `items()` method can be used to loop over both the **key** and **value** simultaneously:

```py
for key, value in d.items():
    print(key, value)
# c 3
# b 2
# a 1

```

While the `values()` method can be used to iterate over only the values, as would be expected:

```py
for key, value in d.values():
    print(key, value)
    # 3
    # 2
    # 1

```

Here, the methods `keys()`, `values()` and `items()` return lists, and there are the three extra methods `iterkeys()` `itervalues()` and `iteritems()` to return iteraters.



## Dictionary with default values


Available in the standard library as [`defaultdict`](https://docs.python.org/3/library/collections.html#collections.defaultdict)

```py
from collections import defaultdict

d = defaultdict(int)
d['key']                         # 0
d['key'] = 5
d['key']                         # 5

d = defaultdict(lambda: 'empty')
d['key']                         # 'empty'
d['key'] = 'full'
d['key']                         # 'full'

```

[*] Alternatively, if you must use the built-in `dict` class, `using dict.setdefault()` will allow you to create a default whenever you access a key that did not exist before:

```py
>>> d = {}
{}
>>> d.setdefault('Another_key', []).append("This worked!")
>>> d
{'Another_key': ['This worked!']}

```

Keep in mind that if you have many values to add, `dict.setdefault()` will create a new instance of the initial value (in this example a `[]`) every time it's called - which may create unnecessary workloads.

[*] **Python Cookbook, 3rd edition, by David Beazley and Brian K. Jones (O’Reilly). Copyright 2013 David Beazley and Brian Jones, 978-1-449-34037-7.**



## Merging dictionaries


Consider the following dictionaries:

```py
>>> fish = {'name': "Nemo", 'hands': "fins", 'special': "gills"}
>>> dog = {'name': "Clifford", 'hands': "paws", 'color': "red"}

```

### Python 3.5+

```py
>>> fishdog = {**fish, **dog}
>>> fishdog
{'hands': 'paws', 'color': 'red', 'name': 'Clifford', 'special': 'gills'}

```

As this example demonstrates, duplicate keys map to their lattermost value (for example "Clifford" overrides "Nemo").

### Python 3.3+

```py
>>> from collections import ChainMap
>>> dict(ChainMap(fish, dog))
{'hands': 'fins', 'color': 'red', 'special': 'gills', 'name': 'Nemo'}

```

With this technique the foremost value takes precedence for a given key rather than the last ("Clifford" is thrown out in favor of "Nemo").

### Python 2.x, 3.x

```py
>>> from itertools import chain
>>> dict(chain(fish.items(), dog.items()))
{'hands': 'paws', 'color': 'red', 'name': 'Clifford', 'special': 'gills'}

```

This uses the lattermost value, as with the `**`-based technique for merging ("Clifford" overrides "Nemo").

```py
>>> fish.update(dog)
>>> fish
{'color': 'red', 'hands': 'paws', 'name': 'Clifford', 'special': 'gills'}

```

`dict.update` uses the latter dict to overwrite the previous one.



## Accessing values of a dictionary


```py
dictionary = {"Hello": 1234, "World": 5678}
print(dictionary["Hello"])

```

The above code will print `1234`.

The string `"Hello"` in this example is called a **key**. It is used to lookup a value in the `dict` by placing the key in square brackets.

The number `1234` is seen after the respective colon in the `dict` definition. This is called the **value** that `"Hello"` **maps to** in this `dict`.

Looking up a value like this with a key that does not exist will raise a `KeyError` exception, halting execution if uncaught. If we want to access a value without risking a `KeyError`, we can use the `dictionary.get` method. By default if the key does not exist, the method will return `None`. We can pass it a second value to return instead of `None` in the event of a failed lookup.

```py
w = dictionary.get("whatever")
x = dictionary.get("whatever", "nuh-uh")

```

In this example `w` will get the value `None` and `x` will get the value `"nuh-uh"`.



## Accessing keys and values


When working with dictionaries, it's often necessary to access all the keys and values in the dictionary, either in a `for` loop, a list comprehension, or just as a plain list.

Given a dictionary like:

```py
mydict = {
    'a': '1',
    'b': '2'
}

```

You can get a list of keys using the `keys()` method:

```py
print(mydict.keys())
# Python2: ['a', 'b']
# Python3: dict_keys(['b', 'a'])

```

If instead you want a list of values, use the `values()` method:

```py
print(mydict.values())
# Python2: ['1', '2']
# Python3: dict_values(['2', '1'])

```

If you want to work with both the key and its corresponding value, you can use the `items()` method:

```py
print(mydict.items())
# Python2: [('a', '1'), ('b', '2')]
# Python3: dict_items([('b', '2'), ('a', '1')])

```

**NOTE:** Because a `dict` is unsorted, `keys()`, `values()`, and `items()` have no sort order. Use `sort()`, `sorted()`, or an `OrderedDict` if you care about the order that these methods return.

**Python 2/3 Difference:** In Python 3, these methods return special iterable objects, not lists, and are the equivalent of the Python 2 `iterkeys()`, `itervalues()`, and `iteritems()` methods. These objects can be used like lists for the most part, though there are some differences. See [PEP 3106](https://www.python.org/dev/peps/pep-3106/) for more details.



## Creating  an ordered dictionary


You can create an ordered dictionary which will follow a determined order when iterating over the keys in the dictionary.

Use `OrderedDict` from the
`collections` module. This will always return the dictionary elements in the original insertion order when iterated over.

```py
from collections import OrderedDict

d = OrderedDict()
d['first'] = 1
d['second'] = 2
d['third'] = 3
d['last'] = 4

# Outputs "first 1", "second 2", "third 3", "last 4"
for key in d:
    print(key, d[key])

```



## Unpacking dictionaries using the ** operator


You can use the `**` keyword argument unpacking operator to deliver the key-value pairs in a dictionary into a function's arguments. A simplified example from the [official documentation](https://docs.python.org/3/tutorial/controlflow.html#unpacking-argument-lists):

```py
>>>
>>> def parrot(voltage, state, action):
...     print("This parrot wouldn't", action, end=' ')
...     print("if you put", voltage, "volts through it.", end=' ')
...     print("E's", state, "!")
...
>>> d = {"voltage": "four million", "state": "bleedin' demised", "action": "VOOM"}
>>> parrot(**d)

This parrot wouldn't VOOM if you put four million volts through it. E's bleedin' demised !

```

As of Python 3.5 you can also use this syntax to merge an arbitrary number of `dict` objects.

```py
>>> fish = {'name': "Nemo", 'hands': "fins", 'special': "gills"}
>>> dog = {'name': "Clifford", 'hands': "paws", 'color': "red"}
>>> fishdog = {**fish, **dog}
>>> fishdog

{'hands': 'paws', 'color': 'red', 'name': 'Clifford', 'special': 'gills'}

```

As this example demonstrates, duplicate keys map to their lattermost value (for example "Clifford" overrides "Nemo").



## Creating a dictionary


Rules for creating a dictionary:

- Every key must be **unique** (otherwise it will be overridden)
- Every key must be **hashable** (can use the `hash` function to hash it; otherwise `TypeError` will be thrown)
- There is no particular order for the keys.

```py
# Creating and populating it with values
stock = {'eggs': 5, 'milk': 2}

# Or creating an empty dictionary
dictionary = {}

# And populating it after
dictionary['eggs'] = 5
dictionary['milk'] = 2

# Values can also be lists
mydict = {'a': [1, 2, 3], 'b': ['one', 'two', 'three']}

# Use list.append() method to add new elements to the values list
mydict['a'].append(4)   # => {'a': [1, 2, 3, 4], 'b': ['one', 'two', 'three']}
mydict['b'].append('four')  # => {'a': [1, 2, 3, 4], 'b': ['one', 'two', 'three', 'four']}

# We can also create a dictionary using a list of two-items tuples
iterable = [('eggs', 5), ('milk', 2)]
dictionary = dict(iterables)

# Or using keyword argument:
dictionary = dict(eggs=5, milk=2)

# Another way will be to use the dict.fromkeys:
dictionary = dict.fromkeys((milk, eggs))  # => {'milk': None, 'eggs': None}
dictionary = dict.fromkeys((milk, eggs), (2, 5))  # => {'milk': 2, 'eggs': 5}

```



## The dict() constructor


The `dict()` constructor can be used to create dictionaries from keyword arguments, or from a single iterable of key-value pairs, or from a single dictionary and keyword arguments.

```py
dict(a=1, b=2, c=3)                   # {'a': 1, 'b': 2, 'c': 3}
dict([('d', 4), ('e', 5), ('f', 6)])  # {'d': 4, 'e': 5, 'f': 6}
dict([('a', 1)], b=2, c=3)            # {'a': 1, 'b': 2, 'c': 3}
dict({'a' : 1, 'b' : 2}, c=3)         # {'a': 1, 'b': 2, 'c': 3}

```



## The trailing comma


Like lists and tuples, you can include a trailing comma in your dictionary.

```py
role = {"By day": "A typical programmer",
        "By night": "Still a typical programmer", }

```

PEP 8 dictates that you should leave a space between the trailing comma and the closing brace.



## All combinations of dictionary values


```py
options = {
    "x": ["a", "b"],
    "y": [10, 20, 30]
}

```

Given a dictionary such as the one shown above, where there is a list
representing  a set of values to explore for the corresponding key. Suppose
you want to explore `"x"="a"` with `"y"=10`, then `"x"="a"` with`"y"=10`, and so
on until you have explored all possible combinations.

You can create a list that returns all such combinations of values using the  following code.

```py
import itertools

options = {
    "x": ["a", "b"],
    "y": [10, 20, 30]}

keys = options.keys()
values = (options[key] for key in keys)
combinations = [dict(zip(keys, combination)) for combination in itertools.product(*values)]
print combinations

```

This gives us the following list stored in the variable `combinations`:

```py
[{'x': 'a', 'y': 10},
 {'x': 'b', 'y': 10},
 {'x': 'a', 'y': 20},
 {'x': 'b', 'y': 20},
 {'x': 'a', 'y': 30},
 {'x': 'b', 'y': 30}]

```



## Dictionaries Example


Dictionaries map keys to values.

```py
car = {}
car["wheels"] = 4
car["color"] = "Red"
car["model"] = "Corvette"

```

Dictionary values can be accessed by their keys.

```py
print "Little " + car["color"] + " " + car["model"] + "!"
# This would print out "Little Red Corvette!"    

```

Dictionaries can also be created in a JSON style:

```py
car = {"wheels": 4, "color": "Red", "model": "Corvette"}

```

Dictionary values can be iterated over:

```py
for key in car:
  print key + ": " + car[key]

# wheels: 4
# color: Red
# model: Corvette

```



#### Syntax


- mydict = {}
- mydict[k] = value
- value = mydict[k]
- value = mydict.get(k)
- value = mydict.get(k, "default_value")



#### Parameters


|Parameter|Details
|------
|key|The desired key to lookup
|value|The value to set or return



#### Remarks


Helpful items to remember when creating a dictionary:

- Every key must be **unique** (otherwise it will be overridden)
- Every key must be **hashable** (can use the `hash` function to hash it; otherwise `TypeError` will be thrown)
- There is no particular order for the keys.

