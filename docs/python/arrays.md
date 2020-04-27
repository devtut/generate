---
metaTitle Arrays
description Access individual elements through indexes, Basic Introduction to Arrays, Append any value to the array using append() method, Insert value in an array using insert() method, Extend python array using extend() method, Add items from list into array using fromlist() method, Remove any array element using remove() method, Remove last array element using pop() method, Fetch any element through its index using index() method, Reverse a python array using reverse() method, Get array buffer information through buffer_info() method, Check for number of occurrences of an element using count() method, Convert array to string using tostring() method, Convert array to a python list with same elements using tolist() method, Append a string to char array using fromstring() method
---

# Arrays




## Access individual elements through indexes


Individual elements can be accessed through indexes. Python arrays are zero-indexed. Here is an example :

```
my_array = array('i', [1,2,3,4,5])
print(my_array[1])
# 2
print(my_array[2])
# 3
print(my_array[0])
# 1

```



## Basic Introduction to Arrays


An array is a data structure that stores values of same data type. In Python, this is the main difference between arrays and lists.

While python lists can contain values corresponding to different data types, arrays in python can only contain values corresponding to same data type. In this tutorial, we will understand the Python arrays with few examples.

If you are new to Python, get started with the Python Introduction article.

To use arrays in python language, you need to import the standard `array` module. This is because array is not a fundamental data type like strings, integer etc. Here is how you can import `array` module in python :

```
from array import *

```

Once you have imported the `array` module, you can declare an array. Here is how you do it:

```
arrayIdentifierName = array(typecode, [Initializers])

```

In the declaration above, `arrayIdentifierName` is the name of array, `typecode` lets python know the type of array and `Initializers` are the values with which array is initialized.

Typecodes are the codes that are used to define the type of array values or the type of array. The table in the parameters section shows the possible values you can use when declaring an array and it's type.

Here is a real world example of python array declaration :

```
my_array = array('i',[1,2,3,4])

```

In the example above, typecode used is `i`. This typecode represents signed integer whose size is 2 bytes.

Here is a simple example of an array containing 5 integers

```
from array import *
my_array = array('i', [1,2,3,4,5])
for i in my_array:
    print(i)
# 1
# 2
# 3
# 4
# 5

```



## Append any value to the array using append() method


```
my_array = array('i', [1,2,3,4,5])
my_array.append(6)
# array('i', [1, 2, 3, 4, 5, 6])

```

Note that the value `6` was appended to the existing array values.



## Insert value in an array using insert() method


We can use the `insert()` method to insert a value at any index of the array. Here is an example :

```
my_array = array('i', [1,2,3,4,5])
my_array.insert(0,0)
#array('i', [0, 1, 2, 3, 4, 5])

```

In the above example, the value 0 was inserted at index 0. Note that the first argument is the index while second argument is the value.



## Extend python array using extend() method


A python array can be extended with more than one value using `extend()` method. Here is an example :

```
my_array = array('i', [1,2,3,4,5])
my_extnd_array = array('i', [7,8,9,10])
my_array.extend(my_extnd_array)
# array('i', [1, 2, 3, 4, 5, 7, 8, 9, 10])

```

We see that the array my_array was extended with values from `my_extnd_array`.



## Add items from list into array using fromlist() method


Here is an example:

```
my_array = array('i', [1,2,3,4,5])
c=[11,12,13]
my_array.fromlist(c)
# array('i', [1, 2, 3, 4, 5, 11, 12, 13])

```

So we see that the values 11,12 and 13 were added from list `c` to `my_array`.



## Remove any array element using remove() method


Here is an example :

```
my_array = array('i', [1,2,3,4,5])
my_array.remove(4)
# array('i', [1, 2, 3, 5])

```

We see that the element 4 was removed from the array.



## Remove last array element using pop() method


`pop` removes the last element from the array. Here is an example :

```
my_array = array('i', [1,2,3,4,5])
my_array.pop()
# array('i', [1, 2, 3, 4])

```

So we see that the last element (`5`) was popped out of array.



## Fetch any element through its index using index() method


`index()` returns first index of the matching value. Remember that arrays are zero-indexed.

```
my_array = array('i', [1,2,3,4,5])
print(my_array.index(5))
# 5
my_array = array('i', [1,2,3,3,5])
print(my_array.index(3))
# 3

```

Note in that second example that only one index was returned, even though the value exists twice in the array



## Reverse a python array using reverse() method


The `reverse()` method does what the name says it will do - reverses the array. Here is an example :

```
my_array = array('i', [1,2,3,4,5])
my_array.reverse()
# array('i', [5, 4, 3, 2, 1])

```



## Get array buffer information through buffer_info() method


This method provides you the array buffer start address in memory and number of elements in array. Here is an example:

```
my_array = array('i', [1,2,3,4,5])
my_array.buffer_info()
(33881712, 5)

```



## Check for number of occurrences of an element using count() method


`count()` will return the number of times and element appears in an array. In the following example we see that the value `3` occurs twice.

```
my_array = array('i', [1,2,3,3,5])
my_array.count(3)
# 2

```



## Convert array to string using tostring() method


`tostring()` converts the array to a string.

```
my_char_array = array('c', ['g','e','e','k'])
# array('c', 'geek')
print(my_char_array.tostring())
# geek

```



## Convert array to a python list with same elements using tolist() method


When you need a Python `list` object, you can utilize the `tolist()` method to convert your array to a list.

```
my_array = array('i', [1,2,3,4,5])
c = my_array.tolist()
# [1, 2, 3, 4, 5]

```



## Append a string to char array using fromstring() method


You are able to append a string to a character array using `fromstring()`

```
my_char_array = array('c', ['g','e','e','k'])
my_char_array.fromstring("stuff")
print(my_char_array)
#array('c', 'geekstuff')

```



#### Parameters


|Parameter|Details
|------
|`b`|Represents signed integer of size 1 byte
|`B`|Represents unsigned integer of size 1 byte
|`c`|Represents character of size 1 byte
|`u`|Represents unicode character of size 2 bytes
|`h`|Represents signed integer of size 2 bytes
|`H`|Represents unsigned integer of size 2 bytes
|`i`|Represents signed integer of size 2 bytes
|`I`|Represents unsigned integer of size 2 bytes
|`w`|Represents unicode character of size 4 bytes
|`l`|Represents signed integer of size 4 bytes
|`L`|Represents unsigned integer of size 4 bytes
|`f`|Represents floating point of size 4 bytes
|`d`|Represents floating point of size 8 bytes

