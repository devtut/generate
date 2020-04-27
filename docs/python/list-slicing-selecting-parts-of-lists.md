---
metaTitle: List slicing (selecting parts of lists)
description: Using the third "step" argument, Selecting a sublist from a list, Reversing a list with slicing, Shifting a list using slicing
---

# List slicing (selecting parts of lists)



## Using the third "step" argument


```
lst = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']

lst[::2]
# Output: ['a', 'c', 'e', 'g']

lst[::3]
# Output: ['a', 'd', 'g']

```



## Selecting a sublist from a list


```
lst = ['a', 'b', 'c', 'd', 'e']

lst[2:4]
# Output: ['c', 'd']

lst[2:]
# Output: ['c', 'd', 'e']

lst[:4]
# Output: ['a', 'b', 'c', 'd']

```



## Reversing a list with slicing


```
a = [1, 2, 3, 4, 5]

# steps through the list backwards (step=-1)
b = a[::-1]

# built-in list method to reverse 'a'
a.reverse()

if a = b:
    print(True)

print(b)

# Output: 
# True
# [5, 4, 3, 2, 1]

```



## Shifting a list using slicing


```
def shift_list(array, s):
    """Shifts the elements of a list to the left or right.

    Args:
        array - the list to shift
        s - the amount to shift the list ('+': right-shift, '-': left-shift)

    Returns:
        shifted_array - the shifted list
    """
    # calculate actual shift amount (e.g., 11 --> 1 if length of the array is 5)
    s %= len(array)

    # reverse the shift direction to be more intuitive
    s *= -1

    # shift array with list slicing
    shifted_array = array[s:] + array[:s]

    return shifted_array

my_array = [1, 2, 3, 4, 5]

# negative numbers
shift_list(my_array, -7)
>>> [3, 4, 5, 1, 2]

# no shift on numbers equal to the size of the array
shift_list(my_array, 5)
>>> [1, 2, 3, 4, 5]

# works on positive numbers
shift_list(my_array, 3)
>>> [3, 4, 5, 1, 2]

```



#### Syntax


- a[start:end] # items start through end-1
- a[start:]    # items start through the rest of the array
- a[:end]      # items from the beginning through end-1
- a[start:end:step] # start through not past end, by step
- a[:]         # a copy of the whole array
- [source](http://stackoverflow.com/questions/509211/explain-pythons-slice-notation)



#### Remarks


- `lst[::-1]` gives you a reversed copy of the list
- `start` or `end` may be a negative number, which means it counts from the end of the array instead of the beginning. So:

```
a[-1]    # last item in the array
a[-2:]   # last two items in the array
a[:-2]   # everything except the last two items

```

([source](http://stackoverflow.com/questions/509211/explain-pythons-slice-notation))

