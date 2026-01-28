---
metaTitle: "Ruby - Multidimensional Arrays"
description: "Initializing a 2D array, Initializing a 3D array, Accessing a nested array, Array flattening"
---

# Multidimensional Arrays


Multidimensional Arrays in Ruby are just arrays whose elements are other arrays.

The only catch is that since Ruby arrays can contain elements of mixed types, you must be confident that the array that you are manipulating is effectively composed of other arrays and not, for example, arrays and strings.



## Initializing a 2D array


Let's first recap how to initialize a 1D ruby array of integers:

```ruby
my_array = [1, 1, 2, 3, 5, 8, 13]

```

Being a 2D array simply an array of arrays, you can initialize it like this:

```ruby
my_array = [
  [1, 1, 2, 3, 5, 8, 13],
  [1, 4, 9, 16, 25, 36, 49, 64, 81],
  [2, 3, 5, 7, 11, 13, 17]
]

```



## Initializing a 3D array


You can go a level further down and add a third layer of arrays. The rules don't change:

```ruby
my_array = [
  [
    [1, 1, 2, 3, 5, 8, 13],
    [1, 4, 9, 16, 25, 36, 49, 64, 81],
    [2, 3, 5, 7, 11, 13, 17]
  ],
  [
    ['a', 'b', 'c', 'd', 'e'],
    ['z', 'y', 'x', 'w', 'v']
  ],
  [
    []
  ]
]

```



## Accessing a nested array


Accessing the 3rd element of the first subarray:

```ruby
my_array[1][2]

```



## Array flattening


Given a multidimensional array:

```ruby
my_array = [[1, 2], ['a', 'b']]

```

the operation of flattening is to decompose all array children into the root array:

```ruby
my_array.flatten

# [1, 2, 'a', 'b']

```

