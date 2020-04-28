---
metaTitle: "Manipulating an Array"
description: "Filtering an array, Removing elements from an array, Sorting an Array, Adding element to start of array, Whitelist only some array keys, Exchange values with keys, Merge two arrays into one array"
---

# Manipulating an Array




## Filtering an array


In order to filter out values from an array and obtain a new array containing all the values that satisfy the filter condition, you can use the `array_filter` function.

### Filtering non-empty values

The simplest case of filtering is to remove all "empty" values:

```php
$my_array = [1,0,2,null,3,'',4,[],5,6,7,8];
$non_empties = array_filter($my_array); // $non_empties will contain [1,2,3,4,5,6,7,8]; 

```

### Filtering by callback

This time we define our own filtering rule. Suppose we want to get only even numbers:

```php
$my_array = [1,2,3,4,5,6,7,8];

$even_numbers = array_filter($my_array, function($number) {
    return $number % 2 === 0;   
});

```

The `array_filter` function receives the array to be filtered as its first argument, and a callback defining the filter predicate as its second.

### Filtering by index

A third parameter can be provided to the `array_filter` function, which allows to tweak which values are passed to the callback. This parameter can be set to either `ARRAY_FILTER_USE_KEY` or `ARRAY_FILTER_USE_BOTH`, which will result in the callback receiving the key instead of the value for each element in the array, or both value and key as its arguments. For example, if you want to deal with indexes istead of values:

```php
$numbers = [16,3,5,8,1,4,6];

$even_indexed_numbers = array_filter($numbers, function($index) {
    return $index % 2 === 0;
}, ARRAY_FILTER_USE_KEY);

```

### Indexes in filtered array

Note that `array_filter` preserves the original array keys. A common mistake would be to try an use `for` loop over the filtered array:

```php
<?php

$my_array = [1,0,2,null,3,'',4,[],5,6,7,8];
$filtered = array_filter($my_array); 

error_reporting(E_ALL); // show all errors and notices

// innocently looking "for" loop
for ($i = 0; $i < count($filtered); $i++) {
   print $filtered[$i];
}

/*
Output:
1
Notice: Undefined offset: 1
2
Notice: Undefined offset: 3
3
Notice: Undefined offset: 5
4
Notice: Undefined offset: 7
*/

```

This happens because the values which were on positions 1 (there was `0`), 3 (`null`), 5 (empty string `''`) and 7 (empty array `[]`) were removed along with their corresponding index keys.

If you need to loop through the result of a filter on an indexed array, you should first call `array_values` on the result of `array_filter` in order to create a new array with the correct indexes:

```php
$my_array = [1,0,2,null,3,'',4,[],5,6,7,8];
$filtered = array_filter($my_array); 
$iterable = array_values($filtered);

error_reporting(E_ALL); // show all errors and notices

for ($i = 0; $i < count($iterable); $i++) {
   print $iterable[$i];
}

// No warnings!

```



## Removing elements from an array


To remove an element inside an array, e.g. the element with the index 1.

```php
$fruit = array("bananas", "apples", "peaches");
unset($fruit[1]);

```

This will remove the apples from the list, but notice that `unset` does not change the indexes of the remaining elements. So `$fruit` now contains the indexes `0` and `2`.

For associative array you can remove like this:

```php
$fruit = array('banana', 'one'=>'apple', 'peaches');

print_r($fruit);
/*
    Array
    (
        [0] => banana
        [one] => apple
        [1] => peaches
    )
*/

unset($fruit['one']); 

```

Now $fruit is

```php
print_r($fruit);

/*
Array
(
    [0] => banana
    [1] => peaches
)
*/

```

Note that

```php
unset($fruit);

```

unsets the variable and thus removes the whole array, meaning none of its elements are accessible anymore.

### Removing terminal elements

[array_shift()](http://php.net/manual/en/function.array-shift.php) - Shift an element off the beginning of array.

Example:

```

 $fruit = array("bananas", "apples", "peaches");
  array_shift($fruit);
  print_r($fruit);

```

Output:

```

Array
(
    [0] => apples
    [1] => peaches
)

```

[array_pop()](http://php.net/manual/en/function.array-pop.php) - Pop the element off the end of array.

Example:

```

 $fruit = array("bananas", "apples", "peaches");
  array_pop($fruit);
  print_r($fruit);

```

Output:

```

Array
(
    [0] => bananas
    [1] => apples
)

```



## Sorting an Array


There are several sort functions for arrays in php:

### sort()

Sort an array in ascending order by value.

```php
$fruits = ['Zitrone', 'Orange', 'Banane', 'Apfel'];
sort($fruits);
print_r($fruits);

```

results in

```php
Array
(
    [0] => Apfel
    [1] => Banane
    [2] => Orange
    [3] => Zitrone
)

```

### rsort()

Sort an array in descending order by value.

```php
$fruits = ['Zitrone', 'Orange', 'Banane', 'Apfel'];
rsort($fruits);
print_r($fruits);

```

results in

```php
Array
(
    [0] => Zitrone
    [1] => Orange
    [2] => Banane
    [3] => Apfel
)

```

### asort()

Sort an array in ascending order by value and preserve the indecies.

```php
$fruits = [1 => 'lemon', 2 => 'orange',  3 => 'banana', 4 => 'apple'];
asort($fruits);
print_r($fruits);

```

results in

```php
Array
(
    [4] => apple
    [3] => banana
    [1] => lemon
    [2] => orange
)

```

### arsort()

Sort an array in descending order by value and preserve the indecies.

```php
$fruits = [1 => 'lemon', 2 => 'orange',  3 => 'banana', 4 => 'apple'];
arsort($fruits);
print_r($fruits);

```

results in

```php
Array
(
    [2] => orange
    [1] => lemon
    [3] => banana
    [4] => apple
)

```

### ksort()

Sort an array in ascending order by key

```php
$fruits = ['d'=>'lemon', 'a'=>'orange', 'b'=>'banana', 'c'=>'apple'];
ksort($fruits);
print_r($fruits);

```

results in

```php
Array
(
    [a] => orange
    [b] => banana
    [c] => apple
    [d] => lemon
)

```

### krsort()

Sort an array in descending order by key.

```php
$fruits = ['d'=>'lemon', 'a'=>'orange', 'b'=>'banana', 'c'=>'apple'];
krsort($fruits);
print_r($fruits);

```

results in

```php
Array
(
    [d] => lemon
    [c] => apple
    [b] => banana
    [a] => orange
)

```

### natsort()

Sort an array in a way a human being would do (natural order).

```php
$files = ['File8.stack', 'file77.stack', 'file7.stack', 'file13.stack', 'File2.stack'];
natsort($files);
print_r($files);

```

results in

```php
Array
(
    [4] => File2.stack
    [0] => File8.stack
    [2] => file7.stack
    [3] => file13.stack
    [1] => file77.stack
)

```

### natcasesort()

Sort an array in a way a human being would do (natural order), but case intensive

```php
$files = ['File8.stack', 'file77.stack', 'file7.stack', 'file13.stack', 'File2.stack'];
natcasesort($files);
print_r($files);

```

results in

```php
Array
(
    [4] => File2.stack
    [2] => file7.stack
    [0] => File8.stack
    [3] => file13.stack
    [1] => file77.stack
)

```

### shuffle()

Shuffles an array (sorted randomly).

```php
$array = ['aa', 'bb', 'cc'];
shuffle($array);
print_r($array);

```

As written in the description it is random so here only one example in what it can result

```php
Array
(
    [0] => cc
    [1] => bb
    [2] => aa
)

```

### usort()

Sort an array with a user defined comparison function.

```php
function compare($a, $b)
{
    if ($a == $b) {
        return 0;
    }
    return ($a < $b) ? -1 : 1;
}

$array = [3, 2, 5, 6, 1];
usort($array, 'compare');
print_r($array);

```

results in

```php
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
    [3] => 5
    [4] => 6
)

```

### uasort()

Sort an array with a user defined comparison function and preserve the keys.

```php
function compare($a, $b)
{
    if ($a == $b) {
        return 0;
    }
    return ($a < $b) ? -1 : 1;
}

$array = ['a' => 1, 'b' => -3, 'c' => 5, 'd' => 3, 'e' => -5];
uasort($array, 'compare');
print_r($array);

```

results in

```php
Array
(
    [e] => -5
    [b] => -3
    [a] => 1
    [d] => 3
    [c] => 5
)

```

### uksort()

Sort an array by keys with a user defined comparison function.

```php
function compare($a, $b)
{
    if ($a == $b) {
        return 0;
    }
    return ($a < $b) ? -1 : 1;
}

$array = ['ee' => 1, 'g' => -3, '4' => 5, 'k' => 3, 'oo' => -5];

uksort($array, 'compare');
print_r($array);

```

results in

```php
Array
(
    [ee] => 1
    [g] => -3
    [k] => 3
    [oo] => -5
    [4] => 5
)

```



## Adding element to start of array


Sometimes you want to add an element to the beginning of an array **without modifying any of the current elements (**order**) within the array**. Whenever this is the case, you can use [`array_unshift()`](http://php.net/array_unshift).

> 
<p>**`array_unshift()`** prepends passed elements to the front of the array.
Note that the list of elements is prepended as a whole, so that the
prepended elements stay in the same order. All numerical array keys
will be modified to start counting from zero while literal keys won't
be touched.</p>


<sup>**Taken from the [PHP documentation](http://php.net/array_unshift#refsect1-function.array-unshift-description) for `array_unshift()`.**</sup>

If you'd like to achieve this, all you need to do is the following:

```php
$myArray = array(1, 2, 3);

array_unshift($myArray, 4);

```

This will now add `4` as the first element in your array. You can verify this by:

```php
print_r($myArray);

```

<sup>**This returns an array in the following order: `4, 1, 2, 3`.**</sup>

Since **`array_unshift`** forces the array to reset the key-value pairs as the new element let the following entries have the keys **`n+1`** it is smarter to create a new array and append the existing array to the newly created array.

Example:

```php
$myArray = array('apples', 'bananas', 'pears');
$myElement = array('oranges');
$joinedArray = $myElement;

foreach ($myArray as $i) {
  $joinedArray[] = $i;
}

```

Output ($joinedArray):

```php
Array ( [0] => oranges [1] => apples [2] => bananas [3] => pears ) 

```

[**Eaxmple/Demo**](http://www.tehplayground.com/#egwNCrZgr)



## Whitelist only some array keys


When you want to allow only certain keys in your arrays, especially when the array comes from request parameters, you can use `array_intersect_key` together with `array_flip`.

If the `parameters` variable doesn't contain any allowed key, then the `filteredParameters` variable will consist of an empty array.

Since PHP 5.6 you can use [`array_filter`](http://php.net/manual/en/function.array-filter.php#refsect1-function.array-filter-changelog) for this task too, passing the [`ARRAY_FILTER_USE_KEY`](http://php.net/manual/en/array.constants.php#constant.array-filter-use-key) flag as the third parameter:

Using `array_filter` gives the additional flexibility of performing an arbitrary test against the key, e.g. `$allowedKeys` could contain regex patterns instead of plain strings. It also more explicitly states the intention of the code than `array_intersect_key()` combined with `array_flip()`.



## Exchange values with keys


`array_flip` function will exchange all keys with its elements.

```php
$colors = array(
    'one' => 'red',
    'two' => 'blue',
    'three' => 'yellow',
);

array_flip($colors); //will output

array(
    'red' => 'one',
    'blue' => 'two',
    'yellow' => 'three'
)

```



## Merge two arrays into one array


```php
$a1 = array("red","green");
$a2 = array("blue","yellow");
print_r(array_merge($a1,$a2));

/*
    Array ( [0] => red [1] => green [2] => blue [3] => yellow )
*/

```

Associative array:

```php
$a1=array("a"=>"red","b"=>"green");
$a2=array("c"=>"blue","b"=>"yellow");
print_r(array_merge($a1,$a2));
/*
    Array ( [a] => red [b] => yellow [c] => blue )
*/

```


1. Merges the elements of one or more arrays together so that the values of one are appended to the end of the previous one. It returns the resulting array.
1. If the input arrays have the same string keys, then the later value for that key will overwrite the previous one. If, however, the arrays contain numeric keys, the later value will not overwrite the original value, but will be appended.
1. Values in the input array with numeric keys will be renumbered with incrementing keys starting from zero in the result array.

