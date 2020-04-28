---
metaTitle: "Processing Multiple Arrays Together"
description: "Merge or concatenate arrays, Array intersection, Changing a multidimensional array to associative array, Combining two arrays (keys from one, values from another)"
---

# Processing Multiple Arrays Together



## Merge or concatenate arrays


```php
$fruit1 = ['apples', 'pears'];
$fruit2 = ['bananas', 'oranges'];

$all_of_fruits = array_merge($fruit1, $fruit2);
// now value of $all_of_fruits is [0 => 'apples', 1 => 'pears', 2 => 'bananas', 3 => 'oranges']

```

Note that `array_merge` will change numeric indexes, but overwrite string indexes

```php
$fruit1 = ['one' => 'apples',  'two' => 'pears'];
$fruit2 = ['one' => 'bananas', 'two' => 'oranges'];

$all_of_fruits = array_merge($fruit1, $fruit2);
// now value of $all_of_fruits is ['one' => 'bananas', 'two' => 'oranges']

```

`array_merge` overwrites the values of the first array with the values of the second array, if it cannot renumber the index.

You can use the `+` operator to merge two arrays in a way that the values of the first array never get overwritten, but it does not renumber numeric indexes, so you lose values of arrays that have an index that is also used in the first array.

```php
$fruit1 = ['one' => 'apples',  'two' => 'pears'];
$fruit2 = ['one' => 'bananas', 'two' => 'oranges'];

$all_of_fruits = $fruit1 + $fruit2;
// now value of $all_of_fruits is ['one' => 'apples', 'two' => 'pears']

$fruit1 = ['apples', 'pears'];
$fruit2 = ['bananas', 'oranges'];

$all_of_fruits = $fruit1 + $fruit2;
// now value of $all_of_fruits is [0 => 'apples', 1 => 'pears']

```



## Array intersection


The `array_intersect` function will return an array of values that exist in all arrays that were passed to this function.

```php
$array_one = ['one', 'two', 'three'];
$array_two = ['two', 'three', 'four'];
$array_three = ['two', 'three'];

$intersect = array_intersect($array_one, $array_two, $array_three);
// $intersect contains ['two', 'three']

```

Array keys are preserved. Indexes from the original arrays are not.

`array_intersect` only check the values of the arrays. `array_intersect_assoc` function will return intersection of arrays with keys.

```php
$array_one = [1 => 'one',2 => 'two',3 => 'three'];
$array_two = [1 => 'one', 2 => 'two', 3 => 'two', 4 => 'three'];
$array_three = [1 => 'one', 2 => 'two'];

$intersect = array_intersect_assoc($array_one, $array_two, $array_three);
// $intersect contains [1 =>'one',2 => 'two']

```

`array_intersect_key` function only check the intersection of keys. It will returns keys exist in all arrays.

```php
$array_one = [1 => 'one',2 => 'two',3 => 'three'];
$array_two = [1 => 'one', 2 => 'two', 3 => 'four'];
$array_three = [1 => 'one', 3 => 'five'];

$intersect = array_intersect_key($array_one, $array_two, $array_three);
// $intersect contains [1 =>'one',3 => 'three']

```



## Changing a multidimensional array to associative array


If you have a multidimensional array like this:

```php
[
    ['foo',  'bar'],
    ['fizz', 'buzz'],
]

```

And you want to change it to an associative array like this:

```php
[
    'foo'  => 'bar',
    'fizz' => 'buzz',
]

```

You can use this code:

```php
$multidimensionalArray = [
    ['foo',  'bar'],
    ['fizz', 'buzz'],
];
$associativeArrayKeys   = array_column($multidimensionalArray, 0);
$associativeArrayValues = array_column($multidimensionalArray, 1);
$associativeArray       = array_combine($associativeArrayKeys, $associativeArrayValues);

```

Or, you can skip setting `$associativeArrayKeys` and `$associativeArrayValues` and use this simple one liner:

```php
$associativeArray = array_combine(array_column($multidimensionalArray, 0), array_column($multidimensionalArray, 1));

```



## Combining two arrays (keys from one, values from another)


The following example shows how to merge two arrays into one associative array, where the key values will be the items of the first array, and the values will be from the second:

```php
$array_one = ['key1', 'key2', 'key3'];
$array_two = ['value1', 'value2', 'value3'];

$array_three = array_combine($array_one, $array_two);
var_export($array_three);

/* 
    array (
      'key1' => 'value1',
      'key2' => 'value2',
      'key3' => 'value3',
    )
*/

```

