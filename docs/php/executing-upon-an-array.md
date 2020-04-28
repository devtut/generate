---
metaTitle: "Executing Upon an Array"
description: "Applying a function to each element of an array, Split array into chunks, Imploding an array into string, array_reduce, Destructuring arrays using list(), Push a Value on an Array"
---

# Executing Upon an Array



## Applying a function to each element of an array


To apply a function to every item in an array, use `array_map()`.  This will return a new array.

```php
$array = array(1,2,3,4,5);
//each array item is iterated over and gets stored in the function parameter.
$newArray = array_map(function($item) {
    return $item + 1;
}, $array);

```

`$newArray` now is `array(2,3,4,5,6);`.

Instead of using an [anonymous function](http://stackoverflow.com/documentation/php/205/anonymous-functions-closures#t=201607212047461984706), you could use a named function. The above could be written like:

```php
function addOne($item) {
    return $item + 1;
}

$array = array(1, 2, 3, 4, 5);
$newArray = array_map('addOne', $array);

```

If the named function is a class method the call of the function has to include a reference to a class object the method belongs to:

```php
class Example {
    public function addOne($item) {
        return $item + 1;
    }

    public function doCalculation() {
        $array = array(1, 2, 3, 4, 5);
        $newArray = array_map(array($this, 'addOne'), $array);
    }
}

```

Another way to apply a function to every item in an array is `array_walk()` and `array_walk_recursive()`.  The callback passed into these functions take both the key/index and value of each array item.  These functions will not return a new array, instead a boolean for success.  For example, to print every element in a simple array:

```php
$array = array(1, 2, 3, 4, 5);
array_walk($array, function($value, $key) {
    echo $value . ' ';
});
// prints "1 2 3 4 5"

```

The value parameter of the callback may be passed by reference, allowing you to change the value directly in the original array:

```php
$array = array(1, 2, 3, 4, 5);
array_walk($array, function(&$value, $key) {
    $value++;
});

```

`$array` now is `array(2,3,4,5,6);`

For nested arrays, `array_walk_recursive()` will go deeper into each sub-array:

```php
$array = array(1, array(2, 3, array(4, 5), 6);
array_walk_recursive($array, function($value, $key) {
    echo $value . ' ';
});
// prints "1 2 3 4 5 6"

```

**Note**: `array_walk` and `array_walk_recursive` let you change the value of array items, but not the keys.  Passing the keys by reference into the callback is valid but has no effect.



## Split array into chunks


[array_chunk()](http://php.net/manual/en/function.array-chunk.php) splits an array into chunks

Let's say we've following single dimensional array,

```php
$input_array = array('a', 'b', 'c', 'd', 'e');

```

Now using **array_chunk()** on above PHP array,

```php
$output_array = array_chunk($input_array, 2);

```

Above code will make chunks of 2 array elements and create a multidimensional array as follow.

```php
Array
(
    [0] => Array
        (
            [0] => a
            [1] => b
        )

    [1] => Array
        (
            [0] => c
            [1] => d
        )

    [2] => Array
        (
            [0] => e
        )

)

```

If all the elements of the array is not evenly divided by the chunk size, last element of the output array will be remaining elements.

If we pass second argument as less then 1 then **E_WARNING** will be thrown and output array will be **NULL**.

|Parameter|Details
|------
|$array (array)|Input array, the array to work on
|$size (int)|Size of each chunk ( Integer value)
|$preserve_keys (boolean) (optional)|If you want output array to preserve the keys set it to **TRUE** otherwise **FALSE**.



## Imploding an array into string


`implode()` combines all the array values but looses all the key info:

```php
$arr = ['a' => "AA", 'b' => "BB", 'c' => "CC"];

echo implode(" ", $arr); // AA BB CC

```

Imploding keys can be done using `array_keys()` call:

```php
$arr = ['a' => "AA", 'b' => "BB", 'c' => "CC"];

echo implode(" ", array_keys($arr)); // a b c

```

Imploding keys with values is more complex but can be done using functional style:

```php
$arr = ['a' => "AA", 'b' => "BB", 'c' => "CC"];

echo implode(" ", array_map(function($key, $val) { 
    return "$key:$val"; // function that glues key to the value
}, array_keys($arr), $arr)); 

// Output: a:AA b:BB c:CC

```



## array_reduce


`array_reduce` reduces array into a single value. Basically, The `array_reduce` will go through every item with the result from last iteration and produce new value to the next iteration.

Usage:
`array_reduce ($array, function($carry, $item){...}, $defaul_value_of_first_carry)`

- $carry is the result from the last round of iteration.
- $item is the value of current position in the array.

**Sum of array**

```php
$result = array_reduce([1, 2, 3, 4, 5], function($carry, $item){
    return $carry + $item;
});

```

result:`15`

**The largest number in array**

```php
$result = array_reduce([10, 23, 211, 34, 25], function($carry, $item){
        return $item > $carry ? $item : $carry;
});

```

result:`211`

**Is all item more than 100**

```php
$result = array_reduce([101, 230, 210, 341, 251], function($carry, $item){
        return $carry && $item > 100;
}, true); //default value must set true

```

result:`true`

**Is any item less than 100**

```php
$result = array_reduce([101, 230, 21, 341, 251], function($carry, $item){
        return $carry || $item < 100;
}, false);//default value must set false

```

result:`true`

**Like implode($array, $piece)**

```php
$result = array_reduce(["hello", "world", "PHP", "language"], function($carry, $item){
        return !$carry ? $item : $carry . "-" . $item ;
});

```

result:`"hello-world-PHP-language"`

if make a implode method, the source code will be :

```php
function implode_method($array, $piece){
    return array_reduce($array, function($carry, $item) use ($piece) {
            return !$carry ? $item : ($carry . $piece . $item);
    });
}

$result = implode_method(["hello", "world", "PHP", "language"], "-");

```

result:`"hello-world-PHP-language"`



## "Destructuring" arrays using list()


Use [list()](http://php.net/manual/en/function.list.php) to quick assign a list of variable values into an array. See also [compact()](http://stackoverflow.com/documentation/php/204/arrays/15737/creating-an-array-of-variables)

```php
// Assigns to $a, $b and $c the values of their respective array elements in           $array with keys numbered from zero
list($a, $b, $c) = $array;

```

With PHP 7.1 (currently in beta) you will be able to use [short list syntax](https://wiki.php.net/rfc/short_list_syntax):

```php
// Assigns to $a, $b and $c the values of their respective array elements in $array with keys numbered from zero
[$a, $b, $c] = $array;

// Assigns to $a, $b and $c the values of the array elements in $array with the keys "a", "b" and "c", respectively
["a" => $a, "b" => $b, "c" => $c] = $array;

```



## Push a Value on an Array


There are two ways to push an element to an array: `array_push` and `$array[] =`

The [array_push](http://php.net/manual/fr/function.array-push.php) is used like this:

```php
$array = [1,2,3];
$newArraySize = array_push($array, 5, 6); // The method returns the new size of the array
print_r($array); // Array is passed by reference, therefore the original array is modified to contain the new elements

```

This code will print:

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

`$array[] =` is used like this:

```php
$array = [1,2,3];
$array[] = 5;
$array[] = 6;
print_r($array);

```

This code will print:

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

