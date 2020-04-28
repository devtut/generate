---
metaTitle: "Array iteration"
description: "Iterating multiple arrays together, Using an incremental index, Using internal array pointers, Using foreach, Using ArrayObject Iterator"
---

# Array iteration



## Iterating multiple arrays together


Sometimes two arrays of the same length need to be iterated together, for example:

```php
$people = ['Tim', 'Tony', 'Turanga'];
$foods = ['chicken', 'beef', 'slurm'];

```

`array_map` is the simplest way to accomplish this:

```php
array_map(function($person, $food) {
    return "$person likes $food\n";
}, $people, $foods);

```

which will output:

```php
Tim likes chicken
Tony likes beef
Turanga likes slurm

```

This can be done through a common index:

```php
assert(count($people) === count($foods));
for ($i = 0; $i < count($people); $i++) {
    echo "$people[$i] likes $foods[$i]\n";
}

```

If the two arrays don't have the incremental keys, `array_values($array)[$i]` can be used to replace `$array[$i]`.

If both arrays have the same order of keys, you can also use a foreach-with-key loop on one of the arrays:

```php
foreach ($people as $index => $person) {
    $food = $foods[$index];
    echo "$person likes $food\n";
}

```

Separate arrays can only be looped through if they are the same length and also have the same key name. This means if you don't supply a key and they are numbered, you will be fine, or if you name the keys and put them in the same order in each array.

You can also use `array_combine`.

```php
$combinedArray = array_combine($people, $foods);
// $combinedArray = ['Tim' => 'chicken', 'Tony' => 'beef', 'Turanga' => 'slurm'];

```

Then you can loop through this by doing the same as before:

```php
foreach ($combinedArray as $person => $meal) {
    echo "$person likes $meal\n";
}

```



## Using an incremental index


This method works by incrementing an integer from 0 to the greatest index in the array.

```php
$colors = ['red', 'yellow', 'blue', 'green'];
for ($i = 0; $i < count($colors); $i++) {
    echo 'I am the color ' . $colors[$i] . '<br>';
}

```

This also allows iterating an array in reverse order without using `array_reverse`, which may result in overhead if the array is large.

```php
$colors = ['red', 'yellow', 'blue', 'green'];
for ($i = count($colors) - 1; $i >= 0; $i--) {
    echo 'I am the color ' . $colors[$i] . '<br>';
}

```

You can skip or rewind the index easily using this method.

```php
$array = ["alpha", "beta", "gamma", "delta", "epsilon"];
for ($i = 0; $i < count($array); $i++) {
    echo $array[$i], PHP_EOL;
    if ($array[$i] === "gamma") {
        $array[$i] = "zeta";
        $i -= 2;
    } elseif ($array[$i] === "zeta") {
        $i++;
    }
}

```

Output:

```php
alpha
beta
gamma
beta
zeta
epsilon

```

For arrays that do not have incremental indices (including arrays with indices in reverse order, e.g. `[1 => "foo", 0 => "bar"]`, `["foo" => "f", "bar" => "b"]`), this cannot be done directly. `array_values` or `array_keys` can be used instead:

```php
$array = ["a" => "alpha", "b" => "beta", "c" => "gamma", "d" => "delta"];
$keys = array_keys($array);
for ($i = 0; $i < count($array); $i++) {
    $key = $keys[$i];
    $value = $array[$key];
    echo "$value is $key\n";
}

```



## Using internal array pointers


Each array instance contains an internal pointer. By manipulating this pointer, different elements of an array can be retrieved from the same call at different times.

### Using [`each`](http://php.net/each)

Each call to `each()` returns the key and value of the current array element, and increments the internal array pointer.

```php
$array = ["f" => "foo", "b" => "bar"];
while (list($key, $value) = each($array)) {
    echo "$value begins with $key";
}

```

### Using [`next`](http://php.net/next)

```php
$array = ["Alpha", "Beta", "Gamma", "Delta"];
while (($value = next($array)) !== false) {
    echo "$value\n";
}

```

Note that this example assumes no elements in the array are identical to boolean `false`. To prevent such assumption, use [`key`](http://php.net/key) to check if the internal pointer has reached the end of the array:

```php
$array = ["Alpha", "Beta", "Gamma", "Delta"];
while (key($array) !== null) {
    echo current($array) . PHP_EOL;
    next($array);
}

```

This also facilitates iterating an array without a direct loop:

```php
class ColorPicker {
    private $colors = ["#FF0064", "#0064FF", "#64FF00", "#FF6400", "#00FF64", "#6400FF"];
    public function nextColor() : string {
        $result = next($colors);
        // if end of array reached
        if (key($colors) === null) {
            reset($colors);
        }
        return $result;
    }
}

```



## Using foreach


### Direct loop

```php
foreach ($colors as $color) {
    echo "I am the color $color<br>";
}

```

### Loop with keys

```php
$foods = ['healthy' => 'Apples', 'bad' => 'Ice Cream'];
foreach ($foods as $key => $food) {
    echo "Eating $food is $key";
}

```

### Loop by reference

In the `foreach` loops in the above examples, modifying the value (`$color` or `$food`) directly doesn't change its value in the array. The `&` operator is required so that the value is a reference pointer to the element in the array.

```php
$years = [2001, 2002, 3, 4];
foreach ($years as &$year) {
    if ($year < 2000) $year += 2000;
}

```

This is similar to:

```php
$years = [2001, 2002, 3, 4];
for($i = 0; $i < count($years); $i++) { // these two lines
    $year = &$years[$i];                // are changed to foreach by reference
    if($year < 2000) $year += 2000;
}

```

### Concurrency

PHP arrays can be modified in any ways during iteration without concurrency problems (unlike e.g. Java `List`s). If the array is iterated by reference, later iterations will be affected by changes to the array. Otherwise, the changes to the array will not affect later iterations (as if you are iterating a copy of the array instead). Compare looping by value:

```php
$array = [0 => 1, 2 => 3, 4 => 5, 6 => 7];
foreach ($array as $key => $value) {
    if ($key === 0) {
        $array[6] = 17;
        unset($array[4]);
    }
    echo "$key => $value\n";
}

```

Output:

```php
0 => 1
2 => 3
4 => 5
6 => 7

```

But if the array is iterated with reference,

```php
$array = [0 => 1, 2 => 3, 4 => 5, 6 => 7];
foreach ($array as $key => &$value) {
    if ($key === 0) {
        $array[6] = 17;
        unset($array[4]);
    }
    echo "$key => $value\n";
}

```

Output:

```php
0 => 1
2 => 3
6 => 17

```

The key-value set of `4 => 5` is no longer iterated, and `6 => 7` is changed to `6 => 17`.



## Using ArrayObject Iterator


Php arrayiterator allows you to modify and unset the values while iterating over arrays and objects.

Example:

```php
$array = ['1' => 'apple', '2' => 'banana', '3' => 'cherry'];

$arrayObject = new ArrayObject($array);

$iterator = $arrayObject->getIterator();

for($iterator; $iterator->valid(); $iterator->next()) {
    echo $iterator->key() . ' => ' . $iterator->current() . "</br>";
}

```

Output:

```php
1 => apple
2 => banana
3 => cherry

```



#### Syntax


- for ($i = 0; $i < count($array); $i++) { incremental_iteration(); }
- for ($i = count($array) - 1; $i >= 0; $i--) { reverse_iteration(); }
- foreach ($data as $datum) { }
- foreach ($data as $key => $datum) { }
- foreach ($data as &$datum) { }



#### Remarks


### Comparison of methods to iterate an array

|Method|Advantage
|------
|`foreach`|The simplest method to iterate an array.
|`foreach` by reference|Simple method to iterate and change elements of an array.
|`for` with incremental index|Allows iterating the array in a free sequence, e.g. skipping or reversing multiple elements
|Internal array pointers|It is no longer necessary to use a loop (so that it can iterate once every function call, signal receive, etc.)

