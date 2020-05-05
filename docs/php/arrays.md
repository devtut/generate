---
metaTitle: "Arrays"
description: "Initializing an Array, Check if key exists, Validating the array type, Creating an array of variables, Checking if a value exists in array, ArrayAccess and Iterator Interfaces"
---

# Arrays


An array is a data structure that stores an arbitrary number of values in a single value. An array in PHP is actually an ordered map, where map is a type that associates values to keys.



## Initializing an Array


An array can be initialized empty:

```php
// An empty array
$foo = array();

// Shorthand notation available since PHP 5.4
$foo = [];

```

An array can be initialized and preset with values:

```php
// Creates a simple array with three strings
$fruit = array('apples', 'pears', 'oranges');

// Shorthand notation available since PHP 5.4
$fruit = ['apples', 'pears', 'oranges'];

```

An array can also be initialized with custom indexes **(also called an associative array)**:

```php
// A simple associative array
$fruit = array(
   'first'  => 'apples',
   'second' => 'pears', 
   'third'  => 'oranges'
);

// Key and value can also be set as follows
$fruit['first'] = 'apples';

// Shorthand notation available since PHP 5.4
$fruit = [
    'first'  => 'apples', 
    'second' => 'pears', 
    'third'  => 'oranges'
];

```

If the variable hasn't been used before, PHP will create it automatically. While convenient, this might make the code harder to read:

```php
$foo[] = 1;     // Array( [0] => 1 )
$bar[][] = 2;   // Array( [0] => Array( [0] => 2 ) )

```

The index will usually continue where you left off. PHP will try to use numeric strings as integers:

```php
$foo = [2 => 'apple', 'melon'];  // Array( [2] => apple, [3] => melon )
$foo = ['2' => 'apple', 'melon']; // same as above
$foo = [2 => 'apple', 'this is index 3 temporarily', '3' => 'melon']; // same as above! The last entry will overwrite the second!

```

To initialize an array with fixed size you can use [`SplFixedArray`](https://secure.php.net/manual/en/class.splfixedarray.php):

```php
$array = new SplFixedArray(3);

$array[0] = 1;
$array[1] = 2;
$array[2] = 3;
$array[3] = 4; // RuntimeException

// Increase the size of the array to 10
$array->setSize(10);

```

Note: An array created using `SplFixedArray` has a reduced memory footprint for large sets of data, but the keys must be integers.

To initialize an array with a dynamic size but with `n` non empty elements (e.g. a placeholder) you can use a loop as follows:

```php
$myArray = array();
$sizeOfMyArray = 5;
$fill = 'placeholder';

for ($i = 0; $i < $sizeOfMyArray; $i++) {
    $myArray[] = $fill;
}

// print_r($myArray); results in the following:
// Array ( [0] => placeholder [1] => placeholder [2] => placeholder [3] => placeholder [4] => placeholder ) 

```

If all your placeholders are the same then you can also create it using the function [`array_fill()`](https://secure.php.net/manual/en/function.array-fill.php):

> 

    array array_fill ( int $start_index , int $num , mixed $value )


This creates and returns an array with `num` entries of `value`, keys starting at `start_index`.

Note: If the `start_index` is negative it will start with the negative index and continue from 0 for the following elements.

```php
$a = array_fill(5, 6, 'banana'); // Array ( [5] => banana, [6] => banana, ..., [10] => banana)
$b = array_fill(-2, 4, 'pear'); // Array ( [-2] => pear, [0] => pear, ..., [2] => pear)

```

Conclusion: With [`array_fill()`](https://secure.php.net/manual/en/function.array-fill.php) you are more limited for what you can actually do. The loop is more flexible and opens you a wider range of opportunities.

Whenever you want an array filled with a range of numbers (e.g. 1-4) you could either append every single element to an array or use the [`range()`](https://secure.php.net/manual/en/function.range.php) function:

> 

    array range ( mixed $start , mixed $end [, number $step = 1 ] )


This function creates an array containing a range of elements. The first two parameters are required, where they set the start and end points of the (inclusive) range. The third parameter is optional and defines the size of the steps being taken. Creating a `range` from `0` to `4` with a `stepsize` of `1`, the resulting array would consist of the following elements: `0`, `1`, `2`, `3`, and `4`. If the step size is increased to `2` (i.e. `range(0, 4, 2)`) then the resulting array would be: `0`, `2`, and `4`.

```php
$array = [];
$array_with_range = range(1, 4);

for ($i = 1; $i <= 4; $i++) {
    $array[] = $i;
}
    
print_r($array); // Array ( [0] => 1 [1] => 2 [2] => 3 [3] => 4 )
print_r($array_with_range); // Array ( [0] => 1 [1] => 2 [2] => 3 [3] => 4 )

```

`range` can work with integers, floats, booleans (which become casted to integers), and strings. Caution should be taken, however, when using floats as arguments due to the floating point precision problem.



## Check if key exists


Use [`array_key_exists()`](http://php.net/manual/en/function.array-key-exists.php) or `isset()` or `!empty()`:

```php
$map = [
    'foo' => 1,
    'bar' => null,
    'foobar' => '',
];

array_key_exists('foo', $map); // true
isset($map['foo']); // true
!empty($map['foo']); // true

array_key_exists('bar', $map); // true
isset($map['bar']); // false
!empty($map['bar']); // false

```

Note that `isset()` treats a `null` valued element as non-existent. Whereas `!empty()` does the same for any element that equals `false` (using a weak comparision; for example, `null`, `''` and `0` are all treated as false by `!empty()`).  While `isset($map['foobar']);` is `true`, `!empty($map['foobar'])` is `false`. This can lead to mistakes (for example, it is easy to forget that the string `'0'` is treated as false) so use of `!empty()` is often frowned upon.

Note also that `isset()` and `!empty()` will work (and return false) if `$map` is not defined at all. This makes them somewhat error-prone to use:

```php
// Note "long" vs "lang", a tiny typo in the variable name.
$my_array_with_a_long_name = ['foo' => true];
array_key_exists('foo', $my_array_with_a_lang_name); // shows a warning
isset($my_array_with_a_lang_name['foo']); // returns false

```

You can also check for ordinal arrays:

```php
$ord = ['a', 'b']; // equivalent to [0 => 'a', 1 => 'b']

array_key_exists(0, $ord); // true
array_key_exists(2, $ord); // false

```

Note that `isset()` has better performance than `array_key_exists()` as the latter is a function and the former a language construct.

You can also use [`key_exists()`](http://php.net/manual/en/function.key-exists.php), which is an alias for `array_key_exists()`.



## Validating the array type


The function [`is_array()`](http://php.net/manual/en/function.is-array.php) returns true if a variable is an array.

```php
$integer = 1337;
$array = [1337, 42];

is_array($integer); // false
is_array($array); // true

```

You can type hint the array type in a function to enforce a parameter type; passing anything else will result in a fatal error.

```php
function foo (array $array) { /* $array is an array */ }

```

You can also use the [`gettype()`](http://php.net/manual/en/function.gettype.php) function.

```php
$integer = 1337;
$array = [1337, 42];

gettype($integer) === 'array'; // false
gettype($array) === 'array'; // true

```



## Creating an array of variables


```php
$username = 'Hadibut';
$email = 'hadibut@example.org';

$variables = compact('username', 'email');
// $variables is now ['username' => 'Hadibut', 'email' => 'hadibut@example.org']

```

This method is often used in frameworks to pass an array of variables between two components.



## Checking if a value exists in array


The function [`in_array()`](http://php.net/manual/en/function.in-array.php) returns true if an item exists in an array.

```php
$fruits = ['banana', 'apple'];

$foo = in_array('banana', $fruits);
// $foo value is true

$bar = in_array('orange', $fruits);
// $bar value is false

```

You can also use the function [`array_search()`](http://php.net/manual/en/function.array-search.php) to get the key of a specific item in an array.

```php
$userdb = ['Sandra Shush', 'Stefanie Mcmohn', 'Michael'];
$pos = array_search('Stefanie Mcmohn', $userdb);
if ($pos !== false) {
    echo "Stefanie Mcmohn found at $pos";
}

```

In PHP 5.5 and later you can use [`array_column()`](http://php.net/manual/en/function.array-column.php) in conjunction with `array_search()`.

This is particularly useful for [checking if a value exists in an associative array](http://stackoverflow.com/questions/6990855/php-check-if-value-and-key-exist-in-multidimensional-array/37935356#37935356):

```php
$userdb = [
    [
        "uid" => '100',
        "name" => 'Sandra Shush',
        "url" => 'urlof100',
    ],
    [
        "uid" => '5465',
        "name" => 'Stefanie Mcmohn',
        "pic_square" => 'urlof100',
    ],
    [
        "uid" => '40489',
        "name" => 'Michael',
        "pic_square" => 'urlof40489',
    ]
];

$key = array_search(40489, array_column($userdb, 'uid'));

```



## ArrayAccess and Iterator Interfaces


Another useful feature is accessing your custom object collections as arrays in PHP. There are two interfaces available in PHP (>=5.0.0) core to support this: `ArrayAccess` and `Iterator`. The former allows you to access your custom objects as array.

**ArrayAccess**

Assume we have a user class and a database table storing all the users. We would like to create a `UserCollection` class that will:

1. allow us to address certain user by their username unique identifier
1. perform basic (not all CRUD, but at least Create, Retrieve and Delete) operations on our users collection

Consider the following source (hereinafter we're using short array creation syntax `[]` available since version 5.4):

```php
class UserCollection implements ArrayAccess {
    protected $_conn;
    
    protected $_requiredParams = ['username','password','email'];
    
    public function __construct() {
        $config = new Configuration();

        $connectionParams = [
            //your connection to the database
        ];
        
        $this->_conn = DriverManager::getConnection($connectionParams, $config);
    }
    
    protected function _getByUsername($username) {
        $ret = $this->_conn->executeQuery('SELECT * FROM `User` WHERE `username` IN (?)',
            [$username]
        )->fetch();
        
        return $ret;
    }
    
    // START of methods required by ArrayAccess interface
    public function offsetExists($offset) {
        return (bool) $this->_getByUsername($offset);
    }

    public function offsetGet($offset) {
        return $this->_getByUsername($offset);
    }

    public function offsetSet($offset, $value) {
        if (!is_array($value)) {
            throw new \Exception('value must be an Array');
        }

        $passed = array_intersect(array_values($this->_requiredParams), array_keys($value));
        if (count($passed) < count($this->_requiredParams)) {
            throw new \Exception('value must contain at least the following params: ' . implode(',', $this->_requiredParams));
        }
        $this->_conn->insert('User', $value);
    }

    public function offsetUnset($offset) {
        if (!is_string($offset)) {
            throw new \Exception('value must be the username to delete');
        }
        if (!$this->offsetGet($offset)) {
            throw new \Exception('user not found');
        }
        $this->_conn->delete('User', ['username' => $offset]);
    }
    // END of methods required by ArrayAccess interface
}

```

then we can :

```php
$users = new UserCollection();

var_dump(empty($users['testuser']),isset($users['testuser']));
$users['testuser'] = ['username' => 'testuser', 
                      'password' => 'testpassword',
                      'email'    => 'test@test.com'];
var_dump(empty($users['testuser']), isset($users['testuser']), $users['testuser']);
unset($users['testuser']);
var_dump(empty($users['testuser']), isset($users['testuser']));

```

which will output the following, assuming there was no `testuser` before we launched the code:

```php
bool(true)
bool(false)
bool(false)
bool(true)
array(17) {
  ["username"]=>
  string(8) "testuser"
  ["password"]=>
  string(12) "testpassword"
  ["email"]=>
  string(13) "test@test.com"
}
bool(true)
bool(false)

```

**IMPORTANT:** `offsetExists` is not called when you check existence of a key with `array_key_exists` function. So the following code will output `false` twice:

```php
var_dump(array_key_exists('testuser', $users));
$users['testuser'] = ['username' => 'testuser', 
                      'password' => 'testpassword',
                      'email'    => 'test@test.com'];
var_dump(array_key_exists('testuser', $users));

```

**Iterator**

Let's extend our class from above with a few functions from `Iterator` interface to allow iterating over it with `foreach` and `while`.

First, we need to add a property holding our current index of iterator, let's add it to the class properties as `$_position`:

```php
// iterator current position, required by Iterator interface methods
protected $_position = 1;

```

Second, let's add `Iterator` interface to the list of interfaces being implemented by our class:

```php
class UserCollection implements ArrayAccess, Iterator {

```

then add the required by the interface functions themselves:

```php
// START of methods required by Iterator interface
public function current () {
    return $this->_getById($this->_position);
}
public function key () {
    return $this->_position;
}
public function next () {
    $this->_position++;
}
public function rewind () {
    $this->_position = 1;
}
public function valid () {
    return null !== $this->_getById($this->_position);
}
// END of methods required by Iterator interface

```

So all in all here is complete source of the class implementing both interfaces. Note that this example is not perfect, because the IDs in the database may not be sequential, but this was written just to give you the main idea: you can address your objects collections in any possible way by implementing `ArrayAccess` and `Iterator` interfaces:

```php
class UserCollection implements ArrayAccess, Iterator {
    // iterator current position, required by Iterator interface methods
    protected $_position = 1;
    
    // <add the old methods from the last code snippet here>
    
    // START of methods required by Iterator interface
    public function current () {
        return $this->_getById($this->_position);
    }
    public function key () {
        return $this->_position;
    }
    public function next () {
        $this->_position++;
    }
    public function rewind () {
        $this->_position = 1;
    }
    public function valid () {
        return null !== $this->_getById($this->_position);
    }
    // END of methods required by Iterator interface
}

```

and a foreach looping through all user objects:

```php
foreach ($users as $user) {
    var_dump($user['id']);
}

```

which will output something like

```php
string(2) "1"
string(2) "2"
string(2) "3"
string(2) "4"
...

```



#### Syntax


- $array = array('Value1', 'Value2', 'Value3'); // Keys default to 0, 1, 2, ...,
- $array = array('Value1', 'Value2', ); // Optional trailing comma
- $array = array('key1' => 'Value1', 'key2' => 'Value2', ); // Explicit keys
- $array = array('key1' => 'Value1', 'Value2', ); // Array ( ['key1'] => Value1 [1] => 'Value2')
- $array = ['key1' => 'Value1', 'key2' => 'Value2', ]; // PHP 5.4+ shorthand
- $array[] = 'ValueX'; // Append 'ValueX' to the end of the array
- $array['keyX'] = 'ValueX'; // Assign 'valueX' to key 'keyX'
- $array += ['keyX' => 'valueX', 'keyY' => 'valueY']; // Adding/Overwrite elements on an existing array



#### Parameters


|Parameter|Detail
|---|---|---|---|---|---|---|---|---|---
|Key|The key is the unique identifier and index of an array. It may be a `string` or an `integer`. Therefore, valid keys would be `'foo', '5', 10, 'a2b', ...`
|Value|For each `key` there is a corresponding value (`null` otherwise **and a notice is emitted upon access**). The value has no restrictions on the input type.



#### Remarks


### See also

- [Manipulating a single array](http://stackoverflow.com/documentation/php/6825/manipulating-an-array)
- [Executing upon an array](http://stackoverflow.com/documentation/php/6826/executing-upon-an-array)
- [Array iteration](http://stackoverflow.com/documentation/php/5727/array-iteration)
- [Processing multiple arrays together](http://stackoverflow.com/documentation/php/6827/processing-multiple-arrays-together)

