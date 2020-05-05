---
metaTitle: "PHP - Types"
description: "Type Comparison, Boolean, Float, Strings, Callable, Null, Type Casting, Resources, Type Juggling, Integers"
---

# Types




## Type Comparison


There are two types of [comparison](http://php.net/manual/en/types.comparisons.php): **loose comparison** with `==` and **strict comparison** with `===`. Strict comparison ensures both the type and value of both sides of the operator are the same.

```php
// Loose comparisons
var_dump(1 == 1); // true
var_dump(1 == "1"); // true
var_dump(1 == true); // true
var_dump(0 == false); // true

// Strict comparisons
var_dump(1 === 1); // true
var_dump(1 === "1"); // false
var_dump(1 === true); // false
var_dump(0 === false); // false

// Notable exception: NAN — it never is equal to anything
var_dump(NAN == NAN); // false
var_dump(NAN === NAN); // false

```

You can also use strong comparison to check if type and value **don't** match using `!==`.

A typical example where the `==` operator is not enough, are functions that can return different types, like [`strpos`](http://php.net/manual/en/function.strpos.php), which returns `false` if the `searchword` is not found, and the match position (`int`) otherwise:

```php
if(strpos('text', 'searchword') == false)
  // strpos returns false, so == comparison works as expected here, BUT:
if(strpos('text bla', 'text') == false)
  // strpos returns 0 (found match at position 0) and 0==false is true.
  // This is probably not what you expect!
if(strpos('text','text') === false)
  // strpos returns 0, and 0===false is false, so this works as expected.

```



## Boolean


[Boolean](http://php.net/manual/en/language.types.boolean.php) is a type, having two values, denoted as `true` or `false`.

This code sets the value of `$foo` as `true` and `$bar` as `false`:

```php
$foo = true;
$bar = false;

```

`true` and `false` are not case sensitive, so `TRUE` and `FALSE` can be used as well, even `FaLsE` is possible. Using lower case is most common and recommended in most code style guides, e.g. [PSR-2](http://www.php-fig.org/psr/psr-2/).

Booleans can be used in if statements like this:

```php
if ($foo) { //same as evaluating if($foo == true)
    echo "true";
}

```

Due to the fact that PHP is weakly typed, if `$foo` above is other than `true` or `false`, it's automatically coerced to a boolean value.<br />
The following values result in `false`:

- a zero value: `0` (integer), `0.0` (float), or `'0'` (string)
- an empty string `''` or array `[]`
- `null` (the content of an unset variable, or assigned to a variable)

Any other value results in `true`.

To avoid this loose comparison, you can enforce strong comparison using `===`, which compares value **and type**. See [Type Comparison](http://stackoverflow.com/documentation/php/232/php-types/3286/type-comparison#t=201608231255370110587) for details.

To convert a type into boolean, you can use the `(bool)`  or `(boolean)` cast before the type.

```php
var_dump((bool) "1"); //evaluates to true

```

or call the [`boolval`](http://php.net/manual/en/function.boolval.php) function:

```php
var_dump( boolval("1") ); //evaluates to true

```

Boolean conversion to a string (note that `false` yields an empty string):

```php
var_dump( (string) true ); // string(1) "1"
var_dump( (string) false ); // string(0) ""

```

Boolean conversion to an integer:

```php
var_dump( (int) true ); // int(1)
var_dump( (int) false ); // int(0)

```

Note that the opposite is also possible:

```php
var_dump((bool) "");        // bool(false)
var_dump((bool) 1);         // bool(true)

```

Also all non-zero will return true:

```php
var_dump((bool) -2);        // bool(true)
var_dump((bool) "foo");     // bool(true)
var_dump((bool) 2.3e5);     // bool(true)
var_dump((bool) array(12)); // bool(true)
var_dump((bool) array());   // bool(false)
var_dump((bool) "false");   // bool(true)

```



## Float


```php
$float = 0.123;

```

> 
For historical reasons "double" is returned by [`gettype()`](http://php.net/manual/en/function.gettype.php) in case of a float, and not simply "float"


Floats are floating point numbers, which allow more output precision than plain integers.

Floats and integers can be used together due to PHP's loose casting of variable types:

```php
$sum = 3 + 0.14;

echo $sum; // 3.14

```

php does not show float as float number like other languages, for example:

```php
$var = 1;
echo ((float) $var); //returns 1 not 1.0

```

### Warning

**Floating point precision**

**(From the [PHP manual page](http://php.net/manual/en/language.types.float.php))**

> 
Floating point numbers have limited precision. Although it depends on the system, PHP typically give a maximum relative error due to rounding in the order of 1.11e-16. Non elementary arithmetic operations may give larger errors, and error **propagation** must be considered when several operations are compounded.


> 
Additionally, rational numbers that are exactly representable as floating point numbers in base 10, like 0.1 or 0.7, do not have an exact representation as floating point numbers in base 2 (binary), which is used internally, no matter the size of the mantissa. Hence, they cannot be converted into their internal binary counterparts without a small loss of precision. This can lead to confusing results: for example, floor((0.1+0.7)*10) will usually return 7 instead of the expected 8, since the internal representation will be something like 7.9999999999999991118....


> 
So never trust floating number results to the last digit, and do not compare floating point numbers directly for equality. If higher precision is necessary, the arbitrary precision math functions and gmp functions are available.




## Strings


A string in PHP is a series of single-byte characters (i.e. there is no native Unicode support) that can be specified in four ways:

### Single Quoted

Displays things almost completely "as is". Variables and most escape sequences will not be interpreted. The exception is that to display a literal single quote, one can escape it with a back slash ', and to display a back slash, one can escape it with another backslash \

```php
$my_string = 'Nothing is parsed, except an escap\'d apostrophe or backslash. $foo\n';
var_dump($my_string);

/*
string(68) "Nothing is parsed, except an escap'd apostrophe or backslash. $foo\n"
*/

```

### Double Quoted

Unlike a single-quoted string, simple variable names and [escape sequences](http://php.net/manual/en/language.types.string.php#language.types.string.syntax.double) in the strings will be evaluated. Curly braces (as in the last example) can be used to isolate complex variable names.

```php
$variable1 = "Testing!";
$variable2 = [ "Testing?", [ "Failure", "Success" ] ];
$my_string = "Variables and escape characters are parsed:\n\n";
$my_string .= "$variable1\n\n$variable2[0]\n\n";
$my_string .= "There are limits: $variable2[1][0]";
$my_string .= "But we can get around them by wrapping the whole variable in braces: {$variable2[1][1]}";
var_dump($my_string);

/*
string(98) "Variables and escape characters are parsed:

Testing!

Testing?

There are limits: Array[0]"

But we can get around them by wrapping the whole variable in braces: Success

*/

```

### Heredoc

In a heredoc string, variable names and escape sequences are parsed in a similar manner to double-quoted strings, though braces are not available for complex variable names. The start of the string is delimited by `<<<`**`identifier`**, and the end by **`identifier`**, where **`identifier`** is any valid PHP name. The ending identifier must appear on a line by itself. No whitespace is allowed before or after the identifier, although like any line in PHP, it must also be terminated by a semicolon.

```php
$variable1 = "Including text blocks is easier";
$my_string = <<< EOF
Everything is parsed in the same fashion as a double-quoted string,
but there are advantages. $variable1; database queries and HTML output
can benefit from this formatting.
Once we hit a line containing nothing but the identifier, the string ends.
EOF;
var_dump($my_string);

/*
string(268) "Everything is parsed in the same fashion as a double-quoted string,
but there are advantages. Including text blocks is easier; database queries and HTML output
can benefit from this formatting.
Once we hit a line containing nothing but the identifier, the string ends."
*/

```

### Nowdoc

A nowdoc string is like the single-quoted version of heredoc, although not even the most basic escape sequences are evaluated. The identifier at the beginning of the string is wrapped in single quotes.

```php
$my_string = <<< 'EOF'
A similar syntax to heredoc but, similar to single quoted strings,
nothing is parsed (not even escaped apostrophes \' and backslashes \\.)
EOF;
var_dump($my_string);

/*
string(116) "A similar syntax to heredoc but, similar to single quoted strings,
nothing is parsed (not even escaped apostrophes \' and backslashes \\.)"
*/

```



## Callable


Callables are anything which can be called as a callback. Things that can be termed a "callback" are as follows:

<li>
Anonymous functions
</li>
<li>
Standard PHP functions (note: **not language constructs**)
</li>
<li>
Static Classes
</li>
<li>
non-static Classes (**using an alternate syntax**)
</li>
<li>
Specific Object/Class Methods
</li>
<li>
Objects themselves, as long as the object is found in key `0` of an array
Example Of referencing an object as an array element:
</li>

```

$obj = new MyClass();
 call_user_func([$obj, 'myCallbackMethod']);

```

Callbacks can be denoted by `callable` [type hint](https://stackoverflow.com/documentation/php/1430/type-hinting) as of PHP 5.4.

```php
$callable = function () {
    return 'value';
};

function call_something(callable $fn) {
    call_user_func($fn);
}

call_something($callable);

```



## Null


PHP represents "no value" with the [`null`](http://php.net/manual/en/language.types.null.php) keyword. It's somewhat similar to the null pointer in C-language and to the NULL value in SQL.

Setting the variable to null:

```php
$nullvar = null; // directly

function doSomething() {} // this function does not return anything
$nullvar = doSomething(); // so the null is assigned to $nullvar

```

Checking if the variable was set to null:

```php
if (is_null($nullvar)) { /* variable is null */ }

if ($nullvar === null) {  /* variable is null */ }

```

### Null vs undefined variable

If the variable was not defined or was unset then any tests against the null will be successful but they will also generate a `Notice: Undefined variable: nullvar`:

```php
$nullvar = null;
unset($nullvar);
if ($nullvar === null) {  /* true but also a Notice is printed */ }
if (is_null($nullvar)) {  /* true but also a Notice is printed */ }

```

Therefore undefined values must be checked with [`isset`](http://php.net/manual/en/function.isset.php):

```php
if (!isset($nullvar)) {  /* variable is null or is not even defined */  }

```



## Type Casting


PHP will generally correctly guess the data type you intend to use from the context it's used in, however sometimes it is useful to manually force a type. This can be accomplished by prefixing the declaration with the name of the required type in parenthesis:

```php
$bool = true;
var_dump($bool); // bool(true)

$int = (int) true;
var_dump($int); // int(1)

$string = (string) true;
var_dump($string); // string(1) "1"
$string = (string) false;
var_dump($string); // string(0) ""

$float = (float) true;
var_dump($float); // float(1)

$array = ['x' => 'y'];
var_dump((object) $array); // object(stdClass)#1 (1) { ["x"]=> string(1) "y" } 

$object = new stdClass();
$object->x = 'y';
var_dump((array) $object); // array(1) { ["x"]=> string(1) "y" }

$string = "asdf";
var_dump((unset)$string); // NULL

```

But be carefull: not all type casts work as one might expect:

```php
// below 3 statements hold for 32-bits systems (PHP_INT_MAX=2147483647)
// an integer value bigger than PHP_INT_MAX is automatically converted to float:
var_dump(       999888777666 ); // float(999888777666)
// forcing to (int) gives overflow:
var_dump((int)  999888777666 ); // int(-838602302)
// but in a string it just returns PHP_INT_MAX
var_dump((int) "999888777666"); // int(2147483647)

var_dump((bool) []);      // bool(false) (empty array)
var_dump((bool) [false]); // bool(true)  (non-empty array)

```



## Resources


A [**resource**](https://secure.php.net/manual/en/language.types.resource.php) is a special type of variable that references an external resource, such as a file, socket, stream, document, or connection.

```php
$file = fopen('/etc/passwd', 'r');

echo gettype($file);
# Out: resource

echo $file;
# Out: Resource id #2

```

There are different (sub-)types of resource. You can check the resource type using [`get_resource_type()`](https://secure.php.net/manual/en/function.get-resource-type.php):

```php
$file = fopen('/etc/passwd', 'r');
echo get_resource_type($file);
#Out: stream

$sock = fsockopen('www.google.com', 80);
echo get_resource_type($sock);
#Out: stream

```

You can find a complete list of built-in resource types [here](https://secure.php.net/manual/en/resource.php).



## Type Juggling


PHP is a weakly-typed language. It does not require explicit declaration of data types.
The context in which the variable is used determines its data type; conversion is done automatically:

```php
$a = "2";             // string 
$a = $a + 2;          // integer (4) 
$a = $a + 0.5;        // float (4.5)
$a = 1 + "2 oranges"; // integer (3)

```



## Integers


Integers in PHP can be natively specified in base 2 (binary), base 8 (octal), base 10 (decimal), or base 16 (hexadecimal.)

```php
$my_decimal = 42;
$my_binary = 0b101010;
$my_octal = 052;
$my_hexadecimal = 0x2a;

echo ($my_binary + $my_octal) / 2;
// Output is always in decimal: 42

```

Integers are 32 or 64 bits long, depending on the platform. The constant `PHP_INT_SIZE` holds integer size in bytes. `PHP_INT_MAX` and (since PHP 7.0) `PHP_INT_MIN` are also available.

```php
printf("Integers are %d bits long" . PHP_EOL, PHP_INT_SIZE * 8);
printf("They go up to %d" . PHP_EOL, PHP_INT_MAX);

```

Integer values are automatically created as needed from floats, booleans, and strings. If an explicit typecast is needed, it can be done with the `(int)` or `(integer)` cast:

```php
$my_numeric_string = "123";
var_dump($my_numeric_string);
// Output: string(3) "123"
$my_integer = (int)$my_numeric_string;
var_dump($my_integer);
// Output: int(123)

```

Integer overflow will be handled by conversion to a float:

```php
$too_big_integer = PHP_INT_MAX + 7;
var_dump($too_big_integer);
// Output: float(9.2233720368548E+18)

```

There is no integer division operator in PHP, but it can be simulated using an implicit cast, which always 'rounds' by just discarding the float-part. As of PHP version 7, an integer division function was added.

```php
$not_an_integer = 25 / 4;
var_dump($not_an_integer);
// Output: float(6.25)
var_dump((int) (25 / 4)); // (see note below)
// Output: int(6)
var_dump(intdiv(25 / 4)); // as of PHP7
// Output: int(6)

```

(Note that the extra parentheses around `(25 / 4)` are needed because the `(int)` cast has higher precedence than the division)

