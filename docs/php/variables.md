---
metaTitle: "Variables"
description: "Accessing A Variable Dynamically By Name (Variable variables), Data Types, Global variable best practices, Default values of uninitialized variables, Variable Value Truthiness and Identical Operator, Getting all defined variables"
---

# Variables




## Accessing A Variable Dynamically By Name (Variable variables)


Variables can be accessed via dynamic variable names. The name of a variable can be stored in another variable, allowing it to be accessed dynamically. Such variables are known as variable variables.

To turn a variable into a variable variable, you put an extra `$` put in front of your variable.

```
$variableName = 'foo';
$foo = 'bar';

// The following are all equivalent, and all output "bar":
echo $foo;
echo ${$variableName};
echo $$variableName;

//similarly,
$variableName  = 'foo';
$$variableName = 'bar';

// The following statements will also output 'bar'
echo $foo; 
echo $$variableName; 
echo ${$variableName};

```

Variable variables are useful for mapping function/method calls:

```
function add($a, $b) {
    return $a + $b;
}

$funcName = 'add';

echo $funcName(1, 2); // outputs 3

```

This becomes particularly helpful in PHP classes:

```
class myClass {
    public function __construct() {
        $functionName = 'doSomething';
        $this->$functionName('Hello World');
    }

    private function doSomething($string) {
        echo $string; // Outputs "Hello World"
    }
}

```

It is possible, but not required to put `$variableName` between `{}`:

```
${$variableName} = $value;

```

The following examples are both equivalent and output "baz":

```
$fooBar = 'baz';
$varPrefix = 'foo';

echo $fooBar;               // Outputs "baz"
echo ${$varPrefix . 'Bar'}; // Also outputs "baz"

```

Using `{}` is only mandatory when the name of the variable is itself an expression, like this:

```
${$variableNamePart1 . $variableNamePart2} = $value;

```

It is nevertheless recommended to always use `{}`, because it's more readable.

While it is not recommended to do so, it is  possible to chain this behavior:

```
$$$$$$$$DoNotTryThisAtHomeKids = $value;

```

> 
It's important to note that the excessive usage of variable variables is considered a bad practice by many developers. Since they're not well-suited for static analysis by modern IDEs, large codebases with many variable variables (or dynamic method invocations) can quickly become difficult to maintain.


### Differences between PHP5 and PHP7

Another reason to always use `{}` or `()`, is that PHP5 and PHP7 have a slightly different way of dealing with dynamic variables, which results in a different outcome in some cases.

In PHP7, dynamic variables, properties, and methods will now be evaluated strictly in left-to-right order, as opposed to the mix of special cases in PHP5. The examples below show how the order of evaluation has changed.

### Case 1 : `$$foo['bar']['baz']`

- PHP5 interpretation : `${$foo['bar']['baz']}`
- PHP7 interpretation : `($$foo)['bar']['baz']`

### Case 2 : `$foo->$bar['baz']`

- PHP5 interpretation : `$foo->{$bar['baz']}`
- PHP7 interpretation : `($foo->$bar)['baz']`

### Case 3 : `$foo->$bar['baz']()`

- PHP5 interpretation : `$foo->{$bar['baz']}()`
- PHP7 interpretation : `($foo->$bar)['baz']()`

### Case 4 : `Foo::$bar['baz']()`

- PHP5 interpretation : `Foo::{$bar['baz']}()`
- PHP7 interpretation : `(Foo::$bar)['baz']()`



## Data Types


There are different data types for different purposes. PHP does not have explicit type definitions, but the type of a variable is determined by the type of the value that is assigned, or by the type that it is casted to. This is a brief overview about the types, for a detailed documentation and examples, see [the PHP types topic](//stackoverflow.com/documentation/php/232/php-types).

There are following data types in PHP: null, boolean, integer, float, string, object, resource and array.

### **Null**

Null can be assigned to any variable. It represents a variable with no value.

```
$foo = null;

```

This invalidates the variable and it's value would be undefined or void if called. The variable is cleared from memory and deleted by the garbage collector.

### **Boolean**

This is the simplest type with only two possible values.

```
$foo = true;
$bar = false;

```

Booleans can be used to control the flow of code.

```
$foo = true;

if ($foo) {
    echo "true";
} else {
    echo "false";
}

```

### **Integer**

An integer is a whole number positive or negative. It can be in used with any number base. The size of an integer is platform-dependent. PHP does not support unsigned integers.

```
$foo = -3;  // negative
$foo = 0;   // zero (can also be null or false (as boolean)
$foo = 123; // positive decimal
$bar = 0123; // octal = 83 decimal
$bar = 0xAB; // hexadecimal = 171 decimal
$bar = 0b1010; // binary = 10 decimal
var_dump(0123, 0xAB, 0b1010); // output: int(83) int(171) int(10)

```

### **Float**

Floating point numbers, "doubles" or simply called "floats" are decimal numbers.

```
$foo = 1.23;
$foo = 10.0;
$bar = -INF;
$bar = NAN;

```

### **Array**

An array is like a list of values. The simplest form of an array is indexed by integer, and ordered by the index, with the first element lying at index 0.

```
$foo = array(1, 2, 3); // An array of integers
$bar = ["A", true, 123 => 5]; // Short array syntax, PHP 5.4+

echo $bar[0];    // Returns "A"
echo $bar[1];    // Returns true
echo $bar[123];  // Returns 5
echo $bar[1234]; // Returns null

```

Arrays can also associate a key other than an integer index to a value. In PHP, all arrays are associative arrays behind the scenes, but when we refer to an 'associative array' distinctly, we usually mean one that contains one or more keys that aren't integers.

```
$array = array();
$array["foo"] = "bar";
$array["baz"] = "quux";
$array[42] = "hello";
echo $array["foo"]; // Outputs "bar"
echo $array["bar"]; // Outputs "quux"
echo $array[42]; // Outputs "hello"

```

### **String**

A string is like an array of characters.

```
$foo = "bar";

```

Like an array, a string can be indexed to return its individual characters:

```
$foo = "bar";
echo $foo[0]; // Prints 'b', the first character of the string in $foo.

```

### **Object**

An object is an instance of a class. Its variables and methods can be accessed with the `->` operator.

```
$foo = new stdClass(); // create new object of class stdClass, which a predefined, empty class
$foo->bar = "baz";
echo $foo->bar; // Outputs "baz"
// Or we can cast an array to an object:
$quux = (object) ["foo" => "bar"];
echo $quux->foo; // This outputs "bar".

```

### **Resource**

Resource variables hold special handles to opened files, database connections, streams, image canvas areas and the like (as it is stated in the [manual](https://secure.php.net/manual/en/language.types.resource.php#language.types.resource.casting)).

```
$fp = fopen('file.ext', 'r'); // fopen() is the function to open a file on disk as a resource.
var_dump($fp); // output: resource(2) of type (stream)

```

To get the type of a variable as a string, use the `gettype()` function:

```
echo gettype(1); // outputs "integer"
echo gettype(true); // "boolean"

```



## Global variable best practices


We can illustrate this problem with the following pseudo-code

```
function foo() {
    global $bob;
    $bob->doSomething();
}

```

Your first question here is an obvious one

> 
Where did `$bob` come from?


Are you confused? Good. You've just learned why globals are confusing and considered a **bad practice**.

If this were a real program, your next bit of fun is to go track down all instances of `$bob` and hope you find the right one (this gets worse if `$bob` is used everywhere). Worse, if someone else goes and defines `$bob` (or you forgot and reused that variable) your code can break (in the above code example, having the wrong object, or no object at all, would cause a fatal error).

Since virtually all PHP programs make use of code like `include('file.php');` your job maintaining code like this becomes exponentially harder the more files you add.

Also, this makes the task of testing your applications very difficult. Suppose you use a global variable to hold your database connection:

```
function foo(\Bar $bob) {
    $bob->doSomething();
}

```

This is **much** easier to understand and maintain. There's no guessing where `$bob` was set up because the caller is responsible for knowing that (it's passing us what we need to know). Better still, we can use [type declarations](http://php.net/manual/en/functions.arguments.php#functions.arguments.type-declaration) to restrict what's being passed.

So we know that `$bob` is either an instance of the `Bar` class, or an instance of a child of `Bar`, meaning we know we can use the methods of that class. Combined with a standard autoloader (available since PHP 5.3), we can now go track down where `Bar` is defined. PHP 7.0 or later includes expanded type declarations, where you can also use scalar types (like `int` or `string`).

**Superglobal variables**

Super globals in PHP are predefined variables, which are always available, can be accessed from any scope throughout the script.

There is no need to do global $variable; to access them within functions/methods, classes or files.

These PHP superglobal variables are listed below:

- [$GLOBALS](http://php.net/manual/en/reserved.variables.globals.php)
- [$_SERVER](http://php.net/manual/en/reserved.variables.server.php)
- [$_REQUEST](http://php.net/manual/en/reserved.variables.request.php)
- [$_POST](http://php.net/manual/en/reserved.variables.post.php)
- [$_GET](http://php.net/manual/en/reserved.variables.get.php)
- [$_FILES](http://php.net/manual/en/reserved.variables.files.php)
- [$_ENV](http://php.net/manual/en/reserved.variables.environment.php)
- [$_COOKIE](http://php.net/manual/en/reserved.variables.cookies.php)
- [$_SESSION](http://php.net/manual/en/reserved.variables.session.php)



## Default values of uninitialized variables


Although not necessary in PHP however it is a very good practice to initialize variables. Uninitialized variables have a default value of their type depending on the context in which they are used:

**Unset AND unreferenced**

```
var_dump($unset_var); // outputs NULL

```

**Boolean**

```
echo($unset_bool ? "true\n" : "false\n"); // outputs 'false' 

```

**String**

```
$unset_str .= 'abc';
var_dump($unset_str); // outputs 'string(3) "abc"'

```

**Integer**

```
$unset_int += 25; // 0 + 25 => 25
var_dump($unset_int); // outputs 'int(25)'

```

**Float/double**

```
$unset_float += 1.25;
var_dump($unset_float); // outputs 'float(1.25)'

```

**Array**

```
$unset_arr[3] = "def";
var_dump($unset_arr); //  outputs array(1) {  [3]=>  string(3) "def" }

```

**Object**

```
$unset_obj->foo = 'bar';
var_dump($unset_obj); // Outputs: object(stdClass)#1 (1) {  ["foo"]=>  string(3) "bar" }

```

Relying on the default value of an uninitialized variable is problematic in the case of including one file into another which uses the same variable name.



## Variable Value Truthiness and Identical Operator


In PHP, variable values have an associated "truthiness" so even non-boolean values will equate to `true` or `false`. This allows any variable to be used in a conditional block, e.g.

```
if ($var == true) { /* explicit version */ }
if ($var) { /* $var == true is implicit */ }

```

Here are some fundamental rules for different types of variable values:

- **Strings** with non-zero length equate to `true` including strings containing only whitepace such as `' '`.
- Empty strings `''` equate to `false`.

```
$var = '';
$var_is_true = ($var == true); // false
$var_is_false = ($var == false); // true

$var = '   ';
$var_is_true = ($var == true); // true
$var_is_false = ($var == false); // false

```

- **Integers** equate to `true` if they are nonzero, while zero equates to `false`.

```
$var = -1;
$var_is_true = ($var == true); // true
$var = 99;
$var_is_true = ($var == true); // true
$var = 0;
$var_is_true = ($var == true); // false

```

- **`null`** equates to `false`

```
$var = null;
$var_is_true = ($var == true); // false
$var_is_false = ($var == false); // true

```

- **Empty** strings `''` and string zero `'0'` equate to `false`.

```
$var = '';
$var_is_true = ($var == true); // false
$var_is_false = ($var == false); // true

$var = '0';
$var_is_true = ($var == true); // false
$var_is_false = ($var == false); // true

```

<li>**Floating-point** values equate to `true` if they are nonzero, while zero values equates to `false`.
<ul>
- `NAN` (PHP's Not-a-Number) equates to `true`, i.e. `NAN == true` is `true`. This is because `NAN` is a **nonzero** floating-point value.
<li>Zero-values include both +0 and -0 as defined by IEEE 754. PHP does not distinguish between +0 and -0 in its double-precision floating-point, i.e. `floatval('0') == floatval('-0')` is `true`.
<ul>
- In fact, `floatval('0') === floatval('-0')`.
- Additionally, both `floatval('0') == false` and `floatval('-0') == false`.

```
$var = NAN;
$var_is_true = ($var == true); // true
$var_is_false = ($var == false); // false

$var = floatval('-0');
$var_is_true = ($var == true); // false
$var_is_false = ($var == false); // true

$var = floatval('0') == floatval('-0');
$var_is_true = ($var == true); // false
$var_is_false = ($var == false); // true

```

**IDENTICAL OPERATOR**

In the [PHP Documentation for Comparison Operators](http://php.net/manual/en/language.operators.comparison.php), there is an Identical Operator `===`. This operator can be used to check whether a variable is **identical** to a reference value:

```
$var = null;
$var_is_null = $var === null; // true
$var_is_true = $var === true; // false
$var_is_false = $var === false; // false

```

It has a corresponding **not identical** operator `!==`:

```
$var = null;
$var_is_null = $var !== null; // false
$var_is_true = $var !== true; // true
$var_is_false = $var !== false; // true

```

The identical operator can be used as an alternative to language functions like `is_null()`.

**USE CASE WITH `strpos()`**

The `strpos($haystack, $needle)` language function is used to locate the index at which `$needle` occurs in `$haystack`, or whether it occurs at all. The `strpos()` function is case sensitive; if case-insensitive find is what you need you can go with `stripos($haystack, $needle)`

The `strpos` & `stripos` function also contains third parameter `offset` (int) which if specified, search will start this number of characters counted from the beginning of the string. Unlike strrpos and strripos, the offset cannot be negative

The function can return:

- `0` if `$needle` is found at the beginning of `$haystack`;
- a non-zero integer specifying the index if `$needle` is found somewhere other than the beginning in `$haystack`;
- and value `false` if `$needle` is **not** found anywhere in `$haystack`.

Because both `0` and `false` have truthiness `false` in PHP but represent distinct situations for `strpos()`, it is important to distinguish between them and use the identical operator `===` to look exactly for `false` and not just a value that equates to `false`.

```
$idx = substr($haystack, $needle);
if ($idx === false) 
{
    // logic for when $needle not found in $haystack
} 
else
{
    // logic for when $needle found in $haystack
}

```

Alternatively, using the **not identical** operator:

```
$idx = substr($haystack, $needle);
if ($idx !== false) 
{
    // logic for when $needle found in $haystack
} 
else
{
    // logic for when $needle not found in $haystack
}

```



## Getting all defined variables


[`get_defined_vars()`](http://php.net/manual/en/function.get-defined-vars.php) returns an array with all the names and values of the variables defined in the scope in which the function is called. If you want to print data you can use standard functions for outputting human-readable data, like [`print_r`](http://php.net/manual/en/function.print-r.php) or [`var_dump`](http://php.net/manual/en/function.var-dump.php).

```
var_dump(get_defined_vars());

```

**Note**: This function usually returns only 4 [superglobals](http://php.net/manual/en/language.variables.superglobals.php): `$_GET`,`$_POST`,`$_COOKIE`,`$_FILES`. Other superglobals are returned only if they have been used somewhere in the code. This is because of the [`auto_globals_jit`](http://php.net/manual/en/ini.core.php#ini.auto-globals-jit) directive which is enabled by default. When it's enabled, the `$_SERVER` and `$_ENV` variables are created when they're first used (Just In Time) instead of when the script starts. If these variables are not used within a script, having this directive on will result in a performance gain.



#### Syntax


- $variable = 'value'; // Assign general variable
- $object->property = 'value'; // Assign an object property
- ClassName::$property = 'value'; // Assign a static class property
- $array[0] = 'value'; // Assign a value to an index of an array
- $array[] = 'value'; // Push an item at the end of an array
- $array['key'] = 'value'; // Assign an array value
- echo $variable; // Echo (print) a variable value
- some_function($variable); // Use variable as function parameter
- unset($variable); // Unset a variable
- $$variable = 'value'; // Assign to a variable variable
- isset($variable); // Check if a variable is set or not
- empty($variable); // Check if a variable is empty or not



#### Remarks


### Type checking

Some of the documentation regarding variables and types mentions that PHP does not use
static typing. This is correct, but PHP does some type checking when it comes to function/method parameters and return values (especially with PHP 7).

You can enforce parameter and return value type-checking by using type-hinting in PHP 7 as follows:

```
<?php

/**
 * Juggle numbers and return true if juggling was
 * a great success.
 */
function numberJuggling(int $a, int $b) : bool
{
    $sum = $a + $b;

    return $sum % 2 === 0;
}

```

> 
**Note:** PHP's [`gettype()`](http://php.net/manual/en/function.gettype.php) for integers and booleans is `integer` and `boolean` respectively. But for type-hinting for such variables you need to use `int` and `bool`. Otherwise PHP won't give you a syntax error, but it will expect `integer` and `boolean` **classes** to be passed.


The above example throws an error in case non-numeric value is given as either the `$a` or `$b` parameter, and if the function returns something else than `true` or `false`. The above example is "loose", as in you can give a float value to `$a` or `$b`. If you wish to enforce strict types, meaning you can only input integers and not floats, add the following to the very beginning of your PHP file:

```
<?php
declare('strict_types=1');

```

Before PHP 7 functions and methods allowed type hinting for the following types:

- `callable` (a callable function or method)
- `array` (any type of array, which can contain other arrays too)
- Interfaces (Fully-Qualified-Class-Name, or FQDN)
- Classes (FQDN)

See also: [Outputting the Value of a Variable](http://stackoverflow.com/documentation/php/6695/outputting-the-value-of-a-variable)

