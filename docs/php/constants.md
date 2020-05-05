---
metaTitle: "PHP - Constants"
description: "Defining constants, Class Constants, Checking if constant is defined, Constant arrays, Using constants"
---

# Constants



## Defining constants


Constants are created using the `const` statement or the `define` function. The convention is to use UPPERCASE letters for constant names.

### Define constant using explicit values

```php
const PI = 3.14; // float
define("EARTH_IS_FLAT", false); // boolean
const "UNKNOWN" = null; // null
define("APP_ENV", "dev"); // string
const MAX_SESSION_TIME = 60 * 60; // integer, using (scalar) expressions is ok

const APP_LANGUAGES = ["de", "en"]; // arrays


define("BETTER_APP_LANGUAGES", ["lu", "de"]); // arrays

```

### Define constant using another constant

if you have one constant you can define another one based on it:

```php
const TAU = PI * 2;
define("EARTH_IS_ROUND", !EARTH_IS_FLAT);
define("MORE_UNKNOWN", UNKNOWN);
define("APP_ENV_UPPERCASE", strtoupper(APP_ENV)); // string manipulation is ok too
// the above example (a function call) does not work with const:
// const TIME = time(); # fails with a fatal error! Not a constant scalar expression
define("MAX_SESSION_TIME_IN_MINUTES", MAX_SESSION_TIME / 60);

const APP_FUTURE_LANGUAGES = [-1 => "es"] + APP_LANGUAGES; // array manipulations


define("APP_BETTER_FUTURE_LANGUAGES", array_merge(["fr"], APP_BETTER_LANGUAGES));

```

### Reserved constants

Some constant names are reserved by PHP and cannot be redefined. All these examples will fail:

```php
define("true", false); // internal constant
define("false", true); // internal constant
define("CURLOPT_AUTOREFERER", "something"); // will fail if curl extension is loaded

```

And a Notice will be issued:

```php
Constant ... already defined in ...

```

### Conditional defines

If you have several files where you may define the same variable (for example, your main config then your local config) then following syntax may help avoiding conflicts:

```php
defined("PI") || define("PI", 3.1415); // "define PI if it's not yet defined"

```

### `const` vs `define`

`define` is a runtime expression while `const` a compile time one.

Thus `define` allows for dynamic values (i.e. function calls, variables etc.) and even dynamic names and conditional definition. It however is always defining relative to the root namespace.

`const` is static (as in allows only operations with other constants, scalars or arrays, and only a restricted set of them, the so called **constant scalar expressions**, i.e. arithmetic, logical and comparison operators as well as array dereferencing), but are automatically namespace prefixed with the currently active namespace.

`const` only supports other constants and scalars as values, and no operations.



## Class Constants


Constants can be defined inside classes using a `const` keyword.

```php
class Foo {
    const BAR_TYPE = "bar";

    // reference from inside the class using self::
    public function myMethod() {
        return self::BAR_TYPE;
    }
}

// reference from outside the class using <ClassName>::
echo Foo::BAR_TYPE;

```

This is useful to store types of items.

```php
<?php

class Logger {
    const LEVEL_INFO = 1;
    const LEVEL_WARNING = 2;
    const LEVEL_ERROR = 3;

    // we can even assign the constant as a default value
    public function log($message, $level = self::LEVEL_INFO) {
        echo "Message level " . $level . ": " . $message;
    }
}

$logger = new Logger();
$logger->log("Info"); // Using default value
$logger->log("Warning", $logger::LEVEL_WARNING); // Using var
$logger->log("Error", Logger::LEVEL_ERROR); // using class

```



## Checking if constant is defined


### Simple check

To check if constant is defined use the `defined` function. Note that this function doesn't care about constant's value, it only cares if the constant exists or not. Even if the value of the constant is `null` or `false` the function will still return `true`.

```php
<?php

define("GOOD", false);

if (defined("GOOD")) {
    print "GOOD is defined" ; // prints "GOOD is defined"

    if (GOOD) {
        print "GOOD is true" ; // does not print anything, since GOOD is false
    }
}

if (!defined("AWESOME")) {
   define("AWESOME", true); // awesome was not defined. Now we have defined it 
}

```

Note that constant becomes "visible" in your code only **after** the line where you have defined it:

```php
<?php

if (defined("GOOD")) {
   print "GOOD is defined"; // doesn't print anyhting, GOOD is not defined yet.
}

define("GOOD", false);

if (defined("GOOD")) {
   print "GOOD is defined"; // prints "GOOD is defined"
}

```

### Getting all defined constants

To get all defined constants including those created by PHP use the `get_defined_constants` function:

```php
<?php

$constants = get_defined_constants();
var_dump($constants); // pretty large list

```

To get only those constants that were defined by your app call the function at the beginning and at the end of your script (normally after the bootstrap process):

```php
<?php

$constants = get_defined_constants();

define("HELLO", "hello"); 
define("WORLD", "world"); 

$new_constants = get_defined_constants();

$myconstants = array_diff_assoc($new_constants, $constants);
var_export($myconstants); 
   
/* 
Output:

array (
  'HELLO' => 'hello',
  'WORLD' => 'world',
) 
*/

```

It's sometimes useful for debugging



## Constant arrays


Arrays can be used as plain constants and class constants from version PHP 5.6 onwards:

### Class constant example

```php
class Answer {
    const C = [2,4];
}

print Answer::C[1] . Answer::C[0]; // 42

```

### Plain constant example

```php
const ANSWER = [2,4];
print ANSWER[1] . ANSWER[0]; // 42

```

Also from version PHP 7.0 this functionality was ported to the [`define`](http://php.net/manual/en/function.define.php) function for plain constants.

```php
define('VALUES', [2, 3]);
define('MY_ARRAY', [
    1,
    VALUES,
]);

print MY_ARRAY[1][1]; // 3

```



## Using constants


To use the constant simply use its name:

```php
if (EARTH_IS_FLAT) {
    print "Earth is flat";
}

print APP_ENV_UPPERCASE;

```

or if you don't know the name of the constant in advance, use the `constant` function:

```php
// this code is equivalent to the above code
$const1 = "EARTH_IS_FLAT";
$const2 = "APP_ENV_UPPERCASE";

if (constant($const1)) {
    print "Earth is flat";
}

print constant($const2);

```



#### Syntax


- define ( string $name , mixed $value [, bool $case_insensitive = false ] )
- const CONSTANT_NAME = VALUE;



#### Remarks


[Constants](http://php.net/manual/en/language.constants.php) are used to store the values that are not supposed to be changed later. They also are often used to store the configuration parameters especially those which define the environment (dev/production).

Constants have types like variables but not all types can be used to initialize a constant. Objects and resources cannot be used as values for constants at all. Arrays can be used as constants starting from PHP 5.6

Some constant names are reserved by PHP. These include `true`, `false`, `null` as well as many module-specific constants.

Constants are usually named using uppercase letters.

