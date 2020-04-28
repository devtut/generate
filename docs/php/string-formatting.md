---
metaTitle: "String formatting"
description: "String interpolation, Extracting/replacing substrings"
---

# String formatting



## String interpolation


You can also use interpolation to interpolate (**insert**) a variable within a string. Interpolation works in double quoted strings and the heredoc syntax only.

```php
$name = 'Joel';

// $name will be replaced with `Joel`
echo "<p>Hello $name, Nice to see you.</p>";
#                ↕
#>   "<p>Hello Joel, Nice to see you.</p>"

// Single Quotes: outputs $name as the raw text (without interpreting it)
echo 'Hello $name, Nice to see you.'; # Careful with this notation
#> "Hello $name, Nice to see you."

```

The [**complex (curly) syntax**](http://php.net/manual/en/language.types.string.php#language.types.string.parsing.complex) format provides another option which requires that you wrap your variable within curly braces `{}`. This can be useful when embedding variables within textual content and helping to prevent  possible ambiguity between textual content and variables.

```php
$name = 'Joel';

// Example using the curly brace syntax for the variable $name
echo "<p>We need more {$name}s to help us!</p>";
#> "<p>We need more Joels to help us!</p>"

// This line will throw an error (as `$names` is not defined)
echo "<p>We need more $names to help us!</p>";
#> "Notice: Undefined variable: names"

```

The `{}` syntax only interpolates variables starting with a `$` into a string. The `{}` syntax **does not** evaluate arbitrary PHP expressions.

```php
// Example tying to interpolate a PHP expression
echo "1 + 2 = {1 + 2}";
#> "1 + 2 = {1 + 2}"

// Example using a constant
define("HELLO_WORLD", "Hello World!!");
echo "My constant is {HELLO_WORLD}";
#> "My constant is {HELLO_WORLD}"

// Example using a function
function say_hello() {
    return "Hello!";
};
echo "I say: {say_hello()}";
#> "I say: {say_hello()}"

```

However, the `{}` syntax does evaluate any array access, property access and function/method calls on variables, array elements or properties:

```php
// Example accessing a value from an array — multidimensional access is allowed
$companions = [0 => ['name' => 'Amy Pond'], 1 => ['name' => 'Dave Random']];
echo "The best companion is: {$companions[0]['name']}";
#> "The best companion is: Amy Pond"

// Example of calling a method on an instantiated object
class Person {
  function say_hello() {
    return "Hello!";
  }
}

$max = new Person();

echo "Max says: {$max->say_hello()}";
#> "Max says: Hello!"

// Example of invoking a Closure — the parameter list allows for custom expressions
$greet = function($num) {
    return "A $num greetings!";
};
echo "From us all: {$greet(10 ** 3)}";
#> "From us all: A 1000 greetings!"

```

Notice that the dollar `$` sign can appear after the opening curly brace `{` as the above examples, or, like in Perl or Shell Script, can appear before it:

```php
$name = 'Joel';

// Example using the curly brace syntax with dollar sign before the opening curly brace
echo "<p>We need more ${name}s to help us!</p>";
#> "<p>We need more Joels to help us!</p>"

```

> 
The `Complex (curly) syntax` is not called as such because it's complex, but rather because it allows for the use of '**complex expressions**'. [Read more about `Complex (curly) syntax`](http://php.net/manual/en/language.types.string.php#language.types.string.parsing.complex)




## Extracting/replacing substrings


Single characters can be extracted using array (square brace) syntax as well as curly brace syntax. These two syntaxes will only return a single character from the string. If more than one character is needed, a function will be required, i.e.- [substr](http://php.net/manual/en/function.substr.php)

Strings, like everything in PHP, are `0`-indexed.

```php
$foo = 'Hello world';

$foo[6]; // returns 'w'
$foo{6}; // also returns 'w'

substr($foo, 6, 1); // also returns 'w'
substr($foo, 6, 2); // returns 'wo'

```

Strings can also be changed one character at a time using the same square brace and curly brace syntax. Replacing more than one character requires a function, i.e.- [substr_replace](http://php.net/manual/en/function.substr-replace.php)

```php
$foo = 'Hello world';

$foo[6] = 'W'; // results in $foo = 'Hello World'
$foo{6} = 'W'; // also results in $foo = 'Hello World'

substr_replace($foo, 'W', 6, 1); // also results in $foo = 'Hello World'
substr_replace($foo, 'Whi', 6, 2); // results in 'Hello Whirled'
// note that the replacement string need not be the same length as the substring replaced

```

