---
metaTitle: "Functional Programming"
description: "Closures, Assignment to variables, Objects as a function, Using outside variables, Using built-in functions as callbacks, Anonymous function, Pure functions, Common functional methods in PHP, Scope, Passing a callback function as a parameter"
---

# Functional Programming


PHP's functional programming relies on functions. Functions in PHP provide organized, reusable code to perform a set of actions. Functions simplify the coding process, prevent redundant logic, and make code easier to follow. This topic describes the declaration and utilization of functions, arguments, parameters, return statements and scope in PHP.



## Closures


**A closure is an anonymous function that can't access outside scope.**

When defining an anonymous function as such, you're creating a "namespace" for that function. It currently only has access to that namespace.

```
$externalVariable = "Hello";
$secondExternalVariable = "Foo"; 
$myFunction = function() { 
  
  var_dump($externalVariable, $secondExternalVariable); // returns two error notice, since the variables aren´t defined 

}

```

It doesn't have access to any external variables. To grant this permission for this namespace to access external variables, you need to introduce it via closures (**`use()`**).

```
$myFunction = function() use($externalVariable, $secondExternalVariable) {
   var_dump($externalVariable, $secondExternalVariable); // Hello Foo
}

```

This is heavily attributed to PHP's tight variable scoping - **If a variable isn't defined within the scope, or isn't brought in with `global` then it does not exist.**

Also note:

> 
<p>Inheriting variables from the parent scope is not the same as using
global variables. Global variables exist in the global scope, which is
the same no matter what function is executing.</p>


> 
<p>The parent scope of a
closure is the function in which the closure was declared (not
necessarily the function it was called from).</p>


<sup>**Taken from the [PHP Documentation for Anonymous Functions](http://php.net/manual/en/functions.anonymous.php)**</sup>

In PHP, closures use an **early-binding** approach. This means that variables passed to the closure's namespace using `use` keyword will have the same values when the closure was defined.

To change this behavior you should pass the variable **by-reference**.

```
$rate = .05;

// Exports variable to closure's scope
$calculateTax = function ($value) use ($rate) {
    return $value * $rate;
};

$rate = .1; 

print $calculateTax(100); // 5

```

```
$rate = .05;

// Exports variable to closure's scope
$calculateTax = function ($value) use (&$rate) { // notice the & before $rate
    return $value * $rate;
};

$rate = .1;

print $calculateTax(100); // 10

```

Default arguments are not implicitly required when defining anonymous functions with/without closures.

```
$message = 'Im yelling at you';

$yell = function() use($message) {
    echo strtoupper($message);
};

$yell(); // returns: IM YELLING AT YOU

```



## Assignment to variables


[Anonymous functions](http://php.net/manual/en/functions.anonymous.php) can be assigned to variables for use as parameters where a callback is expected:

```
$uppercase = function($data) {
    return strtoupper($data);
};

$mixedCase = ["Hello", "World"];
$uppercased = array_map($uppercase, $mixedCase);
print_r($uppercased);

```

These variables can also be used as standalone function calls:

```
echo $uppercase("Hello world!"); // HELLO WORLD!

```



## Objects as a function


```
class SomeClass {
    public function __invoke($param1, $param2) {
        // put your code here
    }
}

$instance = new SomeClass();
$instance('First', 'Second'); // call the __invoke() method

```

An object with an `__invoke` method can be used exactly as any other function.

The `__invoke` method will have access to all properties of the object and will be able to call any methods.



## Using outside variables


The `use` construct is used to import variables into the anonymous function's scope:

```
$divisor = 2332;
$myfunction = function($number) use ($divisor) {
    return $number / $divisor;
};

echo $myfunction(81620); //Outputs 35

```

Variables can also be imported by reference:

```
$collection = [];

$additem = function($item) use (&$collection) {
    $collection[] = $item;
};

$additem(1);
$additem(2);

//$collection is now [1,2]

```



## Using built-in functions as callbacks


In functions taking `callable` as an argument, you can also put a string with PHP built-in function. It's common to use `trim` as `array_map` parameter to remove leading and trailing whitespace from all strings in the array.

```
$arr = ['   one  ', 'two   ', '   three'];
var_dump(array_map('trim', $arr));

// array(3) {
//   [0] =>
//   string(3) "one"
//   [1] =>
//   string(3) "two"
//   [2] =>
//   string(5) "three"
// }

```



## Anonymous function


An anonymous function is just a **function** that doesn't have a name.

```
// Anonymous function
function() {
    return "Hello World!";
};

```

In PHP, an anonymous function is treated like an **expression** and for this reason, it should be ended with a semicolon `;`.

An anonymous function should be **assigned** to a variable.

```
// Anonymous function assigned to a variable
$sayHello = function($name) {
    return "Hello $name!";
};

print $sayHello('John'); // Hello John

```

Or it should be **passed as parameter** of another function.

```
$users = [
    ['name' => 'Alice', 'age' => 20], 
    ['name' => 'Bobby', 'age' => 22], 
    ['name' => 'Carol', 'age' => 17]
];

// Map function applying anonymous function
$userName = array_map(function($user) {
    return $user['name'];
}, $users);

print_r($usersName); // ['Alice', 'Bobby', 'Carol']

```

Or even been **returned** from another function.

Self-executing anonymous functions:

```
// For PHP 7.x
(function () {
    echo "Hello world!";
})();

// For PHP 5.x
call_user_func(function () {
    echo "Hello world!";
});

```

Passing an argument into self-executing anonymous functions:

```
// For PHP 7.x
(function ($name) {
    echo "Hello $name!";
})('John');

// For PHP 5.x
call_user_func(function ($name) {
    echo "Hello $name!";
}, 'John');

```



## Pure functions


A **pure function** is a function that, given the same input, will always return the same output and are **side-effect** free.

```
// This is a pure function
function add($a, $b) {
    return $a + $b;
}

```

Some **side-effects** are **changing the filesystem**, **interacting with databases**, **printing to the screen**.

```
// This is an impure function
function add($a, $b) {
    echo "Adding...";
    return $a + $b;
}

```



## Common functional methods in PHP


### Mapping

Applying a function to all elements of an array :

```
array_map('strtoupper', $array);

```

Be aware that this is the only method of the list where the callback comes first.

### Reducing (or folding)

Reducing an array to a single value :

```
$sum = array_reduce($numbers, function ($carry, $number) {
   return $carry + $number;
});

```

### Filtering

Returns only the array items for which the callback returns `true` :

```
$onlyEven = array_filter($numbers, function ($number) {
    return ($number % 2) === 0;
});

```



## Scope


In PHP, an anonymous function has its own **scope** like any other PHP function.

In JavaScript, an anonymous function can access a variable in outside scope. But in PHP, this is not permitted.

```
$name = 'John';

// Anonymous function trying access outside scope
$sayHello = function() {
    return "Hello $name!";
}

print $sayHello('John'); // Hello !
// With notices active, there is also an Undefined variable $name notice

```



## Passing a callback function as a parameter


There are several PHP functions that accept user-defined callback functions as a parameter, such as: [`call_user_func()`](https://secure.php.net/manual/en/function.call-user-func.php), [`usort()`](https://secure.php.net/manual/en/function.usort.php) and [`array_map()`](https://secure.php.net/manual/en/function.array-map.php).

Depending on where the user-defined callback function was defined there are different ways to pass them:

### Procedural style:

```
function square($number)
{
    return $number * $number;
}

$initial_array = [1, 2, 3, 4, 5];
$final_array = array_map('square', $initial_array);
var_dump($final_array); // prints the new array with 1, 4, 9, 16, 25

```

### Object Oriented style:

```
class SquareHolder
{
    function square($number)
    {
        return $number * $number;
    }
}

$squaredHolder = new SquareHolder();
$initial_array = [1, 2, 3, 4, 5];
$final_array = array_map([$squaredHolder, 'square'], $initial_array);

var_dump($final_array); // prints the new array with 1, 4, 9, 16, 25

```

### Object Oriented style using a static method:

```
class StaticSquareHolder
{
    public static function square($number)
    {
        return $number * $number;
    }
}

$initial_array = [1, 2, 3, 4, 5];
$final_array = array_map(['StaticSquareHolder', 'square'], $initial_array);
// or:
$final_array = array_map('StaticSquareHolder::square', $initial_array); // for PHP >= 5.2.3

var_dump($final_array); // prints the new array with 1, 4, 9, 16, 25

```

