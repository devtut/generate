---
metaTitle: "Variable Scope"
description: "Superglobal variables, Static properties and variables, User-defined global variables"
---

# Variable Scope


Variable scope refers to the regions of code where a variable may be accessed.  This is also referred to as **visibility**.  In PHP scope blocks are defined by functions, classes, and a global scope available throughout an application.



## Superglobal variables


[Superglobal variables](http://stackoverflow.com/documentation/php/3392/superglobal-variables-php/29659/suberglobals-explained#t=201704020208099490915) are defined by PHP and can always be used from anywhere without the `global` keyword.

```
<?php

function getPostValue($key, $default = NULL) {
    // $_POST is a superglobal and can be used without
    // having to specify 'global $_POST;'
    if (isset($_POST[$key])) {
        return $_POST[$key];
    }

    return $default;
}

// retrieves $_POST['username']
echo getPostValue('username');

// retrieves $_POST['email'] and defaults to empty string
echo getPostValue('email', '');

```



## Static properties and variables


Static class properties that are defined with the `public` visibility are functionally the same as global variables.  They can be accessed from anywhere the class is defined.

```
class SomeClass {
    public static int $counter = 0;
}

// The static $counter variable can be read/written from anywhere
// and doesn't require an instantiation of the class
SomeClass::$counter += 1;

```

Functions can also define static variables inside their own scope. These static variables persist through multiple function calls, unlike regular variables defined in a function scope. This can be a very easy and simple way to implement the Singleton design pattern:

```
class Singleton {
    public static function getInstance() {
        // Static variable $instance is not deleted when the function ends
        static $instance;

        // Second call to this function will not get into the if-statement,
        // Because an instance of Singleton is now stored in the $instance
        // variable and is persisted through multiple calls
        if (!$instance) {
            // First call to this function will reach this line,
            // because the $instance has only been declared, not initialized
            $instance = new Singleton();
        }

        return $instance;

    }
}

$instance1 = Singleton::getInstance();
$instance2 = Singleton::getInstance();

// Comparing objects with the '===' operator checks whether they are
// the same instance. Will print 'true', because the static $instance
// variable in the getInstance() method is persisted through multiple calls
var_dump($instance1 === $instance2);

```



## User-defined global variables


The scope outside of any function or class is the global scope.  When a PHP script includes another (using [`include` or `require`](http://stackoverflow.com/documentation/php/2366/control-structures/7786/include-require#t=201703041535155642743)) the scope remains the same. If a script is included outside of any function or class, it's global variables are included in the same global scope, but if a script is included from within a function, the variables in the included script are in the scope of the function.

Within the scope of a function or class method, the `global` keyword may be used to create an access user-defined global variables.

```
<?php

$amount_of_log_calls = 0;

function log_message($message) {
    // Accessing global variable from function scope
    // requires this explicit statement
    global $amount_of_log_calls;

    // This change to the global variable is permanent
    $amount_of_log_calls += 1;

    echo $message;
}

// When in the global scope, regular global variables can be used
// without explicitly stating 'global $variable;'
echo $amount_of_log_calls; // 0

log_message("First log message!");
echo $amount_of_log_calls; // 1

log_message("Second log message!");
echo $amount_of_log_calls; // 2

```

A second way to access variables from the global scope is to use the special PHP-defined $GLOBALS array.

The $GLOBALS array is an associative array with the name of the global variable being the key and the contents of that variable being the value of the array element. Notice how $GLOBALS exists in any scope, this is because $GLOBALS is a superglobal.

This means that the `log_message()` function could be rewritten as:

```
function log_message($message) {
    // Access the global $amount_of_log_calls variable via the
    // $GLOBALS array. No need for 'global $GLOBALS;', since it
    // is a superglobal variable.
    $GLOBALS['amount_of_log_calls'] += 1;

    echo $messsage;
}

```

One might ask, why use the $GLOBALS array when the `global` keyword can also be used to get a global variable's value?  The main reason is using the `global` keyword will bring the variable into scope.  You then can't reuse the same variable name in the local scope.

