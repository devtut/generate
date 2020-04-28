---
metaTitle: "Exception Handling and Error Reporting"
description: "Setting error reporting and where to display them, Logging fatal errors, Exception and Error handling"
---

# Exception Handling and Error Reporting



## Setting error reporting and where to display them


If it's not already done in php.ini, error reporting can be set dynamically and should be set to allow most errors to be shown:

**Syntax**

```php
int error_reporting ([ int $level ] )

```

**Examples**

```php
// should always be used prior to 5.4
error_reporting(E_ALL);

// -1 will show every possible error, even when new levels and constants are added 
// in future PHP versions. E_ALL does the same up to 5.4.
error_reporting(-1);

// without notices
error_reporting(E_ALL & ~E_NOTICE);

// only warnings and notices.
// for the sake of example, one shouldn't report only those
error_reporting(E_WARNING | E_NOTICE);

```

errors will be logged by default by php, normally in a error.log file at the same level than the running script.

in development environment, one can also show them on screen:

```php
ini_set('display_errors', 1);

```

in production however, one should

```php
ini_set('display_errors', 0);

```

and show a friendly problem message through the use of an Exception or Error handler.



## Logging fatal errors


In PHP, a fatal error is a kind of error that cannot be caught, that is, after experiencing a fatal error a program does not resume. However, to log this error or somehow handle the crash you can use `register_shutdown_function` to register shutdown handler.

Reference:

- [http://php.net/manual/en/function.register-shutdown-function.php](http://php.net/manual/en/function.register-shutdown-function.php)
- [http://php.net/manual/en/function.error-get-last.php](http://php.net/manual/en/function.error-get-last.php)
- [http://php.net/manual/en/errorfunc.constants.php](http://php.net/manual/en/errorfunc.constants.php)



## Exception and Error handling


### try/catch

`try..catch` blocks can be used to control the flow of a program where [Exceptions](http://php.net/manual/en/language.exceptions.php) may be thrown. They can be caught and handled gracefully rather than letting PHP stop when one is encountered:

```php
try {
    // Do a bunch of things...
    throw new Exception('My test exception!');
} catch (Exception $ex) {
    // Your logic failed. What do you want to do about that? Log it:
    file_put_contents('my_error_log.txt', $ex->getMessage(), FILE_APPEND);
}

```

The above example would `catch` the Exception thrown in the `try` block and log it's message ("My test exception!") to a text file.

### Catching different Exception types

You can implement multiple `catch` statements for different types of exceptions to be handled in different ways, for example:

```php
try {
    throw new InvalidArgumentException('Argument #1 must be an integer!');
} catch (InvalidArgumentException $ex) {
    var_dump('Invalid argument exception caught: ' . $ex->getMessage());
} catch (Exception $ex) {
    var_dump('Standard exception caught: ' . $ex->getMessage());
}

```

In the above example the first `catch` will be used since it matches first in the order of execution. If you swapped the order of the `catch` statements around, the `Exception` catcher would execute first.

Similarly, if you were to throw an [`UnexpectedValueException`](http://php.net/manual/en/class.unexpectedvalueexception.php) instead you would see the second handler for a standard `Exception` being used.

### finally

If you need something to be done after either a `try` or a `catch` has finished running, you can use a `finally` statement:

```php
try {
    throw new Exception('Hello world');
} catch (Exception $e) {
    echo 'Uh oh! ' . $e->getMessage();
} finally {
    echo " - I'm finished now - home time!";
}

```

The above example would output the following:

> 
Uh oh! Hello world - I'm finished now - home time!


### throwable

In PHP 7 we see the introduction of the [`Throwable`](http://php.net/manual/en/class.throwable.php) interface, which [`Error`](http://php.net/manual/en/class.error.php) as well as [`Exception`](http://php.net/manual/en/class.exception.php) implements. This adds a service contract level between exceptions in PHP 7, and allows you to implement the interface for your own custom exceptions:

```php
$handler = function(\Throwable $ex) {
    $msg = "[ {$ex->getCode()} ] {$ex->getTraceAsString()}";
    mail('admin@server.com', $ex->getMessage(), $msg);
    echo myNiceErrorMessageFunction();
};
set_exception_handler($handler);
set_error_handler($handler);

```

Prior to PHP 7 you can simply typehint `Exception` since as of PHP 5 all exception classes extend it.

