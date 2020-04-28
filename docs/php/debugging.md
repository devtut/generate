---
metaTitle: "Debugging"
description: "Dumping variables, Displaying errors, phpinfo(), Xdebug, phpversion(), Error Reporting (use them both)"
---

# Debugging



## Dumping variables


The [`var_dump`](http://php.net/manual/en/function.var-dump.php) function allows you to dump the contents of a variable (type and value) for debugging.

**Example:**

```
$array = [3.7, "string", 10, ["hello" => "world"], false, new DateTime()];
var_dump($array);

```

**Output:**

```
array(6) {
  [0]=>
  float(3.7)
  [1]=>
  string(6) "string"
  [2]=>
  int(10)
  [3]=>
  array(1) {
    ["hello"]=>
    string(5) "world"
  }
  [4]=>
  bool(false)
  [5]=>
  object(DateTime)#1 (3) {
    ["date"]=>
    string(26) "2016-07-24 13:51:07.000000"
    ["timezone_type"]=>
    int(3)
    ["timezone"]=>
    string(13) "Europe/Berlin"
  }
}

```



## Displaying errors


If you want PHP to display runtime errors on the page, you have to enable [`display_errors`](http://php.net/manual/en/errorfunc.configuration.php#ini.display-errors), either in the `php.ini` or using the [`ini_set`](http://php.net/manual/en/function.ini-set.php) function.

You can choose which errors to display, with the `error_reporting` (or in the ini) function, which accepts [`E_*` constants](http://php.net/manual/en/errorfunc.constants.php), combined using [bitwise operators](http://php.net/manual/en/language.operators.bitwise.php).

PHP can display errors in text or HTML format, depending on the [`html_errors`](http://php.net/manual/en/errorfunc.configuration.php#ini.html-errors) setting.

**Example:**

```
ini_set("display_errors", true);
ini_set("html_errors", false); // Display errors in plain text
error_reporting(E_ALL & ~E_USER_NOTICE); // Display everything except E_USER_NOTICE

trigger_error("Pointless error"); // E_USER_NOTICE
echo $nonexistentVariable; // E_NOTICE
nonexistentFunction(); // E_ERROR

```

**Plain text output:** (HTML format differs between implementations)

```
Notice: Undefined variable: nonexistentVariable in /path/to/file.php on line 7

Fatal error: Uncaught Error: Call to undefined function nonexistentFunction() in /path/to/file.php:8
Stack trace:
#0 {main}
  thrown in /path/to/file.php on line 8

```

> 
**NOTE:** If you have error reporting disabled in php.ini and enable it during runtime, some errors (such as parse errors) won't be displayed, because they occured before the runtime setting was applied.


The common way to handle `error_reporting` is to enable it fully with `E_ALL` constant during the development, and to disable publicly displaying it with `display_errors` on production stage to hide the internals of your scripts.



## phpinfo()


### Warning

**It is imperative that `phpinfo` is only used in a development environment. Never release code containing `phpinfo` into a production environment**

### Introduction

Having said that, it  can be a useful tool in understanding the PHP environment (OS, configuration, versions, paths, modules) in which you are working, especially when chasing a bug. It is a simple built in function:

```
phpinfo();

```

It has one parameter `$what` that allows the output to be customized. The default is `INFO_ALL`, causing it to display all information and is commonly used during development to see the current state of PHP.

You can pass the parameter [`INFO_*` constants](http://php.net/manual/en/function.phpinfo.php#refsect1-function.phpinfo-parameters), combined with bitwise operators to see a customized list.

You can run it in the browser for a nicely formatted detailed look. It also works in PHP CLI, where you can pipe it into `less` for easier view.

### Example

```
phpinfo(INFO_CONFIGURATION | INFO_ENVIRONMENT | INFO_VARIABLES);

```

This will display a list of PHP directives ([`ini_get`](http://php.net/manual/en/function.ini-get.php)), environment ([`$_ENV`](http://php.net/manual/en/reserved.variables.environment.php)) and [predefined](http://php.net/manual/en/language.variables.predefined.php) variables.



## Xdebug


[**Xdebug**](https://xdebug.org) is a PHP extension which provides debugging and profiling capabilities. <br>It uses the DBGp debugging protocol.

There are some nice features in this tool:

- stack traces on errors
- maximum nesting level protection and time tracking
- helpful replacement of standard `var_dump()` function for displaying variables
- allows to log all function calls, including parameters and return values to a file in different formats
- code coverage analysis
- profiling information
- remote debugging (provides interface for debugger clients that interact with running PHP scripts)

As you can see this extension is perfectly suited for development environment. Especially **remote debugging** feature can help you to debug your php code without numerous var_dump's and use normal debugging process as in `C++` or `Java` languages.

Usually installing of this extension is very simple:

```
pecl install xdebug # install from pecl/pear

```

And activate it into your php.ini:

```
zend_extension="/usr/local/php/modules/xdebug.so"

```

In more complicated cases see this [instructions](https://xdebug.org/docs/install)

When you use this tool you should remember that:<br>
[**XDebug is not suitable for production environments**](http://stackoverflow.com/a/3522356/2253302)



## phpversion()


### **Introduction**

When working with various libraries and their associated requirements, it is often necessary to know the version of current PHP parser or one of it's packages.

This function accepts a single optional parameter in the form of extension name: `phpversion('extension')`. If the extension in question is installed, the function will return a string containing version value. However, if the extension not installed `FALSE` will be returned. If the extension name is not provided, the function will return the version of PHP parser itself.

### **Example**

```
print "Current PHP version: " . phpversion();
// Current PHP version: 7.0.8

print "Current cURL version: " . phpversion( 'curl' );
// Current cURL version: 7.0.8
// or
// false, no printed output if package is missing

```



## Error Reporting (use them both)


```
// this sets the configuration option for your environment
ini_set('display_errors', '1');

//-1 will allow all errors to be reported
error_reporting(-1);

```

