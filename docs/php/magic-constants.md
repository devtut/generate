---
metaTitle: "Magic Constants"
description: "Difference between __FUNCTION__ and __METHOD__, Difference between __CLASS__, get_class() and get_called_class(), File & Directory Constants"
---

# Magic Constants



## Difference between __FUNCTION__ and __METHOD__


`__FUNCTION__` returns only the name of the function whereas `__METHOD__` returns the name of the class along with the name of the function:

```php
<?php

class trick
{
    public function doit()
    {
        echo __FUNCTION__;
    }

    public function doitagain()
    {
        echo __METHOD__;
    }
}

$obj = new trick();
$obj->doit(); // Outputs: doit
$obj->doitagain();  // Outputs: trick::doitagain

```



## Difference between __CLASS__, get_class() and get_called_class()


`__CLASS__` magic constant returns the same result as `get_class()` function called without parameters and they both return the name of the class where it was defined (i.e. where you wrote the function call/constant name ).

In contrast, `get_class($this)` and `get_called_class()` functions call, will both return the name of the actual class which was instantiated:

```php
<?php

class Definition_Class {

  public function say(){
     echo '__CLASS__ value: ' . __CLASS__ . "\n";
     echo 'get_called_class() value: ' . get_called_class() . "\n";
     echo 'get_class($this) value: ' . get_class($this) . "\n";
     echo 'get_class() value: ' . get_class() . "\n";
  }
  
}

class Actual_Class extends Definition_Class {}

$c = new Actual_Class();
$c->say();
// Output:
// __CLASS__ value: Definition_Class
// get_called_class() value: Actual_Class
// get_class($this) value: Actual_Class
// get_class() value: Definition_Class

```



## File & Directory Constants


### Current file

You can get the name of the current PHP file (with the absolute path) using the `__FILE__` magic constant. This is most often used as a logging/debugging technique.

```php
echo "We are in the file:" , __FILE__ , "\n";

```

### Current directory

To get the absolute path to the directory where the current file is located use the `__DIR__` magic constant.

```php
echo "Our script is located in the:" , __DIR__ , "\n";

```

To get the absolute path to the directory where the current file is located, use `dirname(__FILE__)`.

```php
echo "Our script is located in the:" , dirname(__FILE__) , "\n";

```

Getting current directory is often used by PHP frameworks to set a base directory:

```php
// index.php of the framework

define(BASEDIR, __DIR__); // using magic constant to define normal constant

```

```php
// somefile.php looks for views:

$view = 'page';
$viewFile = BASEDIR . '/views/' . $view;

```

### Separators

> 
<p>Windows system perfectly understands the `/` in paths so the
`DIRECTORY_SEPARATOR` is used mainly when parsing paths.</p>


Besides magic constants PHP also adds some fixed constants for working with paths:

<li>`DIRECTORY_SEPARATOR` constant for separating directories in a path. Takes value  `/` on *nix, and `\` on Windows.
The example with views can be rewritten with:</li>

```php
$view = 'page';
$viewFile = BASEDIR . DIRECTORY_SEPARATOR .'views' . DIRECTORY_SEPARATOR . $view;

```


- Rarely used `PATH_SEPARATOR` constant for separating paths in the `$PATH` environment variable. It is `;` on Windows, `:` otherwise



#### Remarks


Magic constants are distinguished by their `__CONSTANTNAME__` form.

There are currently eight magical constants that change depending on where they are used. For example, the value of `__LINE__`depends on the line that it's used on in your script.

These special constants are case-insensitive and are as follows:

|Name|Description
|---|---|---|---|---|---|---|---|---|---
|`__LINE__`|The current line number of the file.
|`__FILE__`|The full path and filename of the file with symlinks resolved. If used inside an include, the name of the included file is returned.
|`__DIR__`|The directory of the file. If used inside an include, the directory of the included file is returned. This is equivalent to `dirname(__FILE__)`. This directory name does not have a trailing slash unless it is the root directory.
|`__FUNCTION__`|The current function name
|`__CLASS__`|The class name. The class name includes the namespace it was declared in (e.g. `Foo\Bar`). When used in a trait method, `__CLASS__` is the name of the class the trait is used in.
|`__TRAIT__`|The trait name. The trait name includes the namespace it was declared in (e.g. `Foo\Bar`).
|`__METHOD__`|The class method name.
|`__NAMESPACE__`|The name of the current namespace.

Most common use case for these constants is debugging and logging

