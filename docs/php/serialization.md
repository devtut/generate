---
metaTitle: "Serialization"
description: "Serialization of different types, Security Issues with unserialize"
---

# Serialization



## Serialization of different types


Generates a storable representation of a value.

This is useful for storing or passing PHP values around without losing their type and structure.

To make the serialized string into a PHP value again, use **unserialize()**.

### Serializing a string

```php
$string = "Hello world";    
echo serialize($string);

// Output: 
// s:11:"Hello world";

```

### Serializing a double

```php
$double = 1.5;    
echo serialize($double);

// Output: 
// d:1.5;

```

### Serializing a float

Float get serialized as doubles.

### Serializing an integer

```php
$integer = 65;    
echo serialize($integer);

// Output: 
// i:65;

```

### Serializing a boolean

```php
$boolean = true;    
echo serialize($boolean);

// Output: 
// b:1;

$boolean = false;    
echo serialize($boolean);

// Output: 
// b:0;

```

### Serializing null

```php
$null = null;    
echo serialize($null);

// Output: 
// N;

```

### Serializing an array

```php
$array = array(
    25,
    'String',
    'Array'=> ['Multi Dimension','Array'],
    'boolean'=> true,
    'Object'=>$obj, // $obj from above Example
    null,
    3.445
); 


// This will throw Fatal Error
// $array['function'] = function() { return "function"; };

echo serialize($array);

// Output:
// a:7:{i:0;i:25;i:1;s:6:"String";s:5:"Array";a:2:{i:0;s:15:"Multi Dimension";i:1;s:5:"Array";}s:7:"boolean";b:1;s:6:"Object";O:3:"abc":1:{s:1:"i";i:1;}i:2;N;i:3;d:3.4449999999999998;}

```

### Serializing an object

You can also serialize Objects.

When serializing objects, PHP will attempt to call the member function **__sleep()** prior to serialization. This is to allow the object to do any last minute clean-up, etc. prior to being serialized. Likewise, when the object is restored using **unserialize()** the **__wakeup()** member function is called.

```php
class abc {
    var $i = 1;
    function foo() {
        return 'hello world';
    }
}

$object = new abc(); 
echo serialize($object);

// Output:
// O:3:"abc":1:{s:1:"i";i:1;} 

```

### Note that Closures cannot be serialized:

```php
$function = function () { echo 'Hello World!'; };
$function(); // prints "hello!"

$serializedResult = serialize($function);  // Fatal error: Uncaught exception 'Exception' with message 'Serialization of 'Closure' is not allowed'

```



## Security Issues with unserialize


Using `unserialize` function to unserialize data from user input can be dangerous.

A Warning from php.net

> 
<p>**Warning** Do not pass untrusted user input to unserialize().
Unserialization can result in code being loaded and executed due to
object instantiation and autoloading, and a malicious user may be able
to exploit this. Use a safe, standard data interchange format such as
JSON (via json_decode() and json_encode()) if you need to pass
serialized data to the user.</p>


### **Possible Attacks**

- PHP Object Injection

### PHP Object Injection

PHP Object Injection is an application level vulnerability that could allow an attacker to perform different kinds of malicious attacks, such as Code Injection, SQL Injection, Path Traversal and Application Denial of Service, depending on the context. The vulnerability occurs when user-supplied input is not properly sanitized before being passed to the unserialize() PHP function. Since PHP allows object serialization, attackers could pass ad-hoc serialized strings to a vulnerable unserialize() call, resulting in an arbitrary PHP object(s) injection into the application scope.

In order to successfully exploit a PHP Object Injection vulnerability two conditions must be met:

- The application must have a class which implements a PHP magic method (such as `__wakeup` or `__destruct`) that can be used to carry out malicious attacks, or to start a "POP chain".
- All of the classes used during the attack must be declared when the vulnerable `unserialize()` is being called, otherwise object autoloading must be supported for such classes.

**Example 1  - Path Traversal Attack**

The example below shows a PHP class with an exploitable `__destruct` method:

```php
class Example1
{
   public $cache_file;

   function __construct()
   {
      // some PHP code...
   }

   function __destruct()
   {
      $file = "/var/www/cache/tmp/{$this->cache_file}";
      if (file_exists($file)) @unlink($file);
   }
}

// some PHP code...

$user_data = unserialize($_GET['data']);

// some PHP code...

```

In this example an attacker might be able to delete an arbitrary file via a Path Traversal attack, for e.g. requesting the following URL:

```php
http://testsite.com/vuln.php?data=O:8:"Example1":1:{s:10:"cache_file";s:15:"../../index.php";}

```

**Example 2 - Code Injection attack**

The example below shows a PHP class with an exploitable __wakeup method:

```php
class Example2
{
   private $hook;

   function __construct()
   {
      // some PHP code...
   }

   function __wakeup()
   {
      if (isset($this->hook)) eval($this->hook);
   }
}

// some PHP code...

$user_data = unserialize($_COOKIE['data']);

// some PHP code...

```

In this example an attacker might be able to perform a Code Injection attack by sending an HTTP request like this:

```php
GET /vuln.php HTTP/1.0
Host: testsite.com
Cookie: data=O%3A8%3A%22Example2%22%3A1%3A%7Bs%3A14%3A%22%00Example2%00hook%22%3Bs%3A10%3A%22phpinfo%28%29%3B%22%3B%7D
Connection: close

```

Where the cookie parameter "data" has been generated by the following script:

```php
class Example2
{
   private $hook = "phpinfo();";
}

print urlencode(serialize(new Example2));

```



#### Syntax


- string serialize ( mixed $value )



#### Parameters


|Parameter|Details
|------
|value|The value to be serialized. [serialize()](http://php.net/manual/en/function.serialize.php) handles all types, except the [resource](http://php.net/manual/en/language.types.resource.php)-type. You can even serialize() arrays that contain references to itself. Circular references inside the array/object you are serializing will also be stored. Any other reference will be lost. When serializing objects, PHP will attempt to call the member function [__sleep()](http://php.net/manual/en/language.oop5.magic.php#object.sleep) prior to serialization. This is to allow the object to do any last minute clean-up, etc. prior to being serialized. Likewise, when the object is restored using [unserialize()](http://php.net/manual/en/function.unserialize.php) the [__wakeup()](http://php.net/manual/en/language.oop5.magic.php#object.wakeup) member function is called. Object's private members have the class name prepended to the member name; protected members have a '*' prepended to the member name. These prepended values have null bytes on either side.



#### Remarks


Serialization uses following string structures:

> 
`[..]` are placeholders.


|Type|Structure
|------
|String|`s:[size of string]:[value]`
|Integer|`i:[value]`
|Double|`d:[value]`
|Boolean|`b:[value (true = 1 and false = 0)]`
|Null|`N`
|Object|`O:[object name size]:[object name]:[object size]:{[property name string definition]:[property value definition];(repeated for each property)}`
|Array|`a:[size of array]:{[key definition];[value definition];(repeated for each key value pair)}`

