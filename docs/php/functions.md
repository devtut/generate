---
metaTitle: "Functions"
description: "Variable-length argument lists, Optional Parameters, Passing Arguments by Reference, Basic Function Usage, Function Scope"
---

# Functions



## Variable-length argument lists


PHP 5.6 introduced variable-length argument lists (a.k.a. varargs, variadic arguments), using the `...` token before the argument name to indicate that the parameter is variadic, i.e. it is an array including all supplied parameters from that one onward.

```
function variadic_func($nonVariadic, ...$variadic) {
    echo json_encode($variadic);
}

variadic_func(1, 2, 3, 4); // prints [2,3,4]

```

Type names can be added in front of the `...`:

```
function foo(Bar ...$bars) {}

```

The `&` reference operator can be added before the `...`, but after the type name (if any). Consider this example:

```
class Foo{}
function a(Foo &...$foos){
    $i = 0;
    foreach($a as &$foo){ // note the &
        $foo = $i++;
    }
}
$a = new Foo;
$c = new Foo;
$b =& $c;
a($a, $b);
var_dump($a, $b, $c);

```

Output:

```
int(0)
int(1)
int(1)

```

On the other hand, an array (or `Traversable`) of arguments can be unpacked to be passed to a function in the form of an argument list:

```
var_dump(...hash_algos());

```

Output:

```
string(3) "md2"
string(3) "md4"
string(3) "md5"
...

```

Compare with this snippet without using `...`:

```
var_dump(hash_algos());

```

Output:

```
array(46) {
  [0]=>
  string(3) "md2"
  [1]=>
  string(3) "md4"
  ...
}

```

Therefore, redirect functions for variadic functions can now be easily made, for example:

```
public function formatQuery($query, ...$args){
    return sprintf($query, ...array_map([$mysqli, "real_escape_string"], $args));
}

```

Apart from arrays, `Traversable`s, such as `Iterator` (especially many of its subclasses from SPL) can also be used. For example:

```
$iterator = new LimitIterator(new ArrayIterator([0, 1, 2, 3, 4, 5, 6]), 2, 3);
echo bin2hex(pack("c*", ...$it)); // Output: 020304

```

If the iterator iterates infinitely, for example:

```
$iterator = new InfiniteIterator(new ArrayIterator([0, 1, 2, 3, 4]));
var_dump(...$iterator);

```

Different versions of PHP behave differently:

<li>From PHP 7.0.0 up to PHP 7.1.0 (beta 1):
<ul>
- A segmentation fault will occur
- The PHP process will exit with code 139

- A fatal error of memory exhaustion ("Allowed memory size of %d bytes exhausted") will be shown.
- The PHP process will exit with code 255

> 
Note: HHVM (v3.10 - v3.12) does not support unpacking `Traversable`s. A warning message "Only containers may be unpacked" will be shown in this attempt.




## Optional Parameters


Functions can have optional parameters, for example:

```
function hello($name, $style = 'Formal')
{
    switch ($style) {
        case 'Formal':
            print "Good Day $name";
            break;
        case 'Informal':
            print "Hi $name";
            break;
        case 'Australian':
            print "G'day $name";
            break;
        default:
            print "Hello $name";
            break;
    }
}

hello('Alice');
    // Good Day Alice

hello('Alice', 'Australian');
    // G'day Alice

```



## Passing Arguments by Reference


Function arguments can be passed "By Reference", allowing the function to modify the variable used outside the function:

```
function pluralize(&$word)
{
    if (substr($word, -1) == 'y') {
        $word = substr($word, 0, -1) . 'ies';
    } else {
      $word .= 's';
    }
}

$word = 'Bannana';
pluralize($word);

print $word;
  // Bannanas

```

Object arguments are always passed by reference:

```
function addOneDay($date)
{
    $date->modify('+1 day');
}

$date = new DateTime('2014-02-28');
addOneDay($date);

print $date->format('Y-m-d');
  // 2014-03-01

```

To avoid implicit passing an object by reference, you should `clone` the object.

Passing by reference can also be used as an alternative way to return parameters. For example, the `socket_getpeername` function:

```
bool socket_getpeername ( resource $socket , string &$address [, int &$port ] )

```

This method actually aims to return the address and port of the peer, but since there are two values to return, it chooses to use reference parameters instead. It can be called like this:

```
if(!socket_getpeername($socket, $address, $port)) {
    throw new RuntimeException(socket_last_error());
}
echo "Peer: $address:$port\n";

```

The variables `$address` and `$port` do not need to be defined before. They will:

1. be defined as `null` first,
1. then passed to the function with the predefined `null` value
1. then modified in the function
1. end up defined as the address and port in the calling context.



## Basic Function Usage


A basic function is defined and executed like this:

```
function hello($name)
{
    print "Hello $name";
}

hello("Alice");

```



## Function Scope


Variables inside functions is inside a local scope like this

```
$number = 5
function foo(){
    $number = 10
    return $number
}

foo(); //Will print 10 because text defined inside function is a local variable

```



#### Syntax


- function func_name($parameterName1, $parameterName2) { code_to_run(); }
- function func_name($optionalParameter = default_value) { code_to_run(); }
- function func_name(type_name $parameterName) { code_to_run(); }
- function &returns_by_reference() { code_to_run(); }
- function func_name(&$referenceParameter) { code_to_run(); }
- function func_name(...$variadicParameters) { code_to_run(); } // PHP 5.6+
- function func_name(type_name &...$varRefParams) { code_to_run(); } // PHP 5.6+
- function func_name() : return_type { code_To_run(); } // PHP 7.0+

