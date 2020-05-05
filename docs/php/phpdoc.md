---
metaTitle: "PHP - PHPDoc"
description: "Adding metadata to functions, Describing a variable, Adding metadata to files, Inheriting metadata from parent structures, Describing parameters, Collections"
---

# PHPDoc



## Adding metadata to functions


Function level annotations help IDEs identify return values or potentially dangerous code

```php
/**
 * Adds two numbers together.
 *
 * @param Int $a First parameter to add
 * @param Int $b Second parameter to add
 * @return Int
 */
function sum($a, $b)
{
    return (int) $a + $b;
}

/**
 * Don't run me! I will always raise an exception.
 *
 * @throws Exception Always
 */
function dangerousCode()
{
    throw new Exception('Ouch, that was dangerous!');
}

/**
 * Old structures should be deprecated so people know not to use them.
 *
 * @deprecated
 */
function oldCode()
{
    mysql_connect(/* ... */);
}

```



## Describing a variable


The `@var` keyword can be used to describe the type and usage of:

- a class property
- a local or global variable
- a class or global constant

```php
class Example {
    /** @var string This is something that stays the same */
    const UNCHANGING = "Untouchable";

    /** @var string $some_str This is some string */
    public $some_str;

    /**
     * @var array $stuff    This is a collection of stuff
     * @var array $nonsense These are nonsense
     */
    private $stuff, $nonsense;

    ...
}

```

The type can be one of the built-in PHP types, or a user-defined class, including namespaces.

The name of the variable should be included, but can be omitted if the docblock applies to only one item.



## Adding metadata to files


File level metadata applies to all the code within the file and should be placed at the top of the file:

```php
<?php

/**
 * @author John Doe (jdoe@example.com)
 * @copyright MIT
 */

```



## Inheriting metadata from parent structures


If a class extends another class and would use the same metadata, providing it `@inheritDoc` is a simple way for use the same documentation. If multiple classes inherit from a base, only the base would need to be changed for the children to be affected.

```php
abstract class FooBase
{
    /**
     * @param Int $a First parameter to add
     * @param Int $b Second parameter to add
     * @return Int
     */
    public function sum($a, $b) {}
}

class ConcreteFoo extends FooBase
{
    /**
     * @inheritDoc
     */
    public function sum($a, $b)
    {
        return $a + $b;
    }
}

```



## Describing parameters


```

/**
 * Parameters
 * 
 * @param  int    $int
 * @param  string $string
 * @param  array  $array
 * @param  bool   $bool
 */
function demo_param($int, $string, $array, $bool)
{
}

 /**
 * Parameters - Optional / Defaults
 *
 * @param  int    $int
 * @param  string $string
 * @param  array  $array
 * @param  bool   $bool
 */
function demo_param_optional($int = 5, $string = 'foo', $array = [], $bool = false)
{
}

/**
 * Parameters - Arrays
 * 
 * @param array          $mixed
 * @param int[]          $integers
 * @param string[]       $strings
 * @param bool[]         $bools
 * @param string[]|int[] $strings_or_integers
 */
function demo_param_arrays($mixed,$integers, $strings, $bools, $strings_or_integers)
{
}

/**
 * Parameters - Complex
 * @param array $config 
 * <pre>
 * $params = [
 *         'hostname'     => (string) DB hostname. Required.
 *         'database'     => (string) DB name. Required.
 *         'username'     => (string) DB username. Required.
 * ]
 * </pre>
 */
function demo_param_complex($config)
{
}

```



## Collections


[PSR-5](https://github.com/php-fig/fig-standards/blob/211063eed7f4d9b4514b728d7b1810d9b3379dd1/proposed/phpdoc.md#collections) proposes a form of Generics-style notation for collections.

### Generics Syntax

```php
Type[]
Type<Type>
Type<Type[, Type]...>
Type<Type[|Type]...>

```

Values in a Collection MAY even be another array and even another Collection.

```php
Type<Type<Type>>
Type<Type<Type[, Type]...>>
Type<Type<Type[|Type]...>>

```

### Examples

```php
<?php

/** 
 * @var ArrayObject<string> $name 
 */
$name = new ArrayObject(['a', 'b']);

/** 
 * @var ArrayObject<int> $name 
 */
$name = new ArrayObject([1, 2]);

/** 
 * @var ArrayObject<stdClass> $name 
 */
$name = new ArrayObject([
    new stdClass(), 
    new stdClass()
]);

/** 
 * @var ArrayObject<string|int|stdClass|bool> $name 
 */
$name = new ArrayObject([
    'a', 
    true, 
    1, 
    'b', 
    new stdClass(), 
    'c', 
    2
]);

/**
 * @var ArrayObject<ArrayObject<int>> $name 
 */
$name = new ArrayObject([
    new ArrayObject([1, 2]), 
    new ArrayObject([1, 2])
]);

/** 
 * @var ArrayObject<int, string> $name 
 */
$name = new ArrayObject([
    1 => 'a', 
    2 => 'b'
]);

/** 
 * @var ArrayObject<string, int> $name 
 */
$name = new ArrayObject([
    'a' => 1, 
    'b' => 2
]);

/** 
 * @var ArrayObject<string, stdClass> $name 
 */
$name = new ArrayObject([
    'a' => new stdClass(), 
    'b' => new stdClass()
]);

```



#### Syntax


- @api
- @author [name] [<email address>]
- @copyright <description>
- @deprecated [<"Semantic Version">][:<"Semantic Version">] [<description>]
- @example [URI] [<description>]
- {@example [URI] [:<start>..<end>]}
- @inheritDoc
- @internal
- {@internal [description]}}
- @license [<SPDX identifier>|URI] [name]
- @method [return "Type"] [name](["Type"] [parameter], [...]) [description]
- @package [level 1]\[level 2]\[etc.]
- @param ["Type"] [name] [<description>]
- @property ["Type"] [name] [<description>]
- @return <"Type"> [description]
- @see [URI | "FQSEN"] [<description>]
- @since [<"Semantic Version">] [<description>]
- @throws ["Type"] [<description>]
- @todo [description]
- @uses [file | "FQSEN"] [<description>]
- @var ["Type"] [element_name] [<description>]
- @version ["Semantic Version"] [<description>]
- @filesource - Includes current file in phpDocumentor parsing results
- @link [URI] [<description>] - Link tag helps to define relation or link between [structural elements](https://phpdoc.org/docs/latest/glossary.html#term-structural-elements).



#### Remarks


> 
"PHPDoc" is a section of documentation which provides information on aspects of a "Structural Element" — [PSR-5](https://github.com/php-fig/fig-standards/blob/master/proposed/phpdoc.md)


PHPDoc annotations are comments that provide metadata about all types of structures in PHP. Many popular IDEs are configured by default to utilize PHPDoc annotations to provide code insights and identify possible problems before they arise.

While PHPDoc annotations are not part of the PHP core, they currently hold draft status with [PHP-FIG](http://www.php-fig.org) as [PSR-5](https://github.com/php-fig/fig-standards/blob/master/proposed/phpdoc.md).

All PHPDoc annotations are contained within **DocBlocks** that are demonstrated by a multi-line with two asterisks:

```php
/**
 *
 */

```

The full [PHP-FIG](http://www.php-fig.org) standards draft is [available on GitHub](https://github.com/php-fig/fig-standards/blob/master/proposed/phpdoc.md).

