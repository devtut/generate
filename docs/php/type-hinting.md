---
metaTitle: "Type hinting"
description: "Type hinting scalar types, arrays and callables, Type hinting classes and interfaces, Nullable type hints, Type hinting generic objects, Type Hinting No Return(Void)"
---

# Type hinting



## Type hinting scalar types, arrays and callables


Support for type hinting array parameters (and return values after PHP 7.1) was added in PHP 5.1 with the keyword `array`. Any arrays of any dimensions and types, as well as empty arrays, are valid values.

Support for type hinting callables was added in PHP 5.4. Any value that `is_callable()` is valid for parameters and return values hinted `callable`, i.e. `Closure` objects, function name strings and `array(class_name|object, method_name)`.

If a typo occurs in the function name such that it is not `is_callable()`, a less obvious error message would be displayed:

> 
Fatal error: Uncaught TypeError: Argument 1 passed to foo() must be of the type callable, string/array given


```php
function foo(callable $c) {}
foo("count"); // valid
foo("Phar::running"); // valid
foo(["Phar", "running"); // valid
foo([new ReflectionClass("stdClass"), "getName"]); // valid
foo(function() {}); // valid

foo("no_such_function"); // callable expected, string given

```

Nonstatic methods can also be passed as callables in static format, resulting in a deprecation warning and level E_STRICT error in PHP 7 and 5 respectively.

Method visibility is taken into account. If the **context of the method with the `callable` parameter** does not have access to the callable provided, it will end up as if the method does not exist.

```php
class Foo{
  private static function f(){
    echo "Good" . PHP_EOL;
  }

  public static function r(callable $c){
    $c();
  }
}

function r(callable $c){}

Foo::r(["Foo", "f"]);
r(["Foo", "f"]);

```

Output:

> 
Fatal error: Uncaught TypeError: Argument 1 passed to r() must be callable, array given


Support for type hinting scalar types was added in PHP 7. This means that we gain type hinting support for `boolean`s, `integer`s, `float`s and `string`s.

```php
<?php

function add(int $a, int $b) {
    return $a + $b;
}

var_dump(add(1, 2)); // Outputs "int(3)"

```

By default, PHP will attempt to cast any provided argument to match its type hint. Changing the call to `add(1.5, 2)` gives exactly the same output, since the float `1.5` was cast to `int` by PHP.

To stop this behavior, one must add `declare(strict_types=1);` to the top of every PHP source file that requires it.

```php
<?php

declare(strict_types=1);

function add(int $a, int $b) {
    return $a + $b;
}

var_dump(add(1.5, 2));

```

The above script now produces a fatal error:

> 
Fatal error: Uncaught TypeError: Argument 1 passed to add() must be of the type integer, float given


### An Exception: Special Types

Some PHP functions may return a value of type `resource`. Since this is not a scalar type, but a special type, it is not possible to type hint it.

As an example, `curl_init()` will return a `resource`, as well as `fopen()`. Of course, those two resources aren't compatible to each other. Because of that, PHP 7 will **always** throw the following TypeError when type hinting `resource` explicitly:

> 
TypeError: Argument 1 passed to sample() must be an instance of resource, resource given




## Type hinting classes and interfaces


Type hinting for classes and interfaces was added in PHP 5.

### Class type hint

```php
<?php

class Student
{
    public $name = 'Chris';
}

class School
{
    public $name = 'University of Edinburgh';
}

function enroll(Student $student, School $school)
{
    echo $student->name . ' is being enrolled at ' . $school->name;
}

$student = new Student();
$school = new School();

enroll($student, $school);

```

The above script outputs:

> 
Chris is being enrolled at University of Edinburgh


### Interface type hint

```php
<?php

interface Enrollable {};
interface Attendable {};

class Chris implements Enrollable
{
    public $name = 'Chris';
}

class UniversityOfEdinburgh implements Attendable
{
    public $name = 'University of Edinburgh';
}

function enroll(Enrollable $enrollee, Attendable $premises)
{
    echo $enrollee->name . ' is being enrolled at ' . $premises->name;
}

$chris = new Chris();
$edinburgh = new UniversityOfEdinburgh();

enroll($chris, $edinburgh);

```

The above example outputs the same as before:

> 
Chris is being enrolled at University of Edinburgh


### Self type hints

The `self` keyword can be used as a type hint to indicate that the value must be an instance of the class that declares the method.



## Nullable type hints


### Parameters

Nullable type hint was added in PHP 7.1 using the `?` operator before the type hint.

```php
function f(?string $a) {}
function g(string $a) {}

f(null); // valid
g(null); // TypeError: Argument 1 passed to g() must be of the type string, null given

```

Before PHP 7.1, if a parameter has a type hint, it must declare a default value `null` to accept null values.

```php
function f(string $a = null) {}
function g(string $a) {}

f(null); // valid
g(null); // TypeError: Argument 1 passed to g() must be of the type string, null given

```

### Return values

In PHP 7.0, functions with a return type must not return null.

In PHP 7.1, functions can declare a nullable return type hint. However, the function must still return null, not void (no/empty return statements).

```php
function f() : ?string {
    return null;
}

function g() : ?string {}
function h() : ?string {}

f(); // OK
g(); // TypeError: Return value of g() must be of the type string or null, none returned
h(); // TypeError: Return value of h() must be of the type string or null, none returned

```



## Type hinting generic objects


Since PHP objects don't inherit from any base class (including `stdClass`), there is no support for type hinting a generic object type.

For example, the below will not work.

```php
<?php

function doSomething(object $obj) {
    return $obj;
}

class ClassOne {}
class ClassTwo {}

$classOne= new ClassOne();
$classTwo= new ClassTwo();

doSomething($classOne);
doSomething($classTwo);

```

And will throw a fatal error:

> 
Fatal error: Uncaught TypeError: Argument 1 passed to doSomething() must be an instance of object, instance of OperationOne given


A workaround to this is to declare a degenerate interface that defines no methods, and have all of your objects implement this interface.

```php
<?php

interface Object {}

function doSomething(Object $obj) {
    return $obj;
}

class ClassOne implements Object {}
class ClassTwo implements Object {}

$classOne = new ClassOne();
$classTwo = new ClassTwo();

doSomething($classOne);
doSomething($classTwo);

```



## Type Hinting No Return(Void)


In PHP 7.1, the `void` return type was added. While PHP has no actual `void` value, it is generally understood across programming languages that a function that returns nothing is returning `void`. This should not be confused with returning `null`, as `null` is a value that can be returned.

```php
function lacks_return(): void {
    // valid
}

```

Note that if you declare a `void` return, you cannot return any values or you will get a fatal error:

```php
function should_return_nothing(): void {
    return null; // Fatal error: A void function must not return a value
}

```

However, using return to exit the function is valid:

```php
function returns_nothing(): void {
    return; // valid
}

```



#### Syntax


- function f(ClassName $param) {}
- function f(bool $param) {}
- function f(int $param) {}
- function f(float $param) {}
- function f(string $param) {}
- function f(self $param) {}
- function f(callable $param) {}
- function f(array $param) {}
- function f(?type_name $param) {}
- function f() : type_name {}
- function f() : void {}
- function f() : ?type_name {}



#### Remarks


Type hinting or [type declarations](http://php.net/manual/en/functions.arguments.php#functions.arguments.type-declaration) are a defensive programming practice that ensures a function's parameters are of a specified type. This is particularly useful when type hinting for an interface because it allows the function to guarantee that a provided parameter will have the same methods as are required in the interface.

Passing the incorrect type to a type hinted function will lead to a fatal error:

> 
<p>Fatal error: Uncaught TypeError: Argument **X** passed to **foo()** must be of
the type **RequiredType**, **ProvidedType** given</p>


