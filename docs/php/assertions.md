---
metaTitle: "Assertions"
description: "Assert an Exception is Thrown, Assert an Object is an Instance of a Class, Assert the Value of a Public, Protected and Private property"
---

# Assertions



## Assert an Exception is Thrown


PHPUnit provides the [following functions](https://phpunit.de/manual/current/en/writing-tests-for-phpunit.html#writing-tests-for-phpunit.exceptions) to watch for thrown exceptions, which were released with 5.2.0:

- `expectException($exception)`
- `expectExceptionMessage($message)`
- `expectExceptionCode($code)`
- `expectExceptionMessageRegExp($messageRegExp)`

These are used to watch for an exception to be thrown and inspect the properties of that exception.

Let's start with a math function that divides (just for simplicity).  It will raise an exception if the denominator is zero.

```php
function divide($numerator, $denominator) {
    
    if ($denominator !== 0) {
        return $numerator/$denominator;       
    } else {
        throw new \Exception("Cannot divide by zero", 100);
    }

}

```

Now for the test code.

```php
class DivideTest extends PHPUnit_Framework_TestCase
{

    public function test_divide() {

        $this->assertSame(2,divide(4,2));

        $this->expectException("Exception");
        $this->expectExceptionCode(100);
        $this->expectExceptionMessage("Cannot divide by zero");
        $this->expectExceptionMessageRegExp('/divide by zero$/');

        // the expectations have been set up, now run the code
        // that should throw the exception
        divide(4,0);

        // The following code will not execute, the method has exited
        $this->assertSame(0,1);

    }

}

```

The `test_divide()` function starts by asserting that the function correctly divided 4 by 2 and answered 2.  This assertion will pass.

Next, the expectations for the upcoming exception are set.  Notice, they are set before the code that will throw the exception.  All four assertions are shown for demonstration purposes, but this is normally not necessary.

The `divide(4,0)` will then throw the expected exception and all the expect* function will pass.

**But note that the code `$this->assertSame(0,1)` will not be executed, the fact that it is a failure doesn't matter, because it will not run.  The divide by zero exception causes the test method to exit.  This can be a source of confusion while debugging.**



## Assert an Object is an Instance of a Class


PHPUnit provides the following function to assert whether an object is an instance of a class:

```php
assertInstanceOf($expected, $actual[, $message = ''])

```

The first parameter `$expected` is the name of a class (string).   The second parameter `$actual` is the object to be tested.  `$message` is an optional string you can provide in case it fails.

Let's start with a simple Foo class:

```php
class Foo {

}

```

Somewhere else in a namespace `Crazy`, there is a `Bar` class.

```php
namespace Crazy

class Bar {

}

```

Now, let's write a basic test case that would check an object for these classes

```php
use Crazy\Bar;

class sampleTestClass extends PHPUnit_Framework_TestCase
{

    public function test_instanceOf() {

        $foo = new Foo();
        $bar = new Bar();

        // this would pass
        $this->assertInstanceOf("Foo",$foo);

        // this would pass
        $this->assertInstanceOf("\\Crazy\\Bar",$bar);

        // you can also use the ::class static function that returns the class name
        $this->assertInstanceOf(Bar::class, $bar);

        // this would fail
        $this->assertInstanceOf("Foo", $bar, "Bar is not a Foo");

    }
}    

```

Note that PHPUnit gets grumpy if you send in a classname that doesn't exist.



## Assert the Value of a Public, Protected and Private property


PHPUnit has two assertions to check values of class properties:

```php
assertAttributeSame($expected, $actualAttributeName, $actualClassOrObject, $message = '')
assertAttributeNotSame($expected, $actualAttributeName, $actualClassOrObject, $message = '')

```

These methods will check the value of a object property regardless of the visibility.

Let's start with a class to be tested.  It is a simplified class that has three properties, each with a different visibility:

```php
class Color {

    public $publicColor       = "red";
    protected $protectedColor = "green";
    private $privateColor     = "blue";
    
}

```

Now, to test the value of each property:

```php
class ColorTest extends PHPUnit_Framework_TestCase
{
    public function test_assertAttributeSame() {

        $hasColor = new Color();

        $this->assertAttributeSame("red","publicColor",$hasColor);
        $this->assertAttributeSame("green","protectedColor",$hasColor);
        $this->assertAttributeSame("blue","privateColor",$hasColor);
        
        $this->assertAttributeNotSame("wrong","privateColor",$hasColor);
    }

}

```

As you can see, the assertion works for any visibility, making it easy to peer into protected and private methods.

In addition, there is assertAttributeEquals, assertAttributeContains, assertAttributeContainsOnly, assertAttributeEmpty...etc, matching most assertions involving comparison.

