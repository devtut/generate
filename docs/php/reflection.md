---
metaTitle: "PHP - Reflection"
description: "Accessing private and protected member variables, Feature detection of classes or objects, Testing private/protected methods"
---

# Reflection



## Accessing private and protected member variables


Reflection is often used as part of software testing, such as for the runtime creation/instantiation of mock objects. It's also great for inspecting the state of an object at any given point in time. Here's an example of using Reflection in a unit test to verify a protected class member contains the expected value.

Below is a very basic class for a Car. It has a protected member variable that will contain the value representing the color of the car. Because the member variable is protected we cannot access it directly and must use a getter and setter method to retrieve and set its value respectively.

```php
class Car
{
    protected $color
    
    public function setColor($color)
    {
        $this->color = $color;
    }
    
    public function getColor($color)
    {
        return $this->color;
    }
}

```

To test this many developers will create a Car object, set the car's color using `Car::setColor()`, retrieve the color using `Car::getColor()`, and compare that value to the color they set:

```php
/**
 * @test
 * @covers     \Car::setColor
 */
public function testSetColor()
{
    $color = 'Red';

    $car = new \Car();
    $car->setColor($color);
    $getColor = $car->getColor();
        
    $this->assertEquals($color, $reflectionColor);
}

```

On the surface this seems okay. After all, all `Car::getColor()` does is return the value of the protected member variable `Car::$color`. But this test is flawed in two ways:

1. It exercises `Car::getColor()` which is out of the scope of this test
1. It depends on `Car::getColor()` which may have a bug itself which can make the test have a false positive or negative

Let's look at why we shouldn't use `Car::getColor()` in our unit test and should use Reflection instead. Let's say a developer is assigned a task to add "Metallic" to every car color. So they attempt to modify the `Car::getColor()` to prepend "Metallic" to the car's color:

```php
class Car
{
    protected $color
    
    public function setColor($color)
    {
        $this->color = $color;
    }
    
    public function getColor($color)
    {
        return "Metallic "; $this->color;
    }
}

```

Do you see the error? The developer used a semi-colon instead of the concatenation operator in an attempt to prepend "Metallic" to the car's color. As a result, whenever `Car::getColor()` is called, "Metallic " will be returned regardless of what the car's actual color is. As a result our `Car::setColor()` unit test will fail **even though `Car::setColor()` works perfectly fine and was not affected by this change**.

So how do we verify `Car::$color` contains the value we are setting via `Car::setColor()`? We can use Refelection to inspect the protected member variable directly. So how do we do **that**? We can use Refelection to make the protected member variable accessible to our code so it can retrieve the value.

Let's see the code first and then break it down:

```php
/**
 * @test
 * @covers     \Car::setColor
 */
public function testSetColor()
{
    $color = 'Red';

    $car = new \Car();
    $car->setColor($color);
    
    $reflectionOfCar = new \ReflectionObject($car);
    $protectedColor = $reflectionOfForm->getProperty('color');
    $protectedColor->setAccessible(true);
    $reflectionColor = $protectedColor->getValue($car);
    
    $this->assertEquals($color, $reflectionColor);
}

```

Here is how we are using Reflection to get the value of `Car::$color` in the code above:

1. We create a new [ReflectionObject](http://php.net/manual/en/class.reflectionobject.php) representing our Car object
1. We get a [ReflectionProperty](http://php.net/manual/en/class.reflectionproperty.php) for `Car::$color` (this "represents" the `Car::$color` variable)
1. We make `Car::$color` accessible
1. We get the value of `Car::$color`

As you can see by using Reflection we could get the value of `Car::$color` without having to call `Car::getColor()` or any other accessor function which could cause invalid test results. Now our unit test for `Car::setColor()` is safe and accurate.



## Feature detection of classes or objects


Feature detection of classes can partly be done with the `property_exists` and `method_exists` functions.

```php
class MyClass {
    public $public_field;
    protected $protected_field;
    private $private_field;
    static $static_field;
    const CONSTANT = 0;
    public function public_function() {}
    protected function protected_function() {}
    private function private_function() {}
    static function static_function() {}
}

// check properties
$check = property_exists('MyClass', 'public_field');    // true
$check = property_exists('MyClass', 'protected_field'); // true
$check = property_exists('MyClass', 'private_field');   // true, as of PHP 5.3.0
$check = property_exists('MyClass', 'static_field');    // true
$check = property_exists('MyClass', 'other_field');     // false

// check methods
$check = method_exists('MyClass', 'public_function');    // true
$check = method_exists('MyClass', 'protected_function');    // true
$check = method_exists('MyClass', 'private_function');    // true
$check = method_exists('MyClass', 'static_function');    // true

// however...
$check = property_exists('MyClass', 'CONSTANT');  // false
$check = property_exists($object, 'CONSTANT');    // false

```

With a `ReflectionClass`, also constants can be detected:

```php
$r = new ReflectionClass('MyClass');
$check = $r->hasProperty('public_field');  // true
$check = $r->hasMethod('public_function'); // true
$check = $r->hasConstant('CONSTANT');      // true
// also works for protected, private and/or static members.

```

Note: for `property_exists` and `method_exists`, also an object of the class of interest can be provided instead of the class name. Using reflection, the `ReflectionObject` class should be used instead of `ReflectionClass`.



## Testing private/protected methods


Sometimes it's useful to test private & protected methods as well as public ones.

```php
class Car
{
    /**
     * @param mixed $argument
     *
     * @return mixed
     */
    protected function drive($argument)
    {
        return $argument;
    }

    /**
     * @return bool
     */
    private static function stop()
    {
        return true;
    }
}

```

Easiest way to test drive method is using reflection

```php
class DriveTest
{
    /**
     * @test
     */
    public function testDrive()
    {
        // prepare
        $argument = 1;
        $expected = $argument;
        $car = new \Car();

        $reflection = new ReflectionClass(\Car::class);
        $method = $reflection->getMethod('drive');
        $method->setAccessible(true);

        // invoke logic
        $result = $method->invokeArgs($car, [$argument]);

        // test
        $this->assertEquals($expected, $result);
    }
}

```

If the method is static you pass null in the place of the class instance

```php
class StopTest
{
    /**
     * @test
     */
    public function testStop()
    {
        // prepare
        $expected = true;

        $reflection = new ReflectionClass(\Car::class);
        $method = $reflection->getMethod('stop');
        $method->setAccessible(true);

        // invoke logic
        $result = $method->invoke(null);

        // test
        $this->assertEquals($expected, $result);
    }
}

```

