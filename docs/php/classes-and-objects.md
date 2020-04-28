---
metaTitle: "Classes and Objects"
description: "Class Constants, Abstract Classes, Late static binding, Namespacing and Autoloading, Interfaces, Method and Property Visibility, Final Keyword, Autoloading, Dynamic Binding, Calling a parent constructor when instantiating a child, $this, self and static plus the singleton, Defining a Basic Class, Anonymous Classes"
---

# Classes and Objects


Classes and Objects are used to to make your code more efficient and less repetitive by grouping similar tasks.

A class is used to define the actions and data structure used to build objects. The objects are then built using this predefined structure.



## Class Constants


Class constants provide a mechanism for holding fixed values in a program. That is, they provide a way of giving a name (and associated compile-time checking) to a value like `3.14` or `"Apple"`. Class constants can only be defined with the `const` keyword - the [define](http://php.net/define) function cannot be used in this context.

As an example, it may be convenient to have a shorthand representation for the value of π throughout a program. A class with `const` values provides a simple way to hold such values.

```php
class MathValues {
    const PI = M_PI;
    const PHI = 1.61803;
}

$area = MathValues::PI * $radius * $radius;

```

Class constants may be accessed by using the double colon operator (so-called the scope resolution operator) on a class, much like static variables. Unlike static variables, however, class constants have their values fixed at compile time and cannot be reassigned to (e.g. `MathValues::PI = 7` would produce a fatal error).

Class constants are also useful for defining things internal to a class that might need changing later (but do not change frequently enough to warrant storing in, say, a database). We can reference this internally using the `self` scope resolutor (which works in both instanced and static implementations)

```php
class Labor {
    /** How long, in hours, does it take to build the item? */
    const LABOR_UNITS = 0.26;
    /** How much are we paying employees per hour? */
    const LABOR_COST = 12.75;

    public function getLaborCost($number_units) {
         return (self::LABOR_UNITS * self::LABOR_COST) * $number_units;
    }
}

```

Class constants can only contain scalar values in versions < 5.6

As of PHP 5.6 we can use expressions with constants, meaning math statements and strings with concatenation are acceptable constants

```php
class Labor {
    /** How much are we paying employees per hour? Hourly wages * hours taken to make */
    const LABOR_COSTS = 12.75 * 0.26;

    public function getLaborCost($number_units) {
         return self::LABOR_COSTS * $number_units;
    }
}

```

As of PHP 7.0, constants declared with `define` may now contain arrays.

```php
define("BAZ", array('baz'));

```

Class constants are useful for more than just storing mathematical concepts. For example, if preparing a pie, it might be convenient to have a single `Pie` class capable of taking different kinds of fruit.

```php
class Pie {
    protected $fruit;

    public function __construct($fruit) {
        $this->fruit = $fruit;
    }
}

```

We can then use the `Pie` class like so

```php
$pie = new Pie("strawberry");

```

The problem that arises here is, when instantiating the `Pie` class, no guidance is provided as to the acceptable values. For example, when making a "boysenberry" pie, it might be misspelled "boisenberry". Furthermore, we might not support a plum pie. Instead, it would be useful to have a list of acceptable fruit types already defined somewhere it would make sense to look for them. Say a class named `Fruit`:

```php
class Fruit {
    const APPLE = "apple";
    const STRAWBERRY = "strawberry";
    const BOYSENBERRY = "boysenberry";
}

$pie = new Pie(Fruit::STRAWBERRY);

```

Listing the acceptable values as class constants provides a valuable hint as to the acceptable values which a method accepts. It also ensures that misspellings cannot make it past the compiler. While `new Pie('aple')` and `new Pie('apple')` are both acceptable to the compiler, `new Pie(Fruit::APLE)` will produce a compiler error.

Finally, using class constants means that the actual value of the constant may be modified in a single place, and any code using the constant automatically has the effects of the modification.

Whilst the most common method to access a class constant is `MyClass::CONSTANT_NAME`, it may also be accessed by:

```php
echo MyClass::CONSTANT;

$classname = "MyClass";
echo $classname::CONSTANT; // As of PHP 5.3.0

```

Class constants in PHP are conventionally named all in uppercase with underscores as word separators, although any valid label name may be used as a class constant name.

As of PHP 7.1, class constants may now be defined with different visibilities from the default public scope. This means that both protected and private constants can now be defined to prevent class constants from unnecessarily leaking into the public scope (see [Method and Property Visibility](http://stackoverflow.com/documentation/php/504/classes-and-objects/6471/method-and-property-visibility) ). For example:

```php
class Something {
    const PUBLIC_CONST_A = 1;
    public const PUBLIC_CONST_B = 2;
    protected const PROTECTED_CONST = 3;
    private const PRIVATE_CONST = 4;
}

```

### define vs class constants

Although this is a valid construction:

```php
function bar() { return 2; };

define('BAR', bar());

```

If you try to do the same with class constants, you'll get an error:

```php
function bar() { return 2; };

class Foo {
    const BAR = bar(); // Error: Constant expression contains invalid operations
}

```

But you can do:

```php
function bar() { return 2; };

define('BAR', bar());

class Foo {
    const BAR = BAR; // OK
}

```

For more information, see [constants in the manual](http://php.net/manual/en/language.constants.php).

### Using ::class to retrieve class's name

PHP 5.5 introduced the `::class` syntax to retrieve the full class name, taking namespace scope and `use` statements into account.

```php
namespace foo;
use bar\Bar;
echo json_encode(Bar::class); // "bar\\Bar"
echo json_encode(Foo::class); // "foo\\Foo"
echo json_encode(\Foo::class); // "Foo"

```

The above works even if the classes are not even defined (i.e. this code snippet works alone).

This syntax is useful for functions that require a class name. For example, it can be used with `class_exists` to check a class exists. No errors will be generated regardless of return value in this snippet:

```php
class_exists(ThisClass\Will\NeverBe\Loaded::class, false);

```



## Abstract Classes


An abstract class is a class that cannot be instantiated. Abstract classes can define abstract methods, which are methods without any body, only a definition:

```php
abstract class MyAbstractClass {
    abstract public function doSomething($a, $b);
}

```

Abstract classes should be extended by a child class which can then provide the implementation of these abstract methods.

The main purpose of a class like this is to provide a kind of template that allows children classes to inherit from, "forcing" a structure to adhere to. Lets elaborate on this with an example:

In this example we will be implementing a `Worker` interface. First we define the interface:

```php
interface Worker {
    public function run();
}

```

To ease the development of further Worker implementations, we will create an abstract worker class
that already provides the `run()` method from the interface, but specifies some abstract methods that need to be filled in by any child class:

```php
abstract class AbstractWorker implements Worker {
    protected $pdo;
    protected $logger;

    public function __construct(PDO $pdo, Logger $logger) {
        $this->pdo = $pdo;
        $this->logger = $logger;
    }

    public function run() {
        try {
            $this->setMemoryLimit($this->getMemoryLimit());
            $this->logger->log("Preparing main");
            $this->prepareMain();
            $this->logger->log("Executing main");
            $this->main();
        } catch (Throwable $e) {
            // Catch and rethrow all errors so they can be logged by the worker
            $this->logger->log("Worker failed with exception: {$e->getMessage()}");
            throw $e;
        }
    }

    private function setMemoryLimit($memoryLimit) {
        ini_set('memory_limit', $memoryLimit);
        $this->logger->log("Set memory limit to $memoryLimit");
    }

    abstract protected function getMemoryLimit();

    abstract protected function prepareMain();

    abstract protected function main();
}

```

First of all, we have provided an abstract method `getMemoryLimit()`. Any class extending from `AbstractWorker` needs to provide this method and return its memory limit. The `AbstractWorker` then sets the memory limit and logs it.

Secondly the `AbstractWorker` calls the `prepareMain()` and `main()` methods, after logging that they have been called.

Finally, all of these method calls have been grouped in a `try`-`catch` block. So if any of the abstract methods defined by the child class throws an exception, we will catch that exception, log it and rethrow it. This prevents all child classes from having to implement this themselves.

Now lets define a child class that extends from the `AbstractWorker`:

```php
class TranscactionProcessorWorker extends AbstractWorker {
    private $transactions;

    protected function getMemoryLimit() {
        return "512M";
    }

    protected function prepareMain() {
        $stmt = $this->pdo->query("SELECT * FROM transactions WHERE processed = 0 LIMIT 500");
        $stmt->execute();
        $this->transactions = $stmt->fetchAll();
    }

    protected function main() {
        foreach ($this->transactions as $transaction) {
            // Could throw some PDO or MYSQL exception, but that is handled by the AbstractWorker
            $stmt = $this->pdo->query("UPDATE transactions SET processed = 1 WHERE id = {$transaction['id']} LIMIT 1");
            $stmt->execute();
        }
    }
}

```

As you can see, the `TransactionProcessorWorker` was rather easy to implement, as we only had to specify the memory limit and worry about the actual actions that it needed to perform. No error handling is needed in the `TransactionProcessorWorker` because that is handled in the `AbsractWorker`.

### Important Note

> 
<p>When inheriting from an abstract class, all methods marked abstract in
the parent's class declaration must be defined by the child (or the child itself
must also be marked abstract);
additionally, these methods must be defined with the same (or a less
restricted) visibility. For example, if the abstract method is defined
as protected, the function implementation must be defined as either
protected or public, but not private.</p>


<sup>**Taken from the [PHP Documentation for Class Abstraction](http://php.net/manual/en/language.oop5.abstract.php).**</sup>

If you **do not** define the parent abstract classes methods within the child class, you will be thrown a **Fatal PHP Error** like the following.

> 
<p>**Fatal error:** Class X contains 1 abstract method and must therefore
be declared abstract or implement the remaining methods (X::x)
in</p>




## Late static binding


In PHP 5.3+ and above you can utilize [late static binding](http://php.net/manual/en/language.oop5.late-static-bindings.php) to control which class a static property or method is called from. It was added to overcome the problem inherent with the `self::` scope resolutor. Take the following code

```php
class Horse {
    public static function whatToSay() {
         echo 'Neigh!';
    }

    public static function speak() {
         self::whatToSay();
    }
}

class MrEd extends Horse {
    public static function whatToSay() {
         echo 'Hello Wilbur!';
    }
}

```

You would expect that the `MrEd` class will override the parent `whatToSay()` function. But when we run this we get something unexpected

```php
Horse::speak(); // Neigh!
MrEd::speak(); // Neigh!

```

The problem is that `self::whatToSay();` can only refer to the `Horse` class, meaning it doesn't obey `MrEd`. If we switch to the `static::` scope resolutor, we don't have this problem. This newer method tells the class to obey the instance calling it. Thus we get the inheritance we're expecting

```php
class Horse {
    public static function whatToSay() {
         echo 'Neigh!';
    }

    public static function speak() {
         static::whatToSay(); // Late Static Binding
    }
}

Horse::speak(); // Neigh!
MrEd::speak(); // Hello Wilbur!

```



## Namespacing and Autoloading


Technically, autoloading works by executing a callback when a PHP class is required but not found. Such callbacks usually attempt to load these classes.

Generally, autoloading can be understood as the attempt to load PHP files (especially PHP class files, where a PHP source file is dedicated for a specific class) from appropriate paths according to the class's fully-qualified name (FQN) when a class is needed.

Suppose we have these classes:

Class file for `application\controllers\Base`:

```php
<?php
namespace application\controllers { class Base {...} }

```

Class file for `application\controllers\Control`:

```php
<?php
namespace application\controllers { class Control {...} }

```

Class file for `application\models\Page`:

```php
<?php
namespace application\models { class Page {...} }

```

Under the source folder, these classes should be placed at the paths as their FQNs respectively:

<li>Source folder
<ul>
<li>`applications`
<ul>
<li>`controllers`
<ul>
- `Base.php`
- `Control.php`

- `Page.php`

This approach makes it possible to programmatically resolve the class file path according to the FQN, using this function:

```php
function getClassPath(string $sourceFolder, string $className, string $extension = ".php") {
    return $sourceFolder . "/" . str_replace("\\", "/", $className) . $extension; // note that "/" works as a directory separator even on Windows
}

```

The `spl_autoload_register` function allows us to load a class when needed using a user-defined function:

```php
const SOURCE_FOLDER = __DIR__ . "/src";
spl_autoload_register(function (string $className) {
    $file = getClassPath(SOURCE_FOLDER, $className);
    if (is_readable($file)) require_once $file;
});

```

This function can be further extended to use fallback methods of loading:

```php
const SOURCE_FOLDERS = [__DIR__ . "/src", "/root/src"]);
spl_autoload_register(function (string $className) {
    foreach(SOURCE_FOLDERS as $folder) {
        $extensions = [
            // do we have src/Foo/Bar.php5_int64?
            ".php" . PHP_MAJOR_VERSION . "_int" . (PHP_INT_SIZE * 8),
            // do we have src/Foo/Bar.php7?
            ".php" . PHP_MAJOR_VERSION,
            // do we have src/Foo/Bar.php_int64?
            ".php" . "_int" . (PHP_INT_SIZE * 8),
            // do we have src/Foo/Bar.phps?
            ".phps"
            // do we have src/Foo/Bar.php?
            ".php"
        ];
        foreach($extensions as $ext) {
            $path = getClassPath($folder, $className, $extension);
            if(is_readable($path)) return $path;
        }
    }
});

```

Note that PHP doesn't attempt to load the classes whenever a file that uses this class is loaded. It may be loaded in the middle of a script, or even in shutdown functions . This is one of the reasons why developers, especially those who use autoloading, should avoid replacing executing source files in the runtime, especially in phar files.



## Interfaces


### Introduction

Interfaces are definitions of the public APIs classes must implement to satisfy the interface. They work as "contracts", specifying **what** a set of subclasses does, but **not how** they do it.

Interface definition is much alike class definition, changing the keyword `class` to `interface`:

```php
interface Foo {

}

```

Interfaces can contain methods and/or constants, but no attributes. Interface constants have the same restrictions as class constants. Interface methods are implicitly abstract:

```php
interface Foo {
    const BAR = 'BAR';

    public function doSomething($param1, $param2);
}

```

**Note:** interfaces **must not** declare constructors or destructors, since these are implementation details on the class level.

### Realization

Any class that needs to implement an interface must do so using the `implements` keyword. To do so, the class needs to provide a implementation for every method declared in the interface, respecting the same signature.

A single class **can** implement more than one interface at a time.

```php
interface Foo {
    public function doSomething($param1, $param2);
}

interface Bar {
    public function doAnotherThing($param1);
}


class Baz implements Foo, Bar {
    public function doSomething($param1, $param2) {
        // ...
    }

    public function doAnotherThing($param1) {
        // ...
    }
}

```

When abstract classes implement interfaces, they do not need to implement all methods. Any method not implemented in the base class must then be implemented by the concrete class that extends it:

```php
abstract class AbstractBaz implements Foo, Bar {
    // Partial implementation of the required interface...
    public function doSomething($param1, $param2) {
        // ...
    }
}

class Baz extends AbstractBaz {
    public function doAnotherThing($param1) {
        // ...
    }
}

```

Notice that interface realization is an inherited characteristic. When extending a class that implements an interface, you do not need to redeclare it in the concrete class, because it is implicit.

> 
<p>**Note:**
Prior to PHP 5.3.9, a class could not implement two interfaces that specified a method
with the same name, since it would cause ambiguity. More recent versions of PHP allow
this as long as the duplicate methods have the same signature[[1]](http://php.net/manual/en/language.oop5.interfaces.php).</p>


### Inheritance

Like classes, it is possible to establish an inheritance relationship between interfaces, using the same keyword `extends`. The main difference is that multiple inheritance is allowed for interfaces:

```php
interface Foo {

}

interface Bar {

}

interface Baz extends Foo, Bar {

}

```

### Examples

In the example bellow we have a simple example interface for a vehicle. Vehicles can go forwards and backwards.

```php
interface VehicleInterface {
    public function forward();

    public function reverse();

    ...
}

class Bike implements VehicleInterface {
    public function forward() {
        $this->pedal();
    }

    public function reverse() {
        $this->backwardSteps();
    }

    protected function pedal() {
        ...
    }

    protected function backwardSteps() {
        ...
    }

    ...
}

class Car implements VehicleInterface {
    protected $gear = 'N';

    public function forward() {
        $this->setGear(1);
        $this->pushPedal();
    }

    public function reverse() {
        $this->setGear('R');
        $this->pushPedal();
    }

    protected function setGear($gear) {
        $this->gear = $gear;
    }

    protected function pushPedal() {
        ...
    }

    ...
}

```

Then we create two classes that implement the interface: Bike and Car. Bike and Car internally are very different, but both are vehicles, and must implement the same public methods that VehicleInterface provides.

Typehinting allows methods and functions to request Interfaces. Let's assume that we have a parking garage class, which contains vehicles of all kinds.

```php
class ParkingGarage {
    protected $vehicles = [];

    public function addVehicle(VehicleInterface $vehicle) {
        $this->vehicles[] = $vehicle;
    }
}

```

Because `addVehicle` requires a `$vehicle` of type `VehicleInterface`—not a concrete implementation—we can input both Bikes and Cars, which the ParkingGarage can manipulate and use.



## Method and Property Visibility


There are three visibility types that you can apply to methods (**class/object functions**) and properties (**class/object variables**) within a class, which provide access control for the method or property to which they are applied.

You can read extensively about these in the [PHP Documentation for OOP Visibility](http://php.net/manual/en/language.oop5.visibility.php).

### Public

Declaring a method or a property as `public` allows the method or property to be accessed by:

- The class that declared it.
- The classes that extend the declared class.
- Any external objects, classes, or code outside the class hierarchy.

An example of this `public` access would be:

```php
class MyClass {
    // Property
    public $myProperty = 'test';

    // Method
    public function myMethod() {
        return $this->myProperty;
    }
}

$obj = new MyClass();
echo $obj->myMethod();
// Out: test

echo $obj->myProperty;
// Out: test

```

### Protected

Declaring a method or a property as `protected` allows the method or property to be accessed by:

- The class that declared it.
- The classes that extend the declared class.

This **does not allow** external objects, classes, or code outside the class hierarchy to access these methods or properties. If something using this method/property does not have access to it, it will not be available, and an error will be thrown. **Only** instances of the declared self (or subclasses thereof) have access to it.

An example of this `protected` access would be:

```php
class MyClass {
    protected $myProperty = 'test';

    protected function myMethod() {
        return $this->myProperty;
    }
}

class MySubClass extends MyClass {
    public function run() {
        echo $this->myMethod();
    }
}

$obj = new MySubClass();
$obj->run(); // This will call MyClass::myMethod();
// Out: test

$obj->myMethod(); // This will fail.
// Out: Fatal error: Call to protected method MyClass::myMethod() from context ''

```

<sup>**The example above notes that you can only access the `protected` elements within it's own scope. Essentially: "What's in the house can only be access from inside the house."**</sup>

### Private

Declaring a method or a property as `private` allows the method or property to be accessed by:

- The class that declared it **Only** (not subclasses).

A `private` method or property is only visible and accessible within the class that created it.

Note that objects of the same type will have access to each others private and protected members even though they are not the same instances.

```php
class MyClass {
    private $myProperty = 'test';

    private function myPrivateMethod() {
        return $this->myProperty;
    }

    public function myPublicMethod() {
        return $this->myPrivateMethod();
    }

    public function modifyPrivatePropertyOf(MyClass $anotherInstance) {
        $anotherInstance->myProperty = "new value";
    }
}

class MySubClass extends MyClass {
    public function run() {
        echo $this->myPublicMethod();
    }

    public function runWithPrivate() {
        echo $this->myPrivateMethod();
    }
}

$obj = new MySubClass();
$newObj = new MySubClass();

// This will call MyClass::myPublicMethod(), which will then call
// MyClass::myPrivateMethod();
$obj->run(); 
// Out: test

    
$obj->modifyPrivatePropertyOf($newObj);

$newObj->run();
// Out: new value

echo $obj->myPrivateMethod(); // This will fail.
// Out: Fatal error: Call to private method MyClass::myPrivateMethod() from context ''

echo $obj->runWithPrivate(); // This will also fail.
// Out: Fatal error: Call to private method MyClass::myPrivateMethod() from context 'MySubClass'

```

<sup>**As noted, you can only access the `private` method/property from within it's defined class.**</sup>



## Final Keyword


Def:
**Final** Keyword prevents child classes from overriding a method by prefixing the definition with final. If the class itself is being defined final then it cannot be extended

**Final Method**

```php
class BaseClass {
   public function test() {
       echo "BaseClass::test() called\n";
   }
   
   final public function moreTesting() {
       echo "BaseClass::moreTesting() called\n";
   }
}

class ChildClass extends BaseClass {
   public function moreTesting() {
       echo "ChildClass::moreTesting() called\n";
   }
}
// Results in Fatal error: Cannot override final method BaseClass::moreTesting()

```

**Final Class:**

```php
final class BaseClass {
   public function test() {
       echo "BaseClass::test() called\n";
   }

   // Here it doesn't matter if you specify the function as final or not
   final public function moreTesting() {
       echo "BaseClass::moreTesting() called\n";
   }
}

class ChildClass extends BaseClass {
}
// Results in Fatal error: Class ChildClass may not inherit from final class (BaseClass)

```

**Final constants:** Unlike Java, the `final` keyword is not used for class constants in PHP. Use the keyword `const` instead.

**Why do I have to use `final`?**

1. Preventing massive inheritance chain of doom
1. Encouraging composition
1. Force the developer to think about user public API
1. Force the developer to shrink an object's public API
1. A `final` class can always be made extensible
1. `extends` breaks encapsulation
1. You don't need that flexibility
1. You are free to change the code

**When to avoid `final`:**
Final classes only work effectively under following assumptions:

1. There is an abstraction (interface) that the final class implements
1. All of the public API of the final class is part of that interface



## Autoloading


Nobody wants to `require` or `include` every time a class or inheritance is used. Because it can be painful and is easy to forget, PHP is offering so called autoloading. If you are already using Composer, read about [autoloading using Composer](https://stackoverflow.com/documentation/php/1053/composer/3397/autoloading-with-composer#t=201607252107257288764).

**What exactly is autoloading?**

The name basically says it all. You do not have to get the file where the requested class is stored in, but PHP **auto**matically **load**s it.

**How can I do this in basic PHP without third party code?**

There is the function [`__autoload`](https://secure.php.net/manual/function.autoload.php), but it is considered better practice to use [`spl_autoload_register`](https://secure.php.net/manual/function.spl-autoload-register.php). These functions will be considered by PHP every time a class is not defined within the given space. So adding autoload to an existing project is no problem, as defined classes (via `require` i.e.) will work like before. For the sake of preciseness, the following examples will use anonymous functions, if you use PHP < 5.3, you can define the function and pass it's name as argument to `spl_autoload_register`.

**Examples**

```php
spl_autoload_register(function ($className) {
    $path = sprintf('%s.php', $className);
    if (file_exists($path)) {
        include $path;
    } else {
        // file not found
    }
});

```

The code above simply tries to include a filename with the class name and the appended extension ".php" using [`sprintf`](https://secure.php.net/sprintf). If `FooBar` needs to be loaded, it looks if `FooBar.php` exists and if so includes it.

Of course this can be extended to fit the project's individual need. If `_` inside a class name is used to group, e.g. `User_Post` and `User_Image` both refer to `User`, both classes can be kept in a folder called "User" like so:

```php
spl_autoload_register(function ($className) {
    //                        replace _ by / or \ (depending on OS)
    $path = sprintf('%s.php', str_replace('_', DIRECTORY_SEPARATOR, $className) );
    if (file_exists($path)) {
        include $path;
    } else {
        // file not found
    }
});

```

The class `User_Post` will now be loaded from "User/Post.php", etc.

`spl_autoload_register` can be tailored to various needs. All your files with classes are named "class.CLASSNAME.php"? No problem. Various nesting (`User_Post_Content` => "User/Post/Content.php")? No problem either.

If you want a more elaborate autoloading mechanism - and still don't want to include Composer - you can work without adding third party libraries.

```php
spl_autoload_register(function ($className) {
    $path = sprintf('%1$s%2$s%3$s.php',
        // %1$s: get absolute path
        realpath(dirname(__FILE__)),
        // %2$s: / or \ (depending on OS)
        DIRECTORY_SEPARATOR,
        // %3$s: don't wory about caps or not when creating the files
        strtolower(
            // replace _ by / or \ (depending on OS)
            str_replace('_', DIRECTORY_SEPARATOR, $className)
        )
    );

    if (file_exists($path)) {
        include $path;
    } else {
        throw new Exception(
            sprintf('Class with name %1$s not found. Looked in %2$s.',
                $className,
                $path
            )
        );
    }
});

```

Using autoloaders like this, you can happily write code like this:

```php
require_once './autoload.php'; // where spl_autoload_register is defined

$foo = new Foo_Bar(new Hello_World());

```

Using classes:

```php
class Foo_Bar extends Foo {}

```

```php
class Hello_World implements Demo_Classes {}

```

These examples will be include classes from `foo/bar.php`, `foo.php`, `hello/world.php` and `demo/classes.php`.



## Dynamic Binding


Dynamic binding, also referred as **method overriding** is an example of **run time polymorphism** that occurs when multiple classes contain different implementations of the same method, but the object that the method will be called on is **unknown** until  **run time**.

This is useful if a certain condition dictates which class will be used to perform an action, where the action is named the same in both classes.

```php
interface Animal {
    public function makeNoise();
}

class Cat implements Animal {
    public function makeNoise
    {
        $this->meow();
    }
    ...
}

class Dog implements Animal {
    public function makeNoise {
        $this->bark();
    }
    ...
}

class Person {
    const CAT = 'cat';
    const DOG = 'dog';

    private $petPreference;
    private $pet;

    public function isCatLover(): bool {
        return $this->petPreference == self::CAT;
    }

    public function isDogLover(): bool {
        return $this->petPreference == self::DOG;
    }

    public function setPet(Animal $pet) {
        $this->pet = $pet;
    }

    public function getPet(): Animal {
        return $this->pet;
    }
}

if($person->isCatLover()) {
    $person->setPet(new Cat());
} else if($person->isDogLover()) {
    $person->setPet(new Dog());
}

$person->getPet()->makeNoise();

```

In the above example, the `Animal` class (`Dog|Cat`) which will `makeNoise` is unknown until run time depending on the property within the `User` class.



## Calling a parent constructor when instantiating a child


A common pitfall of child classes is that, if your parent and child both contain a constructor(`__construct()`) method, **only the child class constructor will run**. There may be occasions where you need to run the parent `__construct()` method from it's child. If you need to do that, then you will need to use the [`parent::`](http://php.net/manual/en/keyword.parent.php) scope resolutor:

```php
parent::__construct();

```

Now harnessing that within a real-world situation would look something like:

```php
class Foo {

    function __construct($args) { 
        echo 'parent'; 
    }

}

class Bar extends Foo {

    function __construct($args) {
        parent::__construct($args);
    }
}

```

The above will run the parent `__construct()` resulting in the `echo` being run.



## $this, self and static plus the singleton


> 
<p>Use `$this` to refer to the current object. Use `self` to refer to the
current class. In other words, use  `$this->member` for non-static
members, use `self::$member` for static members.</p>


In the example below, `sayHello()` and `sayGoodbye()` are using `self` and `$this` difference can be observed here.

```php
class Person {
    private $name;

    public function __construct($name) {
        $this->name = $name;
    }

    public function getName() {
        return $this->name;
    }

    public function getTitle() {
        return $this->getName()." the person";
    }

    public function sayHello() {
        echo "Hello, I'm ".$this->getTitle()."<br/>";
    }

    public function sayGoodbye() {
        echo "Goodbye from ".self::getTitle()."<br/>";
    }
}

class Geek extends Person {
    public function __construct($name) {
        parent::__construct($name);
    }

    public function getTitle() {
        return $this->getName()." the geek";
    }
}

$geekObj = new Geek("Ludwig");
$geekObj->sayHello();
$geekObj->sayGoodbye();

```

`static` refers to whatever class in the hierarchy you called the method on. It allows for better reuse of static class properties when classes are inherited.

Consider the following code:

```php
class Car {
    protected static $brand = 'unknown';
    
    public static function brand() {
         return self::$brand."\n";
    }
}

class Mercedes extends Car {
    protected static $brand = 'Mercedes';
}

class BMW extends Car {
    protected static $brand = 'BMW';
}

echo (new Car)->brand();
echo (new BMW)->brand();
echo (new Mercedes)->brand();

```

This doesn't produce the result you want:

> 
<p>unknown<br />
unknown<br />
unknown</p>


That's because `self` refers to the `Car` class whenever method `brand()` is called.

To refer to the correct class, you need to use `static` instead:

```php
class Car {
    protected static $brand = 'unknown';
    
    public static function brand() {
         return static::$brand."\n";
    }
}

class Mercedes extends Car {
    protected static $brand = 'Mercedes';
}

class BMW extends Car {
    protected static $brand = 'BMW';
}

echo (new Car)->brand();
echo (new BMW)->brand();
echo (new Mercedes)->brand();

```

This does produce the desired output:

> 
<p>unknown<br />
BMW<br />
Mercedes</p>


See also [Late static binding](http://stackoverflow.com/documentation/php/504/classes-and-objects/5420/late-static-binding#t=201608122306182437362)

### The singleton

If you have an object that's expensive to create or represents a connection to some external resource you want to reuse, i.e. a database connection where there is no connection pooling or a socket to some other system, you can use the `static` and `self` keywords in a class to make it a singleton. There are strong opinions about whether the singleton pattern should or should not be used, but it does have its uses.

```php
class Singleton {
    private static $instance = null;

    public static function getInstance(){
        if(!isset(self::$instance)){
            self::$instance = new self();
        }
        
        return self::$instance;
    }
    
    private function __construct() {
        // Do constructor stuff
    }
}

```

As you can see in the example code we are defining a private static property `$instance` to hold the object reference. Since this is static this reference is shared across ALL objects of this type.

The `getInstance()`method uses a method know as lazy instantiation to delay creating the object to the last possible moment as you do not want to have unused objects lying around in memory never intended to be used. It also saves time and CPU on page load not having to load more objects than necessary. The method is checking if the object is set, creating it if not, and returning it. This ensures that only one object of this kind is ever created.

We are also setting the constructor to be private to ensure that no one creates it with the `new` keyword from the outside. If you need to inherit from this class just change the `private` keywords to `protected`.

To use this object you just write the following:

```php
$singleton = Singleton::getInstance();

```

Now I DO implore you to use dependency injection where you can and aim for loosely coupled objects, but sometimes that is just not reasonable and the singleton pattern can be of use.



## Defining a Basic Class


An object in PHP contains variables and functions. Objects typically belong to a class, which defines the variables and functions that all objects of this class will contain.

The syntax to define a class is:

```php
class Shape {
    public $sides = 0;
    
    public function description() {
        return "A shape with $this->sides sides.";
    }
}

```

Once a class is defined, you can create an instance using:

```php
$myShape = new Shape();

```

Variables and functions on the object are accessed like this:

```php
$myShape = new Shape();
$myShape->sides = 6;

print $myShape->description(); // "A shape with 6 sides"

```

### Constructor

Classes can define a special `__construct()` method, which is executed as part of object creation. This is often used to
specify the initial values of an object:

```php
class Shape {
    public $sides = 0;
    
    public function __construct($sides) {
        $this->sides = $sides;
    }
    
    public function description() {
        return "A shape with $this->sides sides.";
    }
}

$myShape = new Shape(6);

print $myShape->description(); // A shape with 6 sides

```

### Extending Another Class

Class definitions can extend existing class definitions, adding new variables and functions as well as modifying those defined in the parent class.

Here is a class that extends the previous example:

```php
class Square extends Shape {
    public $sideLength = 0;
    
    public function __construct($sideLength) {
       parent::__construct(4);
       
       $this->sideLength = $sideLength;
    }
    
    public function perimeter() {
        return $this->sides * $this->sideLength;
    }

    public function area() {
        return $this->sideLength * $this->sideLength;
    }
}

```

The `Square` class contains variables and behavior for both the `Shape` class and the `Square` class:

```php
$mySquare = new Square(10);

print $mySquare->description()/ // A shape with 4 sides

print $mySquare->perimeter() // 40

print $mySquare->area() // 100

```



## Anonymous Classes


Anonymous classes were introduced into PHP 7 to enable for quick one-off objects to be easily created. They can take constructor arguments, extend other classes, implement interfaces, and use traits just like normal classes can.

In its most basic form, an anonymous class looks like the following:

```php
new class("constructor argument") {
    public function __construct($param) {
        var_dump($param);
    }
}; // string(20) "constructor argument"

```

Nesting an anonymous class inside of another class does not give it access to private or protected methods or properties of that outer class. Access to protected methods and properties of the outer class can be gained by extending the outer class from the anonymous class. Access to private properties of the outer class can be gained by passing them through to the anonymous class's constructor.

For example:

```php
class Outer {
    private $prop = 1;
    protected $prop2 = 2;

    protected function func1() {
        return 3;
    }

    public function func2() {
        // passing through the private $this->prop property
        return new class($this->prop) extends Outer {
            private $prop3;

            public function __construct($prop) {
                $this->prop3 = $prop;
            }

            public function func3() {
                // accessing the protected property Outer::$prop2
                // accessing the protected method Outer::func1()
                // accessing the local property self::$prop3 that was private from Outer::$prop
                return $this->prop2 + $this->func1() + $this->prop3;
            }
        };
    }
}

echo (new Outer)->func2()->func3(); // 6

```



#### Syntax


- `class <ClassName> [ extends <ParentClassName> ] [ implements <Interface1> [, <Interface2>, ... ] { }` // Class declaration
- `interface <InterfaceName> [ extends <ParentInterface1> [, <ParentInterface2>, ...] ] { }` // Interface declaration
- `use <Trait1> [, <Trait2>, ...]`; // Use traits
- `[ public | protected | private ] [ static ] $<varName>;` // Attribute declaration
- `const <CONST_NAME>;`  // Constant declaration
- `[ public | protected | private ] [ static ] function <methodName>([args...]) { }` // Method declaration



#### Remarks


### Classes and Interface components

Classes may have properties, constants and methods.

- **Properties** hold variables in the scope of the object. They may be initialized on declaration, but only if they contain a primitive value.
- **Constants** must be initialized on declaration and can only contain a primitive value. Constant values are fixed at compile time and may not be assigned at run time.
- **Methods** must have a body, even an empty one, unless the method is declared abstract.

```php
class Foo {
    private $foo = 'foo'; // OK
    private $baz = array(); // OK
    private $bar = new Bar(); // Error!
}

```

Interfaces cannot have properties, but may have constants and methods.

- Interface **constants** must be initialized on declaration and can only contain a primitive value. Constant values are fixed at compile time and may not be assigned at run time.
- Interface **methods** have no body.

```php
interface FooBar {
    const FOO_VALUE = 'bla';
    public function doAnything();
}

```

