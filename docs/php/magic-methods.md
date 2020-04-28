---
metaTitle: "Magic Methods"
description: "__call() and __callStatic(), __get(), __set(), __isset() and __unset(), __construct() and __destruct(), __toString(), __clone(), __invoke(), __sleep() and __wakeup(), __debugInfo()"
---

# Magic Methods



## __call() and __callStatic()


`__call()` and `__callStatic()` are called when somebody is calling nonexistent object method in
object or static context.

```php
class Foo
{
    /**
     * This method will be called when somebody will try to invoke a method in object
     * context, which does not exist, like:
     *
     * $foo->method($arg, $arg1);
     *
     * First argument will contain the method name(in example above it will be "method"),
     * and the second will contain the values of $arg and $arg1 as an array.
     */
    public function __call($method, $arguments)
    {
        // do something with that information here, like overloading
        // or something generic.
        // For sake of example let's say we're making a generic class,
        // that holds some data and allows user to get/set/has via
        // getter/setter methods. Also let's assume that there is some
        // CaseHelper which helps to convert camelCase into snake_case.
        // Also this method is simplified, so it does not check if there
        // is a valid name or
        $snakeName = CaseHelper::camelToSnake($method);
        // Get get/set/has prefix
        $subMethod = substr($snakeName, 0, 3);

        // Drop method name.
        $propertyName = substr($snakeName, 4);

        switch ($subMethod) {
            case "get":
                return $this->data[$propertyName];
            case "set":
                $this->data[$propertyName] = $arguments[0];
                break;
            case "has":
                return isset($this->data[$propertyName]);
            default:
                throw new BadMethodCallException("Undefined method $method");
        }
    }

    /**
     * __callStatic will be called from static content, that is, when calling a nonexistent
     * static method:
     *
     * Foo::buildSomethingCool($arg);
     *
     * First argument will contain the method name(in example above it will be "buildSomethingCool"),
     * and the second will contain the value $arg in an array.
     *
     * Note that signature of this method is different(requires static keyword). This method was not
     * available prior PHP 5.3
     */
    public static function __callStatic($method, $arguments)
    {
        // This method can be used when you need something like generic factory
        // or something else(to be honest use case for this is not so clear to me).
        print_r(func_get_args());
    }
}

```

### Example:

```php
$instance = new Foo();

$instance->setSomeState("foo");
var_dump($instance->hasSomeState());      // bool(true)
var_dump($instance->getSomeState());      // string "foo"

Foo::exampleStaticCall("test");
// outputs:
Array
(
    [0] => exampleCallStatic
    [1] => test
)

```



## __get(), __set(), __isset() and __unset()


Whenever you attempt to retrieve a certain field from a class like so:

```php
$animal = new Animal();
$height = $animal->height;

```

PHP invokes the magic method `__get($name)`, with `$name` equal to `"height"` in this case. Writing to a class field like so:

```php
$animal->height = 10;

```

Will invoke the magic method `__set($name, $value)`, with `$name` equal to `"height"` and `$value` equal to `10`.

PHP also has two built-in functions `isset()`, which check if a variable exists, and `unset()`, which destroys a variable. Checking whether a objects field is set like so:

```php
isset($animal->height);

```

Will invoke the `__isset($name)` function on that object. Destroying a variable like so:

```php
unset($animal->height);

```

Will invoke the `__unset($name)` function on that object.

Normally, when you don't define these methods on your class, PHP just retrieves the field as it is stored in your class. However, you can override these methods to create classes that can hold data like an array, but are usable like an object:

```php
class Example {
    private $data = [];

    public function __set($name, $value) {
        $this->data[$name] = $value;
    }

    public function __get($name) {
        if (!array_key_exists($name, $this->data)) {
            return null;
        }

        return $this->data[$name];
    }

    public function __isset($name) {
        return isset($this->data[$name]);
    }

    public function __unset($name) {
        unset($this->data[$name]);
    }
}

$example = new Example();

// Stores 'a' in the $data array with value 15
$example->a = 15;

// Retrieves array key 'a' from the $data array
echo $example->a; // prints 15

// Attempt to retrieve non-existent key from the array returns null
echo $example->b; // prints nothing

// If __isset('a') returns true, then call __unset('a')
if (isset($example->a)) {
    unset($example->a));
}

```

### empty() function and magic methods

Note that calling [`empty()`](http://php.net/manual/en/function.empty.php) on a class attribute will invoke `__isset()` because as the PHP manual states:

> 
empty() is essentially the concise equivalent to **!isset($var) || $var == false**




## __construct() and __destruct()


`__construct()` is the most common magic method in PHP, because it is used to set up a class when it is initialized. The opposite of the `__construct()` method is the `__destruct()` method. This method is called when there are no more references to an object that you created or when you force its deletion. PHP's garbage collection will clean up the object by first calling its destructor and then removing it from memory.

```php
class Shape {
    public function __construct() {
        echo "Shape created!\n";
    }
}

class Rectangle extends Shape {
    public $width;
    public $height;

    public function __construct($width, $height) {
        parent::__construct();

        $this->width = $width;
        $this->height = $height;
        echo "Created {$this->width}x{$this->height} Rectangle\n"; 
    }

    public function __destruct() {
        echo "Destroying {$this->width}x{$this->height} Rectangle\n";
    }
}

function createRectangle() {
    // Instantiating an object will call the constructor with the specified arguments
    $rectangle = new Rectangle(20, 50);

    // 'Shape Created' will be printed
    // 'Created 20x50 Rectangle' will be printed
}

createRectangle();
// 'Destroying 20x50 Rectangle' will be printed, because
// the `$rectangle` object was local to the createRectangle function, so
// When the function scope is exited, the object is destroyed and its
// destructor is called.

// The destructor of an object is also called when unset is used:
unset(new Rectangle(20, 50));

```



## __toString()


Whenever an object is treated as a string, the `__toString()` method is called. This method should return a string representation of the class.

```php
class User {
    public $first_name;
    public $last_name;
    public $age;

    public function __toString() {
        return "{$this->first_name} {$this->last_name} ($this->age)";
    }
}

$user = new User();
$user->first_name = "Chuck";
$user->last_name = "Norris";
$user->age = 76;

// Anytime the $user object is used in a string context, __toString() is called

echo $user; // prints 'Chuck Norris (76)'

// String value becomes: 'Selected user: Chuck Norris (76)'
$selected_user_string = sprintf("Selected user: %s", $user);

// Casting to string also calls __toString()
$user_as_string = (string) $user;

```



## __clone()


`__clone` is invoked by use of the `clone` keyword. It is used to manipulate object state upon cloning, after the object has been actually cloned.

```php
class CloneableUser
{
    public $name;
    public $lastName;

    /**
     * This method will be invoked by a clone operator and will prepend "Copy " to the
     * name and lastName properties.
     */
    public function __clone()
    {
        $this->name = "Copy " . $this->name;
        $this->lastName = "Copy " . $this->lastName;
    }
}

```

Example:

```php
$user1 = new CloneableUser();
$user1->name = "John";
$user1->lastName = "Doe";

$user2 = clone $user1; // triggers the __clone magic method

echo $user2->name;     // Copy John
echo $user2->lastName; // Copy Doe

```



## __invoke()


This magic method is called when user tries to invoke object as a function. Possible use cases may include
some approaches like functional programming or some callbacks.

```php
class Invokable
{
    /**
     * This method will be called if object will be executed like a function:
     *
     * $invokable();
     *
     * Args will be passed as in regular method call.
     */
    public function __invoke($arg, $arg, ...)
    {
        print_r(func_get_args());
    }
}

// Example:
$invokable = new Invokable();
$invokable([1, 2, 3]);

// optputs:
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)

```



## __sleep() and __wakeup()


`__sleep` and `__wakeup` are methods that are related to the serialization process. `serialize` function
checks if a class has a `__sleep` method. If so, it will be executed before any serialization.
`__sleep` is supposed to return an array of the names of all variables of an object that should be
serialized.

`__wakeup` in turn will be executed by `unserialize` if it is present in class. It's intention is to re-establish
resources and other things that are needed to be initialized upon unserialization.

```php
class Sleepy {
    public $tableName;
    public $tableFields;
    public $dbConnection;

    /**
     * This magic method will be invoked by serialize function.
     * Note that $dbConnection is excluded.
     */
    public function __sleep()
    {
        // Only $this->tableName and $this->tableFields will be serialized.
        return ['tableName', 'tableFields'];
    }

    /**
     * This magic method will be called by unserialize function.
     *
     * For sake of example, lets assume that $this->c, which was not serialized,
     * is some kind of a database connection. So on wake up it will get reconnected.
     */
    public function __wakeup()
    {
        // Connect to some default database and store handler/wrapper returned into
        // $this->dbConnection
        $this->dbConnection = DB::connect();
    }
}

```



## __debugInfo()


> 
This method is called by `var_dump()` when dumping an object to get the properties that should be shown. If the method isn't defined on an object, then all public, protected and private properties will be shown. — [PHP Manual](https://secure.php.net/manual/en/language.oop5.magic.php#object.debuginfo)


```php
class DeepThought {
    public function __debugInfo() {
        return [42];
    }
}

```

```php
var_dump(new DeepThought());

```

The above example will output:

```php
class DeepThought#1 (0) {
}

```

```php
var_dump(new DeepThought());

```

The above example will output:

```php
class DeepThought#1 (1) {
  public ${0} =>
  int(42)
}

```

