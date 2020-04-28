---
metaTitle: "Traits"
description: "Traits to facilitate horizontal code reuse, What is a Trait?, Conflict Resolution, Implementing a Singleton using Traits, Multiple Traits Usage, Changing Method Visibility, Traits to keep classes clean"
---

# Traits



## Traits to facilitate horizontal code reuse


Let's say we have an interface for logging:

```php
interface Logger {
    function log($message);
}

```

Now say we have two concrete implementations of the `Logger` interface: the `FileLogger` and the `ConsoleLogger`.

```php
class FileLogger implements Logger {
    public function log($message) {
        // Append log message to some file
    }
}

class ConsoleLogger implements Logger {
    public function log($message) {
        // Log message to the console
    }
}

```

Now if you define some other class `Foo` which you also want to be able to perform logging tasks, you could do something like this:

```php
class Foo implements Logger {
    private $logger;

    public function setLogger(Logger $logger) {
        $this->logger = $logger;
    }

    public function log($message) {
        if ($this->logger) {
            $this->logger->log($message);
        }
    }
}

```

`Foo` is now also a `Logger`, but its functionality depends on the `Logger` implementation passed to it via `setLogger()`. If we now want class `Bar` to also have this logging mechanism, we would have to duplicate this piece of logic in the `Bar` class.

Instead of duplicating the code, a trait can be defined:

```php
trait LoggableTrait {
    protected $logger;

    public function setLogger(Logger $logger) {
        $this->logger = $logger;
    }

    public function log($message) {
        if ($this->logger) {
            $this->logger->log($message);
        }
    }
}

```

Now that we have defined the logic in a trait, we can use the trait to add the logic to the `Foo` and `Bar` classes:

```php
class Foo {
    use LoggableTrait;
}

class Bar {
    use LoggableTrait;
}

```

And, for example, we can use the `Foo` class like this:

```php
$foo = new Foo();
$foo->setLogger( new FileLogger() );

//note how we use the trait as a 'proxy' to call the Logger's log method on the Foo instance
$foo->log('my beautiful message'); 

```



## What is a Trait?


PHP only allows single inheritance. In other words, a class can only `extend` one other class. But what if you need to include something that doesn't belong in the parent class? Prior to PHP 5.4 you would have to get creative, but in 5.4 Traits were introduced. Traits allow you to basically "copy and paste" a portion of a class into your main class

```php
trait Talk {
    /** @var string */
    public $phrase = 'Well Wilbur...';
    public function speak() {
         echo $this->phrase;
    }
}

class MrEd extends Horse {
    use Talk;
    public function __construct() {
         $this->speak();
    }

    public function setPhrase($phrase) {
         $this->phrase = $phrase;
    }
}

```

So here we have `MrEd`, which is already extending `Horse`. But not all horses `Talk`, so we have a Trait for that. Let's note what this is doing

First, we define our Trait. We can use it with autoloading and Namespaces (see also [Referencing a class or function in a namespace](http://stackoverflow.com/documentation/php/1021/namespaces/3305/referencing-a-class-or-function-in-a-namespace)). Then we include it into our `MrEd` class with the keyword `use`.

You'll note that `MrEd` takes to using the `Talk` functions and variables without defining them. Remember what we said about **copy and paste**? These functions and variables are all defined within the class now, as if this class had defined them.

Traits are most closely related to [Abstract classes](http://stackoverflow.com/documentation/php/504/classes-and-objects/6304/abstract-classes) in that you can define variables and functions. You also cannot instantiate a Trait directly (i.e. `new Trait()`). Traits cannot force a class to implicitly define a function like an Abstract class or an Interface can. Traits are **only** for explicit definitions (since you can `implement` as many Interfaces as you want, see [Interfaces](http://stackoverflow.com/documentation/php/504/classes-and-objects/2754/using-interfaces)).

### When should I use a Trait?

The first thing you should do, when considering a Trait, is to ask yourself this important question

> 
Can I avoid using a Trait by restructuring my code?


More often than not, the answer is going to be **Yes**. Traits are edge cases caused by single inheritance. The temptation to misuse or overuse Traits can be high. But consider that a Trait introduces another source for your code, which means there's another layer of complexity. In the example here, we're only dealing with 3 classes. But Traits mean you can now be dealing with far more than that. For each Trait, your class becomes that much harder to deal with, since you must now go reference each Trait to find out what it defines (and potentially where a collision happened, see [Conflict Resolution](http://stackoverflow.com/documentation/php/999/traits/7271/conflict-resolution)). Ideally, you should keep as few Traits in your code as possible.



## Conflict Resolution


Trying to use several traits into one class could result in issues involving conflicting methods. You need to resolve such conflicts manually.

For example, let's create this hierarchy:

```php
trait MeowTrait {
    public function say() {
        print "Meow \n";
    }
}

trait WoofTrait {
    public function say() {
        print "Woof \n";
    }
}

abstract class UnMuteAnimals {
    abstract function say();
}

class Dog extends UnMuteAnimals {
    use WoofTrait;
}

class Cat extends UnMuteAnimals {
    use MeowTrait;
}

```

Now, let's try to create the following class:

```php
class TalkingParrot extends UnMuteAnimals {
    use MeowTrait, WoofTrait;
}

```

The php interpreter will return a fatal error:

> 
<p>**Fatal error:**
Trait method say has not been applied, because there are collisions with other trait methods on TalkingParrot</p>


To resolve this conflict, we could do this:

- use keyword `insteadof` to use the method from one trait instead of method from another trait
- create an alias for the method with a construct like  `WoofTrait::say as sayAsDog;`

```php
class TalkingParrotV2 extends UnMuteAnimals {
    use MeowTrait, WoofTrait {
        MeowTrait::say insteadof WoofTrait;
        WoofTrait::say as sayAsDog;
    }
}

$talkingParrot = new TalkingParrotV2();
$talkingParrot->say();
$talkingParrot->sayAsDog();

```

This code will produce the following output:

> 
<p>Meow<br />
Woof</p>




## Implementing a Singleton using Traits


**Disclaimer**: **In no way does this example advocate the use of singletons. Singletons are to be used with a lot of care.**

In PHP there is quite a standard way of implementing a singleton:

```php
public class Singleton {
    private $instance;

    private function __construct() { };

    public function getInstance() {
        if (!self::$instance) {
            // new self() is 'basically' equivalent to new Singleton()
            self::$instance = new self();
        }

        return self::$instance;
    }

    // Prevent cloning of the instance
    protected function __clone() { }

    // Prevent serialization of the instance
    protected function __sleep() { }

    // Prevent deserialization of the instance
    protected function __wakeup() { }
}

```

To prevent code duplication, it is a good idea to extract this behaviour into a trait.

```php
trait SingletonTrait {
    private $instance;

    protected function __construct() { };

    public function getInstance() {
        if (!self::$instance) {
            // new self() will refer to the class that uses the trait
            self::$instance = new self();
        }

        return self::$instance;
    }

    protected function __clone() { }
    protected function __sleep() { }
    protected function __wakeup() { }
}

```

Now any class that wants to function as a singleton can simply use the trait:

```php
class MyClass {
    use SingletonTrait;
}

// Error! Constructor is not publicly accessible
$myClass = new MyClass();

$myClass = MyClass::getInstance();

// All calls below will fail due to method visibility
$myClassCopy = clone $myClass; // Error!
$serializedMyClass = serialize($myClass); // Error!
$myClass = deserialize($serializedMyclass); // Error!

```

Even though it is now impossible to serialize a singleton, it is still useful to also disallow the deserialize method.



## Multiple Traits Usage


```php
trait Hello {
    public function sayHello() {
        echo 'Hello ';
    }
}

trait World {
    public function sayWorld() {
        echo 'World';
    }
}

class MyHelloWorld {
    use Hello, World;
    public function sayExclamationMark() {
        echo '!';
    }
}

$o = new MyHelloWorld();
$o->sayHello();
$o->sayWorld();
$o->sayExclamationMark();

```

The above example will output:

```php
Hello World!

```



## Changing Method Visibility


```php
trait HelloWorld {
    public function sayHello() {
        echo 'Hello World!';
    }
}

// Change visibility of sayHello
class MyClass1 {
    use HelloWorld { sayHello as protected; }
}

// Alias method with changed visibility
// sayHello visibility not changed
class MyClass2 {
    use HelloWorld { sayHello as private myPrivateHello; }
}

```

Running this example:

```php
(new MyClass1())->sayHello();
// Fatal error: Uncaught Error: Call to protected method MyClass1::sayHello()

(new MyClass2())->myPrivateHello();
// Fatal error: Uncaught Error: Call to private method MyClass2::myPrivateHello()

(new MyClass2())->sayHello();
// Hello World!

```

So be aware that in the last example in `MyClass2` the original un-aliased method from `trait HelloWorld` stays accessible as-is.



## Traits to keep classes clean


Over time, our classes may implement more and more interfaces. When these interfaces have many methods, the total number of methods in our class will become very large.

For example, let's suppose that we have two interfaces and a class implementing them:

```php
interface Printable {
    public function print();   
    //other interface methods...
}

interface Cacheable {
    //interface methods
}

class Article implements Cachable, Printable {  
    //here we must implement all the interface methods
    public function print(){ {
        /* code to print the article */ 
    }
}

```

Instead of implementing all the interface methods inside the `Article` class, we could use separate Traits to implement these interfaces, **keeping the class smaller and separating the code of the interface implementation from the class.**

From example, to implement the `Printable` interface we could create this trait:

```php
trait PrintableArticle {
    //implements here the interface methods
    public function print() {
        /* code to print the article */ 
    }
}

```

and make the class use the trait:

```php
class Article implements Cachable, Printable {
    use PrintableArticle;
    use CacheableArticle; 
} 

```

The primary benefits would be that our interface-implementation methods will be separated from the rest of the class, and stored in a trait who has the **sole responsibility to implement the interface for that particular type of object.**

