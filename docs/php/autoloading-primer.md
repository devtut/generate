---
metaTitle: "PHP - Autoloading Primer"
description: "Inline class definition, no loading required, Manual class loading with require, Autoloading replaces manual class definition loading, Autoloading as part of a framework solution, Autoloading with Composer"
---

# Autoloading Primer



## Inline class definition, no loading required


```php
// zoo.php
class Animal {
    public function eats($food) {
        echo "Yum, $food!";
    }
}

$animal = new Animal();
$animal->eats('meat');

```

PHP knows what `Animal` is before executing `new Animal`, because PHP reads source files top-to-bottom. But what if we wanted to create new Animals in many places, not just in the source file where it's defined? To do that, we need to **load** the class definition.



## Manual class loading with require


```php
// Animal.php
class Animal {
    public function eats($food) {
         echo "Yum, $food!";
    }
}

// zoo.php
require 'Animal.php';
$animal = new Animal;
$animal->eats('slop');

// aquarium.php
require 'Animal.php';
$animal = new Animal;
$animal->eats('shrimp');

```

Here we have three files. One file ("Animal.php") defines the class. This file has no side effects besides defining the class and neatly keeps all the knowledge about an "Animal" in one place. It's easily version controlled. It's easily reused.

Two files consume the "Animal.php" file by manually `require`-ing the file. Again, PHP reads source files top-to-bottom, so the require goes and finds the "Animal.php" file and makes the `Animal` class definition available before calling `new Animal`.

Now imagine we had dozens or hundreds of cases where we wanted to perform `new Animal`. That would require (pun-intended) many, many `require` statements that are very tedious to code.



## Autoloading replaces manual class definition loading


```php
// autoload.php
spl_autoload_register(function ($class) {
    require_once "$class.php";
});

// Animal.php
class Animal {
    public function eats($food) {
         echo "Yum, $food!";
    }
}

// zoo.php
require 'autoload.php';
$animal = new Animal;
$animal->eats('slop');

// aquarium.php
require 'autoload.php';
$animal = new Animal;
$animal->eats('shrimp');

```

Compare this to the other examples. Notice how `require "Animal.php"` was replaced with `require "autoload.php"`. We're still including an external file at run-time, but rather than including a **specific** class definition we're including logic that can include **any** class. It's a level of indirection that eases our development. Instead of writing one `require` for every class we need, we write one `require` for all classes. We can replace N `require` with 1 `require`.

The magic happens with [spl_autoload_register](http://php.net/manual/en/function.spl-autoload-register.php). This PHP function takes a closure and adds the closure to a **queue** of closures. When PHP encounters a class for which it has no definition, PHP hands the class name to each closure in the queue. If the class exists after calling a closure, PHP returns to its previous business. If the class fails to exist after trying the entire queue, PHP crashes with "Class 'Whatever' not found."



## Autoloading as part of a framework solution


```php
// autoload.php
spl_autoload_register(function ($class) {
    require_once "$class.php";
});

// Animal.php
class Animal {
    public function eats($food) {
         echo "Yum, $food!";
    }
}

// Ruminant.php
class Ruminant extends Animal {
    public function eats($food) {
        if ('grass' === $food) {
            parent::eats($food);
        } else {
            echo "Yuck, $food!";
        }
    }
}

// Cow.php
class Cow extends Ruminant {
}

// pasture.php
require 'autoload.php';
$animal = new Cow;
$animal->eats('grass');

```

Thanks to our generic autoloader, we have access to any class that follows our autoloader naming convention. In this example, our convention is simple: the desired class must have a file in the same directory named for the class and ending in ".php". Notice that the class name exactly matches the file name.

Without autoloading, we would have to manually `require` base classes. If we built an entire zoo of animals, we'd have thousands of require statements that could more easily be replaced with a single autoloader.

In the final analysis, PHP autoloading is a mechanism to help you write less mechanical code so you can focus on solving business problems. All you have to do is **define a strategy that maps class name to file name**. You can roll your own autoloading strategy, as done here. Or, you can use any of the standard ones the PHP community has adopted: [PSR-0](http://www.php-fig.org/psr/psr-0/) or [PSR-4](http://www.php-fig.org/psr/psr-4/). Or, you can use [composer](http://www.getcomposer.org/) to generically define and manage these dependencies.



## Autoloading with Composer


Composer generates a `vendor/autoload.php` file.

You might simply include this file and you will get autoloading for free.

`require __DIR__ . '/vendor/autoload.php';`

This makes working with third-party dependencies very easy.

You can also add your own code to the Autoloader by adding an autoload section to your `composer.json`.

```php
{
    "autoload": {
        "psr-4": {"YourApplicationNamespace\\": "src/"}
    }
}

```

In this section you define the autoload mappings. In this example its a [PSR-4](http://www.php-fig.org/psr/psr-4/) mapping of a namespace to a directory: the `/src` directory resides in your projects root folder, on the same level as the `/vendor` directory is. An example filename would be `src/Foo.php` containing an `YourApplicationNamespace\Foo` class.

**Important:** After adding new entries to the autoload section, you have to re-run the command [`dump-autoload`](https://getcomposer.org/doc/03-cli.md#dump-autoload) to re-generate and update the `vendor/autoload.php` file with the new information.

In addition to `PSR-4` autoloading, Composer also supports `PSR-0`, `classmap` and `files` autoloading. See the [autoload reference](https://getcomposer.org/doc/04-schema.md#autoload) for more information.

When you including the `/vendor/autoload.php` file it will return an instance of the Composer Autoloader. You might store the return value of the include call in a variable and add more namespaces. This can be useful for autoloading classes in a test suite, for example.

```php
$loader = require __DIR__ . '/vendor/autoload.php';
$loader->add('Application\\Test\\', __DIR__);

```



#### Syntax


- require
- spl_autoload_require



#### Remarks


Autoloading, as part of a framework strategy, eases the amount of boilerplate code you have to write.

