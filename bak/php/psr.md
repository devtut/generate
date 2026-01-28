---
metaTitle: "PHP - PSR"
description: "PSR-4: Autoloader, PSR-1: Basic Coding Standard, PSR-8: Huggable Interface"
---

# PSR


The [PSR](http://www.php-fig.org/psr/) (PHP Standards Recommendation) is a series of recommendations put together by the [FIG](http://www.php-fig.org/) (Framework Interop Group).

"The idea behind the group is for project representatives to talk about the commonalities between our projects and find ways we can work together" - [FIG FAQ](http://www.php-fig.org/faqs/)

PSRs can be in the following states: Accepted, Review, Draft or Deprecated.



## PSR-4: Autoloader


[PSR-4](http://www.php-fig.org/psr/psr-4/) is an **accepted recommendation** that outlines the standard for autoloading classes via filenames. This recommendation is recommended as the alternative to the earlier (and now deprecated) [PSR-0](http://www.php-fig.org/psr/psr-0/).

The fully qualified class name should match the following requirement:

```

\<NamespaceName>(\<SubNamespaceNames>)*\<ClassName>

```


- It MUST contain a top level vendor namespace (E.g.: `Alphabet`)
- It MAY contain one or more sub-namespaces (E.g.: `Google\AdWord`)
- It MUST contain an ending class name (E.g.: `KeywordPlanner`)

Thus the final class name would be `Alphabet\Google\AdWord\KeywordPlanner`. The fully qualified class name should also translate into a meaningful file path therefore `Alphabet\Google\AdWord\KeywordPlanner` would be located in `[path_to_source]/Alphabet/Google/AdWord/KeywordPlanner.php`

Starting with PHP 5.3.0, a [custom autoloader function](http://php.net/manual/en/function.spl-autoload-register.php) can be defined to load files based on the path and filename pattern that you define.

```php
# Edit your php to include something like:
spl_autoload_register(function ($class) { include 'classes/' . $class . '.class.php';});

```

Replacing the location ('classes/') and filename extension ('.class.php') with values that apply to your structure.

[Composer](https://stackoverflow.com/documentation/php/1053/composer-dependency-manager) package manager [supports PSR-4](https://getcomposer.org/doc/01-basic-usage.md#autoloading) which means, if you follow the standard, you can load your classes in your project automatically using Composer's vendor autoloader.

```php
# Edit the composer.json file to include
{
    "autoload": {
        "psr-4": {
            "Alphabet\\": "[path_to_source]"
        }
    }
}

```

Regenerate the autoloader file

```php
$ composer dump-autoload

```

Now in your code you can do the following:

```php
<?php

require __DIR__ . '/vendor/autoload.php';
$KeywordPlanner = new Alphabet\Google\AdWord\KeywordPlanner();

```



## PSR-1: Basic Coding Standard


[PSR-1](http://www.php-fig.org/psr/psr-1/) is an **accepted recommendation** and outlines a basic standard recommendation for how code should be written.

- It outlines naming convetions for classes, methods and constants.
- It makes adopting PSR-0 or PSR-4 recommendations a requirement.
- It indicates which PHP tags to use: `<?php` and `<?=` but not `<?`.
- It specifies what file encoding to use (UTF8).
- It also states that files should either declare new symbols (classes, functions, constants, etc.) and cause no other side effects, or execute logic with side effects and not define symbols, but do both.



## PSR-8: Huggable Interface


[PSR-8](https://github.com/php-fig/fig-standards/tree/master/proposed/psr-8-hug) is a spoof PSR (**currently in Draft**) [proposed by Larry Garfield](https://groups.google.com/d/msg/php-fig/pcCMC6Kpq74/fEhWihgz_zMJ) as an April Fools joke on 1 April 2014.

The draft outlines how to define an interface to make an object `Huggable`.

Excert from the code outline:

```php
<?php

namespace Psr\Hug;

/**
 * Defines a huggable object.
 *
 * A huggable object expresses mutual affection with another huggable object.
 */
interface Huggable
{

    /**
     * Hugs this object.
     *
     * All hugs are mutual. An object that is hugged MUST in turn hug the other
     * object back by calling hug() on the first parameter. All objects MUST
     * implement a mechanism to prevent an infinite loop of hugging.
     *
     * @param Huggable $h
     *   The object that is hugging this object.
     */
    public function hug(Huggable $h);
}

```

