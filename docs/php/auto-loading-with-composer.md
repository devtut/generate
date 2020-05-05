---
metaTitle: "PHP - Auto loading with composer"
description: "Autoloading"
---

# Auto loading with composer



## Autoloading


For libraries that specify autoload information, Composer generates a vendor/autoload.php file. You can simply include this file and you will get autoloading for free.

```php
require __DIR__ . '/vendor/autoload.php';

```

This makes it really easy to use third party code. For example: If your project depends on Monolog, you can just start using classes from it, and they will be autoloaded.

```php
$log = new Monolog\Logger('name');
$log->pushHandler(new Monolog\Handler\StreamHandler('app.log', Monolog\Logger::WARNING));
$log->addWarning('Foo');

```

You can even add your own code to the autoloader by adding an `autoload` field to `composer.json`

```php
{
    "autoload": {
        "psr-4": {"Acme\\": "src/"}
    }
}

```

Composer will register a PSR-4 autoloader for the Acme namespace.

You define a mapping from namespaces to directories. The `src` directory would be in your project root, on the same level as vendor directory is. An example filename would be `src/Foo.php` containing an `Acme\Foo` class.

After adding the autoload field, you have to re-run `dump-autoload` to re-generate the `vendor/autoload.php` file.

Including that file will also return the autoloader instance, so you can store the return value of the include call in a variable and add more namespaces. This can be useful for autoloading classes in a test suite, for example.

```php
$loader = require __DIR__ . '/vendor/autoload.php';
$loader->add('Acme\\Test\\', __DIR__);

```

In addition to PSR-4 autoloading, Composer also supports PSR-0, classmap and files autoloading.

