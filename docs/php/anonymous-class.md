---
metaTitle: "Anonymous class"
description: "Simple in-place data wrapper"
---

# Anonymous class


[Anonymous classes](http://php.net/manual/en/language.oop5.anonymous.php) are useful when simple, one-off objects need to be created. They can be used in place of a full class definition.

They can everything a normal class can: pass arguments through to their constructors, extend other classes, implement interfaces, use traits.

Anonymous classes are assigned a name by the engine, This name has to be regarded an implementation detail, which should not be relied upon.



## Simple in-place data wrapper


```php
interface IArrayWrapper {
    public function getProperties(): array;
    public function has(string $name): bool;
    public function __toString();
    // ... 
};

/**
 * Lightweight in-place data wrapper.
 * Demonstrates usage of anonymous class in conjunction with interface.
 * 
 * Provides some basic functionality for managing array data in OO style.
 * Can be used as a wrapper for API request/response data etc.
 * Converts data to JSON with simple `(string)` cast.
 */
new class($data) implements IArrayWrapper
{
    /** @var array */
    private $data;

    public function __construct(array $data)
    {
        $this->data = $data;
    }
    
    public function getProperties(): array
    {
        return is_array($this->data) ? array_keys($this->data) : [] ;
    }

    public function has(string $name): bool
    {
        return (bool)($this->data[$name] ?? false);
    }

    public function get(string $name)
    {
        return $this->data[$name] ?? null;
    }
    
    public function __isset($name)
    {
        return $this->has($name);
    }
    
    public function __get($name)
    {
        return $this->get($name);
    }
    
    public function __toString()
    {
        return json_encode($this->data);
    }
};

```

### Usage

Assume our `$data` as follows and class is stored in `$cls` variable:

```php
$data = ['a' => 'b', 'c' => 'd', 'e' => 5];

```

```php
$cls->a; // b
$cls->b; // null
$cls->e; // 5
isset($cls->a); // true
isset($cls->b); // false
$cls->has('a'); // true
$cls->has('b'); // false
$cls->getProperties(); // Array([0] => a [1] => c [2] => e)
(string)$cls; // {"a":"b","c":"d","e":5}
$cls instanceof IArrayWrapper; // true

```

