---
metaTitle: "PHP - APCu"
description: "Iterating over Entries, Simple storage and retrieval, Store information"
---

# APCu


APCu is a shared memory key-value store for PHP. The memory is shared between
PHP-FPM processes of the same pool. Stored data persists between requests.



## Iterating over Entries


The [`APCUIterator`](http://php.net/manual/en/class.apcuiterator.php) allows to
iterate over entries in the cache:

```php
foreach (new APCUIterator() as $entry) {
    print_r($entry);
}

```

The iterator can be initialized with an optional regular expression to select only  entries with matching keys:

```php
foreach (new APCUIterator($regex) as $entry) {
    print_r($entry);
}

```

Information about a single cache entry can be obtained via:

```php
$key = '…';
$regex = '(^' . preg_quote($key) . '$)';
print_r((new APCUIterator($regex))->current());

```



## Simple storage and retrieval


[`apcu_store`](http://php.net/manual/de/function.apcu-store.php) can be used to store,
[`apcu_fetch`](http://php.net/manual/de/function.apcu-fetch.php) to retrieve values:

```php
$key = 'Hello';
$value = 'World';
apcu_store($key, $value);
print(apcu_fetch('Hello')); // 'World'

```



## Store information


[`apcu_cache_info`](http://php.net/manual/en/function.apcu-cache-info.php) provides
information about the store and its entries:

```php
print_r(apcu_cache_info());

```

> 
<p>Note that invoking `apcu_cache_info()` without limit will return the complete
data currently stored.<br />
To only get the meta data, use `apcu_cache_info(true)`.<br />
To get information about certain cache entries better use `APCUIterator`.</p>


