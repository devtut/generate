---
metaTitle: "Cache"
description: "Caching using memcache, Cache Using APC Cache"
---

# Cache



## Caching using memcache


Memcache is a distributed object caching system and uses `key-value` for storing small data. Before you start calling `Memcache` code into PHP, you need to make sure that it is installed. That can be done using `class_exists` method in php. Once it is validated that the module is installed, you start with connecting to memcache server instance.

```
if (class_exists('Memcache')) {
  $cache = new Memcache();
  $cache->connect('localhost',11211);
}else {
  print "Not connected to cache server";
}

```

This will validate that Memcache php-drivers are installed and connect to memcache server instance running on localhost.

> 
Memcache runs as a daemon and is called **memcached**


In the example above we only connected to a single instance, but you can also connect to multiple servers using

```
if (class_exists('Memcache')) {
  $cache = new Memcache();
  $cache->addServer('192.168.0.100',11211);
  $cache->addServer('192.168.0.101',11211);
}

```

Note that in this case unlike connect , there wont be any active connection until you try to store or fetch a value.

In caching there are three important operations that needs to be implemented

1. **Store data :** Add new data to memcached server
1. **Get data :** Fetch data from memcached server
1. **Delete data :** Delete already existing data from memcached server

### Store data

`$cache` or memcached class object has a `set` method that takes in a key,value and time to save the value for (ttl).

```
$cache->set($key, $value, 0, $ttl);

```

Here $ttl or time to live is time in seconds that you want memcache to store the pair on server.

### Get data

`$cache` or memcached class object has a `get` method that takes in a key and returns the corresponding value.

```
$value = $cache->get($key);

```

> 
In case there is no value set for the key it will return **null**


### Delete data

Sometimes you might have the need to delete some cache value.`$cache` or memcache instance has a `delete` method that can be used for the same.

```
$cache->delete($key);

```

### Small scenario for caching

Let us assume a simple blog. It will be having multiple posts on landing page that get fetched from database with each page load. In order to reduce the sql queries we can use memcached to cache the posts. Here is a very small implementation

```
if (class_exists('Memcache')) {
  $cache = new Memcache();
  $cache->connect('localhost',11211);
    if(($data = $cache->get('posts')) != null) {
      // Cache hit
      // Render from cache
    } else {
      // Cache miss
      // Query database and save results to database
      // Assuming $posts is array of posts retrieved from database
      $cache->set('posts', $posts,0,$ttl);
    }
}else {
  die("Error while connecting to cache server");
}

```



## Cache Using APC Cache


The Alternative PHP Cache (APC) is a free and open opcode cache for PHP. Its goal is to provide a free, open, and robust framework for caching and optimizing PHP intermediate code.

[installation](http://php.net/manual/en/apc.installation.php)

```
sudo apt-get install php-apc
sudo /etc/init.d/apache2 restart

```

Add Cache:

```
apc_add ($key, $value , $ttl);
$key = unique cache key
$value = cache value
$ttl = Time To Live;

```

Delete Cache:

```
apc_delete($key);

```

Set  Cache Example:

```
if (apc_exists($key)) {
    echo "Key exists: ";
    echo apc_fetch($key);
} else {
    echo "Key does not exist";
    apc_add ($key, $value , $ttl);
}

```

[Performance](http://stackoverflow.com/questions/1794342/memcache-vs-apc-for-a-single-server-site-data-caching):

APC is nearly [5 times](https://www.percona.com/blog/2006/09/27/apc-or-memcached/) faster than Memcached.



#### Remarks


### Installation

You can install memcache using pecl

```
pecl install memcache

```

